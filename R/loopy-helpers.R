# Helpers for loopy()

# ------------------------------------------------------------------------------
# For stages

remove_stage <- function(x) {
	stages <- c("model_stage", "predict_stage", "post_stage")
	x[, !(names(x) %in% stages)]
}

has_pre_param <- function(x) {
	any(names(x) != "model_stage")
}

has_mod_param <- function(x) {
	any(names(x) != "predict_stage")
}

# ------------------------------------------------------------------------------

has_sub_param <- function(x) {
	not_post_list <- names(x) != "post_stage"
	has_param_col <- any(not_post_list)
	if (!has_param_col) {
		return(FALSE)
	}
	param_col_nm <- names(x)[not_post_list]
	param_col <- x[[param_col_nm]]
	two_plus_vals <- length(param_col) > 1
	two_plus_vals
}

get_sub_param <- function(x) {
	not_post_list <- names(x) != "post_stage"
	names(x)[not_post_list]
}

# ------------------------------------------------------------------------------
# from workflows
has_tailor <- function(x) {
	"tailor" %in% names(x$post$actions)
}
#
has_tailor_tuned <- function(x) {
	if (!has_tailor(x)) {
		res <- FALSE
	} else {
		res <- any(tune_args(x)$source == "tailor")
	}
	res
}
has_tailor_estimated <- function(x) {
	if (!has_tailor(x)) {
		res <- FALSE
	} else {
		post <- hardhat::extract_postprocessor(x)
		res <- tailor::tailor_requires_fit(post)
	}
	res
}

# ------------------------------------------------------------------------------
# PRediction and postprocessing

# TODO add eval_time
sched_predict_wrapper <- function(sched, wflow_current, static) {
	outputs <- get_output_columns(wflow_current, syms = TRUE)

	if (has_sub_param(sched$predict_stage[[1]])) {
		sub_param <- get_sub_param(sched$predict_stage[[1]])
		sub_list <- sched$predict_stage[[1]] %>%
			dplyr::select(dplyr::all_of(sub_param)) %>%
			as.list()
	} else {
		sub_list <- NULL
	}

	processed_data_pred <- forge_from_workflow(static$data_perf, wflow_current)
	processed_data_pred$outcomes <- processed_data_pred$outcomes %>%
		dplyr::mutate(.row = static$ind_perf)

	pred <- NULL
	for (type_iter in static$pred_types) {
		tmp_res <- predict_wrapper(
			model = wflow_current %>% hardhat::extract_fit_parsnip(),
			new_data = processed_data_pred$predictors,
			type = type_iter,
			eval_time = static$eval_time,
			subgrid = sub_list
		)
		pred <- vctrs::vec_cbind(pred, tmp_res)
	}

	pred <- pred %>%
		dplyr::mutate(.row = static$ind_perf) %>%
		dplyr::full_join(processed_data_pred$outcomes, by = ".row") %>%
		dplyr::relocate(
			c(
				dplyr::all_of(static$y_name),
				dplyr::starts_with(".pred"),
				dplyr::any_of(".eval_time"),
				.row
			),
			.before = dplyr::everything()
		)

	pred
}

pred_post_strategy <- function(x) {
	if (has_tailor(x)) {
		if (has_tailor_tuned(x) | has_tailor_estimated(x)) {
			# TODO do we need to check estimation?
			# There is no way to get around having to estimate/fit the tailor object
			# for each postprocessing tuning combination
			res <- "loop over pred and post"
		} else {
			# For a set of predictions, or a set of submodel predictions, we can
			# just apply the tailor object (i.e. predict) to the set(s)
			res <- "predict and post at same time"
		}
	} else {
		# No postproessing, stop at prediction, submodels or not
		res <- "just predict"
	}
	res
}

predict_only <- function(wflow_current, sched, grid, static) {
	pred <- sched_predict_wrapper(sched, wflow_current, static)

	if (has_sub_param(sched$predict_stage[[1]])) {
		sub_param <- get_sub_param(sched$predict_stage[[1]])
		pred <- pred %>%
			tidyr::unnest(.pred) %>%
			vctrs::vec_cbind(grid %>% dplyr::select(-dplyr::all_of(sub_param)))
	} else {
		pred <- pred %>% vctrs::vec_cbind(grid)
	}
	pred
}

predict_post_one_shot <- function(wflow_current, sched, grid, static) {
	# ----------------------------------------------------------------------------
	# Get all predictions

	pred <- sched_predict_wrapper(sched, wflow_current, static)

	if (has_sub_param(sched$predict_stage[[1]])) {
		sub_param <- get_sub_param(sched$predict_stage[[1]])
		pred <- pred %>%
			tidyr::unnest(.pred) %>%
			vctrs::vec_cbind(grid %>% dplyr::select(-dplyr::all_of(sub_param)))
	} else {
		pred <- pred %>% vctrs::vec_cbind(grid)
	}

	# ----------------------------------------------------------------------------
	# 'fit' the tailor object to use to postprocess

	outputs <- get_output_columns(wflow_current, syms = TRUE)

	post_obj <- wflow_current %>%
		hardhat::extract_postprocessor() %>%
		fit(
			.data = pred[1, ],
			outcome = !!outputs$outcome[[1]],
			estimate = !!outputs$estimate[[1]],
			probabilities = c(!!!outputs$probabilities)
		)

	pred <- predict(post_obj, pred)

	pred
}

predict_post_loop <- function(wflow_current, sched, grid, static) {
	outputs <- get_output_columns(wflow_current, syms = TRUE)

	# ----------------------------------------------------------------------------
	# Generate all predictions then nest for each candidate

	tune_id <- rlang::syms(static$param_info$id)

	pred <- sched_predict_wrapper(sched, wflow_current, static)

	if (has_sub_param(sched$predict_stage[[1]])) {
		sub_param <- get_sub_param(sched$predict_stage[[1]])
		pred <- pred %>%
			tidyr::unnest(.pred) %>%
			vctrs::vec_cbind(grid %>% dplyr::select(-dplyr::all_of(sub_param)))
	} else {
		pred <- pred %>% vctrs::vec_cbind(grid)
	}

	pred <- pred %>% dplyr::group_nest(!!!tune_id, .key = "res")
	# pred$res is class "vctrs_list_of" and that will prevent us from pushing
	# updates values into the column, so we'll convert it into a basic list
	pred$res <- as.list(pred$res)

	num_pred_iter <- nrow(pred)

	# ----------------------------------------------------------------------------
	# Now, for each set of predictions, postprocess for each post-candidate

	post_obj <- hardhat::extract_postprocessor(wflow_current)

	for (prd in seq_len(num_pred_iter)) {
		current_pred <- sched$predict_stage[[1]][prd, ]
		num_post_iter <- nrow(current_pred$post_stage[[1]])
		current_predictions <- pred$res[[prd]]

		new_pred <- NULL
		for (post in seq_len(num_post_iter)) {
			current_post <- current_pred$post_stage[[1]][post, ]
			current_post_obj <- finalize_tailor(post_obj, as.list(current_post))

			current_post_obj <- current_post_obj %>%
				fit(
					.data = pred$res[[prd]],
					outcome = !!outputs$outcome[[1]],
					estimate = !!outputs$estimate[[1]],
					probabilities = c(!!!outputs$probabilities)
				)

			# Need another version of nesting to insert the sequence of post tuning
			# parameters
			new_pred <- dplyr::bind_rows(
				new_pred,
				predict(current_post_obj, current_predictions) %>%
					vctrs::vec_cbind(current_post)
			)
		}
		pred$res[[prd]] <- new_pred
	}

	pred %>% tidyr::unnest(res)
}

predictions <- function(wflow_current, sched, static, grid) {
	strategy <- pred_post_strategy(wflow_current)

	if (strategy == "just predict") {
		pred <- predict_only(wflow_current, sched, grid, static)
	} else if (strategy == "predict and post at same time") {
		pred <- predict_post_one_shot(wflow_current, sched, grid, static)
	} else {
		pred <- predict_post_loop(wflow_current, sched, grid, static)
	}
	if (tibble::is_tibble(pred)) {
		pred <- dplyr::as_tibble(pred)
	}
	pred %>%
		dplyr::relocate(
			c(
				dplyr::all_of(static$y_name),
				dplyr::starts_with(".pred"),
				dplyr::any_of(".eval_time"),
				.row
			),
			.before = dplyr::everything()
		)
}

# ------------------------------------------------------------------------------
# Fitting/training functions

pre_update_fit <- function(wflow_current, grid, static) {
	pre_proc <- hardhat::extract_preprocessor(wflow_current)

	if (inherits(pre_proc, "recipe")) {
		grid <- remove_stage(grid)
		pre_proc_param <- hardhat::extract_parameter_set_dials(pre_proc)
		pre_proc_id <- pre_proc_param$id

		if (length(pre_proc_id) > 0) {
			grid <- grid[, pre_proc_id]
			pre_proc <- finalize_recipe(pre_proc, grid)
			wflow_current <- set_workflow_recipe(wflow_current, pre_proc)
		}
	}
	workflows::.fit_pre(wflow_current, static$data_fit)
}

model_update_fit <- function(wflow_current, grid) {
	mod_spec <- hardhat::extract_spec_parsnip(wflow_current)

	grid <- remove_stage(grid)
	pre_proc_param <- hardhat::extract_parameter_set_dials(mod_spec)
	pre_proc_id <- pre_proc_param$id

	if (length(pre_proc_id) > 0) {
		grid <- grid[, pre_proc_id]
		mod_spec <- finalize_model(mod_spec, grid)
		wflow_current <- set_workflow_spec(wflow_current, mod_spec)
	}

	# .catch_and_log_fit()
	.fit_model(wflow_current, workflows::control_workflow())
}

# not currently used
post_update_fit <- function(wflow_current, grid, post_data) {
	post_spec <- hardhat::extract_postprocessor(wflow_current)

	grid <- remove_stage(grid)
	post_proc_param <- hardhat::extract_parameter_set_dials(post_spec)
	post_proc_id <- post_proc_param$id

	if (length(post_proc_id) > 0) {
		grid <- grid[, post_proc_id]
		post_spec <- finalize_tailor(post_spec, grid)
		wflow_current <- set_workflow_tailor(wflow_current, post_spec)
	}

	wflow_current <- .fit_post(wflow_current, post_data) # static
	.fit_finalize(wflow_current)
}

# ------------------------------------------------------------------------------
# Misc functions

rebind_grid <- function(...) {
	list(...) %>% purrr::map(remove_stage) %>% purrr::list_cbind()
}

get_output_columns <- function(x, syms = FALSE) {
	# This needs a fitted model or workflow
	pred_cols <- parsnip::.get_prediction_column_names(x, syms = TRUE)
	res <- c(list(outcome = rlang::syms(outcome_names(x))), pred_cols)
	res
}

get_data_subsets <- function(wflow, split, split_args = NULL) {
	dat <- list(
		data_perf = rsample::assessment(split),
		ind_perf = as.integer(split, data = "assessment")
	)
	if (workflows::.workflow_includes_calibration(wflow)) {
		# if the workflow has a postprocessor that needs training (i.e. calibration),
		# further split the analysis data into an "inner" analysis and
		# assessment set.
		# * the preprocessor and model (excluding the post-processor) are fitted
		#   on `analysis(inner_split(split))`, the inner analysis set (just
		#   referred to as analysis)
		# * that model generates predictions on `assessment(inner_split(split))`,
		#   the calibration set
		# * the post-processor is trained on the predictions generated from the
		#   calibration set
		# * the model (including the post-processor) generates predictions on the
		#   assessment set and those predictions are assessed with performance metrics
		split <- rsample::inner_split(split, split_args = split_args)

		dat$data_cal <- vctrs::vec_slice(split$data, calibration_rows)
		dat$ind_cal <- as.integer(split, data = "assessment")
	}

	dat$data_fit <- rsample::analysis(split)
	dat$ind_fit <- as.integer(split, data = "analysis")
	dat
}

# ------------------------------------------------------------------------------
# pre-allocating predictions

initialize_pred_reserve <- function(predictions, grid_size) {
	if (tibble::is_tibble(predictions)) {
		predictions <- dplyr::as_tibble(predictions)
	}
	grid_size <- max(1, grid_size)
	ptype <- predictions[0, ]
	size <- nrow(predictions) * grid_size
	res <- ptype[1:size, ]
	dplyr::as_tibble(res)
}

replace_reserve_rows <- function(iter, chunk) {
	start_loc <- (iter - 1) * chunk + 1
	end_loc <- iter * chunk
	start_loc:end_loc
}

update_reserve <- function(reserve, iter, predictions, grid_size) {
	grid_size <- min(1, grid_size)
	pred_size <- nrow(predictions)

	if (is.null(reserve)) {
		reserve <- initialize_pred_reserve(predictions, grid_size)
	} else {
		if (tibble::is_tibble(predictions)) {
			predictions <- dplyr::as_tibble(predictions)
		}
	}
	reserve[replace_reserve_rows(iter, pred_size), ] <- predictions
	reserve
}
