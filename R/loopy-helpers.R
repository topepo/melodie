# Helpers for loopy()

# ------------------------------------------------------------------------------

# Note: in loop(), we add more elements for the outcome name(s(), and the
# data partitions
make_static <- function(
	workflow,
	param_info,
	metrics,
	eval_time,
	split_args,
	control
) {
	list(
		wflow = workflow,
		param_info = param_info,
		post_estimation = workflows::.workflow_includes_calibration(workflow),
		metrics = metrics,
		metric_info = tibble::as_tibble(metrics),
		pred_types = determine_pred_types(workflow, metrics),
		eval_time = eval_time,
		split_args = split_args,
		control = control
	)
}

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

# This is run on a `predict_stage` column:
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

# This is run on a `predict_stage` column:
get_sub_param <- function(x) {
	not_post_list <- names(x) != "post_stage"
	names(x)[not_post_list]
}

# ------------------------------------------------------------------------------
# from workflows
# nocov start
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
# nocov end

# ------------------------------------------------------------------------------
# Prediction and postprocessing

# TODO add eval_time
# Basic prediction on a data set (holdout of calibration) including submodels.
# Note that if there are submodels, the results only have the grid values for
# the submodel parameter (not the whole grid).
sched_predict_wrapper <- function(
	sched,              # TODO rename this to be more informative with what stage
	wflow_current,
	static,
	estimation = FALSE  # TODO rename this to be more informative
) {
	outputs <- get_output_columns(wflow_current, syms = TRUE)

	has_submodel <- has_sub_param(sched$predict_stage[[1]])
	if (has_submodel) {
		sub_param <- get_sub_param(sched$predict_stage[[1]])
		sub_list <- sched$predict_stage[[1]] %>%
			dplyr::select(dplyr::all_of(sub_param)) %>%
			as.list()
	} else {
		sub_list <- NULL
		sub_param <- character(0)
	}

	if (estimation & static$post_estimation) {
		.data <- static$data_cal
		.ind <- static$ind_cal
	} else {
		.data <- static$data_perf
		.ind <- static$ind_perf
	}

	processed_data_pred <- forge_from_workflow(.data, wflow_current)
	processed_data_pred$outcomes <- processed_data_pred$outcomes %>%
		dplyr::mutate(.row = .ind)

	pred <- NULL
	for (type_iter in static$pred_types) {
		tmp_res <- predict_wrapper(
			model = wflow_current %>% hardhat::extract_fit_parsnip(),
			new_data = processed_data_pred$predictors,
			type = type_iter,
			eval_time = static$eval_time,
			subgrid = sub_list
		)
		tmp_res$.row <- .ind
		if (has_submodel) {
		  tmp_res <- tidyr::unnest(tmp_res, cols = c(.pred))
		}

		if (is.null(pred)) {
		  pred <- tmp_res
		} else {
		  pred <- dplyr::full_join(pred, tmp_res, by = c(sub_param, ".row"))
		}
	}

	pred <- pred %>%
		dplyr::full_join(processed_data_pred$outcomes, by = ".row")

	pred
}

pred_post_strategy <- function(wflow) {
	if (has_tailor(wflow)) {
		if (has_tailor_tuned(wflow)) {
			if (has_tailor_estimated(wflow)) {
				res <- "estimation_and_tuning"
			} else {
				res <- "no_estimation_but_tuning"
			}
		} else {
			# Not tuned

			if (has_tailor_estimated(wflow)) {
				res <- "estimation_but_no_tuning"
			} else {
				res <- "no_estimation_or_tuning"
			}
		}
	} else {
		# no tailor
		res <- "predict_only"
	}

	res
}

# Prediction when there there is no postprocessor. Submodels are processed too
predict_only <- function(
	wflow_current,
	sched,
	grid,
	static,
	estimation = FALSE
) {
	pred <- sched_predict_wrapper(sched, wflow_current, static, estimation)

	# When there are submodels, the grid is already in 'pred'. Otherwise merge
	# them into the predictions
	if (!has_sub_param(sched$predict_stage[[1]])) {
	  pred <- vctrs::vec_cbind(pred, grid)
	}

	reorder_pred_cols(pred, static$y_name)
}

predictions <- function(wflow_current, sched, static, grid) {
	strategy <- pred_post_strategy(wflow_current)

	if (strategy == "predict_only") {
		pred <- predict_only(wflow_current, sched, grid, static)
	} else if (strategy == "no_estimation_or_tuning") {
		pred <- post_no_estimation_or_tuning(wflow_current, sched, grid, static)
	} else if (strategy == "no_estimation_but_tuning") {
		pred <- post_no_estimation_but_tuning(wflow_current, sched, grid, static)
	} else if (strategy == "estimation_but_no_tuning") {
		pred <- post_estimation_but_no_tuning(wflow_current, sched, grid, static)
	} else {
		pred <- post_estimation_and_tuning(wflow_current, sched, grid, static)
	}

	# TODO return pred and fitted workflow(s) IF extraction

	if (!tibble::is_tibble(pred)) {
		# TODO probably not needed
		pred <- dplyr::as_tibble(pred)
	}
	pred
}

# Get the raw predictions for the calibration and assessment sets, train a single
# tailor, then apply it to the assessment data
post_estimation_but_no_tuning <- function(wflow_current, sched, grid, static) {
	cal_predictions <- predict_only(
		wflow_current,
		sched,
		grid,
		static,
		estimation = TRUE
	)
	perf_predictions <- predict_only(
		wflow_current,
		sched,
		grid,
		static,
		estimation = FALSE
	)

	outputs <- get_output_columns(wflow_current, syms = TRUE)

	post_obj <- wflow_current %>%
		hardhat::extract_postprocessor() %>%
		fit(
			.data = cal_predictions,
			outcome = !!outputs$outcome[[1]],
			estimate = !!outputs$estimate[[1]],
			probabilities = c(!!!outputs$probabilities)
		)

	res <- predict(post_obj, perf_predictions)
	reorder_pred_cols(res, static$y_name)
}

# Get the raw predictions for the calibration and assessment sets, looping over
# tuning parameters to train tailors, then apply them to the assessment data
post_estimation_and_tuning <- function(wflow_current, sched, grid, static) {
	post_obj <- hardhat::extract_postprocessor(wflow_current)
	post_id <- setdiff(static$param_info$id, names(grid))

	post_candidates <- sched$predict_stage[[1]] %>%
		tidyr::unnest(cols = c(post_stage)) %>%
		dplyr::select(dplyr::all_of(post_id)) %>%
		dplyr::distinct()

	post_predictions <- NULL
	for (j in 1:nrow(post_candidates)) {
		current_post_param <- post_candidates[j, ]

		# Finalize current tailor with parameters and stuff back in the workflow
		tmp_post <- finalize_tailor(post_obj, current_post_param)
		tmp_wflow <- set_workflow_tailor(wflow_current, tmp_post)
		tmp_pred <- post_estimation_but_no_tuning(
			tmp_wflow,
			sched,
			grid,
			static
		) %>%
			vctrs::vec_cbind(current_post_param)
		post_predictions <- dplyr::bind_rows(post_predictions, tmp_pred)
	}
	reorder_pred_cols(post_predictions, static$y_name)
}

# Get the predictions for the assessment set, "train" a single tailor, then
# apply it to all of the data
post_no_estimation_or_tuning <- function(wflow_current, sched, grid, static) {
	raw_predictions <- predict_only(wflow_current, sched, grid, static)

	outputs <- get_output_columns(wflow_current, syms = TRUE)

	post_obj <- wflow_current %>%
		hardhat::extract_postprocessor() %>%
		fit(
			.data = raw_predictions,
			outcome = !!outputs$outcome[[1]],
			estimate = !!outputs$estimate[[1]],
			probabilities = c(!!!outputs$probabilities)
		)

	pred <- predict(post_obj, raw_predictions)
	reorder_pred_cols(pred, static$y_name)
}

# Get the predictions for the assessment set, "train" a single tailor, then
# apply it to all of the data
post_no_estimation_but_tuning <- function(wflow_current, sched, grid, static) {
	post_obj <- hardhat::extract_postprocessor(wflow_current)

	# Get unique set of post-processing combinations to loop over
	post_candidates <- sched$predict_stage[[1]] %>%
		tidyr::unnest(cols = c(post_stage)) %>%
		dplyr::select(dplyr::any_of(static$param_info$id)) %>%
		dplyr::distinct()

	post_id <- names(post_candidates)

	# We need to attach the grid to the results so collect parameters
	# unrelated to postprocessing (there should be only 1 row in `grid`)

	non_post_grid <- dplyr::select(grid, -dplyr::all_of(!!!post_id))

	post_predictions <- NULL
	for (j in 1:nrow(post_candidates)) {
		current_post_param <- post_candidates[j, ]
		tmp_grid <- vctrs::vec_cbind(non_post_grid, current_post_param)

		# Finalize current tailor with parameters and stuff back in the workflow
		tmp_post <- finalize_tailor(post_obj, current_post_param)
		tmp_wflow <- set_workflow_tailor(wflow_current, tmp_post)
		tmp_pred <- post_no_estimation_or_tuning(tmp_wflow, sched, tmp_grid, static)
		post_predictions <- dplyr::bind_rows(post_predictions, tmp_pred)
	}
	reorder_pred_cols(post_predictions, static$y_name)
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
	mod_param <- hardhat::extract_parameter_set_dials(mod_spec)
	mod_id <- mod_param$id

	if (length(mod_id) > 0) {
		grid <- grid[, mod_id]
		mod_spec <- finalize_model(mod_spec, grid)
		wflow_current <- set_workflow_spec(wflow_current, mod_spec)
	}

	# .catch_and_log_fit()
	.fit_model(wflow_current, workflows::control_workflow())
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

		dat$ind_cal <- as.integer(split, data = "assessment")
		dat$data_cal <- vctrs::vec_slice(split$data, dat$ind_cal)
	}

	dat$data_fit <- rsample::analysis(split)
	dat$ind_fit <- as.integer(split, data = "analysis")
	dat
}

# ------------------------------------------------------------------------------
# pre-allocating predictions

initialize_pred_reserve <- function(predictions, grid_size) {
	if (!tibble::is_tibble(predictions)) {
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

# ------------------------------------------------------------------------------
# Add .config to grid

get_config_key <- function(grid, wflow) {
	info <- tune_args(wflow)
	key <- grid

	only_param <- setdiff(info$id, names(grid))
	if (length(only_param) > 0) {
	  cli::cli_abort(
	    "Some parameters are tagged for tuning but are not in the grid:
      {.arg {only_param}}", call = NULL)
	}

  only_grid <- setdiff(names(grid), info$id)
  if (length(only_grid) > 0) {
    cli::cli_abort(
      "Some parameters are in the grid but are not tagged for tuning:
      {.arg {only_grid}}", call = NULL)
  }

	pre_param <- info$id[info$source == "recipe"]
	if (length(pre_param) > 0) {
		key <- make_config_labs(grid, pre_param) %>%
			dplyr::full_join(key, by = pre_param)
	} else {
		key <- key %>%
			dplyr::mutate(pre = "pre0")
	}

	mod_param <- info$id[info$source == "model_spec"]
	if (length(mod_param) > 0) {
		key <- make_config_labs(grid, mod_param, "mod") %>%
			dplyr::full_join(key, by = mod_param)
	} else {
		key <- key %>%
			dplyr::mutate(mod = "mod0")
	}

	post_param <- info$id[info$source == "tailor"]
	if (length(post_param) > 0) {
		key <- make_config_labs(grid, post_param, "post") %>%
			dplyr::full_join(key, by = post_param)
	} else {
		key <- key %>%
			dplyr::mutate(post = "post0")
	}

	key$.config <- paste(key$pre, key$mod, key$post, sep = "_")
	key$.config <- gsub("_$", "", key$.config)
	key %>%
		dplyr::arrange(.config) %>%
		dplyr::select(dplyr::all_of(info$id), .config)
}

make_config_labs <- function(grid, param, val = "pre") {
	res <- grid %>%
		dplyr::select(dplyr::all_of(param)) %>%
		dplyr::distinct() %>%
		dplyr::arrange(!!!rlang::syms(param)) %>%
		dplyr::mutate(
			num = format(dplyr::row_number()),
			num = gsub(" ", "0", num),
			{{ val }} := paste0(val, num)
		) %>%
		dplyr::select(-num)

	res
}

determine_pred_types <- function(wflow, metrics) {
  model_mode <- extract_spec_parsnip(wflow)$mode

  pred_types <- unique(metrics_info(metrics)$type)
  if (melodie:::has_tailor(wflow)) {
    post <- extract_postprocessor(wflow)
    post_out <- purrr::map(post$adjustments, ~ .x$outputs)
    post_in <- purrr::map(post$adjustments, ~ .x$inputs)
    post_types <- unlist(c(post_out, post_in))
    post_types[grepl("probability", post_types)] <- "prob"
    post_cls <- purrr::map(post$adjustments, class)
    post_cls <- unlist(post_cls)
    if (any(post_cls == "probability_calibration")) {
      post_types <- c(post_types, "class", "prob")
    }
    post_cls <- unique(post_cls)
    pred_types <- unique(c(pred_types, post_types))
  }

  if (any(pred_types == "everything")) {
    if (model_mode == "regression") {
      pred_types <- c(pred_types, "numeric")
    } else if (model_mode == "classification") {
      pred_types <- c(pred_types, "class", "prob")
    } else if (model_mode == "censored regression") {
      pred_types <- c(pred_types, "static_survival_metric", "dynamic_survival_metric")
    } else {
      cli::cli_abort("No prediction types are known for mode {.val model_mode}.")
    }

    pred_types <- pred_types[pred_types != "everything"]
  }

  sort(unique(pred_types))
}

reorder_pred_cols <- function(x, y_name) {
  # x %>%
  #   dplyr::relocate(dplyr::any_of(".row"), .before = dplyr::everything()) %>%
  #   dplyr::relocate(dplyr::any_of(".eval_time"), .before = dplyr::everything()) %>%
  #   dplyr::relocate(dplyr::matches(".pred_[A-Za-z]"), .before = dplyr::everything()) %>%
  #   dplyr::relocate(dplyr::matches("^\\.pred_class$"), .before = dplyr::everything()) %>%
  #   dplyr::relocate(dplyr::matches("^\\.pred_time$"), .before = dplyr::everything()) %>%
  #   dplyr::relocate(dplyr::matches("^\\.pred$"), .before = dplyr::everything()) %>%
  #   dplyr::relocate(dplyr::all_of(y_name), .before = dplyr::everything())

  x %>%
    dplyr::relocate(
      dplyr::all_of(y_name),
      dplyr::matches("^\\.pred_time$"),
      dplyr::matches("^\\.pred$"),
      dplyr::matches("^\\.pred_class$"),
      dplyr::matches(".pred_[A-Za-z]"),
      dplyr::any_of(".eval_time"),
      dplyr::any_of(".row"),
      .before = dplyr::everything())
}
