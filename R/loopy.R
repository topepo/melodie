#' Iterate over workflow settings
#'
#' @param sched A preprocessing schedule.
#' @param grid A parameter grid.
#'
#' @export
#' @export
loopy <- function(resamples, grid, static) {
	# Initialize some objects

  split <- resamples$splits[[1]]
  split_labs <-
    resamples %>%
    dplyr::select(dplyr::starts_with("id"))

	pred_reserve <- metric_reserve <- NULL
	pred_iter <- 0
	# TODO add extras and notes, maybe may an unber object like `static`

	sched <- get_tune_schedule(static$wflow, static$param_info, grid)

	# append data partitions here; these are the same for the duration of this function
	static <- c(static, get_data_subsets(static$wflow, split, static$split_args))

	# ----------------------------------------------------------------------------
	# Iterate over preprocessors

	num_pre_iter <- nrow(sched)

	for (pre in seq_len(num_pre_iter)) {
		current_pre <- sched[pre, ]
		current_wflow <- pre_update_fit(static$wflow, current_pre, static)
		num_mod_iter <- nrow(current_pre$model_stage[[1]])

		# --------------------------------------------------------------------------
		# Iterate over model parameters

		for (mod in seq_len(num_mod_iter)) {
			current_model <- current_pre$model_stage[[1]][mod, ]
			current_wflow <- model_update_fit(current_wflow, current_model)

			num_pred_iter <- nrow(current_model$predict_stage[[1]])
			current_grid <- rebind_grid(current_pre, current_model)

			# ------------------------------------------------------------------------
			# Iterate over predictions and postprocessors

			pred <- predictions(
				wflow_current = current_wflow,
				sched = current_model,
				static = static,
				grid = current_grid
			)

			# ------------------------------------------------------------------------
			# Allocate predictions to an overall object

			pred_iter <- pred_iter + 1
			pred_reserve <- update_reserve(pred_reserve, pred_iter, pred, nrow(grid))
		} # model loop
	} # pre loop

	pred_reserve <- vctrs::vec_cbind(pred_reserve, split_labs)

	all_metrics <- pred_reserve %>%
		dplyr::group_by(!!!rlang::syms(static$param_info$id)) %>%
		tune:::.estimate_metrics(
			metric = static$metrics,
			param_names = static$param_info$id,
			outcome_name = static$y_name,
			event_level = static$control$event_level,
			metrics_info = static$metrics_info
		)

	list(metrics = all_metrics, predictions = pred_reserve)
}

# ------------------------------------------------------------------------------

# This will take a grid and make a list of subgrids that should be used when
# we parallel process over grid candidates. The function will make 1-row grids
# except when there is a submodel parameter. In that case, it will create a
# subgrid that has fixed values for non-submodel parameters and the associated
# values of the submodel.
get_row_wise_grid <- function(wflow, grid) {
	param_tuned <- tune_args(wflow)$id
	submodel <- wflow %>%
		hardhat::extract_spec_parsnip() %>%
		tune:::get_submodel_info() %>%
		dplyr::filter(has_submodel) %>%
		purrr::pluck("id")

	const_param <- setdiff(param_tuned, submodel)
	const_param <- rlang::syms(const_param)

	if (length(submodel) == 0) {
		inds <- 1:nrow(grids)
	} else {
		grid_inds <- grid %>%
			parsnip::add_rowindex() %>%
			dplyr::group_nest(!!!const_param) %>%
			dplyr::mutate(inds = dplyr::row_number()) %>%
			tidyr::unnest(c(data)) %>%
			dplyr::select(-.row)
		grid <- grid_inds[, param_tuned]
		inds <- grid_inds$inds
	}
	vctrs::vec_split(grid, inds)$val
}
