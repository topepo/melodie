tune_grid_loop_new <- function(
	resamples,
	grid,
	workflow,
	param_info,
	metrics,
	eval_time,
	control,
	split_args
) {
	if (is.null(param_info)) {
		param_info <- hardhat::extract_parameter_set_dials(workflow)
	}

	mtr_info <- tibble::as_tibble(metrics)

	# ------------------------------------------------------------------------------
	# Collect "static" data into a single object for a cleaner interface

	static <- list(
		wflow = workflow,
		param_info = param_info,

		metrics = metrics,
		metric_info = mtr_info,
		y_name = outcome_names(workflow),
		pred_types = unique(metrics_info(metrics)$type),
		eval_time = eval_time,

		split_args = split_args,
		control = control,

		future = list(
			future.label = "tune-grid-%d",
			future.stdout = TRUE, # maybe NA
			future.seed = TRUE,
			future.globals = c(), # add options from control?
			future.packages = unique(c(required_pkgs(workflow), control$pkgs)),
			doFuture.rng.onMisuse = "ignore"
		)
	)

	# ------------------------------------------------------------------------------
	# Control execution

	if (control$parallel_over == "resamples") {
		# The default: Loop over splits, process whole grid
		res <- future.apply::future_lapply(resamples, ~loopy(.x, grid, static))
	} else {
		# Multiple resamples but preprocessing is cheap (or just a validation set).
		# Loop over grid rows and splits
		candidates <- get_row_wise_grid(workflow, grid)
		inds <- tidyr::crossing(s = seq_along(candidates), b = seq_along(resamples))
		inds <- vctrs::vec_split(inds, by = 1:nrow(inds))
		res <- future.apply::future_lapply(
			inds,
			~loopy(resamples[[.x$b]], candidates[[.x$s]], static)
		)
	}

	# ------------------------------------------------------------------------------
	# Aggregate results into components
}
