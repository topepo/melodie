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
	mtr_info <- tibble::as_tibble(metrics)

	control <- check_parallel_over(control, resamples)

	resamples <- vec_list_rowwise(resamples)

	# if (!catalog_is_active()) {
	#   initialize_catalog(control = control)
	# }

	# TODO Do we need to evaluate the call to foreach in a local environment using a
	# clone of the current environment as the current version does

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
		control = control
	)

	tm_pkgs <- c("rsample", "workflows", "hardhat", "tune", "parsnip", "tailor")
	load_pkgs <- c(required_pkgs(workflow), control$pkgs, tm_pkgs)
	load_pkgs <- unique(load_pkgs)

	par_opt <- list(
		future.label = "tune-grid-%d",
		future.stdout = TRUE, # maybe NA
		future.seed = TRUE,
		# future.globals = c(), # add options from control?
		future.packages = quote(load_pkgs)
	)

	# ------------------------------------------------------------------------------
	# Control execution

	if (control$parallel_over == "resamples") {
		# The default: Loop over splits, process whole grid

		cl <- loop_call(control, par_opt)
		res <- rlang::eval_bare(cl)
	} else {
		# If multiple resamples but preprocessing is cheap (or just a validation set).
		# Loop over grid rows and splits
		candidates <- get_row_wise_grid(workflow, grid)
		# Break all combinations of resamples and candidates into a list of integers
		# for each combination.
		inds <- tidyr::crossing(s = seq_along(candidates), b = seq_along(resamples))
		inds <- vec_list_rowwise(inds)

		cl <- loop_call(control, par_opt)
		res <- rlang::eval_bare(cl)
	}

	# ------------------------------------------------------------------------------
	# Separate results into different components

	res <- dplyr::bind_rows(res)
	resamples <- dplyr::bind_rows(resamples)
	id_cols <- grep("^id", names(resamples), value = TRUE)

	if (control$parallel_over == "resamples") {
		res <- dplyr::full_join(resamples, res, by = id_cols)
	} else {
		# TODO Re-group the results so each row is a resample
		res <- dplyr::full_join(resamples, res, by = id_cols)
	}

	res
}

vec_list_rowwise <- function(x) {
	vctrs::vec_split(x, by = 1:nrow(x))$val
}

check_parallel_over <- function(control, resamples) {
	if (is.null(control$parallel_over)) {
		control$parallel_over <- "resamples"
	}
	if (length(resamples$splits) == 1) {
		control$parallel_over <- "everything"
	}
	control
}

loop_call <- function(ctrl, opts) {
	if (ctrl$allow_par) {
		if (ctrl$parallel_over == "resamples") {
			cl <- rlang::call2(
				"future_lapply",
				.ns = "future.apply",
				X = quote(resamples),
				FUN = "loopy",
				quote(grid),
				quote(static),
				!!!opts
			)
		} else {
			cl <- rlang::call2(
				"future_lapply",
				.ns = "future.apply",
				X = quote(inds),
				FUN = "loopy2",
				quote(resamples),
				quote(candidates),
				quote(static),
				!!!opts
			)
		}
	} else {
		if (ctrl$parallel_over == "resamples") {
			cl <- rlang::call2(
				"lapply",
				X = quote(resamples),
				FUN = "loopy",
				quote(grid),
				quote(static)
			)
		} else {
			cl <- rlang::call2(
				"lapply",
				X = quote(inds),
				FUN = "loopy2",
				quote(resamples),
				quote(candidates),
				quote(static)
			)
		}
	}
	cl
}
