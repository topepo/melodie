#' Control aspects of the grid search process
#'
#' @export
control_grid <- function(
	verbose = FALSE,
	allow_par = TRUE,
	extract = NULL,
	save_pred = FALSE,
	pkgs = NULL,
	save_workflow = FALSE,
	event_level = "first",
	parallel_over = NULL,
	backend_options = NULL
) {
	# Any added arguments should also be added in superset control functions
	# in other packages

	# add options for seeds per resample
	check_bool(verbose)
	check_bool(allow_par)
	check_bool(save_pred)
	check_bool(save_workflow)
	check_string(event_level)
	check_character(pkgs, allow_null = TRUE)
	check_function(extract, allow_null = TRUE)

	val_parallel_over(parallel_over, "control_grid()")

	res <- list(
		verbose = verbose,
		allow_par = allow_par,
		extract = extract,
		save_pred = save_pred,
		pkgs = pkgs,
		save_workflow = save_workflow,
		event_level = event_level,
		parallel_over = parallel_over,
		backend_options = backend_options
	)

	class(res) <- c("control_grid", "control_resamples")
	res
}

#' @export
print.control_grid <- function(x, ...) {
	cat("grid/resamples control object\n")
	invisible(x)
}

# ------------------------------------------------------------------------------

val_parallel_over <- function(parallel_over, where) {
	check_string(parallel_over, allow_null = TRUE)
	if (!is.null(parallel_over)) {
		rlang::arg_match0(
			parallel_over,
			c("resamples", "everything"),
			"parallel_over"
		)
	}

	invisible(NULL)
}

#' @export
#' @keywords internal
#' @rdname control_grid
new_backend_options <- function(..., class = character()) {
	out <- rlang::list2(...)

	if (any(rlang::names2(out) == "")) {
		cli::cli_abort("All backend options must be named.")
	}

	structure(out, class = c(class, "tune_backend_options"))
}
