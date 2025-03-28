test_that("pre_update_fit() with formulas", {
	skip_if_not_installed("modeldata")

	data("two_class_dat", package = "modeldata")
	two_class_rs <- mc_cv(two_class_dat, times = 2)
	rs_split <- two_class_rs$splits[[1]]

	# ----------------------------------------------------------------------------

	form_wflow <- workflow(Class ~ ., dt_spec)

	form_stc <- melodie:::make_static(
		form_wflow,
		param_info = form_wflow %>% extract_parameter_set_dials(),
		metrics = metric_set(brier_class, spec),
		eval_time = NULL,
		split_args = rsample::.get_split_args(two_class_rs),
		control = control_grid()
	)
	form_stc <- c(
		form_stc,
		melodie:::get_data_subsets(form_stc$wflow, rs_split, form_stc$split_args),
		y_name = "Class"
	)

	form_res <- melodie:::pre_update_fit(form_wflow, dt_grid, form_stc)
	expect_s3_class(form_res, "workflow")
	form_pre_res <- form_res %>% extract_mold()
	expect_named(form_pre_res, c("predictors", "outcomes", "blueprint", "extras"))
})

# ------------------------------------------------------------------------------

test_that("pre_update_fit() with recipes", {
	skip_if_not_installed("modeldata")

	data("two_class_dat", package = "modeldata")
	two_class_rs <- mc_cv(two_class_dat, times = 2)
	rs_split <- two_class_rs$splits[[1]]

	# ----------------------------------------------------------------------------

	rec <- recipe(Class ~ ., data = two_class_dat) %>%
		step_normalize(A, B)

	rec_wflow <- workflow(rec, dt_spec)

	rec_stc <- melodie:::make_static(
		rec_wflow,
		param_info = rec_wflow %>% extract_parameter_set_dials(),
		metrics = metric_set(brier_class, spec),
		eval_time = NULL,
		split_args = rsample::.get_split_args(two_class_rs),
		control = control_grid()
	)
	rec_stc <- c(
		rec_stc,
		melodie:::get_data_subsets(rec_stc$wflow, rs_split, rec_stc$split_args)
	)

	rec_res <- melodie:::pre_update_fit(rec_wflow, dt_grid, rec_stc)
	expect_s3_class(rec_res, "workflow")
	rec_pre_res <- rec_res %>% extract_mold()
	expect_named(rec_pre_res, c("predictors", "outcomes", "blueprint", "extras"))
	rec_pre_res <- rec_res %>% extract_recipe()
	expect_s3_class(rec_pre_res, "recipe")
})

test_that("pre_update_fit() with tuned recipes", {
	skip_if_not_installed("modeldata")

	data("two_class_dat", package = "modeldata")
	two_class_rs <- mc_cv(two_class_dat, times = 2)
	rs_split <- two_class_rs$splits[[1]]

	# ----------------------------------------------------------------------------

	rec <- recipe(Class ~ ., data = two_class_dat) %>%
		step_pca(A, B, num_comp = tune("comps"))

	rec_grid <- tibble(comps = 1)

	rec_wflow <- workflow(rec, dt_spec)

	rec_stc <- melodie:::make_static(
		rec_wflow,
		param_info = rec_wflow %>% extract_parameter_set_dials(),
		metrics = metric_set(brier_class, spec),
		eval_time = NULL,
		split_args = rsample::.get_split_args(two_class_rs),
		control = control_grid()
	)
	rec_stc <- c(
		rec_stc,
		melodie:::get_data_subsets(rec_stc$wflow, rs_split, rec_stc$split_args)
	)

	rec_res <- melodie:::pre_update_fit(rec_wflow, rec_grid, rec_stc)
	expect_s3_class(rec_res, "workflow")
	rec_pre_res <- rec_res %>% extract_mold()
	expect_named(rec_pre_res, c("predictors", "outcomes", "blueprint", "extras"))
	rec_pre_res <- rec_res %>% extract_recipe()
	expect_s3_class(rec_pre_res, "recipe")
	rec_pre_data <- bake(rec_pre_res, new_data = two_class_dat[1:2, ])
	expect_named(rec_pre_data, c("Class", "PC1"))
})

# ------------------------------------------------------------------------------

test_that("pre_update_fit() with selectors", {
	skip_if_not_installed("modeldata")

	data("two_class_dat", package = "modeldata")
	two_class_rs <- mc_cv(two_class_dat, times = 2)
	rs_split <- two_class_rs$splits[[1]]

	# ----------------------------------------------------------------------------

	vars_wflow <- workflow(spec = dt_spec) %>%
		add_variables(outcomes = c(Class), predictors = c(A, B))

	vars_stc <- melodie:::make_static(
		vars_wflow,
		param_info = vars_wflow %>% extract_parameter_set_dials(),
		metrics = metric_set(brier_class, spec),
		eval_time = NULL,
		split_args = rsample::.get_split_args(two_class_rs),
		control = control_grid()
	)
	vars_stc <- c(
		vars_stc,
		melodie:::get_data_subsets(vars_stc$wflow, rs_split, vars_stc$split_args)
	)

	vars_res <- melodie:::pre_update_fit(vars_wflow, dt_grid, vars_stc)
	expect_s3_class(vars_res, "workflow")
	vars_pre_res <- vars_res %>% extract_mold()
	expect_named(vars_pre_res, c("predictors", "outcomes", "blueprint", "extras"))
})

test_that("model_update_fit() for classification", {
	skip_if_not_installed("modeldata")

	data("two_class_dat", package = "modeldata")
	two_class_rs <- mc_cv(two_class_dat, times = 2)
	rs_split <- two_class_rs$splits[[1]]

	# ----------------------------------------------------------------------------

	dt_wflow <- workflow(Class ~ ., dt_spec)
	dt_grid <- tibble(min_n = 5)

	dt_stc <- melodie:::make_static(
		dt_wflow,
		param_info = dt_wflow %>% extract_parameter_set_dials(),
		metrics = metric_set(brier_class, spec),
		eval_time = NULL,
		split_args = rsample::.get_split_args(two_class_rs),
		control = control_grid()
	)
	dt_stc <- c(
		dt_stc,
		melodie:::get_data_subsets(dt_stc$wflow, rs_split, dt_stc$split_args)
	)

	dt_0_res <- melodie:::pre_update_fit(dt_wflow, dt_grid, dt_stc)
	dt_res <- melodie:::model_update_fit(dt_0_res, dt_grid)
	expect_s3_class(dt_res, "workflow")
	dt_res <- dt_res %>% extract_fit_parsnip()
	expect_s3_class(dt_res, c("_C5.0", "model_fit"))
})
