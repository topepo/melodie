test_that("post with no estimation, with tuning, no submodels, classification", {
	skip_if_not_installed("modeldata")
	skip_if_not_installed("C50")

	cls <- make_post_data()

	dt_cut_grid <- tidyr::crossing(dt_grid, thresh_grid)

	wflow <- workflow(class ~ ., dt_spec, cls_post)

	wflow_fit <- wflow %>%
		finalize_workflow(dt_cut_grid[1, ]) %>%
		.fit_pre(data = analysis(cls$split)) %>%
		.fit_model(control = control_workflow())

	# ------------------------------------------------------------------------------
	# class and prob predictions

	static_1 <- melodie:::make_static(
		wflow,
		param_info = wflow %>% extract_parameter_set_dials(),
		metrics = metric_set(accuracy, brier_class),
		eval_time = NULL,
		split_args = cls$args,
		control = control_resamples()
	)

	data_1 <- melodie:::get_data_subsets(wflow, cls$split)
	static_1 <- c(static_1, data_1)
	static_1$y_name <- cls$y

	sched <- melodie:::schedule_grid(dt_cut_grid, static_1$wflow)

	res_1 <- melodie:::post_no_estimation_but_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = dt_cut_grid[1, ],
		static = static_1
	)
	plist_1 <- cls_sim_plist %>% mutate(min_n = double(0), cut = double(0))

	expect_equal(res_1[0, ], plist_1)
	expect_equal(
		nrow(res_1),
		nrow(assessment(cls$split)) * 2 # 2 values of cut for min_n = 2
	)
	expect_equal(
		res_1$.row,
		rep(as.integer(cls$split, data = "assessment"), 2)
	)
	expect_equal(unique(res_1$min_n), dt_cut_grid$min_n[1])
	expect_equal(unique(res_1$cut), dt_cut_grid$cut[dt_cut_grid$min_n == 2])

	expect_equal(
		res_1 %>%
			dplyr::filter(min_n == 2 & cut == 1 / 3 & .pred_class_1 >= 1 / 3) %>%
			summarize(events = mean(.pred_class == "class_1")) %>%
			purrr::pluck("events"),
		1
	)

	# ------------------------------------------------------------------------------
	# class predictions only

	acc_mtr <- metric_set(accuracy)
	static_2 <- static_1
	static_2$metrics <- acc_mtr
	static_2$metric_info <- tibble::as_tibble(acc_mtr)
	static_2$pred_types <- melodie:::determine_pred_types(static_2$wflow, acc_mtr)

	res_2 <- melodie:::post_no_estimation_but_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = dt_cut_grid[1, ],
		static = static_2
	)
	plist_2 <- cls_sim_plist %>% mutate(min_n = double(0), cut = double(0))

	expect_equal(res_2[0, ], plist_2)
	expect_equal(res_2[0, ], plist_2)
	expect_equal(
		nrow(res_2),
		nrow(assessment(cls$split)) * 2 # 2 values of cut for min_n = 2
	)
	expect_equal(
		res_2$.row,
		rep(as.integer(cls$split, data = "assessment"), 2)
	)
	expect_equal(unique(res_2$min_n), dt_cut_grid$min_n[1])
	expect_equal(unique(res_2$cut), dt_cut_grid$cut[dt_cut_grid$min_n == 2])

	expect_equal(
		res_2 %>%
			dplyr::filter(min_n == 2 & cut == 1 / 3 & .pred_class_1 >= 1 / 3) %>%
			summarize(events = mean(.pred_class == "class_1")) %>%
			purrr::pluck("events"),
		1
	)

	# ------------------------------------------------------------------------------
	# class probabilities only

	brier_mtr <- metric_set(brier_class)
	static_3 <- static_1
	static_3$metrics <- brier_mtr
	static_3$metric_info <- tibble::as_tibble(brier_mtr)
	static_3$pred_types <- melodie:::determine_pred_types(
		static_3$wflow,
		brier_mtr
	)

	res_3 <- melodie:::post_no_estimation_but_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = dt_cut_grid[1, ],
		static = static_3
	)
	plist_3 <- cls_sim_plist %>%
		mutate(min_n = double(0), cut = double(0))

	expect_equal(res_3[0, ], plist_3)
	expect_equal(
		nrow(res_3),
		nrow(assessment(cls$split)) * 2 # 2 values of cut for min_n = 2
	)
	expect_equal(
		res_3$.row,
		rep(as.integer(cls$split, data = "assessment"), 2)
	)
	expect_equal(unique(res_3$min_n), dt_cut_grid$min_n[1])
	expect_equal(unique(res_3$cut), dt_cut_grid$cut[dt_cut_grid$min_n == 2])

	expect_equal(
		res_3 %>%
			dplyr::filter(min_n == 2 & cut == 1 / 3 & .pred_class_1 >= 1 / 3) %>%
			summarize(events = mean(.pred_class == "class_1")) %>%
			purrr::pluck("events"),
		1
	)
})

test_that("post with no estimation, with tuning, no submodels, regression", {
	skip_if_not_installed("modeldata")
	skip_if_not_installed("C50")

	reg <- make_post_data("regression")

	wflow <- workflow(outcome ~ ., svm_fixed_spec, reg_lower)

	svm_fixed_param <- wflow %>%
		extract_parameter_set_dials() %>%
		update(lo = lower_limit(c(10000, Inf)))

	wflow_fit <- wflow %>%
		.fit_pre(data = analysis(reg$split)) %>%
		.fit_model(control = control_workflow())

	# ------------------------------------------------------------------------------

	static_1 <- melodie:::make_static(
		wflow,
		param_info = svm_fixed_param,
		metrics = metric_set(rmse),
		eval_time = NULL,
		split_args = reg$args,
		control = control_resamples()
	)

	data_1 <- melodie:::get_data_subsets(wflow, reg$split)
	static_1 <- c(static_1, data_1)
	static_1$y_name <- reg$y

	sched <- melodie:::schedule_grid(lo_grid, static_1$wflow)

	res_1 <- melodie:::post_no_estimation_but_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = lo_grid[1, ],
		static = static_1
	)
	plist_1 <- reg_sim_plist %>% mutate(lo = integer(0))

	expect_equal(res_1[0, ], plist_1)
	expect_equal(
		nrow(res_1),
		nrow(assessment(reg$split)) * 2 # 2 values of lower_limit
	)
	expect_equal(
		res_1$.row,
		rep(as.integer(reg$split, data = "assessment"), 2)
	)

	expect_equal(unique(res_1$lo), lo_grid$lo)

	expect_equal(res_1 %>% filter(.pred > lo) %>% nrow(), 0L)
})

test_that("post with no estimation, with tuning and submodels, classification", {
	skip_if_not_installed("modeldata")
	skip_if_not_installed("C50")
})

test_that("post with no estimation, with tuning and submodels, regression", {
	skip_if_not_installed("modeldata")
	skip_if_not_installed("C50")

	reg <- make_post_data("regression")

	wflow <- workflow(outcome ~ ., svm_fixed_spec, reg_lower)
})
