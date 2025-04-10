test_that("post with estimation, no tuning or submodels, classification", {
	skip_if_not_installed("modeldata")
	skip_if_not_installed("C50")

	cls <- make_post_data()

	wflow <- workflow(class ~ ., dt_spec, cls_cal)

	wflow_fit <- wflow %>%
		finalize_workflow(dt_grid[1, ]) %>%
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
	static_1$y_name <- "class"

	sched <- melodie:::schedule_grid(dt_grid, static_1$wflow)

	uncal_fit <- workflow(class ~ ., dt_spec) %>%
		finalize_workflow(dt_grid[1, ]) %>%
		fit(data_1$data_fit)

	uncal_pred <- uncal_fit %>%
		predict(new_data = data_1$data_perf)

	uncal_prob <- uncal_fit %>%
		predict(new_data = data_1$data_perf, type = "prob")

	res_1 <- melodie:::post_estimation_but_no_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = dt_grid[1, ],
		static = static_1
	)
	plist_1 <- cls_sim_plist %>% mutate(min_n = double(0))
	expect_equal(res_1[0, ], plist_1)

	expect_equal(nrow(res_1), nrow(assessment(cls$split)))
	expect_equal(res_1$.row, as.integer(cls$split, data = "assessment"))
	expect_equal(data_1$ind_perf, as.integer(cls$split, data = "assessment"))
	expect_equal(unique(res_1$min_n), dt_grid$min_n[1])
	expect_false(isTRUE(all.equal(res_1$.pred_class_1, uncal_prob$.pred_class_1)))
	expect_false(isTRUE(all.equal(res_1$.pred_class, uncal_pred$.pred_class)))

	# ------------------------------------------------------------------------------
	# class predictions only

	acc_mtr <- metric_set(accuracy)
	static_2 <- static_1
	static_2$metrics <- acc_mtr
	static_2$metric_info <- tibble::as_tibble(acc_mtr)
	static_2$pred_types <- melodie:::determine_pred_types(static_2$wflow, acc_mtr)

	res_2 <- melodie:::post_estimation_but_no_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = dt_grid[1, ],
		static = static_2
	)
	plist_2 <- cls_sim_plist %>% mutate(min_n = double(0))
	expect_equal(res_2[0, ], plist_2)
	expect_equal(nrow(res_2), nrow(assessment(cls$split)))
	expect_equal(res_2$.row, as.integer(cls$split, data = "assessment"))
	expect_equal(unique(res_2$min_n), dt_grid$min_n[1])
	expect_false(isTRUE(all.equal(res_2$.pred_class, uncal_pred$.pred_class)))

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

	res_3 <- melodie:::post_estimation_but_no_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = dt_grid[1, ],
		static = static_3
	)
	plist_3 <- cls_sim_plist %>% mutate(min_n = double(0))

	expect_equal(res_3[0, ], plist_3)
	expect_equal(nrow(res_3), nrow(assessment(cls$split)))
	expect_equal(res_3$.row, as.integer(cls$split, data = "assessment"))
	expect_equal(unique(res_3$min_n), dt_grid$min_n[1])

	expect_equal(res_3$.pred_class_1, res_1$.pred_class_1)
})

test_that("post with estimation, no tuning or submodels, regression", {
	skip_if_not_installed("kernlab")
	skip_if_not_installed("modeldata")

	reg <- make_post_data("regression")

	wflow <- workflow(outcome ~ ., svm_spec, reg_cal)

	wflow_fit <- wflow %>%
		finalize_workflow(svm_grid[1, ]) %>%
		.fit_pre(data = analysis(reg$split)) %>%
		.fit_model(control = control_workflow())

	# ------------------------------------------------------------------------------

	static_1 <- melodie:::make_static(
		wflow,
		param_info = wflow %>% extract_parameter_set_dials(),
		metrics = metric_set(rmse),
		eval_time = NULL,
		split_args = reg$args,
		control = control_resamples()
	)

	data_1 <- melodie:::get_data_subsets(wflow, reg$split)
	static_1 <- c(static_1, data_1)
	static_1$y_name <- reg$y

	sched <- melodie:::schedule_grid(svm_grid, static_1$wflow)

	uncal_pred <- workflow(outcome ~ ., svm_spec) %>%
		finalize_workflow(svm_grid[1, ]) %>%
		fit(data_1$data_fit) %>%
		predict(new_data = data_1$data_perf)

	res_1 <- melodie:::post_estimation_but_no_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = svm_grid[1, ],
		static = static_1
	)
	plist_1 <- reg_sim_plist %>% mutate(degree = integer(0))

	expect_equal(res_1[0, ], plist_1)
	expect_equal(nrow(res_1), nrow(assessment(reg$split)))
	expect_equal(res_1$.row, as.integer(reg$split, data = "assessment"))
	expect_equal(data_1$ind_perf, as.integer(reg$split, data = "assessment"))
	expect_equal(unique(res_1$degree), svm_grid$degree[1])
	expect_false(isTRUE(all.equal(res_1$.pred, uncal_pred$.pred)))
})

test_that("post with estimation, no tuning or submodels, classification", {
	skip_if_not_installed("modeldata")
	skip_if_not_installed("C50")

	cls <- make_post_data()
})

test_that("post with estimation, no tuning or submodels, regression", {
	skip_if_not_installed("kernlab")
	skip_if_not_installed("modeldata")

	reg <- make_post_data("regression")

	wflow <- workflow(outcome ~ ., knn_reg_spec, reg_cal)

	wflow_fit <- wflow %>%
		finalize_workflow(knn_grid[1, ]) %>%
		.fit_pre(data = analysis(reg$split)) %>%
		.fit_model(control = control_workflow())

	# ------------------------------------------------------------------------------

	static_1 <- melodie:::make_static(
		wflow,
		param_info = wflow %>% extract_parameter_set_dials(),
		metrics = metric_set(rmse),
		eval_time = NULL,
		split_args = reg$args,
		control = control_resamples()
	)

	data_1 <- melodie:::get_data_subsets(wflow, reg$split)
	static_1 <- c(static_1, data_1)
	static_1$y_name <- reg$y

	sched <- melodie:::schedule_grid(knn_grid, static_1$wflow)

	uncal_pred <- workflow(outcome ~ ., knn_reg_spec) %>%
		finalize_workflow(knn_grid[1, ]) %>%
		fit(data_1$data_fit) %>%
		predict(new_data = data_1$data_perf)

	res_1 <- melodie:::post_estimation_but_no_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = knn_grid[1, ],
		static = static_1
	)
	plist_1 <- reg_sim_plist %>% mutate(neighbors = double(0))

	expect_equal(res_1[0, ], plist_1)
	expect_equal(nrow(res_1), nrow(assessment(reg$split)) * nrow(knn_grid))
	expect_equal(
		res_1$.row,
		rep(as.integer(reg$split, data = "assessment"), each = nrow(knn_grid))
	)
	expect_equal(sort(unique(res_1$neighbors)), sort(unique(knn_grid$neighbors)))
	expect_true(all(res_1$.pred != uncal_pred))
})
