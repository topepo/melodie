test_that("post with no estimation, tuning or submodels, classification", {
	skip_if_not_installed("modeldata")
	skip_if_not_installed("C50")

	cls <- make_post_data()

	wflow <- workflow(class ~ ., dt_spec, cls_tenth)

	wflow_fit <- wflow |>
		finalize_workflow(dt_grid[1, ]) |>
		.fit_pre(data = analysis(cls$split)) |>
		.fit_model(control = control_workflow())

	# ------------------------------------------------------------------------------
	# class and prob predictions

	static_1 <- melodie:::make_static(
		wflow,
		param_info = wflow |> extract_parameter_set_dials(),
		metrics = metric_set(accuracy, brier_class),
		eval_time = NULL,
		split_args = cls$args,
		control = control_resamples()
	)

	data_1 <- melodie:::get_data_subsets(wflow, cls$split)
	static_1 <- c(static_1, data_1)
	static_1$y_name <- cls$y

	sched <- melodie:::schedule_grid(dt_grid, static_1$wflow)

	res_1 <- melodie:::post_no_estimation_or_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = dt_grid[1, ],
		static = static_1
	)
	plist_1 <- cls_sim_plist |> mutate(min_n = double(0))

	expect_equal(res_1[0, ], plist_1)
	expect_equal(nrow(res_1), nrow(assessment(cls$split)))
	expect_equal(res_1$.row, as.integer(cls$split, data = "assessment"))
	expect_equal(unique(res_1$min_n), dt_grid$min_n[1])

	ge_tenth <- res_1$.pred_class_1 >= 1 / 10

	expect_equal(
		mean(res_1$.pred_class[ge_tenth] == "class_1"),
		1.0
	)

	# ------------------------------------------------------------------------------
	# class predictions only

	acc_mtr <- metric_set(accuracy)
	static_2 <- static_1
	static_2$metrics <- acc_mtr
	static_2$metric_info <- tibble::as_tibble(acc_mtr)
	static_2$pred_types <- melodie:::determine_pred_types(static_2$wflow, acc_mtr)

	res_2 <- melodie:::post_no_estimation_or_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = dt_grid[1, ],
		static = static_2
	)
	plist_2 <- cls_sim_plist |> mutate(min_n = double(0))

	expect_equal(res_2[0, ], plist_2)
	expect_equal(nrow(res_2), nrow(assessment(cls$split)))
	expect_equal(res_2$.row, as.integer(cls$split, data = "assessment"))
	expect_equal(unique(res_2$min_n), dt_grid$min_n[1])

	ge_tenth <- res_2$.pred_class_1 >= 1 / 10

	expect_equal(
		mean(res_2$.pred_class[ge_tenth] == "class_1"),
		1.0
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

	res_3 <- melodie:::post_no_estimation_or_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = dt_grid[1, ],
		static = static_3
	)
	plist_3 <- cls_sim_plist |> mutate(min_n = double(0))

	expect_equal(res_3[0, ], plist_3)
	expect_equal(nrow(res_3), nrow(assessment(cls$split)))
	expect_equal(res_3$.row, as.integer(cls$split, data = "assessment"))
	expect_equal(unique(res_3$min_n), dt_grid$min_n[1])

	expect_equal(res_3$.pred_class_1, res_1$.pred_class_1)
})

test_that("post with no estimation, tuning or submodels, regression", {
	skip_if_not_installed("kernlab")

	reg <- make_post_data("regression")

	wflow <- workflow(outcome ~ ., svm_spec, reg_post)

	wflow_fit <- wflow |>
		finalize_workflow(svm_grid[1, ]) |>
		.fit_pre(data = analysis(reg$split)) |>
		.fit_model(control = control_workflow())

	# ------------------------------------------------------------------------------

	static_1 <- melodie:::make_static(
		wflow,
		param_info = wflow |> extract_parameter_set_dials(),
		metrics = metric_set(rmse),
		eval_time = NULL,
		split_args = reg$args,
		control = control_resamples()
	)

	data_1 <- melodie:::get_data_subsets(wflow, reg$split)
	static_1 <- c(static_1, data_1)
	static_1$y_name <- reg$y

	sched <- melodie:::schedule_grid(svm_grid, static_1$wflow)

	res_1 <- melodie:::post_no_estimation_or_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = svm_grid[1, ],
		static = static_1
	)
	plist_1 <- reg_sim_plist |> mutate(degree = integer(0))

	expect_equal(res_1[0, ], plist_1)
	expect_equal(nrow(res_1), nrow(assessment(reg$split)))
	expect_equal(res_1$.row, as.integer(reg$split, data = "assessment"))
	expect_equal(unique(res_1$degree), svm_grid$degree[1])
	expect_true(all(res_1$.pred > 5000))
})

test_that("post with no estimation or tuning, with submodels, classification", {
	skip_if_not_installed("modeldata")
	skip_if_not_installed("kknn")

	cls <- make_post_data()

	wflow <- workflow(class ~ ., knn_cls_spec, cls_tenth)

	wflow_fit <- wflow |>
		finalize_workflow(knn_grid[1, ]) |>
		.fit_pre(data = analysis(cls$split)) |>
		.fit_model(control = control_workflow())

	# ------------------------------------------------------------------------------
	# class and prob predictions

	static_1 <- melodie:::make_static(
		wflow,
		param_info = wflow |> extract_parameter_set_dials(),
		metrics = metric_set(accuracy, brier_class),
		eval_time = NULL,
		split_args = cls$args,
		control = control_resamples()
	)

	data_1 <- melodie:::get_data_subsets(wflow, cls$split)
	static_1 <- c(static_1, data_1)
	static_1$y_name <- cls$y

	sched <- melodie:::schedule_grid(knn_grid, static_1$wflow)

	res_1 <- melodie:::post_no_estimation_or_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = knn_grid[1, ],
		static = static_1
	)
	plist_1 <- cls_sim_plist |> mutate(neighbors = double(0))

	expect_equal(res_1[0, ], plist_1)
	expect_equal(nrow(res_1), nrow(assessment(cls$split)) * nrow(knn_grid))
	expect_equal(
		res_1$.row,
		rep(as.integer(cls$split, data = "assessment"), each = nrow(knn_grid))
	)
	expect_equal(sort(unique(res_1$neighbors)), sort(unique(knn_grid$neighbors)))

	ge_tenth <- res_1$.pred_class_1 >= 1 / 10

	expect_equal(
		mean(res_1$.pred_class[ge_tenth] == "class_1"),
		1.0
	)

	# ------------------------------------------------------------------------------
	# class predictions only

	acc_mtr <- metric_set(accuracy)
	static_2 <- static_1
	static_2$metrics <- acc_mtr
	static_2$metric_info <- tibble::as_tibble(acc_mtr)
	static_2$pred_types <- melodie:::determine_pred_types(static_2$wflow, acc_mtr)

	res_2 <- melodie:::post_no_estimation_or_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = knn_grid[1, ],
		static = static_2
	)
	plist_2 <- cls_sim_plist |> mutate(neighbors = double(0))

	expect_equal(res_2[0, ], plist_2)
	expect_equal(nrow(res_2), nrow(assessment(cls$split)) * nrow(knn_grid))
	expect_equal(
		res_2$.row,
		rep(as.integer(cls$split, data = "assessment"), each = nrow(knn_grid))
	)
	expect_equal(sort(unique(res_2$neighbors)), sort(unique(knn_grid$neighbors)))

	ge_tenth <- res_2$.pred_class_1 >= 1 / 10

	expect_equal(
		mean(res_2$.pred_class[ge_tenth] == "class_1"),
		1.0
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

	res_3 <- melodie:::post_no_estimation_or_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = knn_grid[1, ],
		static = static_3
	)
	plist_3 <- cls_sim_plist |> mutate(neighbors = double(0))

	expect_equal(res_3[0, ], plist_3)
	expect_equal(nrow(res_3), nrow(assessment(cls$split)) * nrow(knn_grid))
	expect_equal(
		res_3$.row,
		rep(as.integer(cls$split, data = "assessment"), each = nrow(knn_grid))
	)
	expect_equal(sort(unique(res_3$neighbors)), sort(unique(knn_grid$neighbors)))

	expect_equal(res_3$.pred_class_1, res_1$.pred_class_1)
})

test_that("post with no estimation or tuning, with submodels, regression", {
	skip_if_not_installed("kknn")

	reg <- make_post_data("regression")

	wflow <- workflow(outcome ~ ., knn_reg_spec, reg_post)

	wflow_fit <- wflow |>
		finalize_workflow(knn_grid[1, ]) |>
		.fit_pre(data = analysis(reg$split)) |>
		.fit_model(control = control_workflow())

	# ------------------------------------------------------------------------------

	static_1 <- melodie:::make_static(
		wflow,
		param_info = wflow |> extract_parameter_set_dials(),
		metrics = metric_set(rmse),
		eval_time = NULL,
		split_args = reg$args,
		control = control_resamples()
	)

	data_1 <- melodie:::get_data_subsets(wflow, reg$split)
	static_1 <- c(static_1, data_1)
	static_1$y_name <- reg$y

	sched <- melodie:::schedule_grid(knn_grid, static_1$wflow)

	res_1 <- melodie:::post_no_estimation_or_tuning(
		wflow_current = wflow_fit,
		sched = sched$model_stage[[1]][1, ],
		grid = knn_grid[1, ],
		static = static_1
	)
	plist_1 <- reg_sim_plist |> mutate(neighbors = double(0))

	expect_equal(res_1[0, ], plist_1)
	expect_equal(nrow(res_1), nrow(assessment(reg$split)) * nrow(knn_grid))
	expect_equal(
		res_1$.row,
		rep(as.integer(reg$split, data = "assessment"), each = nrow(knn_grid))
	)
	expect_equal(sort(unique(res_1$neighbors)), sort(unique(knn_grid$neighbors)))
	expect_true(all(res_1$.pred > 5000))
})
