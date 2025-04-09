test_that("determinig prediction/post strategy", {
  wflow_1 <- workflow(class ~ ., logistic_reg())
  expect_equal(
    melodie:::pred_post_strategy(wflow_1),
   "predict_only"
  )

  # with submodel
  wflow_2 <- workflow(class ~ ., glmn_spec)
  expect_equal(
    melodie:::pred_post_strategy(wflow_2),
    "predict_only"
  )

  # mutate postprocessor
  wflow_3 <- workflow(class ~ ., glmn_spec, reg_post)
  expect_equal(
    melodie:::pred_post_strategy(wflow_3),
    "no_estimation_or_tuning"
  )

  # calibration postprocessor with no tuning
  wflow_4 <- workflow(class ~ ., glmn_spec, cls_est_post)
  expect_equal(
    melodie:::pred_post_strategy(wflow_4),
    "estimation_but_no_tuning"
  )

  # # calibration postprocessor with tuning
  wflow_5 <- workflow(class ~ ., glmn_spec, cls_cal_tune_post)
  expect_equal(
    melodie:::pred_post_strategy(wflow_5),
    "estimation_and_tuning"
  )

  # calibration postprocessor with tuning but no estimation
  wflow_6 <- workflow(class ~ ., glmn_spec, cls_post)
  expect_equal(
    melodie:::pred_post_strategy(wflow_6),
    "no_estimation_but_tuning"
  )

})

test_that("prediction only, no submodels, classification", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("C50")

  data("two_class_dat", package = "modeldata")
  two_class_rs <- mc_cv(two_class_dat, times = 2)
  rs_split <- two_class_rs$splits[[1]]
  mc_cv_args <- rsample::.get_split_args(two_class_rs)

  wflow <-  workflow(Class ~ ., dt_spec)
  dt_grid <- tibble(min_n = c(10, 20))

  wflow_fit <-
    wflow %>%
    finalize_workflow(dt_grid[1,]) %>%
    .fit_pre(data = analysis(rs_split)) %>%
    .fit_model(control = control_workflow())

  # ------------------------------------------------------------------------------
  # class and prob predictions

  static_1 <- melodie:::make_static(
    wflow,
    param_info = wflow %>% extract_parameter_set_dials(),
    metrics = metric_set(accuracy, brier_class),
    eval_time = NULL,
    split_args = mc_cv_args,
    control = control_resamples()
  )

  data_1 <- melodie:::get_data_subsets(wflow, rs_split)
  static_1 <- c(static_1, data_1)
  static_1$y_name <- "Class"

  sched <- melodie:::schedule_grid(dt_grid, static_1$wflow)

  res_1 <-
    melodie:::predict_only(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = dt_grid[1,],
      static = static_1,
      estimation = FALSE
    )
  plist_1 <-
    tibble(
      Class = two_class_dat$Class[0],
      .pred_class = two_class_dat$Class[0],
      .pred_Class1 = double(0),
      .pred_Class2 = double(0),
      .row = integer(0),
      min_n = double(0)
    )
  expect_equal(res_1[0,], plist_1)
  expect_equal(nrow(res_1), nrow(assessment(rs_split)))
  expect_equal(res_1$.row, as.integer(rs_split, data = "assessment"))
  expect_equal(unique(res_1$min_n), dt_grid$min_n[1])

  # ------------------------------------------------------------------------------
  # class predictions only

  acc_mtr <- metric_set(accuracy)
  static_2 <- static_1
  static_2$metrics <- acc_mtr
  static_2$metric_info <- tibble::as_tibble(acc_mtr)
  static_2$pred_types <- melodie:::determine_pred_types(static_2$wflow, acc_mtr)

  res_2 <-
    melodie:::predict_only(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = dt_grid[1,],
      static = static_2,
      estimation = FALSE
    )
  plist_2 <-
    tibble(
      Class = two_class_dat$Class[0],
      .pred_class = two_class_dat$Class[0],
      .row = integer(0),
      min_n = double(0)
    )
  expect_equal(res_2[0,], plist_2)
  expect_equal(nrow(res_2), nrow(assessment(rs_split)))
  expect_equal(res_2$.row, as.integer(rs_split, data = "assessment"))
  expect_equal(unique(res_2$min_n), dt_grid$min_n[1])

  # ------------------------------------------------------------------------------
  # class probabilities only

  brier_mtr <- metric_set(brier_class)
  static_3 <- static_1
  static_3$metrics <- brier_mtr
  static_3$metric_info <- tibble::as_tibble(brier_mtr)
  static_3$pred_types <- melodie:::determine_pred_types(static_3$wflow, brier_mtr)

  res_3 <-
    melodie:::predict_only(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = dt_grid[1,],
      static = static_3,
      estimation = FALSE
    )
  plist_3 <-
    tibble(
      Class = two_class_dat$Class[0],
      .pred_Class1 = double(0),
      .pred_Class2 = double(0),
      .row = integer(0),
      min_n = double(0)
    )
  expect_equal(res_3[0,], plist_3)
  expect_equal(nrow(res_3), nrow(assessment(rs_split)))
  expect_equal(res_3$.row, as.integer(rs_split, data = "assessment"))
  expect_equal(unique(res_3$min_n), dt_grid$min_n[1])
})

test_that("prediction only, no submodels, regression", {
  skip_if_not_installed("kernlab")

  reg_rs <- mc_cv(puromycin, times = 2)
  rs_split <- reg_rs$splits[[1]]
  mc_cv_args <- rsample::.get_split_args(reg_rs)

  svm_spec <- svm_poly(mode = "regression", cost = 1, degree = tune())

  wflow <-  workflow(rate ~ ., svm_spec)
  svm_grid <- tibble(degree = 1:2)

  wflow_fit <-
    wflow %>%
    finalize_workflow(svm_grid[1,]) %>%
    .fit_pre(data = analysis(rs_split)) %>%
    .fit_model(control = control_workflow())

  # ------------------------------------------------------------------------------

  static_1 <- melodie:::make_static(
    wflow,
    param_info = wflow %>% extract_parameter_set_dials(),
    metrics = metric_set(rmse),
    eval_time = NULL,
    split_args = mc_cv_args,
    control = control_resamples()
  )

  data_1 <- melodie:::get_data_subsets(wflow, rs_split)
  static_1 <- c(static_1, data_1)
  static_1$y_name <- "rate"

  sched <- melodie:::schedule_grid(svm_grid, static_1$wflow)

  res_1 <-
    melodie:::predict_only(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = svm_grid[1,],
      static = static_1,
      estimation = FALSE
    )
  plist_1 <-
    tibble(
      rate = puromycin$rate[0],
      .pred = puromycin$rate[0],
      .row = integer(0),
      degree = integer(0)
    )
  expect_equal(res_1[0,], plist_1)
  expect_equal(nrow(res_1), nrow(assessment(rs_split)))
  expect_equal(res_1$.row, as.integer(rs_split, data = "assessment"))
  expect_equal(unique(res_1$degree), svm_grid$degree[1])

})


test_that("prediction only, with submodels, classification", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  data("two_class_dat", package = "modeldata")
  two_class_rs <- mc_cv(two_class_dat, times = 2)
  rs_split <- two_class_rs$splits[[1]]
  mc_cv_args <- rsample::.get_split_args(two_class_rs)

  wflow <-  workflow(Class ~ ., knn_cls_spec)
  knn_grid <- tibble(neighbors = 1:3)

  wflow_fit <-
    wflow %>%
    finalize_workflow(knn_grid[1,]) %>%
    .fit_pre(data = analysis(rs_split)) %>%
    .fit_model(control = control_workflow())

  # ------------------------------------------------------------------------------
  # class and prob predictions

  static_1 <- melodie:::make_static(
    wflow,
    param_info = wflow %>% extract_parameter_set_dials(),
    metrics = metric_set(accuracy, brier_class),
    eval_time = NULL,
    split_args = mc_cv_args,
    control = control_resamples()
  )

  data_1 <- melodie:::get_data_subsets(wflow, rs_split)
  static_1 <- c(static_1, data_1)
  static_1$y_name <- "Class"

  sched <- melodie:::schedule_grid(knn_grid, static_1$wflow)

  res_1 <-
    melodie:::predict_only(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = knn_grid[1,],
      static = static_1,
      estimation = FALSE
    )
  plist_1 <-
    tibble(
      Class = two_class_dat$Class[0],
      .pred_class = two_class_dat$Class[0],
      .pred_Class1 = double(0),
      .pred_Class2 = double(0),
      .row = integer(0),
      neighbors = double(0)
    )
  expect_equal(res_1[0,], plist_1)
  expect_equal(nrow(res_1), nrow(assessment(rs_split)) * nrow(knn_grid))
  expect_equal(
    res_1$.row,
    rep(as.integer(rs_split, data = "assessment"), each = nrow(knn_grid))
  )
  expect_equal(sort(unique(res_1$neighbors)), sort(unique(knn_grid$neighbors)))

  # ------------------------------------------------------------------------------
  # class predictions only

  acc_mtr <- metric_set(accuracy)
  static_2 <- static_1
  static_2$metrics <- acc_mtr
  static_2$metric_info <- tibble::as_tibble(acc_mtr)
  static_2$pred_types <- melodie:::determine_pred_types(static_2$wflow, acc_mtr)

  res_2 <-
    melodie:::predict_only(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = knn_grid[1,],
      static = static_2,
      estimation = FALSE
    )
  plist_2 <-
    tibble(
      Class = two_class_dat$Class[0],
      .pred_class = two_class_dat$Class[0],
      .row = integer(0),
      neighbors = double(0)
    )
  expect_equal(res_2[0,], plist_2)
  expect_equal(nrow(res_2), nrow(assessment(rs_split)) * nrow(knn_grid))
  expect_equal(
    res_2$.row,
    rep(as.integer(rs_split, data = "assessment"), each = nrow(knn_grid))
  )
  expect_equal(sort(unique(res_2$neighbors)), sort(unique(knn_grid$neighbors)))

  # ------------------------------------------------------------------------------
  # class probabilities only

  brier_mtr <- metric_set(brier_class)
  static_3 <- static_1
  static_3$metrics <- brier_mtr
  static_3$metric_info <- tibble::as_tibble(brier_mtr)
  static_3$pred_types <- melodie:::determine_pred_types(static_3$wflow, brier_mtr)

  res_3 <-
    melodie:::predict_only(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = knn_grid[1,],
      static = static_3,
      estimation = FALSE
    )
  plist_3 <-
    tibble(
      Class = two_class_dat$Class[0],
      .pred_Class1 = double(0),
      .pred_Class2 = double(0),
      .row = integer(0),
      neighbors = double(0)
    )
  expect_equal(res_3[0,], plist_3)
  expect_equal(nrow(res_3), nrow(assessment(rs_split)) * nrow(knn_grid))
  expect_equal(
    res_3$.row,
    rep(as.integer(rs_split, data = "assessment"), each = nrow(knn_grid))
  )
  expect_equal(sort(unique(res_3$neighbors)), sort(unique(knn_grid$neighbors)))
})


test_that("prediction only, with submodels, regression", {
  skip_if_not_installed("kknn")

  reg_rs <- mc_cv(puromycin, times = 2)
  rs_split <- reg_rs$splits[[1]]
  mc_cv_args <- rsample::.get_split_args(reg_rs)

  wflow <-  workflow(rate ~ ., knn_reg_spec)
  knn_grid <- tibble(neighbors = 1:3)

  wflow_fit <-
    wflow %>%
    finalize_workflow(knn_grid[1,]) %>%
    .fit_pre(data = analysis(rs_split)) %>%
    .fit_model(control = control_workflow())

  # ------------------------------------------------------------------------------

  static_1 <- melodie:::make_static(
    wflow,
    param_info = wflow %>% extract_parameter_set_dials(),
    metrics = metric_set(rmse),
    eval_time = NULL,
    split_args = mc_cv_args,
    control = control_resamples()
  )

  data_1 <- melodie:::get_data_subsets(wflow, rs_split)
  static_1 <- c(static_1, data_1)
  static_1$y_name <- "rate"

  sched <- melodie:::schedule_grid(knn_grid, static_1$wflow)

  res_1 <-
    melodie:::predict_only(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = knn_grid[1,],
      static = static_1,
      estimation = FALSE
    )
  plist_1 <-
    tibble(
      rate = puromycin$rate[0],
      .pred = puromycin$rate[0],
      .row = integer(0),
      neighbors = double(0)
    )
  expect_equal(res_1[0,], plist_1)
  expect_equal(nrow(res_1), nrow(assessment(rs_split)) * nrow(knn_grid))
  expect_equal(
    res_1$.row,
    rep(as.integer(rs_split, data = "assessment"), each = nrow(knn_grid))
  )
  expect_equal(sort(unique(res_1$neighbors)), sort(unique(knn_grid$neighbors)))

})

# post_no_estimation_or_tuning

test_that("post with no estimation, tuning or submodels, classification", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("C50")

  data("two_class_dat", package = "modeldata")
  two_class_rs <- mc_cv(two_class_dat, times = 2)
  rs_split <- two_class_rs$splits[[1]]
  mc_cv_args <- rsample::.get_split_args(two_class_rs)

  wflow <-  workflow(Class ~ ., dt_spec, cls_tenth)
  dt_grid <- tibble(min_n = c(10, 20))

  wflow_fit <-
    wflow %>%
    finalize_workflow(dt_grid[1,]) %>%
    .fit_pre(data = analysis(rs_split)) %>%
    .fit_model(control = control_workflow())

  # ------------------------------------------------------------------------------
  # class and prob predictions

  static_1 <- melodie:::make_static(
    wflow,
    param_info = wflow %>% extract_parameter_set_dials(),
    metrics = metric_set(accuracy, brier_class),
    eval_time = NULL,
    split_args = mc_cv_args,
    control = control_resamples()
  )

  data_1 <- melodie:::get_data_subsets(wflow, rs_split)
  static_1 <- c(static_1, data_1)
  static_1$y_name <- "Class"

  sched <- melodie:::schedule_grid(dt_grid, static_1$wflow)

  # wflow_current, sched, grid, static
  res_1 <-
    melodie:::post_no_estimation_or_tuning(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = dt_grid[1,],
      static = static_1
    )
  plist_1 <-
    tibble(
      Class = two_class_dat$Class[0],
      .pred_class = two_class_dat$Class[0],
      .pred_Class1 = double(0),
      .pred_Class2 = double(0),
      .row = integer(0),
      min_n = double(0)
    )
  expect_equal(res_1[0,], plist_1)
  expect_equal(nrow(res_1), nrow(assessment(rs_split)))
  expect_equal(res_1$.row, as.integer(rs_split, data = "assessment"))
  expect_equal(unique(res_1$min_n), dt_grid$min_n[1])

  ge_tenth <- res_1$.pred_Class1 >= 1 / 10

  expect_equal(
    mean(res_1$.pred_class[ge_tenth] == "Class1"),
    1.0
  )

  # ------------------------------------------------------------------------------
  # class predictions only

  acc_mtr <- metric_set(accuracy)
  static_2 <- static_1
  static_2$metrics <- acc_mtr
  static_2$metric_info <- tibble::as_tibble(acc_mtr)
  static_2$pred_types <- melodie:::determine_pred_types(static_2$wflow, acc_mtr)

  res_2 <-
    melodie:::post_no_estimation_or_tuning(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = dt_grid[1,],
      static = static_2
    )
  plist_2 <-
    tibble(
      Class = two_class_dat$Class[0],
      .pred_class = two_class_dat$Class[0],
      .pred_Class1 = double(0),
      .pred_Class2 = double(0),
      .row = integer(0),
      min_n = double(0)
    )
  expect_equal(res_2[0,], plist_2)
  expect_equal(nrow(res_2), nrow(assessment(rs_split)))
  expect_equal(res_2$.row, as.integer(rs_split, data = "assessment"))
  expect_equal(unique(res_2$min_n), dt_grid$min_n[1])

  ge_tenth <- res_2$.pred_Class1 >= 1 / 10

  expect_equal(
    mean(res_2$.pred_class[ge_tenth] == "Class1"),
    1.0
  )

  # ------------------------------------------------------------------------------
  # class probabilities only

  brier_mtr <- metric_set(brier_class)
  static_3 <- static_1
  static_3$metrics <- brier_mtr
  static_3$metric_info <- tibble::as_tibble(brier_mtr)
  static_3$pred_types <- melodie:::determine_pred_types(static_3$wflow, brier_mtr)

  res_3 <-
    melodie:::post_no_estimation_or_tuning(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = dt_grid[1,],
      static = static_3
    )
  plist_3 <-
    tibble(
      Class = two_class_dat$Class[0],
      .pred_class = two_class_dat$Class[0],
      .pred_Class1 = double(0),
      .pred_Class2 = double(0),
      .row = integer(0),
      min_n = double(0)
    )
  expect_equal(res_3[0,], plist_3)
  expect_equal(nrow(res_3), nrow(assessment(rs_split)))
  expect_equal(res_3$.row, as.integer(rs_split, data = "assessment"))
  expect_equal(unique(res_3$min_n), dt_grid$min_n[1])

  expect_equal(res_3$.pred_Class1, res_1$.pred_Class1)
})

test_that("post with no estimation, tuning or submodels, regression", {
  skip_if_not_installed("kernlab")

  reg_rs <- mc_cv(puromycin, times = 2)
  rs_split <- reg_rs$splits[[1]]
  mc_cv_args <- rsample::.get_split_args(reg_rs)

  svm_spec <- svm_poly(mode = "regression", cost = 1, degree = tune())

  wflow <-  workflow(rate ~ ., svm_spec, reg_post)
  svm_grid <- tibble(degree = 1:2)

  wflow_fit <-
    wflow %>%
    finalize_workflow(svm_grid[1,]) %>%
    .fit_pre(data = analysis(rs_split)) %>%
    .fit_model(control = control_workflow())

  # ------------------------------------------------------------------------------

  static_1 <- melodie:::make_static(
    wflow,
    param_info = wflow %>% extract_parameter_set_dials(),
    metrics = metric_set(rmse),
    eval_time = NULL,
    split_args = mc_cv_args,
    control = control_resamples()
  )

  data_1 <- melodie:::get_data_subsets(wflow, rs_split)
  static_1 <- c(static_1, data_1)
  static_1$y_name <- "rate"

  sched <- melodie:::schedule_grid(svm_grid, static_1$wflow)

  res_1 <-
    melodie:::post_no_estimation_or_tuning(
      wflow_current = wflow_fit,
      sched = sched$model_stage[[1]][1,],
      grid = svm_grid[1,],
      static = static_1
    )
  plist_1 <-
    tibble(
      rate = puromycin$rate[0],
      .pred = puromycin$rate[0],
      .row = integer(0),
      degree = integer(0)
    )
  expect_equal(res_1[0,], plist_1)
  expect_equal(nrow(res_1), nrow(assessment(rs_split)))
  expect_equal(res_1$.row, as.integer(rs_split, data = "assessment"))
  expect_equal(unique(res_1$degree), svm_grid$degree[1])
  expect_true(all(res_1$.pred < 0))
})


# post_estimation_but_no_tuning

# post_no_estimation_but_tuning

# estimation_and_tuning

#
