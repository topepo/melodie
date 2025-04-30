test_that("predict classification - no submodels - no calibration", {
  skip_if_not_installed("modeldata")

  cls <- make_post_data()

  pca_rec <- recipe(class ~ ., data = cls$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  wflow <- workflow(pca_rec, logistic_reg())
  wflow_fit <- fit(wflow, cls$data)

  class_only <- metric_set(accuracy)
  prob_only <- metric_set(brier_class)
  both_types <- metric_set(brier_class, accuracy)

  data_1 <- melodie:::get_data_subsets(wflow, cls$rs$splits[[1]], cls$args)

  fac_0 <- factor(levels = levels(cls$data$class))

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------
  # Only predict classes

  static_class <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = class_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_class <- melodie:::update_static(static_class, data_1)
  static_class$y_name <- "class"

  class_res <- melodie:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    class_res[0, ],
    tibble(.pred_class = fac_0, .row = integer(0), class = fac_0)
  )
  expect_equal(nrow(class_res), nrow(assessment(cls$rs$splits[[1]])))

  # ----------------------------------------------------------------------------
  # Only predict probabilities

  static_prob <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = prob_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_prob <- melodie:::update_static(static_prob, data_1)
  static_prob$y_name <- "class"

  prob_res <- melodie:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    prob_res[0, ],
    tibble(
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      .row = integer(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res), nrow(assessment(cls$rs$splits[[1]])))

  # ----------------------------------------------------------------------------
  # Both prediction types

  static_both <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = both_types,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_both <- melodie:::update_static(static_both, data_1)
  static_both$y_name <- "class"

  both_res <- melodie:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    both_res[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(both_res), nrow(assessment(cls$rs$splits[[1]])))

  # ------------------------------------------------------------------------------
  # bad arg

  expect_snapshot(
    melodie:::predict_all_types(
      wflow_fit,
      static_both,
      submodel_grid = NULL,
      predictee = "potato"
    ),
    error = TRUE
  )

  static_bad <- static_both
  static_bad$post_estimation <- TRUE
  expect_snapshot(
    melodie:::predict_all_types(
      wflow_fit,
      static_bad,
      submodel_grid = NULL,
      predictee = "calibration"
    ),
    error = TRUE
  )
})

test_that("predict classification - no submodels - with calibration", {
  skip_if_not_installed("modeldata")

  cls <- make_post_data()

  pca_rec <- recipe(class ~ ., data = cls$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  cal_pst <- tailor() |> adjust_probability_calibration()

  wflow <- workflow(pca_rec, logistic_reg(), cal_pst)
  wflow_fit <- fit(wflow, cls$data, calibration = cls$data)

  class_only <- metric_set(accuracy)
  prob_only <- metric_set(brier_class)
  both_types <- metric_set(brier_class, accuracy)

  data_1 <- melodie:::get_data_subsets(wflow, cls$rs$splits[[1]], cls$args)

  fac_0 <- factor(levels = levels(cls$data$class))

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------
  # Only predict classes

  static_class <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = class_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_class <- melodie:::update_static(static_class, data_1)
  static_class$y_name <- "class"

  class_res_prd <- melodie:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    class_res_prd[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(class_res_prd), nrow(data_1$pred$data))

  ###

  class_res_cal <- melodie:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = NULL,
    predictee = "calibration"
  )

  expect_equal(
    class_res_cal[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(class_res_cal), nrow(data_1$cal$data))

  # ----------------------------------------------------------------------------
  # Only predict probabilities

  static_prob <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = prob_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_prob <- melodie:::update_static(static_prob, data_1)
  static_prob$y_name <- "class"

  prob_res_prd <- melodie:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    prob_res_prd[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_prd), nrow(data_1$pred$data))

  ###

  prob_res_cal <- melodie:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = NULL,
    predictee = "calibration"
  )

  expect_equal(
    prob_res_cal[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_cal), nrow(data_1$cal$data))

  # ----------------------------------------------------------------------------
  # Both prediction types

  static_both <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = both_types,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_both <- melodie:::update_static(static_both, data_1)
  static_both$y_name <- "class"

  both_res_prd <- melodie:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    both_res_prd[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_prd), nrow(data_1$pred$data))

  ###

  both_res_cal <- melodie:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = NULL,
    predictee = "calibration"
  )

  expect_equal(
    both_res_cal[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(both_res_cal), nrow(data_1$cal$data))
})

test_that("predict classification - with submodels - no calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  cls <- make_post_data()

  pca_rec <- recipe(class ~ ., data = cls$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  knn_cls_spec <- parsnip::nearest_neighbor(
    mode = "classification",
    neighbors = 12
  )

  five_neighbors <- tibble(neighbors = 5)

  wflow <- workflow(pca_rec, knn_cls_spec)
  wflow_fit <- fit(wflow, cls$data)

  class_only <- metric_set(accuracy)
  prob_only <- metric_set(brier_class)
  both_types <- metric_set(brier_class, accuracy)

  data_1 <- melodie:::get_data_subsets(wflow, cls$rs$splits[[1]], cls$args)

  fac_0 <- factor(levels = levels(cls$data$class))

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------
  # Only predict classes

  static_class <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = class_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_class <- melodie:::update_static(static_class, data_1)
  static_class$y_name <- "class"

  class_res <- melodie:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    class_res[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      class = fac_0
    )
  )
  expect_equal(nrow(class_res), nrow(assessment(cls$rs$splits[[1]])))

  # ----------------------------------------------------------------------------
  # Only predict probabilities

  static_prob <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = prob_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_prob <- melodie:::update_static(static_prob, data_1)
  static_prob$y_name <- "class"

  prob_res <- melodie:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    prob_res[0, ],
    tibble(
      neighbors = double(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      .row = integer(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res), nrow(assessment(cls$rs$splits[[1]])))

  # ----------------------------------------------------------------------------
  # Both prediction types

  static_both <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = both_types,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_both <- melodie:::update_static(static_both, data_1)
  static_both$y_name <- "class"

  both_res <- melodie:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    both_res[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(both_res), nrow(assessment(cls$rs$splits[[1]])))

  # ------------------------------------------------------------------------------
  # bad arg

  expect_snapshot(
    melodie:::predict_all_types(
      wflow_fit,
      static_both,
      submodel_grid = five_neighbors,
      predictee = "potato"
    ),
    error = TRUE
  )

  static_bad <- static_both
  static_bad$post_estimation <- TRUE
  expect_snapshot(
    melodie:::predict_all_types(
      wflow_fit,
      static_bad,
      submodel_grid = five_neighbors,
      predictee = "calibration"
    ),
    error = TRUE
  )
})

test_that("predict classification - with submodels - with calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  cls <- make_post_data()

  pca_rec <- recipe(class ~ ., data = cls$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  knn_cls_spec <- parsnip::nearest_neighbor(
    mode = "classification",
    neighbors = 12
  )

  five_neighbors <- tibble(neighbors = 5)

  cal_pst <- tailor() |> adjust_probability_calibration()

  wflow <- workflow(pca_rec, knn_cls_spec, cal_pst)
  wflow_fit <- fit(wflow, cls$data, calibration = cls$data)

  class_only <- metric_set(accuracy)
  prob_only <- metric_set(brier_class)
  both_types <- metric_set(brier_class, accuracy)

  data_1 <- melodie:::get_data_subsets(wflow, cls$rs$splits[[1]], cls$args)

  fac_0 <- factor(levels = levels(cls$data$class))

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------
  # Only predict classes

  static_class <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = class_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_class <- melodie:::update_static(static_class, data_1)
  static_class$y_name <- "class"

  class_res_prd <- melodie:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    class_res_prd[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(class_res_prd), nrow(data_1$pred$data))

  ###

  class_res_cal <- melodie:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = five_neighbors,
    predictee = "calibration"
  )

  expect_equal(
    class_res_cal[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(class_res_cal), nrow(data_1$cal$data))

  # ----------------------------------------------------------------------------
  # Only predict probabilities

  static_prob <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = prob_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_prob <- melodie:::update_static(static_prob, data_1)
  static_prob$y_name <- "class"

  prob_res_prd <- melodie:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    prob_res_prd[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_prd), nrow(data_1$pred$data))

  ###

  prob_res_cal <- melodie:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = five_neighbors,
    predictee = "calibration"
  )

  expect_equal(
    prob_res_cal[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_cal), nrow(data_1$cal$data))

  # ----------------------------------------------------------------------------
  # Both prediction types

  static_both <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = both_types,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_both <- melodie:::update_static(static_both, data_1)
  static_both$y_name <- "class"

  both_res_prd <- melodie:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    both_res_prd[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_prd), nrow(data_1$pred$data))

  ###

  both_res_cal <- melodie:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = five_neighbors,
    predictee = "calibration"
  )

  expect_equal(
    both_res_cal[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(both_res_cal), nrow(data_1$cal$data))
})

test_that("predict regression - no submodels - no calibration", {
  skip_if_not_installed("modeldata")

  reg <- make_post_data(mode = "regression")

  pca_rec <- recipe(outcome ~ ., data = reg$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  wflow <- workflow(pca_rec, linear_reg())
  wflow_fit <- fit(wflow, reg$data)

  reg_mtr <- metric_set(rmse)

  data_1 <- melodie:::get_data_subsets(wflow, reg$rs$splits[[1]], reg$args)

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------

  static <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = reg_mtr,
    eval_time = NULL,
    split_args = reg$args,
    control = ctrl
  )

  static <- melodie:::update_static(static, data_1)
  static$y_name <- "outcome"

  class_res <- melodie:::predict_all_types(
    wflow_fit,
    static,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    class_res[0, ],
    tibble(.pred = double(0), .row = integer(0), outcome = double(0))
  )
  expect_equal(nrow(class_res), nrow(assessment(reg$rs$splits[[1]])))
})

test_that("predict regression - no submodels - with calibration", {
  skip_if_not_installed("modeldata")

  reg <- make_post_data(mode = "regression")

  pca_rec <- recipe(outcome ~ ., data = reg$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  reg_pst <- tailor() |> adjust_numeric_calibration()

  wflow <- workflow(pca_rec, linear_reg(), reg_pst)
  wflow_fit <- fit(wflow, reg$data, calibration = reg$data)

  reg_mtr <- metric_set(rmse)

  data_1 <- melodie:::get_data_subsets(wflow, reg$rs$splits[[1]], reg$args)

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------

  static <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = reg_mtr,
    eval_time = NULL,
    split_args = reg$args,
    control = ctrl
  )

  static <- melodie:::update_static(static, data_1)
  static$y_name <- "outcome"

  class_res_prd <- melodie:::predict_all_types(
    wflow_fit,
    static,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    class_res_prd[0, ],
    tibble(
      .pred = double(0),
      .row = integer(0),
      outcome = double(0)
    )
  )
  expect_equal(nrow(class_res_prd), nrow(data_1$pred$data))
})

test_that("predict regression - with submodels - no calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  reg <- make_post_data(mode = "regression")

  pca_rec <- recipe(outcome ~ ., data = reg$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  knn_reg_spec <- parsnip::nearest_neighbor(
    mode = "regression",
    neighbors = 12
  )

  five_neighbors <- tibble(neighbors = 5)

  wflow <- workflow(pca_rec, knn_reg_spec)
  wflow_fit <- fit(wflow, reg$data)

  reg_mtr <- metric_set(rmse)

  data_1 <- melodie:::get_data_subsets(wflow, reg$rs$splits[[1]], reg$args)

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------

  static <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = reg_mtr,
    eval_time = NULL,
    split_args = reg$args,
    control = ctrl
  )

  static <- melodie:::update_static(static, data_1)
  static$y_name <- "outcome"

  class_res <- melodie:::predict_all_types(
    wflow_fit,
    static,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    class_res[0, ],
    tibble(
      neighbors = double(0),
      .pred = double(0),
      .row = integer(0),
      outcome = double(0)
    )
  )
  expect_equal(nrow(class_res), nrow(assessment(reg$rs$splits[[1]])))
})

test_that("predict regression - with submodels - with calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  reg <- make_post_data(mode = "regression")

  pca_rec <- recipe(outcome ~ ., data = reg$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  knn_reg_spec <- parsnip::nearest_neighbor(
    mode = "regression",
    neighbors = 12
  )

  five_neighbors <- tibble(neighbors = 5)

  reg_pst <- tailor() |> adjust_numeric_calibration()

  wflow <- workflow(pca_rec, knn_reg_spec, reg_pst)
  wflow_fit <- fit(wflow, reg$data, calibration = reg$data)

  reg_mtr <- metric_set(rmse)

  data_1 <- melodie:::get_data_subsets(wflow, reg$rs$splits[[1]], reg$args)

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------

  static <- melodie:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = reg_mtr,
    eval_time = NULL,
    split_args = reg$args,
    control = ctrl
  )

  static <- melodie:::update_static(static, data_1)
  static$y_name <- "outcome"

  class_res_prd <- melodie:::predict_all_types(
    wflow_fit,
    static,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    class_res_prd[0, ],
    tibble(
      neighbors = double(0),
      .pred = double(0),
      .row = integer(0),
      outcome = double(0)
    )
  )
  expect_equal(nrow(class_res_prd), nrow(data_1$pred$data))

  ###

  class_res_cal <- melodie:::predict_all_types(
    wflow_fit,
    static,
    submodel_grid = five_neighbors,
    predictee = "calibration"
  )

  expect_equal(
    class_res_cal[0, ],
    tibble(
      neighbors = double(0),
      .pred = double(0),
      .row = integer(0),
      outcome = double(0)
    )
  )
  expect_equal(nrow(class_res_cal), nrow(data_1$cal$data))
})

