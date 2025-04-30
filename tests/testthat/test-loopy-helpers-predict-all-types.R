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
    class_res[0,],
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
    prob_res[0,],
    tibble(.pred_class_1 = double(0), .pred_class_2 = double(0), .row = integer(0), class = fac_0)
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
