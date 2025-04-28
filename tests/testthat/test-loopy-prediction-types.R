test_that("determinig types of predictions required", {
  wflow_1 <- workflow(class ~ ., glmn_spec, reg_post)
  expect_equal(
    melodie:::determine_pred_types(wflow_1, metric_set(rmse)),
    "numeric"
  )

  wflow_2 <- workflow(class ~ ., dt_spec)
  expect_equal(
    melodie:::determine_pred_types(wflow_2, metric_set(accuracy)),
    "class"
  )
  expect_equal(
    melodie:::determine_pred_types(wflow_2, metric_set(brier_class)),
    "prob"
  )
  expect_equal(
    melodie:::determine_pred_types(wflow_2, metric_set(accuracy, brier_class)),
    c("class", "prob")
  )

  wflow_3 <- workflow(class ~ ., dt_spec, cls_cal_tune_post)
  expect_equal(
    melodie:::determine_pred_types(wflow_3, metric_set(accuracy)),
    c("class", "prob")
  )
  expect_equal(
    melodie:::determine_pred_types(wflow_3, metric_set(brier_class)),
    c("class", "prob")
  )

  just_cal <- tailor() |> adjust_probability_calibration()
  wflow_4 <- workflow(class ~ ., dt_spec, just_cal)
  expect_equal(
    melodie:::determine_pred_types(wflow_4, metric_set(accuracy)),
    c("class", "prob")
  )
  expect_equal(
    melodie:::determine_pred_types(wflow_4, metric_set(brier_class)),
    c("class", "prob")
  )

  just_thresh <- tailor() |> adjust_probability_threshold()
  wflow_5 <- workflow(class ~ ., dt_spec, just_thresh)
  expect_equal(
    melodie:::determine_pred_types(wflow_5, metric_set(accuracy)),
    c("class", "prob")
  )
  expect_equal(
    melodie:::determine_pred_types(wflow_5, metric_set(brier_class)),
    c("class", "prob")
  )
})
