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
