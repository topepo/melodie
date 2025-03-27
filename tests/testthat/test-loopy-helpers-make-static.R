test_that("maker static object", {
  skip_if_not_installed("modeldata")

  data("two_class_dat", package = "modeldata")
  two_class_rs <- mc_cv(two_class_dat, times = 2)
  mc_cv_args <- rsample::.get_split_args(two_class_rs)

  wflow <- workflow(Class ~ ., dt_spec, cls_est_post)

  res <- melodie:::make_static(
    wflow,
    param_info = wflow %>% extract_parameter_set_dials(),
    metrics = metric_set(accuracy),
    eval_time = NULL,
    split_args = mc_cv_args,
    control = control_resamples()
  )

  expect_true(is.list(res))
  expect_named(
    res,
    c("wflow", "param_info", "post_estimation", "metrics", "metric_info",
      "pred_types", "eval_time", "split_args", "control")
  )
})
