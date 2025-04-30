test_that("preprocessor error doesn't stop grid", {
  # Errors in preprocessing since the formula method will cry about list columns
  ames <- modeldata::ames[, c(72, 40:45)]
  ames$First_Flr_SF <- as.list(ames$First_Flr_SF)

  set.seed(1234)
  folds <- rsample::vfold_cv(ames, 2)

  exp <- bind_cols(
    folds,
    tibble(
      .metrics = list(NULL)
    )
  )

  res_fit <- melodie_grid(
    parsnip::nearest_neighbor("regression", "kknn", dist_power = tune()),
    Sale_Price ~ .,
    folds,
    grid = 2,
    control = control_grid(allow_par = FALSE)
  )

  expect_identical(
    dplyr::select(res_fit, -.notes),
    exp,
    ignore_attr = TRUE
  )

  expect_identical(nrow(res_fit$.notes[[1]]), 1L)
  expect_identical(ncol(res_fit$.notes[[1]]), 3L)
  expect_true(all(vapply(res_fit$.notes[[1]], is.character, logical(1))))
})

test_that("model error doesn't stop grid", {
  # Errors in modeling since the recipe passes the list columns through
  ames <- modeldata::ames[, c(72, 40:45)]
  ames$First_Flr_SF <- as.list(ames$First_Flr_SF)

  set.seed(1234)
  folds <- rsample::vfold_cv(ames, 2)

  exp <- bind_cols(
    folds,
    tibble(
      .metrics = list(NULL)
    )
  )

  rec_spec <- recipe(Sale_Price ~ ., ames)
  mod_spec <- parsnip::nearest_neighbor(
    "regression",
    "kknn",
    dist_power = tune()
  )

  wf_spec <- workflow(rec_spec, mod_spec)

  res_fit <- melodie_grid(
    wf_spec,
    folds,
    grid = 2,
    control = control_grid(allow_par = FALSE)
  )

  expect_identical(
    dplyr::select(res_fit, -.notes),
    exp,
    ignore_attr = TRUE
  )

  expect_identical(nrow(res_fit$.notes[[1]]), 2L)
  expect_identical(ncol(res_fit$.notes[[1]]), 3L)
  expect_true(all(vapply(res_fit$.notes[[1]], is.character, logical(1))))
})

test_that("prediction error doesn't stop grid", {
  # Errors in predictions as we have injected NA values into the testing splits
  ames <- modeldata::ames[, c(72, 40:45)]

  set.seed(1234)
  folds <- rsample::vfold_cv(ames, 2)
  for (i in seq_len(2)) {
    train_ids <- setdiff(seq_len(nrow(ames)), folds$splits[[i]]$in_id)
    folds$splits[[i]]$data$First_Flr_SF[train_ids] <- NA
  }

  exp <- bind_cols(
    folds,
    tibble(
      .metrics = list(NULL)
    )
  )

  rec_spec <- recipe(Sale_Price ~ ., ames)
  mod_spec <- parsnip::nearest_neighbor(
    "regression",
    "kknn",
    dist_power = tune()
  )

  wf_spec <- workflow(rec_spec, mod_spec)

  res_fit <- melodie_grid(
    wf_spec,
    folds,
    grid = 2,
    control = control_grid(allow_par = FALSE)
  )

  expect_identical(
    dplyr::select(res_fit, -.notes),
    exp,
    ignore_attr = TRUE
  )

  expect_identical(nrow(res_fit$.notes[[1]]), 2L)
  expect_identical(ncol(res_fit$.notes[[1]]), 3L)
  expect_true(all(vapply(res_fit$.notes[[1]], is.character, logical(1))))
})

test_that("capturing error correctly in notes", {
  assign(
    "prep.step_logging_helper",
    prep.step_logging_helper,
    envir = .GlobalEnv
  )
  assign(
    "bake.step_logging_helper",
    bake.step_logging_helper,
    envir = .GlobalEnv
  )
  ames <- modeldata::ames[, c(72, 40:45)]

  set.seed(1234)
  folds <- rsample::vfold_cv(ames, 2)

  rec_spec <- recipe(Sale_Price ~ ., ames) |>
    step_logging_helper(type = "error")
  mod_spec <- parsnip::nearest_neighbor(
    "regression",
    "kknn",
    dist_power = tune()
  )

  wf_spec <- workflow(rec_spec, mod_spec)

  res_fit <- melodie_grid(
    wf_spec,
    folds,
    grid = 2,
    control = control_grid(allow_par = FALSE)
  )

  exp <- tibble::tibble(
    location = "preprocessor 1/1",
    type = "error",
    note = "testing error"
  )
  expect_identical(res_fit$.notes[[1]], exp)
  expect_identical(res_fit$.notes[[2]], exp)

  expect_true(
    all(vapply(res_fit$.metrics, NROW, integer(1)) == 0)
  )
  rm(list = "prep.step_logging_helper", envir = .GlobalEnv)
  rm(list = "bake.step_logging_helper", envir = .GlobalEnv)
})

test_that("capturing warning correctly in notes", {
  assign(
    "prep.step_logging_helper",
    prep.step_logging_helper,
    envir = .GlobalEnv
  )
  assign(
    "bake.step_logging_helper",
    bake.step_logging_helper,
    envir = .GlobalEnv
  )

  ames <- modeldata::ames[, c(72, 40:45)]

  set.seed(1234)
  folds <- rsample::vfold_cv(ames, 2)

  rec_spec <- recipe(Sale_Price ~ ., ames) |>
    step_logging_helper(type = "warning")
  mod_spec <- parsnip::nearest_neighbor(
    "regression",
    "kknn",
    dist_power = tune()
  )

  wf_spec <- workflow(rec_spec, mod_spec)
  res_fit <- melodie_grid(
    wf_spec,
    folds,
    grid = 2,
    control = control_grid(allow_par = FALSE)
  )

  exp <- tibble::tibble(
    location = "preprocessor 1/1",
    type = "warning",
    note = "testing warning"
  )
  expect_identical(res_fit$.notes[[1]], exp)
  expect_identical(res_fit$.notes[[2]], exp)

  expect_true(
    all(vapply(res_fit$.metrics, NROW, integer(1)) > 0)
  )
  rm(list = "prep.step_logging_helper", envir = .GlobalEnv)
  rm(list = "bake.step_logging_helper", envir = .GlobalEnv)
})

test_that("doesn't capturing message in notes", {
  assign(
    "prep.step_logging_helper",
    prep.step_logging_helper,
    envir = .GlobalEnv
  )
  assign(
    "bake.step_logging_helper",
    bake.step_logging_helper,
    envir = .GlobalEnv
  )

  ames <- modeldata::ames[, c(72, 40:45)]

  set.seed(1234)
  folds <- rsample::vfold_cv(ames, 2)

  rec_spec <- recipe(Sale_Price ~ ., ames) |>
    step_logging_helper(type = "message")
  mod_spec <- parsnip::nearest_neighbor(
    "regression",
    "kknn",
    dist_power = tune()
  )

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_snapshot(
    res_fit <- melodie_grid(
      wf_spec,
      folds,
      grid = 2,
      control = control_grid(allow_par = FALSE)
    )
  )

  exp <- tibble::tibble(
    location = character(0),
    type = character(0),
    note = character(0)
  )

  expect_identical(res_fit$.notes[[1]], exp)
  expect_identical(res_fit$.notes[[2]], exp)

  expect_true(
    all(vapply(res_fit$.metrics, NROW, integer(1)) > 0)
  )
  rm(list = "prep.step_logging_helper", envir = .GlobalEnv)
  rm(list = "bake.step_logging_helper", envir = .GlobalEnv)
})

test_that("captures xgboost C errors", {
  # xgboost is gonna complain because one of the predictors is Inf
  ames <- modeldata::ames[, c(72, 40:45)]
  ames$Second_Flr_SF <- Inf

  set.seed(1234)
  folds <- rsample::vfold_cv(ames, 2)

  rec_spec <- recipe(Sale_Price ~ ., ames)
  mod_spec <- parsnip::boost_tree(
    "regression",
    "xgboost",
    trees = tune()
  )

  wf_spec <- workflow(rec_spec, mod_spec)

  res_fit <- melodie_grid(
    wf_spec,
    folds,
    grid = 2,
    control = control_grid(allow_par = FALSE)
  )

  expect_identical(
    nrow(collect_notes(res_fit)),
    2L
  )
})
