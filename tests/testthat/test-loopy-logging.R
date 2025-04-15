test_that("preprocessor error doesn't stop grid", {
  # Errors in preprocessing since the formula method will cry about list columns
  ames <- modeldata::ames[, c(72, 40:45)]
  ames$First_Flr_SF <- as.list(ames$First_Flr_SF)

  set.seed(1234)
  folds <- rsample::vfold_cv(ames, 2)

  exp <- bind_cols(
    folds,
    tibble(
      .metrics = list(NULL),
      .notes = list(tibble())
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
    res_fit, 
    exp,
    ignore_attr = TRUE
  )
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
      .metrics = list(NULL),
      .notes = list(tibble())
    )
  )

  rec_spec <- recipe(Sale_Price ~ ., ames)
  mod_spec <- parsnip::nearest_neighbor("regression", "kknn", dist_power = tune())

  wf_spec <- workflow(rec_spec, mod_spec)
  
  res_fit <- melodie_grid(
    wf_spec,
    folds,
    grid = 2,
    control = control_grid(allow_par = FALSE)
  )

  expect_identical(
    res_fit, 
    exp,
    ignore_attr = TRUE
  )
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
      .metrics = list(NULL),
      .notes = list(tibble())
    )
  )

  rec_spec <- recipe(Sale_Price ~ ., ames)
  mod_spec <- parsnip::nearest_neighbor("regression", "kknn", dist_power = tune())

  wf_spec <- workflow(rec_spec, mod_spec)
  
  res_fit <- melodie_grid(
    wf_spec,
    folds,
    grid = 2,
    control = control_grid(allow_par = FALSE)
  )

  expect_identical(
    res_fit, 
    exp,
    ignore_attr = TRUE
  )
})
