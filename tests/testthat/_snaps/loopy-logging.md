# preprocessor error doesn't stop grid

    Code
      res_fit <- melodie_grid(parsnip::nearest_neighbor("regression", "kknn",
        dist_power = tune()), Sale_Price ~ ., folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# model error doesn't stop grid

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# prediction error doesn't stop grid

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# postprocessing error doesn't stop grid

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# capturing error correctly in notes

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# capturing warning correctly in notes

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))

# doesn't capturing message in notes

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      testing message
      testing message

# captures kknn R errors

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# captures xgboost C errors

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# captures cli styled errors

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

