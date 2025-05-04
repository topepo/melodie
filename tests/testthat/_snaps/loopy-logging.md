# preprocessor error doesn't stop grid

    Code
      res_fit <- melodie_grid(parsnip::nearest_neighbor("regression", "kknn",
        dist_power = tune()), Sale_Price ~ ., folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error: invalid type (list) for variable 'First_Flr_SF'

# model error doesn't stop grid

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))

# prediction error doesn't stop grid

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))

# capturing error correctly in notes

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error: testing error

# capturing warning correctly in notes

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | warning: testing warning

# doesn't capturing message in notes

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      testing message
      testing message

# emitter works with errors

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error: testing error

