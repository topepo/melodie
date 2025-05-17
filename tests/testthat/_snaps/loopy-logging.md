# preprocessor error doesn't stop grid

    Code
      res_fit <- melodie_grid(parsnip::nearest_neighbor("regression", "kknn",
        dist_power = tune()), Sale_Price ~ ., folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error: invalid type (list) for variable 'First_Flr_SF'
      There were issues with some computations   A: x1
      There were issues with some computations   A: x2
      
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

# capturing error correctly in notes

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error: Error in `step_logging_helper()`:
      Caused by error in `prep.step_logging_helper()`:
      ! testing error
      There were issues with some computations   A: x1
      There were issues with some computations   A: x2
      
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# capturing warning correctly in notes

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | warning: testing warning
      There were issues with some computations   A: x2
      There were issues with some computations   A: x2
      

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
    Message
      > A | error: Error in `step_logging_helper()`:
      Caused by error in `prep.step_logging_helper()`:
      ! testing error
      There were issues with some computations   A: x1
      There were issues with some computations   A: x2
      
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# emitter works with errors

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error: Error in `step_logging_helper()`:
      Caused by error in `prep.step_logging_helper()`:
      ! testing error
      There were issues with some computations   A: x1
      There were issues with some computations   A: x2
      
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

