# predict classification - no submodels - no calibration

    Code
      melodie:::predict_all_types(wflow_fit, static_both, submodel_grid = NULL,
        predictee = "potato")
    Condition
      Error in `melodie:::predict_all_types()`:
      ! `predictee` must be one of "assessment" or "calibration", not "potato".

---

    Code
      melodie:::predict_all_types(wflow_fit, static_bad, submodel_grid = NULL,
        predictee = "calibration")
    Condition
      Error:
      ! Calibration data were requested but not reserved.

