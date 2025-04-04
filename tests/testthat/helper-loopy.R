library(parsnip)
library(workflows)
library(tune)
library(rsample)
library(recipes)
library(yardstick)
library(dplyr)
library(dials)
library(rlang)
library(tailor)

# ------------------------------------------------------------------------------

dt_spec <- parsnip::decision_tree(
	mode = "classification",
	min_n = tune(),
	engine = "C5.0"
)
dt_grid <- tibble::tibble(min_n = c(2, 4))

cls_post <- tailor::tailor() %>%
	tailor::adjust_probability_threshold(threshold = tune("cut"))

cls_est_post <- tailor::tailor() %>%
	tailor::adjust_probability_calibration(method = "logistic")

# https://github.com/tidymodels/tailor/issues/72
# cls_cal_tune_post <- tailor::tailor() %>%
#   tailor::adjust_probability_calibration(method = tune())

# ------------------------------------------------------------------------------

puromycin <- tibble::as_tibble(Puromycin)
puromycin_rec <- recipes::recipe(rate ~ ., data = puromycin) %>%
	recipes::step_dummy(state)

puromycin_tune_rec <- puromycin_rec %>%
	recipes::step_poly(conc, degree = tune())

knn_spec <- parsnip::nearest_neighbor(mode = "regression", neighbors = tune())

reg_post <- tailor::tailor() %>%
	tailor::adjust_predictions_custom(.pred = 1 / .pred)

glmn_spec <- parsnip::linear_reg(penalty = tune(), mixture = tune()) %>%
	parsnip::set_engine("glmnet")
