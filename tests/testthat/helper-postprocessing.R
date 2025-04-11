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
library(tibble)

# ------------------------------------------------------------------------------

dt_spec <- parsnip::decision_tree(
	mode = "classification",
	min_n = tune(),
	engine = "C5.0"
)

knn_cls_spec <- parsnip::nearest_neighbor(mode = "classification", neighbors = tune())

cls_tenth <- tailor::tailor() %>%
  tailor::adjust_probability_threshold(threshold = 1 / 10)

cls_post <- tailor::tailor() %>%
	tailor::adjust_probability_threshold(threshold = tune("cut"))

thresh_grid <- tibble::tibble(cut = (1:2) / 3)

cls_est_post <- tailor::tailor() %>%
	tailor::adjust_probability_calibration(method = "logistic")

cls_cal_tune_post <- tailor::tailor() %>%
  tailor::adjust_probability_calibration(method = tune()) %>%
  tailor::adjust_probability_threshold(threshold = tune("cut"))

cls_cal <- tailor::tailor() %>%
  tailor::adjust_probability_calibration()

fac_2c <- structure(integer(0), levels = c("Class1", "Class2"), class = "factor")
cls_two_class_plist <-
  tibble::tibble(
  Class = fac_2c,
  .pred_class = fac_2c,
  .pred_Class1 = double(0),
  .pred_Class2 = double(0),
  .row = integer(0),
)

sim_2c <- structure(integer(0), levels = c("class_1", "class_2"), class = "factor")
cls_sim_plist <-
  tibble::tibble(
    class = sim_2c,
    .pred_class = sim_2c,
    .pred_class_1 = double(0),
    .pred_class_2 = double(0),
    .row = integer(0),
  )

# ------------------------------------------------------------------------------

dt_grid <- tibble::tibble(min_n = c(2, 4))
knn_grid <- tibble::tibble(neighbors = 1:3)
svm_grid <- tibble::tibble(degree = 1:2)
lo_grid <- tibble::tibble(lo = c(1000, 2000))

make_post_data <- function(mode = "classification") {
  set.seed(1)
  if (mode == "classification") {
    dat <- modeldata::sim_classification(1000)
    nm <- "class"
  } else if (mode == "regression") {
    dat <- modeldata::sim_regression(1000)
    nm <- "outcome"
  } else {
    cli::abort("Only have modes for classification and regression so far")
  }
  rs <- rsample::mc_cv(dat, times = 2)
  rs_split <- rs$splits[[1]]
  rs_args <- rsample::.get_split_args(rs)
  list(data = dat, rs = rs, split = rs_split, args = rs_args, y = nm)
}

# ------------------------------------------------------------------------------

puromycin <- tibble::as_tibble(Puromycin)
puromycin_rec <- recipes::recipe(rate ~ ., data = puromycin) %>%
	recipes::step_dummy(state)

puromycin_tune_rec <- puromycin_rec %>%
	recipes::step_poly(conc, degree = tune())

knn_reg_spec <- parsnip::nearest_neighbor(mode = "regression", neighbors = tune())
svm_spec <- parsnip::svm_poly(mode = "regression", cost = 1, degree = tune())
svm_fixed_spec <- parsnip::svm_poly(mode = "regression", cost = 1, degree = 1)

reg_post <- tailor::tailor() %>%
	tailor::adjust_predictions_custom(.pred = .pred + 10000)

reg_cal <- tailor::tailor() %>%
  tailor::adjust_numeric_calibration()

reg_lower <- tailor::tailor() %>%
  tailor::adjust_numeric_range(lower_limit = tune("lo"))

glmn_spec <- parsnip::linear_reg(penalty = tune(), mixture = tune()) %>%
	parsnip::set_engine("glmnet")

reg_sim_plist <- tibble::tibble(
  outcome = double(0),
  .pred = double(0),
  .row = integer(0)
)

puromycin_plist <- tibble::tibble(
  rate = puromycin$rate[0],
  .pred = puromycin$rate[0],
  .row = integer(0)
)
