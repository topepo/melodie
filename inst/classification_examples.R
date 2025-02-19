if (FALSE) {
  pak::pak(
    c(
      paste0("tidymodels/",
             c("agua", "baguette", "bonsai", "broom", "butcher", "censored",
               "dials", "discrim", "embed", "finetune", "hardhat",
               "infer", "modeldata", "modeldatatoo", "multilevelmod", "parsnip",
               "plsmod", "probably", "recipes", "rsample", "rules", "spatialsample",
               "stacks", "tailor", "textrecipes", "themis", "tune", "workflows",
               "workflowsets", "yardstick")
      ),
      paste0("tidyverse/", c("dplyr", "ggplot2", "purrr", "tibble", "tidyr"))
    ),
    ask = FALSE)

  library(future.mirai)
  plan(mirai_multisession)
}

# ------------------------------------------------------------------------------

library(tidymodels)
library(tailor)
library(discrim)
library(melodie)
library(rlang)
library(probably)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)
options(future.globals.maxSize = 1.0 * 1e9)

# ------------------------------------------------------------------------------

set.seed(1)
sim_tr <-
  sim_classification(1000, intercept = -8) %>%
  bind_cols(sim_noise(1000, 400))
sim_rs <- vfold_cv(sim_tr)

# ------------------------------------------------------------------------------

adjust_thresh_25 <-
  tailor() %>%
  adjust_probability_threshold(threshold = 0.25)

adjust_thresh_tune <-
  tailor() %>%
  adjust_probability_threshold(threshold = tune())

adjust_cal <-
  tailor() %>%
  adjust_probability_calibration(method = "isotonic_boot")

adjust_thresh_cal <-
  tailor() %>%
  adjust_probability_calibration(method = "isotonic_boot") %>%
  adjust_probability_threshold(threshold = tune())

mod_bst <- boost_tree(trees = tune(), min_n = tune(), mode = "classification")
mod_rf <- rand_forest(min_n = tune(), mode = "classification")
mod_nb <- naive_Bayes(smoothness = tune())

cls_mtr <- metric_set(j_index, sensitivity, specificity, brier_class, roc_auc)
cls_mtr <- metric_set(sensitivity, specificity, roc_auc)
cls_br_mtr <- metric_set(sensitivity, specificity, brier_class)

# ------------------------------------------------------------------------------

nb_wflow <- workflow(class ~ ., mod_nb)
nb_param <- nb_wflow  %>% extract_parameter_set_dials()
nb_reg <- nb_param %>% grid_regular(levels = 3)
set.seed(1)
nb_sfd <- tibble(smoothness = runif(20, max = 2))

nb_reg_res <-
  nb_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = nb_reg,
    control = control_grid(allow_par = TRUE),
    metrics = cls_mtr
  )

autoplot(nb_reg_res)

###

nb_sfd_res <-
  nb_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = nb_sfd,
    control = control_grid(allow_par = TRUE),
    metrics = cls_mtr
  )

autoplot(nb_sfd_res)

# ------------------------------------------------------------------------------

nb_25_wflow <- workflow(class ~ ., mod_nb, adjust_thresh_25)
nb_25_param <- nb_25_wflow  %>% extract_parameter_set_dials()
nb_25_reg <- nb_25_param %>% grid_regular(levels = 3)
set.seed(1)
nb_25_sfd <- tibble(smoothness = runif(20, max = 2))

nb_25_reg_res <-
  nb_25_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = nb_25_reg,
    control = control_grid(allow_par = TRUE),
    metrics = cls_mtr
  )

autoplot(nb_25_reg_res)

###

nb_25_sfd_res <-
  nb_25_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = nb_25_sfd,
    control = control_grid(allow_par = TRUE),
    metrics = cls_mtr
  )

autoplot(nb_25_sfd_res)

nb_25_sfd_res %>%
  collect_metrics() %>%
  filter(.metric %in% c("sensitivity", "specificity")) %>%
  select(smoothness, .metric, thresh_25 = mean) %>%
  full_join(
    nb_sfd_res %>%
      collect_metrics() %>%
      filter(.metric %in% c("sensitivity", "specificity")) %>%
      select(smoothness, .metric, thresh_50 = mean),
    by = c("smoothness", ".metric")
  ) %>%
  ggplot(aes(thresh_25, thresh_50)) +
  geom_point() +
  facet_wrap(~.metric)

# ------------------------------------------------------------------------------

nb_thresh_wflow <- workflow(class ~ ., naive_Bayes(), adjust_thresh_tune)
nb_thresh_param <- nb_thresh_wflow  %>% extract_parameter_set_dials()
nb_thresh_reg <- nb_thresh_param %>% grid_regular(levels = 3)
set.seed(1)
nb_thresh_sfd <- tibble(threshold = runif(20))

nb_thresh_reg_res <-
  nb_thresh_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = nb_thresh_reg,
    control = control_grid(allow_par = TRUE, save_pred = TRUE),
    metrics = cls_br_mtr
  )

autoplot(nb_thresh_reg_res)

collect_predictions(nb_thresh_reg_res) %>%
  ggplot(aes(.pred_class_1)) +
  geom_histogram(col = "white") +
  facet_grid(.config ~ class) +
  theme_bw()

collect_predictions(nb_thresh_reg_res) %>%
  cal_plot_windowed(truth = class, estimate = .pred_class_1)


###

nb_thresh_sfd_res <-
  nb_thresh_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = nb_thresh_sfd,
    control = control_grid(allow_par = TRUE),
    metrics = cls_br_mtr
  )

autoplot(nb_thresh_sfd_res)

# ------------------------------------------------------------------------------

nb_thresh_cal_wflow <- workflow(class ~ ., naive_Bayes(), adjust_thresh_cal)
nb_thresh_cal_param <- nb_thresh_cal_wflow  %>% extract_parameter_set_dials()
nb_thresh_cal_reg <- nb_thresh_cal_param %>% grid_regular(levels = 3)
set.seed(1)
nb_thresh_cal_sfd <- tibble(threshold = runif(20))

nb_thresh_cal_reg_res <-
  nb_thresh_cal_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = nb_thresh_cal_reg,
    control = control_grid(allow_par = TRUE, save_pred = TRUE),
    metrics = cls_br_mtr
  )

autoplot(nb_thresh_cal_reg_res)
collect_predictions(nb_thresh_cal_reg_res) %>%
  ggplot(aes(.pred_class_1)) +
  geom_histogram(col = "white") +
  facet_grid(.config ~ class) +
  theme_bw()

collect_predictions(nb_thresh_cal_reg_res) %>%
  cal_plot_windowed(truth = class, estimate = .pred_class_1)

###

nb_thresh_cal_sfd_res <-
  nb_thresh_cal_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = nb_thresh_cal_sfd,
    control = control_grid(allow_par = TRUE),
    metrics = cls_br_mtr
  )

autoplot(nb_thresh_cal_sfd_res)

nb_thresh_cal_sfd_res %>%
  collect_metrics() %>%
  # filter(.metric %in% c("sensitivity", "specificity")) %>%
  select(threshold, .metric, cal = mean) %>%
  full_join(
    nb_thresh_sfd_res %>%
      collect_metrics() %>%
      # filter(.metric %in% c("sensitivity", "specificity")) %>%
      select(threshold, .metric, uncal = mean),
    by = c("threshold", ".metric")
  ) %>%
  ggplot(aes(uncal, cal)) +
  geom_point() +
  facet_wrap(~.metric)
