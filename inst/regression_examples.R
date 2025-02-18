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
library(melodie)
library(rlang)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

set.seed(1)
sim_tr <- sim_regression(500)
sim_cal <- sim_regression(100)
sim_rs <- vfold_cv(sim_tr)

# ------------------------------------------------------------------------------

rec <-
  recipe(outcome ~ ., data = sim_tr) %>%
  step_spline_natural(predictor_01, deg_free = tune("df_1"))

adjust_min_zero <-
  tailor::tailor() %>%
  tailor::adjust_numeric_range(lower_limit = 0)

adjust_min_tune <-
  tailor::tailor() %>%
  tailor::adjust_numeric_range(lower_limit = tune())

adjust_cal <-
  tailor::tailor() %>%
  tailor::adjust_numeric_calibration(method = "linear")

mod_bst <- parsnip::boost_tree(trees = tune(), min_n = tune(), mode = "regression")
mod_rf <- parsnip::rand_forest(min_n = tune(), mode = "regression")

reg_mtr <- metric_set(rmse, rsq, mae)

# ------------------------------------------------------------------------------

pre_mod_wflow <- workflow(rec, mod_rf)
pre_mod_param <- pre_mod_wflow  %>% extract_parameter_set_dials()
pre_mod_reg <- pre_mod_param %>% grid_regular(levels = 3)
pre_mod_sfd <- pre_mod_param %>% grid_space_filling(size = 10)

pre_submod_wflow <- workflow(rec, mod_bst)
pre_submod_param <- pre_submod_wflow  %>% extract_parameter_set_dials()
pre_submod_reg <- pre_submod_param %>% grid_regular(levels = 3)
pre_submod_sfd <- pre_submod_param %>% grid_space_filling(size = 10)

pre_submod_min_wflow <- workflow(rec, mod_bst, adjust_min_zero)
pre_submod_min_param <- pre_submod_min_wflow  %>% extract_parameter_set_dials()
pre_submod_min_reg <- pre_submod_min_param %>% grid_regular(levels = 3)
pre_submod_min_sfd <- pre_submod_min_param %>% grid_space_filling(size = 10)

pre_submod_tune_wflow <- workflow(rec, mod_bst, adjust_min_tune)
pre_submod_tune_param <-
  pre_submod_tune_wflow %>%
  extract_parameter_set_dials() %>%
  update(lower_limit = lower_limit(c(-1, 2)))
pre_submod_tune_reg <- pre_submod_tune_param %>% grid_regular(levels = 3)
pre_submod_tune_sfd <- pre_submod_tune_param %>% grid_space_filling(size = 10)

pre_submod_cal_wflow <- workflow(rec, mod_bst, adjust_cal)
pre_submod_cal_param <- pre_submod_cal_wflow  %>% extract_parameter_set_dials()
pre_submod_cal_reg <- pre_submod_cal_param %>% grid_regular(levels = 3)
pre_submod_cal_sfd <- pre_submod_cal_param %>% grid_space_filling(size = 10)


# ------------------------------------------------------------------------------
# no submodels

pre_mod_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = pre_mod_reg
  )

pre_mod_res <-
  pre_mod_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = pre_mod_reg,
    control = control_grid(parallel_over = "everything", save_pred = TRUE)
  )

# ------------------------------------------------------------------------------
# Submodels via boosting

bst_reg_tune <-
  pre_submod_wflow %>%
  tune::tune_grid(
    resamples = sim_rs,
    grid = pre_submod_reg
  )

bst_reg <-
  pre_submod_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = pre_submod_reg,
    control = control_grid(parallel_over = "everything", save_pred = TRUE, allow_par = FALSE)
  )

bst_sfd <-
  pre_submod_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = pre_submod_sfd,
    control = control_grid(parallel_over = "everything", save_pred = TRUE, allow_par = FALSE)
  )

bst_reg_tune %>%
  filter(id == "Fold01") %>%
  pluck(".metrics") %>%
  pluck(1)%>%
  inner_join(pre_submod_reg[2,])

bst_reg %>%
  filter(id == "Fold01") %>%
  pluck(".metrics") %>%
  pluck(1)%>%
  inner_join(pre_submod_reg[2,])


# ------------------------------------------------------------------------------
# Submodels via boosting with simple postprocessor

bst_min_reg <-
  pre_submod_min_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = pre_submod_min_reg,
    control = control_grid(save_pred = TRUE)
  )

bst_min_reg %>%
  filter(id == "Fold01") %>%
  pluck(".metrics") %>%
  pluck(1)%>%
  inner_join(pre_submod_reg[2,])

bst_reg %>%
  filter(id == "Fold01") %>%
  pluck(".metrics") %>%
  pluck(1)%>%
  inner_join(pre_submod_reg[2,])

bst_sfd <-
  pre_submod_min_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = pre_submod_min_sfd,
    control = control_grid(parallel_over = "everything", save_pred = TRUE)
  )

# ------------------------------------------------------------------------------
# Submodels via boosting with tunable postprocessor

bst_tune_reg <-
  pre_submod_tune_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = pre_submod_tune_reg,
    control = control_grid(allow_par = FALSE)
  )


# ------------------------------------------------------------------------------
# Submodels via boosting with simple calibrator

bst_tune_reg <-
  pre_submod_cal_wflow %>%
  melodie_grid(
    resamples = sim_rs,
    grid = pre_submod_cal_reg,
    control = control_grid(allow_par = FALSE)
  )


