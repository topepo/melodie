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
}

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
sim_rs <- vfold_cv(sim_tr)

# ------------------------------------------------------------------------------

rec <-
  recipe(outcome ~ ., data = sim_tr) %>%
  step_spline_natural(predictor_01, deg_free = tune("df_1"))

adjust_min_zero <-
  tailor::tailor() %>%
  tailor::adjust_numeric_range(lower_limit = 0)

adjust_min_tune<-
  tailor::tailor() %>%
  tailor::adjust_numeric_range(lower_limit = tune())

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

# ------------------------------------------------------------------------------

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


