library(parsnip)
library(workflows)
library(tune)
library(rsample)
library(recipes)
library(yardstick)
library(dplyr)
library(rlang)

dt_spec <- parsnip::decision_tree(
	mode = "classification",
	min_n = tune(),
	engine = "C5.0"
)
dt_grid <- tibble::tibble(min_n = c(2, 4))
