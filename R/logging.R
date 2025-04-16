.catch_and_log <- function(.expr) {
  tmp <- catcher(.expr)
  tmp$res
}

catcher <- function(expr) {
  signals <- list()
  add_cond <- function(cnd) {
    signals <<- append(signals, list(cnd))
    rlang::cnd_muffle(cnd)
  }
  res <- try(
    withCallingHandlers(warning = add_cond, expr), 
    silent = TRUE
  )
  list(res = res, signals = signals)
}

is_failure <- function(x) {
  inherits(x, "try-error")
}
