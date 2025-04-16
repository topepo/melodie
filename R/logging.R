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

has_log_notes <- function(x) {
  is_failure(x)
  #"notes" %in% names(attributes(x))
}

append_log_notes <- function(notes, x, location) {
  if (is_failure(x)) {
    note <- attr(x, "condition")$message
  } else {
    note <- attr(x, "notes")
  }
  tibble::add_row(
    notes, 
    location = location,
    type = "error",
    note = note
  )
}

remove_log_notes <- function(x) {
  attr(x, "notes") <- NULL
  x
}