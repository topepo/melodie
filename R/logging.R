.catch_and_log <- function(.expr) {
  tmp <- catcher(.expr)
  tmp
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
  attr(res, "notes") <- signals
  res
}

is_failure <- function(x) {
  inherits(x, "try-error")
}

has_log_notes <- function(x) {
  is_failure(x) || NROW(attr(x, "notes")) > 0
}

append_log_notes <- function(notes, x, location) {
  if (is_failure(x)) {
    type <- "error"
    x <- attr(x, 'condition')
    note <- conditionMessage(x)
  } else {
    type <- "warning"
    note <- attr(x, "notes")
    note <- note[[1]]$message
  }
  tibble::add_row(
    notes,
    location = unclass(location),
    type = type,
    note = note
  )
}

remove_log_notes <- function(x) {
  attr(x, "notes") <- NULL
  x
}
