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

melodie_env <-
  rlang::new_environment(
    data = list(
      progress_env = NULL,
      progress_active = FALSE,
      progress_catalog = NULL
    )
  )

catalog_log <- function(x) {
  catalog <- rlang::env_get(melodie_env, "progress_catalog")

  if (x$note %in% catalog$note) {
    idx <- match(x$note, catalog$note)
    catalog$n[idx] <- catalog$n[idx] + 1
  } else {
    new_id <- nrow(catalog) + 1
    catalog <- tibble::add_row(
      catalog,
      tibble::tibble(
        type = x$type,
        note = x$note,
        n = 1,
        id = new_id
      )
    )

    # construct issue summary
    color <- if (x$type == "warning") cli::col_yellow else cli::col_red
    # pad by nchar(label) + nchar("warning") + additional spaces and symbols
    pad <- nchar(x$note) + 14L
    justify <- paste0("\n", strrep("\u00a0", pad))
    note <- gsub("\n", justify, x$note)
    # pad `nchar("warning") - nchar("error")` spaces to the right of the `:`
    if (x$type == "error") {
      note <- paste0("\u00a0\u00a0", note)
    }
    msg <- glue::glue(
      "{color(cli::style_bold(LETTERS[new_id]))} | {color(x$type)}: {x$note}"
    )
    cli::cli_alert(msg)
  }

  rlang::env_bind(melodie_env, progress_catalog = catalog)

  return(NULL)
}

initialize_catalog <- function(env = rlang::caller_env()) {
  catalog <-
    tibble::new_tibble(
      list(
        type = character(0),
        note = character(0),
        n = numeric(0),
        id = numeric(0)
      ),
      nrow = 0
    )

  rlang::env_bind(melodie_env, progress_catalog = catalog)
  withr::defer(
    rlang::env_bind(melodie_env, progress_catalog = NULL),
    envir = env
  )
}
