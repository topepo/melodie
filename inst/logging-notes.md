---
title: "melodie logging infrastructure"
format: html
---

The looking code is set up in the `loopy()`, around the areas where external code is executed at:

- preprocessing
- model fitting
- prediction
- post-processing
- post-processing prediction
- extraction

The code follows the structure outlined below:

```r
current_wflow <- .catch_and_log(
  finalize_fit_pre(static$wflow, current_sched_pre, static)
)
if (has_log_notes(current_wflow)) {
 location <- glue::glue("preprocessor {iter_pre}/{num_iterations_pre}")
 notes <- append_log_notes(notes, current_wflow, location)
  catalog_log(notes)
 if (is_failure(current_wflow)) {
 next
 }
 current_wflow <- remove_log_notes(current_wflow)
}
```

The logging has 2 stages: catching and logging. The catching stage is done in `.catch_and_log()`, which evaluates the relevant fitting code. If the code throws a warning or error, it will be captured such that it can be logged.

If a warning or error is captured, then `has_log_notes()` triggers the logging stage. First, it appends the warnings and errors to the `notes` object to be returned in the `.notes` element. Next, the `catalog_log()` call makes it so the emitter updates. Giving the user updates on how many errors and warnings have been emitted. This is purely for printing/progress purposes and has nothing saved in the final object.

If an error was thrown, the current fitting iteration is skipped, and the next model/grid is done. This allows 1 model/grid point to fail while allowing the rest to fit.

`remove_log_notes()` scrubs the attached warnings away from the object so it will be clean when it moves on to the next step.
