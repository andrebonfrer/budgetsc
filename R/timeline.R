# timeline.R ------------------------------------------------------------------
# The calendar is the one thing every stage must agree on. This file reads
# inst/timeline.yml once, exposes the anchors, and provides the two week rules:
#
#   week_of(date)                 the Monday-week CONTAINING a date
#                                 (how minBudgetDate becomes a treatment week)
#   first_week_on_or_after(date)  the first Monday-week starting on or after a
#                                 date (what the old launch.wID code computed)
#
# They differ only for dates that are not Mondays. Nothing else in the package
# may compute a wID by hand.

.tl_env <- new.env(parent = emptyenv())

#' Path to the timeline file in use
#'
#' Resolution order: option `budgetsc.timeline`, then the installed
#' `inst/timeline.yml`, then `inst/timeline.yml` relative to the working
#' directory (development).
#' @return Character path.
#' @export
timeline_path <- function() {
  p <- getOption("budgetsc.timeline")
  if (!is.null(p)) return(p)
  p <- system.file("timeline.yml", package = "budgetsc")
  if (nzchar(p)) return(p)
  if (file.exists("inst/timeline.yml")) return("inst/timeline.yml")
  stop("timeline.yml not found; set options(budgetsc.timeline = <path>).", call. = FALSE)
}

#' Load the study timeline
#'
#' @param path Optional path to a timeline YAML; default [timeline_path()].
#' @param refresh Logical; re-read even if cached. Default FALSE.
#' @return A list of class `bsc_timeline` with elements `origin` (Date),
#'   `events` (named list of Dates), `horizon` (named list of integers) and
#'   `path`.
#' @export
bsc_timeline <- function(path = NULL, refresh = FALSE) {
  path <- path %||% timeline_path()
  key <- normalizePath(path, mustWork = TRUE)
  if (!refresh && !is.null(.tl_env[[key]])) return(.tl_env[[key]])

  raw <- yaml::read_yaml(path)
  origin <- as.Date(raw$origin)
  if (is.na(origin)) stop("timeline: `origin` is not a valid date.", call. = FALSE)
  if (format(origin, "%u") != "1")
    stop("timeline: `origin` must be a Monday (got ", format(origin, "%A"), ").", call. = FALSE)

  events <- lapply(raw$events, as.Date)
  bad <- names(events)[vapply(events, is.na, logical(1))]
  if (length(bad)) stop("timeline: invalid date for event(s): ", paste(bad, collapse = ", "), call. = FALSE)

  horizon <- lapply(raw$horizon, as.integer)

  tl <- structure(list(origin = origin, events = events, horizon = horizon, path = key),
                  class = "bsc_timeline")
  .tl_env[[key]] <- tl
  tl
}

#' @export
print.bsc_timeline <- function(x, ...) {
  cat("budgetsc timeline (", x$path, ")\n", sep = "")
  print(timeline_table(x))
  cat("horizon: ", paste(names(x$horizon), unlist(x$horizon), sep = " = ", collapse = ", "), "\n")
  invisible(x)
}

#' Week index of the Monday-week containing a date
#'
#' `wID = floor((date - origin) / 7) + 1`, so the origin week is 1.
#' @param date Date or character coercible to Date.
#' @param origin Date; default the timeline origin.
#' @return Integer vector.
#' @export
week_of <- function(date, origin = NULL) {
  origin <- origin %||% bsc_timeline()$origin
  as.integer(floor(as.numeric(as.Date(date) - as.Date(origin)) / 7)) + 1L
}

#' Index of the first Monday-week starting on or after a date
#'
#' Equals [week_of()] for Mondays and `week_of() + 1` otherwise. This is what
#' `bdata[tdate >= launch.date, min(wID)]` computed in the original scripts.
#' @inheritParams week_of
#' @return Integer vector.
#' @export
first_week_on_or_after <- function(date, origin = NULL) {
  origin <- origin %||% bsc_timeline()$origin
  d <- as.Date(date)
  w <- week_of(d, origin)
  is_monday <- format(d, "%u") == "1"
  as.integer(ifelse(is_monday, w, w + 1L))
}

#' Monday on which a week index starts
#' @param wID Integer vector.
#' @inheritParams week_of
#' @return Date vector.
#' @export
week_start <- function(wID, origin = NULL) {
  origin <- origin %||% bsc_timeline()$origin
  as.Date(origin) + (as.integer(wID) - 1L) * 7L
}

#' Week index of a named timeline event
#'
#' @param event Event name in `timeline.yml` (e.g. `"public_launch"`).
#' @param rule `"containing"` (default, [week_of()]) or `"on_or_after"`
#'   ([first_week_on_or_after()]).
#' @param tl A `bsc_timeline`; default [bsc_timeline()].
#' @return Integer scalar.
#' @export
event_week <- function(event, rule = c("containing", "on_or_after"), tl = NULL) {
  rule <- match.arg(rule)
  tl <- tl %||% bsc_timeline()
  d <- tl$events[[event]]
  if (is.null(d)) stop("timeline: unknown event '", event, "'.", call. = FALSE)
  if (rule == "containing") week_of(d, tl$origin) else first_week_on_or_after(d, tl$origin)
}

#' Table of timeline events with both week rules
#' @param tl A `bsc_timeline`; default [bsc_timeline()].
#' @return data.table with `event`, `date`, `weekday`, `wID_containing`,
#'   `wID_on_or_after`, `week_starts`.
#' @export
timeline_table <- function(tl = NULL) {
  tl <- tl %||% bsc_timeline()
  d <- as.Date(unlist(lapply(tl$events, format, "%Y-%m-%d")))
  data.table::data.table(
    event           = names(tl$events),
    date            = d,
    weekday         = format(d, "%a"),
    wID_containing  = week_of(d, tl$origin),
    wID_on_or_after = first_week_on_or_after(d, tl$origin),
    week_starts     = week_start(week_of(d, tl$origin), tl$origin)
  )
}
