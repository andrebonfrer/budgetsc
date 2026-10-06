# utils.R ---------------------------------------------------------------------
# Small helpers shared across the package. Nothing here touches the filesystem
# except bsc_log(), which appends to a run's log.

#' Winsorise a numeric vector at quantiles
#'
#' Same behaviour as the original `winsorise()` in the R0 scripts: values below
#' the lower quantile are raised to it, values above the upper quantile are
#' lowered to it.
#'
#' @param x Numeric vector.
#' @param probs Length-2 numeric in \[0, 1\]. Default `c(0.01, 0.99)`.
#' @param na.rm Logical. Default TRUE.
#' @return Numeric vector of the same length as `x`.
#' @export
winsorise <- function(x, probs = c(0.01, 0.99), na.rm = TRUE) {
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  if (length(probs) != 2L || any(probs < 0 | probs > 1))
    stop("`probs` must be two values in [0, 1].", call. = FALSE)
  qr <- stats::quantile(x, probs = probs, na.rm = na.rm, names = FALSE)
  pmin(pmax(x, qr[1]), qr[2])
}


# Formula strings come from specs, and R's as.formula() gives an unhelpful
# "attempt to set an attribute on NULL" for a string that is a call but has no
# leading `~` (for example a right-hand side on its own). Check first and say
# what is wrong, quoting the offending text.
.as_formula_checked <- function(x, what = "formula") {
  bad <- function(why) stop(sprintf("%s is not a valid formula (%s). Got: %s", what, why,
                                    paste(deparse(x), collapse = " ")), call. = FALSE)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
    bad("it must be a single non-empty string")
  ff <- tryCatch(str2lang(x), error = function(e) bad(paste("it does not parse:", conditionMessage(e))))
  if (!(is.call(ff) && is.symbol(ff[[1L]]) && identical(as.character(ff[[1L]]), "~")))
    bad("it must contain a '~', for example 'budgetdummy ~ age + income_mean' or 'budgetdummy ~ 1'")
  stats::as.formula(x)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Identify the machine running this session
#'
#' Used in lock owner files and logs so that a stale lock can be traced to a
#' host and process. The host part is the environment variable
#' `BUDGETSC_HOST_LABEL` when set (e.g. `vm1` in that machine's `~/.Renviron`),
#' otherwise the system nodename.
#' @return Character scalar `"<label-or-hostname>:<pid>"`.
#' @export
host_id <- function() {
  lab <- Sys.getenv("BUDGETSC_HOST_LABEL", unset = "")
  paste0(if (nzchar(lab)) lab else Sys.info()[["nodename"]], ":", Sys.getpid())
}

#' Append a timestamped line to a run's log
#'
#' @param run A `bsc_run` object (see [run_register()]), or a directory path.
#' @param ... Character pieces pasted together as the message.
#' @param file Log file name inside the run directory. Default `"run.log"`.
#' @return Invisibly, the line written.
#' @export
bsc_log <- function(run, ..., file = "run.log") {
  dir <- if (inherits(run, "bsc_run")) run$dir else run
  line <- sprintf("%s | %s | %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  host_id(), paste0(..., collapse = ""))
  cat(line, "\n", file = file.path(dir, file), append = TRUE, sep = "")
  invisible(line)
}

# Canonical form for hashing: recursively sorts names, drops NULL/empty
# elements, converts integers to doubles and dates to strings, and turns
# unnamed lists of scalars into vectors. Result: a spec hashes identically
# before and after a YAML round trip (YAML does not preserve integer vs
# double or character(0) vs list()).
.canonicalise <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (is.list(x)) {
    x <- lapply(x, .canonicalise)
    x <- x[!vapply(x, is.null, logical(1))]
    if (!length(x)) return(NULL)
    if (!is.null(names(x)) && all(nzchar(names(x)))) {
      x <- x[order(names(x))]
    } else if (all(vapply(x, function(e) is.atomic(e) && length(e) == 1L, logical(1)))) {
      x <- unlist(x, use.names = FALSE)
      if (is.numeric(x)) x <- as.numeric(x)
    }
    return(x)
  }
  if (is.numeric(x)) return(as.numeric(x))
  x
}

# Set a value in a nested list using a dotted path, e.g. "sample.n_lags".
.set_dotted <- function(lst, path, value) {
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]
  if (length(parts) == 1L) { lst[[parts]] <- value; return(lst) }
  head <- parts[1]
  if (is.null(lst[[head]])) lst[[head]] <- list()
  lst[[head]] <- .set_dotted(lst[[head]], paste(parts[-1], collapse = "."), value)
  lst
}

.get_dotted <- function(lst, path) {
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]
  for (p in parts) {
    if (is.null(lst[[p]])) return(NULL)
    lst <- lst[[p]]
  }
  lst
}

utils::globalVariables(c(".", ".N", ".SD", "wID", "customer_id", "role", "adopt_wID",
                         "week_start", "status", "run_id", "name", "created", "host",
                         "spec_hash", "D", "hazard", "u", "S", "idx", "lat", "cum"))
