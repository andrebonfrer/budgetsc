# locks.R ---------------------------------------------------------------------
# Directory locks for a shared NFSv4 filesystem.
#
# Why directories: mkdir is atomic across hosts on POSIX filesystems including
# NFS, whereas file.create() is not, and advisory fcntl locks (package
# filelock) are unreliable over NFS. A lock is a directory
#   <dir>/<name>.lock/
#     owner       host:pid, start time, R session id
#     heartbeat   rewritten by lock_heartbeat(); its mtime is the liveness signal
#
# Stale takeover: a lock whose heartbeat is older than `stale_after` is
# considered abandoned (a crashed VM). To avoid two reclaimers racing, the
# reclaimer first renames the stale directory to a unique name (rename is
# atomic; only one process can win) and only then creates a fresh lock.

#' Path of a lock directory
#' @param dir Directory holding the lock.
#' @param name Lock name (without `.lock`).
#' @return Character path.
#' @export
lock_path <- function(dir, name) file.path(dir, paste0(name, ".lock"))

.lock_write_owner <- function(path, note = "") {
  writeLines(c(
    paste0("owner=", host_id()),
    paste0("started=", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("note=", note)
  ), file.path(path, "owner"))
  .lock_touch(path)
}

.lock_touch <- function(path) {
  writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), file.path(path, "heartbeat"))
}

#' Age of a lock's heartbeat in seconds
#' @param dir Directory holding the lock.
#' @param name Lock name (without `.lock`).
#' @return Numeric seconds, or `NA` if no lock exists.
#' @export
lock_age <- function(dir, name) {
  hb <- file.path(lock_path(dir, name), "heartbeat")
  if (!file.exists(hb)) {
    p <- lock_path(dir, name)
    if (!dir.exists(p)) return(NA_real_)
    # lock dir exists without heartbeat (crashed between mkdir and write): use dir mtime
    return(as.numeric(difftime(Sys.time(), file.info(p)$mtime, units = "secs")))
  }
  as.numeric(difftime(Sys.time(), file.info(hb)$mtime, units = "secs"))
}

#' Is a lock stale?
#' @inheritParams lock_age
#' @param stale_after Seconds without a heartbeat after which a lock may be
#'   reclaimed. Default 3 hours.
#' @return Logical; FALSE if no lock exists.
#' @export
lock_is_stale <- function(dir, name, stale_after = 3 * 3600) {
  a <- lock_age(dir, name)
  !is.na(a) && a > stale_after
}

#' Acquire a directory lock
#'
#' @inheritParams lock_is_stale
#' @param wait Logical; poll until acquired (or `timeout`). Default FALSE.
#' @param poll Seconds between polls when waiting. Default 5.
#' @param timeout Seconds to wait before giving up. Default `Inf`.
#' @param note Free text stored in the owner file.
#' @return A `bsc_lock` object, or `NULL` if the lock could not be acquired.
#' @export
lock_acquire <- function(dir, name, stale_after = 3 * 3600, wait = FALSE,
                         poll = 5, timeout = Inf, note = "") {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- lock_path(dir, name)
  t0 <- Sys.time()
  repeat {
    if (dir.create(path, showWarnings = FALSE)) {
      .lock_write_owner(path, note)
      return(structure(list(dir = dir, name = name, path = path, owner = host_id(),
                            acquired = Sys.time()), class = "bsc_lock"))
    }
    if (lock_is_stale(dir, name, stale_after)) {
      stale_name <- paste0(path, ".stale.", format(Sys.time(), "%Y%m%d%H%M%S"), ".",
                           sample.int(1e6, 1))
      if (file.rename(path, stale_name)) {        # atomic: only one reclaimer wins
        unlink(stale_name, recursive = TRUE)
        next                                      # retry the mkdir immediately
      }
    }
    if (!wait || as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) return(NULL)
    Sys.sleep(poll)
  }
}

#' Refresh a lock's heartbeat
#' @param lock A `bsc_lock`.
#' @return The lock, invisibly.
#' @export
lock_heartbeat <- function(lock) {
  stopifnot(inherits(lock, "bsc_lock"))
  if (dir.exists(lock$path)) .lock_touch(lock$path)
  invisible(lock)
}

#' Release a lock
#' @param lock A `bsc_lock`.
#' @return TRUE invisibly (also when the lock was already gone).
#' @export
lock_release <- function(lock) {
  stopifnot(inherits(lock, "bsc_lock"))
  if (dir.exists(lock$path)) unlink(lock$path, recursive = TRUE)
  invisible(TRUE)
}

#' Inspect a lock
#' @inheritParams lock_age
#' @return `NULL` if no lock; otherwise a list with `owner`, `started`,
#'   `note`, `age_secs`.
#' @export
lock_info <- function(dir, name) {
  path <- lock_path(dir, name)
  if (!dir.exists(path)) return(NULL)
  own <- file.path(path, "owner")
  kv <- if (file.exists(own)) readLines(own, warn = FALSE) else character(0)
  get <- function(k) { v <- sub(paste0("^", k, "="), "", kv[startsWith(kv, paste0(k, "="))]); if (length(v)) v[1] else NA_character_ }
  list(owner = get("owner"), started = get("started"), note = get("note"),
       age_secs = lock_age(dir, name))
}

#' Evaluate an expression while holding a lock
#'
#' The lock is released on exit, including on error. If the lock cannot be
#' acquired the expression is not run and `NULL` is returned (or an error is
#' raised when `must = TRUE`).
#' @inheritParams lock_acquire
#' @param expr Expression to evaluate.
#' @param must Logical; error if the lock is unavailable. Default FALSE.
#' @return Value of `expr`, or `NULL`.
#' @export
with_lock <- function(dir, name, expr, stale_after = 3 * 3600, wait = FALSE,
                      poll = 5, timeout = Inf, must = FALSE, note = "") {
  lk <- lock_acquire(dir, name, stale_after = stale_after, wait = wait,
                     poll = poll, timeout = timeout, note = note)
  if (is.null(lk)) {
    if (must) stop("Could not acquire lock '", name, "' in ", dir, call. = FALSE)
    return(NULL)
  }
  on.exit(lock_release(lk), add = TRUE)
  force(expr)
}

#' @export
print.bsc_lock <- function(x, ...) {
  cat("bsc_lock '", x$name, "' held by ", x$owner, " since ",
      format(x$acquired, "%H:%M:%S"), "\n  ", x$path, "\n", sep = "")
  invisible(x)
}
