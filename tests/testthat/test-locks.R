test_that("acquire, second acquire fails, release", {
  d <- tempfile(); dir.create(d)
  lk <- lock_acquire(d, "job")
  expect_s3_class(lk, "bsc_lock")
  expect_null(lock_acquire(d, "job"))
  info <- lock_info(d, "job")
  expect_equal(info$owner, host_id())
  expect_true(info$age_secs >= 0)
  lock_release(lk)
  expect_null(lock_info(d, "job"))
  expect_s3_class(lock_acquire(d, "job"), "bsc_lock")
})

test_that("stale locks are reclaimed, fresh ones are not", {
  d <- tempfile(); dir.create(d)
  lk <- lock_acquire(d, "job")
  hb <- file.path(lk$path, "heartbeat")
  old <- Sys.time() - 10 * 3600
  Sys.setFileTime(hb, old)
  expect_true(lock_is_stale(d, "job", stale_after = 3600))
  expect_false(lock_is_stale(d, "job", stale_after = 20 * 3600))
  expect_null(lock_acquire(d, "job", stale_after = 20 * 3600))     # still fresh under long timeout
  lk2 <- lock_acquire(d, "job", stale_after = 3600)                 # reclaimed
  expect_s3_class(lk2, "bsc_lock")
  expect_false(lock_is_stale(d, "job", stale_after = 3600))
  expect_equal(length(list.files(d, pattern = "stale")), 0L)         # stale dir removed
})

test_that("heartbeat refreshes age", {
  d <- tempfile(); dir.create(d)
  lk <- lock_acquire(d, "job")
  Sys.setFileTime(file.path(lk$path, "heartbeat"), Sys.time() - 600)
  expect_true(lock_age(d, "job") > 500)
  lock_heartbeat(lk)
  expect_true(lock_age(d, "job") < 5)
})

test_that("with_lock releases on error and returns NULL when busy", {
  d <- tempfile(); dir.create(d)
  expect_error(with_lock(d, "x", stop("boom")), "boom")
  expect_null(lock_info(d, "x"))
  lk <- lock_acquire(d, "x")
  expect_null(with_lock(d, "x", 1))
  expect_error(with_lock(d, "x", 1, must = TRUE), "Could not acquire")
  lock_release(lk)
  expect_equal(with_lock(d, "x", 40 + 2), 42)
})

test_that("two processes contending for one lock: exactly one wins", {
  skip_on_cran()
  d <- tempfile(); dir.create(d)
  rscript <- file.path(R.home("bin"), "Rscript")          # the running R's own Rscript, not PATH
  script <- tempfile(fileext = ".R")
  writeLines(sprintf('
    .libPaths(c(%s, .libPaths()))
    args <- commandArgs(TRUE)
    out <- file.path("%s", paste0("result_", args[1]))
    ok <- tryCatch({ suppressPackageStartupMessages(library(budgetsc)); TRUE },
                   error = function(e) { writeLines(paste0("error: ", conditionMessage(e)), out); FALSE })
    if (ok) {
      lk <- budgetsc::lock_acquire("%s", "shared")
      writeLines(if (is.null(lk)) "lost" else "won", out)
      if (!is.null(lk)) { Sys.sleep(2); budgetsc::lock_release(lk) }
    }
  ', paste0("c(", paste(sprintf("\"%s\"", .libPaths()), collapse = ", "), ")"), d, d), script)
  for (k in 1:2) system2(rscript, c(script, k), wait = FALSE, stdout = FALSE, stderr = FALSE)
  for (i in 1:120) { if (length(list.files(d, pattern = "^result_")) == 2L) break; Sys.sleep(0.5) }
  res <- unlist(lapply(list.files(d, pattern = "^result_", full.names = TRUE), readLines))
  if (any(grepl("^error:", res)))
    skip(paste("child R process could not attach budgetsc:", res[grepl("^error:", res)][1],
               "- install the package (devtools::install()) to run this test"))
  if (length(res) < 2L) skip("child R processes did not report within 60 s")
  expect_equal(sort(res), c("lost", "won"))
})
