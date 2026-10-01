with_root <- function(code) {
  root <- tempfile(); dir.create(root)
  old <- options(budgetsc.root = root); on.exit(options(old), add = TRUE)
  force(code)
}

test_that("registering a run creates the directory, spec files and registry row", {
  with_root({
    run <- run_register(spec_main())
    expect_s3_class(run, "bsc_run")
    expect_true(file.exists(file.path(run$dir, "spec.yml")))
    expect_true(file.exists(file.path(run$dir, "provenance.rds")))
    reg <- registry_read()
    expect_equal(nrow(reg), 1L)
    expect_equal(reg$run_id, run$id)
    # same spec again -> same run, no new row
    run2 <- run_register(spec_main())
    expect_equal(run2$id, run$id)
    expect_equal(nrow(registry_read()), 1L)
    # different spec -> new run
    s3 <- spec_main("sample.n_lags" = 23L); s3$name <- "main_L23"
    run3 <- run_register(s3)
    expect_equal(nrow(registry_read()), 2L)
    expect_equal(run_load(run3$id)$spec$sample$n_lags, 23L)
    expect_equal(run_load("main_later_adopters")$id, run$id)
  })
})

test_that("job table and worker process pending outcomes exactly once", {
  with_root({
    run <- run_register(spec_main("post.outcomes" = c("y1", "y2", "y3")))
    saveRDS(1, file.path(run$dir, "sc_fit.rds")); saveRDS(1, file.path(run$dir, "panel.rds"))
    jt <- job_table(run, "post")
    expect_equal(jt$state, rep("pending", 3))
    calls <- new.env(); calls$n <- 0L
    dummy <- function(run, job, lock) {
      calls$n <- calls$n + 1L
      saveRDS(list(job = job), file.path(run$dir, "post", paste0(job, ".rds")))
    }
    out <- worker(dummy, once = TRUE)
    expect_equal(nrow(out), 3L)
    expect_true(all(out$ok))
    expect_equal(calls$n, 3L)
    expect_true(stage_done(run, "post"))
    expect_equal(job_table(run, "post")$state, rep("done", 3))
    # nothing left: worker returns empty
    out2 <- worker(dummy, once = TRUE)
    expect_equal(nrow(out2), 0L)
    expect_equal(calls$n, 3L)
    st <- run_status()
    expect_equal(st$post_done, 3L)
    expect_equal(st$post_pending, 0L)
  })
})

test_that("a failing job is logged, not fatal, and stays pending", {
  with_root({
    run <- run_register(spec_main("post.outcomes" = c("bad", "good")))
    saveRDS(1, file.path(run$dir, "sc_fit.rds")); saveRDS(1, file.path(run$dir, "panel.rds"))
    f <- function(run, job, lock) {
      if (job == "bad") stop("sampler exploded")
      saveRDS(1, file.path(run$dir, "post", paste0(job, ".rds")))
    }
    out <- worker(f, once = TRUE)
    expect_equal(out[job == "bad", ok], FALSE)
    expect_equal(out[job == "good", ok], TRUE)
    expect_equal(job_table(run, "post")[job == "bad", state], "failed")
    expect_true(file.exists(file.path(run$dir, "post", "bad.failed")))
    expect_true(any(grepl("ERROR post/bad", readLines(file.path(run$dir, "run.log")))))
  })
})

test_that("run_clean removes intermediates but not the registry", {
  with_root({
    run <- run_register(spec_main())
    saveRDS(1, file.path(run$dir, "panel.rds"))
    saveRDS(1, file.path(run$dir, "sc_fit.rds"))
    rm <- run_clean(run)
    expect_equal(length(rm), 2L)
    expect_equal(nrow(registry_read()), 1L)
  })
})


test_that("post jobs are blocked until the SC stage has written its files", {
  with_root({
    run <- run_register(spec_main("post.outcomes" = c("y1", "y2")))
    expect_equal(job_table(run, "post")$state, rep("blocked", 2))
    expect_null(job_claim(run, "post", "y1"))
    # a worker serving only "post" does nothing for this run
    out <- worker(function(run, job, lock) stop("should not run"), stages = "post", once = TRUE)
    expect_equal(nrow(out), 0L)
    saveRDS(1, file.path(run$dir, "sc_fit.rds")); saveRDS(1, file.path(run$dir, "panel.rds"))
    expect_equal(job_table(run, "post")$state, rep("pending", 2))
    expect_s3_class(job_claim(run, "post", "y1"), "bsc_lock")
  })
})

test_that("job_reset_failed makes failed jobs pending again", {
  with_root({
    run <- run_register(spec_main("post.outcomes" = c("bad", "good")))
    saveRDS(1, file.path(run$dir, "sc_fit.rds")); saveRDS(1, file.path(run$dir, "panel.rds"))
    f <- function(run, job, lock) { if (job == "bad") stop("boom"); saveRDS(1, file.path(run$dir, "post", paste0(job, ".rds"))) }
    worker(f, once = TRUE)
    expect_equal(job_table(run, "post")[job == "bad", state], "failed")
    expect_equal(run_status()$post_failed, 1L)
    job_reset_failed(run)
    expect_equal(job_table(run, "post")[job == "bad", state], "pending")
  })
})
