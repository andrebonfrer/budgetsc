test_that("default spec validates and prints", {
  s <- spec_default()
  expect_s3_class(s, "bsc_spec")
  expect_silent(spec_validate(s))
  expect_output(print(s), "budgetsc spec")
})

test_that("spec_id ignores name and notes but not content", {
  a <- spec_main()
  b <- spec_main(); b$name <- "other"; b$notes <- "x"
  c <- spec_main("sample.n_lags" = 23L)
  expect_equal(spec_id(a), spec_id(b))
  expect_false(spec_id(a) == spec_id(c))
  expect_equal(nchar(spec_id(a)), 10L)
  expect_equal(nchar(spec_id(a, full = TRUE)), 40L)
})

test_that("spec_id is order-invariant", {
  a <- spec_main()
  b <- a; b$sc <- rev(b$sc)
  expect_equal(spec_id(a), spec_id(b))
})

test_that("spec_modify uses dotted paths and validates", {
  s <- spec_modify(spec_main(), "sc.match_end" = 4L, "post.gibbs.n_iter" = 500L, "post.gibbs.burn_in" = 200L)
  expect_equal(s$sc$match_end, 4L)
  expect_equal(s$post$gibbs$n_iter, 500L)
  expect_error(spec_modify(spec_main(), "sample.design" = "bogus"), "sample.design")
  expect_error(spec_modify(spec_main(), "post.gibbs.burn_in" = 5000L), "burn_in")
  expect_error(spec_modify(spec_main(), "sc.match_end" = 99L), "match_end")
})

test_that("YAML round trip preserves the id", {
  s <- spec_main("sample.n_lags" = 23L)
  f <- tempfile(fileext = ".yml")
  spec_write(s, f)
  s2 <- spec_read(f)
  expect_equal(spec_id(s), spec_id(s2))
  expect_equal(s2$sample$n_lags, 23L)
})

test_that("pilot spec uses pilot horizon", {
  p <- spec_pilot()
  expect_equal(p$sample$cohort, "pilot")
  expect_equal(p$sample$n_lags, bsc_timeline()$horizon$n_lags_pilot)
})
