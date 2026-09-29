test_that("timeline loads and origin is a Monday", {
  tl <- bsc_timeline()
  expect_s3_class(tl, "bsc_timeline")
  expect_equal(format(tl$origin, "%u"), "1")
  expect_equal(tl$origin, as.Date("2020-01-06"))
})

test_that("week rules give the agreed anchors", {
  expect_equal(week_of("2020-01-06"), 1L)
  expect_equal(week_of("2020-01-12"), 1L)          # Sunday of week 1
  expect_equal(week_of("2020-01-13"), 2L)
  expect_equal(week_of("2020-08-01"), 30L)         # pilot start -> wID 30
  expect_equal(week_start(30L), as.Date("2020-07-27"))
  expect_equal(week_of("2021-05-03"), 70L)         # public launch Monday
  expect_equal(week_of("2021-05-01"), 69L)         # the Saturday before sits in 69
  expect_equal(first_week_on_or_after("2021-05-01"), 70L)
  expect_equal(first_week_on_or_after("2021-05-03"), 70L)
  expect_equal(week_of("2022-12-26"), 156L)
})

test_that("event_week and timeline_table agree", {
  tt <- timeline_table()
  expect_true(all(c("event", "date", "wID_containing", "wID_on_or_after") %in% names(tt)))
  expect_equal(event_week("public_launch"), 70L)
  expect_equal(event_week("pilot_start"), 30L)
  expect_equal(tt[event == "public_launch", wID_containing], 70L)
})

test_that("a non-Monday origin is rejected", {
  f <- tempfile(fileext = ".yml")
  writeLines(c("origin: 2020-01-07", "events:", "  public_launch: 2021-05-03", "horizon:", "  n_leads: 40"), f)
  expect_error(bsc_timeline(f), "Monday")
})
