test_that("real model releases do not break table freshness assessments", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")
  skip_if_not_installed("dm")
  root <- withr::local_tempdir()
  lake <- dataraft.lake::dr_open_lake(root, backend = "duckdb")
  withr::defer(dataraft.lake::dr_close_lake(lake))
  contract <- dataraft.core::dr_contract(
    "orders.contract",
    columns = c(id = "integer"),
    max_age_hours = 24
  )
  dataraft.core::dr_publish(
    dataraft.core::dr_product(
      "orders",
      data.frame(id = 1:3),
      contract = contract
    ),
    to = lake
  )
  dataraft.core::dr_publish(
    dataraft.core::dr_product(
      "portfolio",
      dm::dm(customers = data.frame(id = 1:2))
    ),
    to = lake
  )

  summary <- dr_freshness(lake, at = Sys.time() + 3600)
  expect_setequal(
    summary$asset,
    c("orders", "portfolio.customers", "portfolio")
  )
  table <- summary[summary$asset == "orders", ]
  model <- summary[summary$asset == "portfolio", ]
  expect_identical(table$freshness, "current")
  expect_equal(table$max_age_hours, 24)
  expect_identical(model$freshness, "unknown")
  expect_identical(model$max_age_hours, NA_real_)
  expect_identical(model$published_quality, "passed")
})

test_that("unversioned and missing references cannot decode NA registry rows", {
  snapshot <- list(
    releases = data.frame(
      asset = c("model", "missing", "table"),
      release_id = c("one", "two", "three"),
      release_order = 1:3,
      published_at = "2026-01-01T00:00:00Z",
      contract = c("model-signature", NA_character_, "orders@1"),
      quality = "passed"
    ),
    runs = data.frame(
      asset = character(),
      started_at = character(),
      status = character()
    ),
    assets = data.frame(
      kind = c("contract", "contract"),
      id = c("orders", NA_character_),
      version = c("1", "1"),
      definition = c('{"max_age_hours":24}', NA_character_)
    ),
    events = data.frame()
  )
  summary <- catalog_summary(
    snapshot,
    as.POSIXct("2026-01-01 01:00:00", tz = "UTC")
  )
  expect_identical(summary$freshness, c("unknown", "unknown", "current"))
  expect_identical(summary$max_age_hours, c(NA_real_, NA_real_, 24))
  # Matching but corrupt evidence must still fail instead of becoming unknown.
  snapshot$assets$definition[[1]] <- NA_character_
  failure <- tryCatch(catalog_summary(snapshot), error = identity)
  expect_s3_class(failure, "error")
})
