test_that("catalog freshness selects publication sequence despite clock skew", {
  snapshot <- list(
    releases = data.frame(
      asset = c("orders", "orders"),
      release_id = c("newer-clock", "newer-publication"),
      release_order = c(1, 2),
      published_at = c("2026-01-02T00:00:00Z", "2026-01-01T00:00:00Z"),
      contract = c("orders@1", "orders@1"),
      quality = c("passed", "passed")
    ),
    runs = data.frame(
      asset = character(),
      started_at = character(),
      status = character()
    ),
    assets = data.frame(
      kind = character(),
      id = character(),
      version = character()
    ),
    events = data.frame()
  )
  expect_identical(catalog_summary(snapshot)$release_id, "newer-publication")
  snapshot$releases$release_order <- NULL
  error <- tryCatch(catalog_summary(snapshot), error = identity)
  expect_s3_class(error, "dataraft_error_catalog")
  snapshot$exported_at <- "2026-01-03T00:00:00Z"
  expect_identical(catalog_summary(snapshot)$release_id, "newer-clock")
})


test_that("publication order stays exact above the JSON double precision limit", {
  releases <- data.frame(
    release_id = c("older", "latest", "earliest"),
    release_order = c("9007199254740992", "9007199254740993", "9")
  )
  restored <- jsonlite::fromJSON(jsonlite::toJSON(releases))
  expect_identical(
    catalog_order_releases(restored)$release_id,
    c("latest", "older", "earliest")
  )
  if (requireNamespace("bit64", quietly = TRUE)) {
    releases$release_order <- bit64::as.integer64(releases$release_order)
    expect_identical(
      catalog_order_releases(releases)$release_id,
      c("latest", "older", "earliest")
    )
  }
})
