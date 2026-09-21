test_that("catalog specifications reject unsafe endpoints and unsupported types", {
  expect_error(
    dr_catalog_openlineage("https://user:secret@example.test"),
    "without credentials"
  )
  expect_error(
    dr_catalog_openmetadata("https://example.test?token=secret", "schema"),
    "without credentials"
  )
  expect_error(
    dr_catalog_openmetadata("https://example.test", "schema", request = 1),
    "request must"
  )
  adapter <- dr_catalog_openmetadata("https://example.test", "schema")
  expect_error(
    openmetadata_table(
      adapter,
      list(product = "orders", schema = list(details = "list"))
    ),
    "supported scalar column"
  )
})
