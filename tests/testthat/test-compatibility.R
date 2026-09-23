test_that("catalog exports delegate to adapters", {
  expect_identical(
    dr_catalog_openlineage,
    dataraft.adapters::dr_catalog_openlineage
  )
  expect_identical(dr_catalog_app, dataraft.adapters::dr_catalog_app)
})
