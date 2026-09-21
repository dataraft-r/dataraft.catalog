test_that("component attachment exports only prefixed names", {
  exports <- getNamespaceExports("dataraft.catalog")
  expect_true(all(startsWith(exports, "dr_")))
  expect_length(
    intersect(exports, c("ident", "exec", "abort", "now", "%||%")),
    0L
  )
})
