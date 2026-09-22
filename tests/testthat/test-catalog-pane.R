test_that("pane returns a Shiny app without opening a browser", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  snapshot <- withr::local_tempfile(fileext = ".json")
  writeLines('{"assets":[]}', snapshot)
  withr::local_options(viewer = function(url) stop("Must not launch"))
  app <- dr_catalog_pane(snapshot = snapshot, launch = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("Shiny controls the actual viewer URL and blocking return value", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  snapshot <- withr::local_tempfile(fileext = ".json")
  writeLines('{"assets":[]}', snapshot)
  opened <- character()
  withr::local_options(viewer = function(url) opened <<- c(opened, url))
  testthat::local_mocked_bindings(
    runApp = function(appDir, launch.browser, port) {
      expect_s3_class(appDir, "shiny.appobj")
      expect_identical(port, 6543L)
      expect_length(opened, 0L)
      launch.browser("http://127.0.0.1:6543")
      "stopped"
    },
    .package = "shiny"
  )
  expect_identical(
    dr_catalog_pane(snapshot = snapshot, launch = TRUE, port = 6543L),
    "stopped"
  )
  expect_identical(opened, "http://127.0.0.1:6543")
})

test_that("unavailable or failing viewers fall back to the browser", {
  opened <- character()
  testthat::local_mocked_bindings(
    browseURL = function(url, ...) opened <<- c(opened, url),
    .package = "utils"
  )
  withr::local_options(viewer = NULL)
  catalog_viewer("http://127.0.0.1:4321")
  withr::local_options(viewer = "unavailable")
  catalog_viewer("http://127.0.0.1:4322")
  withr::local_options(viewer = function(url) stop("Viewer unavailable"))
  catalog_viewer("http://127.0.0.1:4323")
  expect_identical(opened, paste0("http://127.0.0.1:", 4321:4323))
})
