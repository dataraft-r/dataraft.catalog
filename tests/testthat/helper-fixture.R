fixture <- function(...) {
  testthat::skip_if_not_installed("dataraft.lake")
  env <- new.env(parent = parent.frame())
  sys.source(
    system.file("test-fixtures", "lake.R", package = "dataraft.lake"),
    env
  )
  env$fixture(...)
}
fixture_cleanup <- function(f) {
  dataraft.lake::dr_disconnect_lake(f$lake)
  unlink(f$root, recursive = TRUE)
}
