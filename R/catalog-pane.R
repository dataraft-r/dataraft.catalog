#' Open the read-only catalog in an IDE viewer
#'
#' The viewer receives the actual listening URL from Shiny. If no IDE viewer is
#' available, the system browser opens instead. Launching runs Shiny in the
#' current R process and blocks that session until the app stops. Keep a supplied
#' lake connection open for that time; this function neither disconnects it nor
#' creates a background server. An exported snapshot can be used independently
#' of a live lake connection.
#'
#' @inheritParams dr_catalog_app
#' @param ... Additional arguments to [shiny::runApp()], such as `port` and
#'   `host`. The `launch.browser` callback is managed by this function.
#' @returns A Shiny app object when `launch = FALSE`; otherwise the value
#'   returned when `shiny::runApp()` stops.
#' @export
#' @examplesIf requireNamespace("shiny", quietly = TRUE) && requireNamespace("bslib", quietly = TRUE)
#' snapshot <- tempfile(fileext = ".json")
#' writeLines('{"assets":[]}', snapshot)
#' app <- dr_catalog_pane(snapshot = snapshot, launch = FALSE)
#' class(app)
#' unlink(snapshot)
dr_catalog_pane <- function(
  lake = NULL,
  snapshot = NULL,
  launch = interactive(),
  refresh_seconds = 30,
  ...
) {
  app <- dr_catalog_app(
    lake = lake,
    snapshot = snapshot,
    launch = FALSE,
    refresh_seconds = refresh_seconds
  )
  if (!isTRUE(launch)) {
    return(app)
  }
  shiny::runApp(app, launch.browser = catalog_viewer, ...)
}

catalog_viewer <- function(url) {
  viewer <- getOption("viewer")
  if (is.function(viewer)) {
    tryCatch(viewer(url), error = function(error) utils::browseURL(url))
  } else {
    utils::browseURL(url)
  }
  invisible(NULL)
}
