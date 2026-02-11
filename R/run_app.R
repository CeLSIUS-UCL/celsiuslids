#' Run the CeLSIUS LIDS Generator Application
#'
#'
#' @param port The TCP port that the application should listen on. If the port
#'   is not specified, a random port will be chosen.
#' @param launch.browser If \code{TRUE} (the default), the application will open
#'   in the system's default web browser.
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#'
#' @return This function does not return a value; it launches the Shiny app.
#'
#' @examples
#' \dontrun{
#' # Run the app with default settings
#' run_lids_app()
#'
#' # Run on a specific port without opening browser
#' run_lids_app(port = 3838, launch.browser = FALSE)
#' }
#'
#' @export
#' @import shiny
run_lids_app <- function(port = NULL, launch.browser = TRUE, ...) {
  app_dir <- system.file("app", package = "celsiuslids")
  
  if (app_dir == "") {
    stop(
      "Could not find the app directory. ",
      "Try re-installing the `celsiuslids` package.",
      call. = FALSE
    )
  }
  
  shiny::runApp(
    appDir = app_dir,
    port = port,
    launch.browser = launch.browser,
    ...
  )
}
