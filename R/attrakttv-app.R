#' Run attrakttv app
#'
#' This function starts the attrakttv shiny app.
#' @param display.mode `auto` by default, can also be `showcase`.
#' See [runApp][shiny::runApp].
#' @param launch.browser Boolean, set `TRUE` to open the app in the browser.
#' See [runApp][shiny::runApp].
#' @export
#' @import shiny
#' @examples
#' \dontrun{
#' attrakttv_app()
#' }
attrakttv_app <- function(
  display.mode = "auto", launch.browser = getOption("shiny.launch.browser", interactive())) {
  appDir <- system.file("app", package = "attrakttv")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `attrakttv`.", call. = FALSE)
  }

  if (!file.exists(cache_db_path())) {
    db_init()
  }

  shiny::runApp(appDir, display.mode = display.mode, launch.browser = launch.browser)
}
