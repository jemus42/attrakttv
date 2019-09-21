
#' Run attrakttv app
#'
#' This function starts the attrakttv shiny app.
#' @param display.mode \code{auto} by default, can also be \code{showcase}.
#' See \link[shiny]{runApp}.
#' @param launch.browser Boolean, set \code{TRUE} to open the app in the browser.
#' See \link[shiny]{runApp}.
#' @export
#' @import shiny
#' @import shinythemes
#' @examples
#' \dontrun{
#' attrakttv()
#' }
attrakttv <- function(display.mode = "auto",
                        launch.browser = getOption("shiny.launch.browser", interactive())) {
  appDir <- system.file("app", package = "attrakttv")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `attrakttv`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = display.mode, launch.browser = launch.browser)
}
