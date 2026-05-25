#' Run attrakttv app
#'
#' This function starts the attrakttv shiny app.
#' @param port Integer, port to listen on. Defaults to `7842` so local
#' testing always lands on the same URL. Override with the `attrakttv.port`
#' option or by passing this argument explicitly.
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
  port = getOption("attrakttv.port", 7842),
  display.mode = "auto",
  launch.browser = getOption("shiny.launch.browser", interactive())) {
  appDir <- system.file("app", package = "attrakttv")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `attrakttv`.", call. = FALSE)
  }

  shiny::runApp(
    appDir,
    port = port,
    display.mode = display.mode,
    launch.browser = launch.browser
  )
}
