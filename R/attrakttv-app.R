#' Run attrakttv app
#'
#' This function starts the attrakttv shiny app.
#' @param port Integer, port to listen on. Defaults to `7842` so local
#' testing always lands on the same URL. Override with the `attrakttv.port`
#' option or by passing this argument explicitly.
#' @param host Character, host address to bind. Defaults to `"127.0.0.1"`
#' (local dev). Set to `"0.0.0.0"` to listen on all interfaces, e.g. inside
#' a container. Override via the `shiny.host` option or this argument.
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
  host = getOption("shiny.host", "127.0.0.1"),
  display.mode = "auto",
  launch.browser = getOption("shiny.launch.browser", interactive())
) {
  appDir <- system.file("app", package = "attrakttv")
  if (appDir == "") {
    stop(
      "Could not find shiny directory. Try re-installing `attrakttv`.",
      call. = FALSE
    )
  }

  shiny::runApp(
    appDir,
    port = port,
    host = host,
    display.mode = display.mode,
    launch.browser = launch.browser
  )
}
