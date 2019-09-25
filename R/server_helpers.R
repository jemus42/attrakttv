#' Append hovertext to episodes df
#'
#' @param episodes A tbl as it will arrive in `renderPlotly`
#'
#' @return The same tbl with a new column `hovertext`
#' @export
#' @importFrom glue glue
#' @importFrom dplyr mutate
make_hoverinfo <- function(episodes) {
  episodes %>%
    mutate(hovertext = glue::glue("
      <b>{season_title}</b>
      Episode {episode}: “{title}”
      <b>Aired</b>:  {first_aired}
      <b>Rating</b>: {round(rating, 2)} (<i>“{rating_label(rating)}”</i>)
      Based on {votes} votes.
    "))
}