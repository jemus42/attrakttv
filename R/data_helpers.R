#' Prepare a show info object for the database
#'
#' @param show A show object as returned by [tRakt::trakt.shows.summary] or [tRakt::trakt.search]
#' with `extended = "full"`.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#' @importFrom dplyr mutate select matches rename
#' @importFrom purrr map_chr
#' @examples
#' tRakt::trakt.search("Futurama", type = "shows") %>%
#'   cleanup_show_summary()
cleanup_show_summary <- function(show) {
  show %>%
    select(-matches("^type$|^score$|^avail.*translations$")) %>%
    mutate(
      genres = map_chr(genres, paste0, collapse = ", ")
    ) %>%
    rename(show_id = trakt)
}

#' Get a poster from fanart.tv
#'
#' @param tvdbid THe tvdb id.
#' @param api_key Defaults to `Sys.getenv("fanarttv_api_key")`.
#'
#' @return A `character` with a poster url. If there's no result, return is `""` (`character(1)`)
#' @export
#' @importFrom httr GET content
#' @importFrom dplyr bind_rows filter arrange pull
#' @importFrom purrr pluck
#' @examples
#' \dontrun{
#' get_fanart_poster()
#' }
get_fanart_poster <- function(tvdbid, api_key = Sys.getenv("fanarttv_api_key")) {
  if (api_key == "") {
    stop("Need to set a fanart.tv API key. Set env variable 'fanarttv_api_key'")
  }

  query <- paste0("https://webservice.fanart.tv/v3/tv/", tvdbid, "?api_key=", api_key)
  ret <- content(GET(query))

  url <- NULL

  # Try tvposter first
  if (rlang::has_name(ret, "tvposter")) {
    url <- pluck(ret, "tvposter") %>%
      bind_rows() %>%
      filter(lang == "en") %>%
      arrange(likes) %>%
      head(1) %>%
      pull(url)
  } else if (rlang::has_name(ret, "seasonposter")) {
    url <- pluck(ret, "seasonposter") %>%
      bind_rows() %>%
      filter(lang == "en") %>%
      arrange(likes) %>%
      head(1) %>%
      pull(url)
  }

  if (is.character(url)) {
    url
  } else {
    cliapp::cli_alert_danger("No fanart: {ret$name} ({tvdbid})")
    character(1)
  }
}
