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
#' \dontrun{
#' tRakt::trakt.search("Futurama", type = "show") %>%
#'   cleanup_show_summary()
#' }

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
#' @param tvdbid The tvdb id.
#' @param api_key Defaults to `Sys.getenv("fanarttv_api_key")`.
#'
#' @return A `character` with a poster url. If there's no result, return is `""` (`character(1)`)
#' @export
#' @importFrom httr GET content
#' @importFrom dplyr bind_rows filter arrange pull
#' @importFrom purrr pluck
#' @importFrom utils head
#' @examples
#' \dontrun{
#' get_fanart_poster()
#' }
get_fanart_poster <- function(tvdbid, api_key = Sys.getenv("fanarttv_api_key")) {
  if (api_key == "") {
    stop("Need to set a fanart.tv API key. Set env variable 'fanarttv_api_key'")
  }

  query <- paste0("https://webservice.fanart.tv/v3/tv/", tvdbid, "?api_key=", api_key)
  ret <- httr::content(httr::GET(query))

  url <- NULL

  # Try tvposter first
  if (rlang::has_name(ret, "tvposter")) {
    url <- pluck(ret, "tvposter") %>%
      bind_rows() %>%
      arrange(desc(likes), lang) %>%
      head(1) %>%
      pull(url)
  } else if (rlang::has_name(ret, "seasonposter")) {
    url <- pluck(ret, "seasonposter") %>%
      bind_rows() %>%
      arrange(desc(likes), lang) %>%
      head(1) %>%
      pull(url)
  }

  if (is.character(url) & !(identical(url, character(0)))) {
    url
  } else {
    cliapp::cli_alert_danger("No fanart: {ret$name} ({tvdbid})")
    character(1)
  }
}

#' Datetime of last week
#'
#' @param days `integer [7]`: How many days ago? Must be an integer or coercible to an integer.
#' @param unix `logical [TRUE]`: Return a unix timestamp (which the db understands).
#'
#' @return Either a `POSIXct` or `numeric`, see `unix`.
#' @export
#' @importFrom lubridate now days
#' @examples
#' days_ago(1)
#' days_ago(unix = FALSE)
days_ago <- function(days = 7L, unix = TRUE) {
  days <- as.integer(days)
  res <- lubridate::now(tzone = "UTC") - lubridate::days(days)

  if (unix) {
    res <- as.numeric(res)
  }

  res
}


#' Label user ratings
#'
#' @param x Numeric rating in [1, 10]. Will be rounded to te nearest integer.
#'
#' @return `character` of same length as `x`.
#' @export
rating_label <- function(x) {
  x <- round(x)

  dplyr::case_when(
    x == 1  ~ "Weak Sauce :(",
    x == 2  ~ "Terrible",
    x == 3  ~ "Bad",
    x == 4  ~ "Poor",
    x == 5  ~ "Meh",
    x == 6  ~ "Fair",
    x == 7  ~ "Good",
    x == 8  ~ "Great",
    x == 9  ~ "Superb",
    x == 10 ~ "Totally Ninja!",
    TRUE    ~ "??"
  )
}

#' Label country codes
#'
#' @param x A country code like `"us"`
#'
#' @return `character` of same length as `x`
#' @export
#' @examples
#' country_label(c("us", "de", "lwaflaf"))
country_label <- function(x) {

  country_codes <- tRakt::countries$name
  names(country_codes) <- tRakt::countries$code
  res <- country_codes[x]
  res[is.na(res)] <- "N/A"
  res

}
