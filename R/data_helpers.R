#' Prepare a show info object for the database
#'
#' Normalises results from `tRakt::shows_summary()` or `tRakt::search_query()`
#' (`extended = "full"`) into a stable column set safe for SQLite storage:
#' list-columns are flattened or dropped, the `trakt` id is renamed to
#' `show_id`, and search-only columns (`score`, `type`, `social_ids`) and the
#' summary-only `trailer`/`total_runtime` are removed so both call paths
#' produce the same shape.
#'
#' @param show A show object as returned by `tRakt::shows_summary()` or
#' `tRakt::search_query()` with `extended = "full"`.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#' @importFrom dplyr mutate rename any_of
#' @importFrom purrr map_chr
#' @examples
#' \dontrun{
#' tRakt::search_query("Futurama", type = "show") |>
#'   cleanup_show_summary()
#' }
cleanup_show_summary <- function(show) {
  keep <- c(
    "trakt",
    "slug",
    "title",
    "year",
    "overview",
    "tagline",
    "status",
    "country",
    "language",
    "network",
    "runtime",
    "rating",
    "votes",
    "comment_count",
    "aired_episodes",
    "first_aired",
    "updated_at",
    "genres",
    "certification",
    "airs_day",
    "airs_time",
    "airs_timezone",
    "original_title",
    "homepage",
    "imdb",
    "tmdb",
    "tvdb",
    "plex_guid",
    "plex_slug"
  )

  show <- show[, intersect(keep, names(show)), drop = FALSE]

  if ("genres" %in% names(show)) {
    show$genres <- map_chr(show$genres, paste0, collapse = ", ")
  }

  rename(show, show_id = "trakt")
}

#' Normalise a seasons tibble for the database
#'
#' Keeps a stable subset of columns from `tRakt::seasons_summary()` output so
#' the seasons table schema is independent of tRakt version drift and per-show
#' variation in returned fields. List-columns and anything we don't display
#' are dropped.
#'
#' @param seasons The seasons tibble from `tRakt::seasons_summary()`, after
#' `show_id` has been attached.
#' @return a [tibble][tibble::tibble-package]
#' @export
cleanup_seasons <- function(seasons) {
  keep <- c(
    "show_id",
    "season",
    "title",
    "rating",
    "votes",
    "first_aired",
    "aired_episodes",
    "episode_count",
    "overview",
    "network",
    "updated_at",
    "total_runtime",
    "original_title",
    "tmdb",
    "tvdb",
    "plex_guid"
  )
  seasons[, intersect(keep, names(seasons)), drop = FALSE]
}

#' Normalise an episodes tibble for the database
#'
#' Keeps a stable subset of columns from the per-episode rows returned by
#' `tRakt::seasons_summary(episodes = TRUE)`. Same rationale as
#' [cleanup_seasons()].
#'
#' @param episodes The episodes tibble from
#' `dplyr::bind_rows(seasons_summary(...)$episodes)`, after `show_id` has
#' been attached.
#' @return a [tibble][tibble::tibble-package]
#' @export
cleanup_episodes <- function(episodes) {
  keep <- c(
    "show_id",
    "season",
    "episode",
    "title",
    "rating",
    "votes",
    "first_aired",
    "comment_count",
    "runtime",
    "overview",
    "released",
    "updated_at",
    "original_title",
    "episode_abs",
    "episode_type",
    "imdb",
    "tmdb",
    "tvdb",
    "plex_guid"
  )
  episodes[, intersect(keep, names(episodes)), drop = FALSE]
}

#' Get a poster from fanart.tv
#'
#' Returns a poster URL when fanart.tv has one; on any failure (missing key,
#' network error, non-200 response, no matching artwork) returns `""` so the
#' caller can fall back to a placeholder. Failure is logged via `cli` but
#' never propagated as an error -- the app keeps working without posters.
#'
#' @param tvdbid The tvdb id.
#' @param api_key Defaults to `Sys.getenv("fanarttv_api_key")`.
#'
#' @return A `character(1)`: poster URL on success, `""` otherwise.
#' @export
#' @importFrom httr GET content status_code
#' @importFrom dplyr bind_rows arrange pull desc
#' @importFrom purrr pluck
#' @importFrom utils head
#' @examples
#' \dontrun{
#' get_fanart_poster(81189)
#' }
get_fanart_poster <- function(
  tvdbid,
  api_key = Sys.getenv("fanarttv_api_key")
) {
  blank <- character(1)

  if (!nzchar(api_key)) {
    cli::cli_alert_warning("No fanart.tv API key set; skipping poster lookup")
    return(blank)
  }
  if (length(tvdbid) == 0 || is.na(tvdbid) || !nzchar(as.character(tvdbid))) {
    return(blank)
  }

  query <- paste0(
    "https://webservice.fanart.tv/v3/tv/",
    tvdbid,
    "?api_key=",
    api_key
  )
  resp <- tryCatch(httr::GET(query), error = function(e) NULL)
  if (is.null(resp)) {
    cli::cli_alert_danger("fanart request failed for tvdb {tvdbid}")
    return(blank)
  }
  if (httr::status_code(resp) >= 400) {
    cli::cli_alert_info(
      "fanart HTTP {httr::status_code(resp)} for tvdb {tvdbid} (likely no artwork)"
    )
    return(blank)
  }

  ret <- tryCatch(httr::content(resp), error = function(e) NULL)
  if (!is.list(ret)) {
    cli::cli_alert_danger("fanart response not a list for tvdb {tvdbid}")
    return(blank)
  }

  url <- NULL
  pick_top <- function(key) {
    items <- pluck(ret, key)
    if (length(items) == 0) {
      return(NULL)
    }
    tryCatch(
      items |>
        bind_rows() |>
        arrange(desc(likes), lang) |>
        head(1) |>
        pull(url),
      error = function(e) NULL
    )
  }
  url <- pick_top("tvposter") %||% pick_top("seasonposter")

  if (is.character(url) && length(url) == 1 && nzchar(url)) {
    return(url)
  }

  show_name <- pluck(ret, "name", .default = "?")
  cli::cli_alert_info("No fanart artwork: {show_name} (tvdb {tvdbid})")
  blank
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
#' @param x Numeric rating in `[1, 10]`. Will be rounded to the nearest integer.
#'
#' @return `character` of same length as `x`.
#' @export
rating_label <- function(x) {
  x <- round(x)

  dplyr::case_when(
    x == 1 ~ "Weak Sauce :(",
    x == 2 ~ "Terrible",
    x == 3 ~ "Bad",
    x == 4 ~ "Poor",
    x == 5 ~ "Meh",
    x == 6 ~ "Fair",
    x == 7 ~ "Good",
    x == 8 ~ "Great",
    x == 9 ~ "Superb",
    x == 10 ~ "Totally Ninja!",
    TRUE ~ "??"
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
  country_codes <- tRakt::trakt_countries$name
  names(country_codes) <- tRakt::trakt_countries$code
  res <- country_codes[x]
  res[is.na(res)] <- "N/A"
  res
}

#' Label language codes
#'
#' @param x A language code like `"en"`
#'
#' @return `character` of same length as `x`
#' @export
#' @examples
#' language_label(c("us", "de", "lwaflaf"))
language_label <- function(x) {
  language_codes <- tRakt::trakt_languages$name
  names(language_codes) <- tRakt::trakt_languages$code
  res <- language_codes[x]
  res[is.na(res)] <- "N/A"
  res
}


#' Convert back and forth between slugs and show_ids
#'
#' @param show_id,slug A `show_id` or `slug`, of which _one_ must
#' be supplied to get the other.
#' @inheritParams cache_add_data
#'
#' @return `character(1)`
#' @export
#' @importFrom RSQLite dbDisconnect
#' @examples
#' convert_ids(slug = "futurama")
#' convert_ids(show_id = "614")
convert_ids <- function(show_id = NULL, slug = NULL, cache_db_con) {
  if (missing(cache_db_con)) {
    cache_db_con <- cache_db()
    on.exit(dbDisconnect(cache_db_con))
  }

  shows <- tbl(cache_db_con, "shows") |> select(show_id, slug) |> collect()

  if (!is.null(slug)) {
    ids <- shows$show_id
    names(ids) <- shows$slug
    ids[slug]
  } else if (!is.null(show_id)) {
    show_id <- stringr::str_remove(show_id, "^cache:")

    slugs <- shows$slug
    names(slugs) <- shows$show_id
    slugs[show_id]
  } else {
    stop("Gotta pick one")
  }
}


#' Convert numeric timestam to Date
#'
#' Note: Hardcoded UTC timezone and origin of `1970-01-01`
#' @param x A numeric timestamp in unix epoch.
#'
#' @return Object of class `Date`.
#' @export
#'
#' @examples
#' x <- c(927079200, 958960800, 989805600, 1060567200, 1315533600, 1378346400)
#' unix_date(x)
unix_date <- function(x) {
  as.POSIXct(x, tz = "UTC", origin = "1970-01-01") |>
    as.Date()
}

#' Convert numeric timestamp to POSIXct
#'
#' @inheritParams unix_date
#'
#' @return Object of class `POSIXct`.
#' @export
#' @examples
#' x <- c(927079200, 958960800, 989805600, 1060567200, 1315533600, 1378346400)
#' unix_datetime(x)
unix_datetime <- function(x) {
  as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
}
