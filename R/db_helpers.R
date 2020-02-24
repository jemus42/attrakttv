#' Get the path to the db file
#'
#' The directory is set to `getOption("trakt_db_path", default = "~/db")`.
#' @param name Optional: Name of db file. Defaults to `tRakt.db`
#'
#' @return `character(1)`
#' @export
#' @importFrom fs file_size
#' @importFrom cliapp cli_alert_info
#' @examples
#' cache_db_path()
cache_db_path <- function(name = "tRakt.db") {
  path <- file.path(getOption("trakt_db_path", default = "~/db"), name)
  cli_alert_info("Database path: {path} ({file_size(path)})")
  invisible(path)
}

#' Make a connection to the db
#' @param path The path to the SQLite db, defaults to [cache_db_path()]
#' @param pool `logical(1) [TRUE]`: Return a [pool::Pool]. Since `pool` doesn't support `dbSendStatement`
#' (yet?), `pool = FALSE` is required for function that delete rows like [cache_drop_old_rows].
#' @return A `conn` [DBI] thingy _or_ `pool`.
#' @export
#' @importFrom RSQLite dbConnect SQLite
#' @importFrom pool dbPool
#' @examples
#' \dontrun{
#' cache_db_con <- cache_db()
#'
#' is_already_cached("shows", 1390, cache_db_con)
#' }
cache_db <- function(pool = TRUE, path = cache_db_path()) {
  if (pool) {
    dbPool(
      drv = SQLite(),
      dbname = path,
      idleTimeout = 300
    )
  } else {
    dbConnect(SQLite(), path)
  }
}

#' Check if a table exists in db, if not, create it
#'
#' @param table_name A db table name, e.g. "shows".
#' @param reference_table A table used as a template for (re)creation of the db table.
#' @param cache_db_con A connection, see [cache_db]
#'
#' @return Nothing
#' @export
#' @importFrom RSQLite dbExistsTable dbCreateTable
#' @examples
#' \dontrun{
#' TRUE
#' }
check_cache_table <- function(table_name, reference_table, cache_db_con) {
  if (!(dbExistsTable(cache_db_con, table_name))) {
    dbCreateTable(cache_db_con, table_name, reference_table)
  }
}

#' Is the show in the cache already?
#'
#' @inheritParams check_cache_table
#' @inheritParams cache_add_show
#'
#' @return `logical(1)`, `TRUE` if the `show_id` is already present in the table.
#' @export
#' @importFrom RSQLite dbExistsTable
#' @importFrom dplyr tbl pull
#' @examples
#' \dontrun{
#' TRUE
#' }
is_already_cached <- function(table_name, show_id, cache_db_con) {
  if (dbExistsTable(cache_db_con, table_name)) {
    cached_ids <- tbl(cache_db_con, table_name) %>%
      pull(show_id) %>%
      unique()

    already_cached <- show_id %in% cached_ids
  } else {
    already_cached <- FALSE
  }

  already_cached
}

#' Add a show to the cache
#'
#' @param show_query A search query, e.g. "Futurama".
#' @param show_id A show id, in the database the `trakt ID` is used.
#' @param replace `logical [FALSE]` Delete (if exists) & replace data?
#' @param cache_db_con A connection, see [cache_db]
#'
#' @return `NULL` _if_ the `search_query` yields no result, the `show_id` otherwise.
#' @export
#' @importFrom tRakt shows_summary
#' @importFrom cliapp cli_alert_info
cache_add_show <- function(show_query = NULL, show_id = NULL, replace = FALSE, cache_db_con) {
  if (!is.null(show_query)) {
    ret_show_id <- cache_add_show_query(
      show_query = show_query,
      replace = replace,
      cache_db_con = cache_db_con
    )

    if (is.null(ret_show_id)) {
      return(NULL)
    }
  } else if (!is.null(show_id)) {
    if (getOption("caching_debug")) cli_alert_info("Want to add show '{show_id}'")

    show_id <- as.character(show_id)
    already_cached <- is_already_cached("shows", show_id, cache_db_con = cache_db_con)

    if ((already_cached & replace) | (!already_cached)) {
      ret <- shows_summary(show_id, extended = "full")
      ret <- cleanup_show_summary(ret)
      cache_add_data("shows", ret, replace, cache_db_con)

      ret_show_id <- ret$show_id
    } else if (getOption("caching_debug")) {
      cli_alert_info("Show '{show_id}' already cached, not downloading")
    }
  } else {
    stop("Gotta pick one yo")
  }

  if (!is.null(show_id)) invisible(show_id) else invisible(ret_show_id)
}


#' Add a show by query
#'
#' Just a workhorse behind [cache_add_show].
#'
#' @inheritParams cache_add_show
#' @importFrom cliapp cli_alert_info
#' @importFrom tRakt search_query
#' @keywords internal
cache_add_show_query <- function(show_query, replace = FALSE, cache_db_con) {
  ret <- search_query(
    show_query,
    type = "show", n_results = 1, extended = "full"
  )

  if (identical(ret, tibble())) {
    return(NULL)
  }

  ret <- cleanup_show_summary(ret)

  already_cached <- is_already_cached("shows", ret$show_id, cache_db_con = cache_db_con)

  if ((already_cached & replace) | (!already_cached)) {
    cache_add_data(
      table_name = "shows",
      new_data = ret,
      replace = replace,
      cache_db_con = cache_db_con
    )
  } else if (getOption("caching_debug")) {
    cli_alert_info("Show '{ret$show_id}' already cached, not updating")
  }

  invisible(ret$show_id)
}


#' Add episodes to the cache
#'
#' @inheritParams cache_add_show
#' @return Nothing
#' @export
#' @importFrom cliapp cli_alert_info
#' @importFrom tRakt seasons_summary
#' @importFrom dplyr pull bind_rows select mutate
#' @examples
#' \dontrun{
#' TRUE
#' }
cache_add_episodes <- function(show_id, replace = FALSE, cache_db_con) {
  show_id <- as.character(show_id)
  already_cached <- is_already_cached("episodes", show_id, cache_db_con)

  if ((already_cached & replace) | (!already_cached)) {
    ret <- seasons_summary(show_id, extended = "full", episodes = TRUE)

    episodes <- ret %>%
      pull(episodes) %>%
      bind_rows() %>%
      select(-matches("available_translations")) %>%
      mutate(show_id = show_id)

    seasons <- ret %>%
      select(-episodes) %>%
      mutate(show_id = show_id)

    cache_add_data("seasons", seasons, replace = replace, cache_db_con)
    cache_add_data("episodes", episodes, replace = replace, cache_db_con)
  } else if (getOption("caching_debug")) {
    cli_alert_info(
      "Episodes for '{show_id}' already cached, not replacing"
    )
  }
}

#' Add a poster to the cache
#'
#' @inheritParams cache_add_show
#'
#' @return Nothing
#' @export
#' @importFrom dplyr tbl collect filter pull tibble
cache_add_poster <- function(show_id, replace = FALSE, cache_db_con) {

  if (!is_already_cached("posters", show_id, cache_db_con)) {

    tvdbid <- tbl(cache_db_con, "shows") %>%
      collect() %>%
      filter(show_id == "60356") %>%
      pull(tvdb)

    res <- tibble(
      show_id = show_id,
      show_poster = get_fanart_poster(tvdbid = tvdbid)
    )

    cache_add_data("posters", res, cache_db_con = cache_db_con)
  }
}

#' Add data to some db table
#'
#' @inheritParams cache_add_show
#' @param table_name Name of the db table to add data to.
#' @param new_data The new data to add.
#'
#' @return Nothing
#' @export
#' @importFrom cliapp cli_alert_info cli_alert_success
#' @importFrom DBI dbWriteTable dbSendStatement dbClearResult
#' @import dplyr
#' @importFrom rlang has_name
#' @importFrom lubridate now
#' @importFrom glue glue_sql
#' @importFrom RSQLite dbSendStatement dbClearResult dbWriteTable
#' @importFrom cliapp cli_alert_danger
#' @examples
#' \dontrun{
#' TRUE
#' }
cache_add_data <- function(table_name, new_data, replace = FALSE, cache_db_con) {
  # cached | replace | what do
  # TRUE   | TRUE    | -> drop, write
  # TRUE   | FALSE   | -> do nothing
  # FALSE  | TRUE    | -> write
  # FALSE  | FALSE   | -> write
  new_data <- new_data %>%
    mutate(cache_date = as.numeric(now(tzone = "UTC")))

  if (has_name(new_data, "first_aired")) {
    new_data <- new_data %>%
      mutate(first_aired = as.numeric(first_aired))
  }
  if (has_name(new_data, "updated_at")) {
    new_data <- new_data %>%
      mutate(updated_at = as.numeric(updated_at))
  }

  # Only check/create table after cache_date has been added
  check_cache_table(table_name, new_data, cache_db_con)

  # Not needed once I settle on a global ID / name
  matching_id <- "show_id"

  current_id <- new_data %>%
    pull(!!sym(matching_id)) %>%
    unique() %>%
    as.character()

  # Get ids of data already in cache
  cached_ids <- tbl(cache_db_con, table_name) %>%
    pull(!!sym(matching_id)) %>%
    unique()

  already_cached <- current_id %in% cached_ids

  # Delete if already cached and replace = TRUE
  if (already_cached & replace) {
    if (getOption("caching_debug", default = FALSE)) {
      cli_alert_danger("Deleting and replacing show '{current_id}' at '{table_name}'")
    }

    query <- glue_sql("
      DELETE FROM {table_name}
      WHERE ({`matching_id`} = {current_id});
    ", .con = cache_db_con)

    res <- dbSendStatement(cache_db_con, query)
    # dbHasCompleted(res)
    # dbGetRowsAffected(res)
    dbClearResult(res)

    dbWriteTable(cache_db_con, table_name, new_data, append = TRUE)
  }

  if (!already_cached) {
    if (getOption("caching_debug", default = FALSE)) {
      cli_alert_success("'{current_id}' not in cache, writing to '{table_name}'")
    }

    dbWriteTable(cache_db_con, table_name, new_data, append = TRUE)
  }

  if (already_cached & !replace & getOption("caching_debug", default = FALSE)) {
    cli_alert_info("Not replacing '{current_id}' data already in '{table_name}'")
  }
}


#' Drop individual rows from a table
#'
#' @inheritParams cache_add_data
#' @param where_id Column in table to use for matching.
#' @param is_id Value of `where_id` to delete.
#' @return Nothing
#' @importFrom RSQLite dbSendStatement dbClearResult
#' @importFrom glue glue_sql
#' @export
cache_delete_rows <- function(table_name, where_id, is_id, cache_db_con) {
  query <- glue_sql("
      DELETE FROM {table_name}
      WHERE ({`where_id`} IN ({is_id*}));
    ", .con = cache_db_con)

  res <- dbSendStatement(cache_db_con, query)
  # dbHasCompleted(res)
  # dbGetRowsAffected(res)
  dbClearResult(res)
}

#' Drop old rows
#'
#' @inheritParams cache_add_data
#' @param threshold_days `integer [7]`: Drop records older than that.
#'
#' @return Nothing
#' @export
#' @importFrom dplyr tbl filter pull
#' @examples
#' \dontrun{
#' cache_drop_old_rows("episodes", 4, cache_db_con)
#' cache_drop_old_rows("seasons", 4, cache_db_con)
#' }
cache_drop_old_rows <- function(table_name, threshold_days = 7, cache_db_con) {

  cutoff_time <- days_ago(threshold_days)

  to_delete <- tbl(cache_db_con, table_name) %>%
    filter(cache_date < cutoff_time) %>%
    pull(show_id)

  cache_delete_rows(
    table_name = table_name,
    where_id = "show_id",
    is_id = to_delete,
    cache_db_con = cache_db_con
  )
}

#' Update outdated episodes (in bulk per show)
#'
#' @param criterion `character(1) ["aired"]` Which criterion to use for updating. Currently
#' only `"aired"` is implemented, which updates all seasons if there are episodes that
#' aired after `cache_date`
#'
#' @return Nothing
#' @export
#' @importFrom dplyr filter distinct pull select left_join tbl
#' @importFrom purrr pwalk
#' @importFrom cliapp cli_h2
#' @examples
#' \dontrun{
#' cache_update_episodes()
#' }
cache_update_episodes <- function(criterion = "aired") {

  # pool doesn't do dbSendStatement yet :(
  # Need RSQLite for this.
  cache_db_con <- cache_db(pool = FALSE)

  episodes <- tbl(cache_db_con, "episodes")

  if (criterion == "aired") {
    episodes <- episodes %>% filter(first_aired > cache_date)
  }

  shows_to_replace <- episodes %>%
    distinct(show_id) %>%
    left_join(
      tbl(cache_db_con, "shows") %>% select(show_id, title),
      by = "show_id"
    ) %>%
    collect()

  pwalk(shows_to_replace, ~{
    cli_h2("Replacing episodes for {.y} ({.x})")
    cache_add_episodes(.x, replace = TRUE, cache_db_con)
  })
}
