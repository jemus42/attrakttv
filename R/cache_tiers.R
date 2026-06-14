# Tiered refresh logic — classifies cached shows by activity level and
# decides whether to refetch them on a per-show basis. Two entry points:
#
#  * cache_refresh_if_stale() — single show, called from the Shiny server
#    on the on-search path (synchronous, hot-tier only).
#  * cache_refresh_tick() / cache_refresh_loop() — sidecar sweep over all
#    cached shows. Loop is the container entry; tick is the unit of work.

#' Classify a cached show by refresh tier
#'
#' Tiers govern how often a show is refetched from trakt. See
#' `inst/config.default.yml` for thresholds.
#'
#'  * `hot`    — actively airing (returning + recent ep)
#'  * `warm`   — between seasons or recently concluded
#'  * `cold`   — long-finished show (ratings drift slowly)
#'  * `future` — unaired (planned / in production / no ep yet)
#'
#' @param show_row A one-row tibble from the `shows` table. Needs `status`.
#' @param last_aired_days `numeric(1)` Days since the most recently aired
#'   episode, or `NA` if no episode has aired yet.
#' @param config Output of [attrakttv_config()].
#' @return `character(1)`.
#' @export
show_tier <- function(show_row, last_aired_days, config = attrakttv_config()) {
  cfg <- config$refresh
  status <- tolower(show_row$status %||% "")

  if (status %in% c("planned", "in production") || is.na(last_aired_days)) {
    return("future")
  }

  is_returning <- status %in% c("returning series", "continuing")
  is_ended <- status %in% c("ended", "canceled", "cancelled")

  if (is_returning && last_aired_days <= cfg$hot_last_ep_days) {
    return("hot")
  }
  if (is_returning && last_aired_days <= cfg$warm_last_ep_days) {
    return("warm")
  }
  if (is_ended && last_aired_days <= cfg$warm_ended_days) {
    return("warm")
  }

  "cold"
}

#' Decide whether a cached show should be refreshed now
#'
#' @inheritParams show_tier
#' @param trigger `"on_search"` (synchronous, hot-only) or `"background"`
#'   (sidecar tick, all tiers per config).
#' @return `logical(1)`.
#' @export
need_refresh <- function(
  show_row,
  last_aired_days,
  trigger = c("on_search", "background"),
  config = attrakttv_config()
) {
  trigger <- match.arg(trigger)
  cfg <- config$refresh
  tier <- show_tier(show_row, last_aired_days, config)

  age_days <- as.numeric(
    difftime(
      Sys.time(),
      as.POSIXct(show_row$cache_date, origin = "1970-01-01", tz = "UTC"),
      units = "days"
    )
  )

  if (trigger == "on_search") {
    if (tier == "hot") {
      return(age_days >= cfg$on_search_max_age_days)
    }
    return(FALSE)
  }

  threshold <- switch(
    tier,
    hot = cfg$hot_refresh_days,
    warm = cfg$warm_refresh_days,
    future = cfg$future_refresh_days,
    cold = cfg$cold_refresh_days
  )

  if (is.null(threshold)) {
    return(FALSE)
  }
  age_days >= threshold
}

#' Last-aired-days lookup for a single cached show
#'
#' Helper used by both refresh paths. Returns `NA_real_` if the episodes
#' table is missing, has no rows for `target_id`, or all `first_aired`
#' values are `NA`.
#'
#' @param target_id `character(1)` show_id.
#' @param cache_db_con DBI connection.
#' @keywords internal
#' @importFrom RSQLite dbExistsTable
#' @importFrom dplyr tbl filter summarize collect
.last_aired_days <- function(target_id, cache_db_con) {
  if (!dbExistsTable(cache_db_con, "episodes")) {
    return(NA_real_)
  }
  eps <- tbl(cache_db_con, "episodes") |>
    filter(show_id == !!target_id) |>
    summarize(last_aired = max(first_aired, na.rm = TRUE)) |>
    collect()
  if (nrow(eps) == 0 || is.na(eps$last_aired) || !is.finite(eps$last_aired)) {
    return(NA_real_)
  }
  as.numeric(
    difftime(
      Sys.time(),
      as.POSIXct(eps$last_aired, origin = "1970-01-01", tz = "UTC"),
      units = "days"
    )
  )
}

#' Refetch one show + its episodes with replace = TRUE
#'
#' Wraps the two `cache_add_*` calls in a single tryCatch so a flaky trakt
#' response doesn't tear down the surrounding loop. Caller is responsible
#' for choosing whether to call this (e.g. via [need_refresh]).
#'
#' @inheritParams .last_aired_days
#' @param title Optional show title for log lines.
#' @param tier Optional tier label for log lines.
#' @return `logical(1)` — `TRUE` if refresh succeeded.
#' @keywords internal
#' @importFrom cli cli_alert_info cli_alert_danger
.do_refresh <- function(
  target_id,
  cache_db_con,
  title = target_id,
  tier = "?"
) {
  cli_alert_info("Refreshing {title} (id={target_id}, tier={tier})")
  tryCatch(
    {
      cache_add_show(
        show_id = target_id,
        replace = TRUE,
        cache_db_con = cache_db_con
      )
      cache_add_episodes(
        show_id = target_id,
        replace = TRUE,
        cache_db_con = cache_db_con
      )
      TRUE
    },
    error = function(e) {
      cli_alert_danger("Refresh failed for {title}: {conditionMessage(e)}")
      FALSE
    }
  )
}

#' Refresh one show if its cache is stale per tier
#'
#' Opens its own RSQLite connection (pool can't do `dbSendStatement` which
#' the `replace = TRUE` delete path needs). Safe to call from any context.
#'
#' @param target_id `character(1)` show_id.
#' @param trigger see [need_refresh].
#' @param config see [attrakttv_config].
#' @return `logical(1)` — `TRUE` iff a refresh actually ran.
#' @export
#' @importFrom RSQLite dbExistsTable dbDisconnect
#' @importFrom dplyr tbl filter collect
cache_refresh_if_stale <- function(
  target_id,
  trigger = "on_search",
  config = attrakttv_config()
) {
  con <- cache_db(pool = FALSE)
  on.exit(dbDisconnect(con), add = TRUE)

  if (!dbExistsTable(con, "shows")) {
    return(invisible(FALSE))
  }

  row <- tbl(con, "shows") |>
    filter(show_id == !!as.character(target_id)) |>
    collect()
  if (nrow(row) == 0) {
    return(invisible(FALSE))
  }

  last_days <- .last_aired_days(target_id, con)
  if (!need_refresh(row, last_days, trigger, config)) {
    return(invisible(FALSE))
  }

  tier <- show_tier(row, last_days, config)
  invisible(.do_refresh(target_id, con, title = row$title, tier = tier))
}

#' Walk every cached show, refresh those that are stale per tier
#'
#' Sidecar entry point. Loads `shows` + a per-show `last_aired` from
#' `episodes` in two queries (not N+1), then iterates. Each show that
#' refreshes does so via its own `cache_add_*` calls under the shared
#' RSQLite connection.
#'
#' @param config see [attrakttv_config].
#' @return Invisibly a list with counts: `walked`, `refreshed`, `errors`,
#'   `skipped`.
#' @export
#' @importFrom RSQLite dbExistsTable dbDisconnect
#' @importFrom dplyr tbl collect group_by summarize left_join mutate
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success
cache_refresh_tick <- function(config = attrakttv_config()) {
  con <- cache_db(pool = FALSE)
  on.exit(dbDisconnect(con), add = TRUE)

  cli_h1("attrakttv refresh tick (UTC: {format(Sys.time(), tz='UTC')})")

  if (!dbExistsTable(con, "shows")) {
    cli_alert_info("No shows table yet; nothing to refresh.")
    return(invisible(list(
      walked = 0L,
      refreshed = 0L,
      errors = 0L,
      skipped = 0L
    )))
  }

  shows <- tbl(con, "shows") |> collect()

  last_aired_tbl <- if (dbExistsTable(con, "episodes")) {
    tbl(con, "episodes") |>
      group_by(show_id) |>
      summarize(last_aired = max(first_aired, na.rm = TRUE)) |>
      collect()
  } else {
    tibble::tibble(show_id = character(0), last_aired = numeric(0))
  }

  shows <- shows |>
    left_join(last_aired_tbl, by = "show_id") |>
    mutate(
      last_aired_days = as.numeric(
        difftime(
          Sys.time(),
          as.POSIXct(last_aired, origin = "1970-01-01", tz = "UTC"),
          units = "days"
        )
      )
    )

  refreshed <- 0L
  errors <- 0L
  skipped <- 0L

  for (i in seq_len(nrow(shows))) {
    row <- shows[i, , drop = FALSE]
    if (!need_refresh(row, row$last_aired_days, "background", config)) {
      skipped <- skipped + 1L
      next
    }
    tier <- show_tier(row, row$last_aired_days, config)
    ok <- .do_refresh(row$show_id, con, title = row$title, tier = tier)
    if (ok) refreshed <- refreshed + 1L else errors <- errors + 1L
  }

  cli_alert_success(
    "Walked {nrow(shows)} shows: {refreshed} refreshed, {skipped} skipped, {errors} errors"
  )
  invisible(list(
    walked = nrow(shows),
    refreshed = refreshed,
    errors = errors,
    skipped = skipped
  ))
}

#' Sidecar loop — tick forever on the configured interval
#'
#' Container entry. Never returns under normal operation. Tick failures
#' are caught + logged so a flaky trakt response doesn't kill the loop;
#' the container restart policy handles unrecoverable crashes.
#'
#' @param config see [attrakttv_config].
#' @return Does not return.
#' @export
#' @importFrom cli cli_h1 cli_alert_danger cli_alert_info
cache_refresh_loop <- function(config = attrakttv_config()) {
  interval_seconds <- config$refresh$tick_interval_hours * 3600
  cli_h1(
    "attrakttv refresh sidecar starting (tick = {config$refresh$tick_interval_hours}h)"
  )
  repeat {
    tryCatch(
      cache_refresh_tick(config),
      error = function(e) {
        cli_alert_danger("Tick crashed: {conditionMessage(e)}")
      }
    )
    cli_alert_info("Sleeping {interval_seconds}s until next tick")
    Sys.sleep(interval_seconds)
  }
}
