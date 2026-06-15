#### Loading libraries ####
library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(tRakt)
library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(cli)
library(attrakttv)

# Init db if needed
if (!file.exists(cache_db_path())) {
  cli::cli_alert_warning("Trying to init db at {cache_db_path()}")
  db_init()
}

# Database connection -----
cache_db_con <- cache_db()

# Helper: lazy tbl if the table exists, else an empty tibble shaped like the
# real table so the app boots cleanly against a fresh db. Tables are created
# on first write by cache_add_data() -- see [[trakt-version-coupling]] memory.
# Each accessor is a closure so it re-resolves after a table is created
# mid-session (e.g. posters written by the show_info reactive).
empty_tbl <- function(...) {
  cols <- list(...)
  tibble::as_tibble(lapply(cols, \(type) {
    switch(
      type,
      character = character(0),
      integer = integer(0),
      numeric = numeric(0)
    )
  }))
}

cache_tbl_factory <- function(con, name, empty) {
  function() {
    if (DBI::dbExistsTable(con, name)) tbl(con, name) else empty
  }
}

cache_shows_tbl <- cache_tbl_factory(
  cache_db_con,
  "shows",
  empty_tbl(
    show_id = "character",
    slug = "character",
    title = "character",
    year = "integer",
    rating = "numeric",
    votes = "integer",
    country = "character",
    language = "character",
    network = "character",
    status = "character",
    overview = "character",
    aired_episodes = "integer",
    runtime = "integer",
    tvdb = "character"
  )
)
cache_posters_tbl <- cache_tbl_factory(
  cache_db_con,
  "posters",
  empty_tbl(
    show_id = "character",
    show_poster = "character"
  )
)
cache_seasons_tbl <- cache_tbl_factory(
  cache_db_con,
  "seasons",
  empty_tbl(
    show_id = "character",
    season = "integer",
    title = "character",
    rating = "numeric",
    votes = "integer",
    first_aired = "numeric",
    aired_episodes = "integer",
    episode_count = "integer"
  )
)
cache_episodes_tbl <- cache_tbl_factory(
  cache_db_con,
  "episodes",
  empty_tbl(
    show_id = "character",
    season = "integer",
    episode = "integer",
    title = "character",
    rating = "numeric",
    votes = "integer",
    first_aired = "numeric",
    comment_count = "integer",
    runtime = "integer"
  )
)

#### Setting some values ----
app_title <- glue("attrakttv v{utils::packageVersion('attrakttv')}")

## Define some HTML characters
bullet <- HTML("&#8226;")

cached_shows <- cache_shows_tbl() |> collect()

if (nrow(cached_shows) == 0) {
  show_ids <- ""
} else {
  # Order: groups of same-titled shows sorted by their best entry's
  # popularity (so a hit reboot lifts the original alongside it); within a
  # group, by year so reboots/spinoffs stay adjacent (e.g. Scrubs 2001
  # directly above Scrubs 2026). Popularity = rating * log10(votes + 10)
  # so a 9.0 with 50k votes outweighs a 9.5 with 100.
  cached_shows <- cached_shows |>
    mutate(
      .pop = ifelse(is.na(rating) | is.na(votes), 0, rating * log10(votes + 10))
    ) |>
    group_by(title) |>
    mutate(.group_pop = max(.pop)) |>
    ungroup() |>
    arrange(desc(.group_pop), title, year) |>
    select(-.pop, -.group_pop)

  show_ids <- paste0("cache:", cached_shows$show_id)
  names(show_ids) <- as.character(glue(
    "{cached_shows$title} ({cached_shows$year})"
  ))

  # Append empty string for placeholder text
  show_ids <- c("", show_ids)
}

# Helper functions ----

# Show status
# https://trakt.docs.apiary.io/#reference/shows/summary/get-a-single-show
# Note: When getting full extended info,
# the status field can have a value of returning series (airing right now),
# in production (airing soon), planned (in development), canceled, or ended.

# label_show_status <- function(status) {
#   bs3_badge <- function(badge_type, label) {
#     glue('<span class="label label-{badge_type}">{label}</span>')
#   }
#   # bs4_badge <- function(badge_type, label) {
#   #   glue('<span class="badge badge-{badge_type}">{label}</span>')
#   # }
#   status <- stringr::str_to_title(status)
#
#   case_when(
#     status %in% c("ended") ~ bs3_badge("default", status),
#     status %in% c("returning series") ~ bs3_badge("primary", status),
#     status %in% c("in production", "planned") ~ bs3_badge("info", status),
#     status %in% c("canceled") ~ bs3_badge("danger", status),
#     TRUE ~ bs3_badge("default", status)
#   )
# }

# Shiny start/stop ----
# Close pool on stop
onStop(\() {
  pool::poolClose(pool = cache_db_con)
})
