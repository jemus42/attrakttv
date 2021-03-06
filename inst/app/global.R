#### Loading libraries ####
# if (!("devtools" %in% installed.packages())) install.packages("devtools")
# devtools::install_deps(upgrade = "never")

library(shiny)
library(shinyjs)
library(DT)
#library(ggplot2)
library(plotly)
library(tRakt)
# library(RSQLite)
library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(cliapp)
library(kableExtra)
library(attrakttv)

# Init db if needed
if (!file.exists(cache_db_path())) {
  cliapp::cli_alert_warning("Trying to init db at {cache_db_path()}")
  db_init()
}

# Database connection -----
cache_db_con <- cache_db()

cache_shows_tbl    <- tbl(cache_db_con, "shows")
cache_posters_tbl  <- tbl(cache_db_con, "posters")
cache_seasons_tbl  <- tbl(cache_db_con, "seasons")
cache_episodes_tbl <- tbl(cache_db_con, "episodes")

#### Setting some values ----
app_title <- glue("attrakttv v{desc::desc_get_version()}")

## Define some HTML characters
bullet <- HTML("&#8226;")
# mu     <- HTML("&#956;")
# sigma  <- HTML("&#963;")

cached_shows <- cache_shows_tbl %>% collect()

# Jumble around a little so you're not always shown the same few shows
cached_shows <- bind_rows(
    cached_shows %>% filter(rating >= 7 & votes >= 1000) %>% sample_frac(1),
    cached_shows %>% filter(!(rating >= 7 & votes >= 1000))
  )


show_ids <- paste0("cache:", cached_shows$show_id)

if (identical(show_ids, "cache:")) {
  show_ids <- ""
} else {
  names(show_ids) <- glue(
    "{cached_shows$title} ({cached_shows$year})"
  ) %>%
    as.character()

  # Append empty string to initialize empty (with displayed placeholder text)
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
onStop(function() {
  pool::poolClose(pool = cache_db_con)
})
