# Make "seed" tbls for db init

seed_episodes <- tbl(cache_db_con, "episodes") %>%
  head(1) %>%
  collect()
usethis::use_data(seed_episodes, overwrite = TRUE)

seed_shows <- tbl(cache_db_con, "shows") %>%
  head(1) %>%
  collect()
usethis::use_data(seed_episodes, overwrite = TRUE)

seed_seasons <- tbl(cache_db_con, "seasons") %>%
  head(1) %>%
  collect()
usethis::use_data(seed_episodes, overwrite = TRUE)

seed_requests <- tbl(cache_db_con, "requests") %>%
  head(1) %>%
  collect()
usethis::use_data(seed_episodes, overwrite = TRUE)

seed_posters <- tbl(cache_db_con, "posters") %>%
  head(1) %>%
  collect()
usethis::use_data(seed_episodes, overwrite = TRUE)
