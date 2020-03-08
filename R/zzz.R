
.onLoad <- function(libname, pkgname) {
  # op <- options()
  # op.trakt <- list(
  #   trakt_db_path = "~/db"
  # )
  # toset <- !(names(op.trakt) %in% names(op))
  # if (any(toset)) options(op.trakt[toset])

  if (!file.exists(Sys.getenv("trakt_db_path", unset = tempdir()))) {
    dir.create(Sys.getenv("trakt_db_path", unset = "~/db"), recursive = TRUE)

    db_init()
  }

  invisible()
}


globalVariables(c(
  "trakt", "cache_date", "available_translations", "genres", "updated_at",
  "first_aired", "show_id", "likes", "tvdb", ".", "lang"
))
