
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.trakt <- list(
    trakt_db_path = "~/db"
  )
  toset <- !(names(op.trakt) %in% names(op))
  if (any(toset)) options(op.trakt[toset])

  if (!file.exists(getOption("trakt_db_path"))) {
    dir.create(getOption("trakt_db_path"), recursive = TRUE)
  }

  invisible()
}


globalVariables(c(
  "trakt", "cache_date", "available_translations", "genres", "updated_at",
  "first_aired", "show_id", "likes", "tvdb", ".", "lang"
))
