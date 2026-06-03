#' Load runtime config
#'
#' Resolution order:
#'  1. `path` argument (if given)
#'  2. `ATTRAKTTV_CONFIG` env var
#'  3. `/etc/attrakttv/config.yml` (container convention)
#'  4. Baked-in `inst/config.default.yml`
#'
#' User config (any of 1-3) is merged on top of the baked default with
#' [utils::modifyList()], which recurses into nested lists, so partial
#' overrides only need to set the keys they want to change.
#'
#' @param path Optional explicit path to a YAML config file.
#' @return A named list, e.g. `list(refresh = list(tick_interval_hours = 12, ...))`.
#' @export
#' @importFrom yaml read_yaml
#' @examples
#' \dontrun{
#' cfg <- attrakttv_config()
#' cfg$refresh$tick_interval_hours
#' }
attrakttv_config <- function(path = NULL) {
  defaults_path <- system.file("config.default.yml", package = "attrakttv")
  if (!nzchar(defaults_path)) {
    stop("Bundled config.default.yml not found in installed package.")
  }
  defaults <- yaml::read_yaml(defaults_path)

  candidate <- if (!is.null(path)) {
    path
  } else if (nzchar(Sys.getenv("ATTRAKTTV_CONFIG"))) {
    Sys.getenv("ATTRAKTTV_CONFIG")
  } else if (file.exists("/etc/attrakttv/config.yml")) {
    "/etc/attrakttv/config.yml"
  } else {
    NULL
  }

  if (!is.null(candidate) && file.exists(candidate)) {
    user_cfg <- yaml::read_yaml(candidate)
    utils::modifyList(defaults, user_cfg)
  } else {
    defaults
  }
}
