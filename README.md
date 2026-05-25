
<!-- README.md is generated from README.Rmd. Please edit that file -->

# attrakttv

<!-- badges: start -->

<!-- badges: end -->

A Shiny app for browsing TV show episode ratings, built on
[{tRakt}](https://github.com/jemus42/tRakt) and the
[trakt.tv](https://trakt.tv) API.

The app caches show / season / episode data in a local SQLite database
so repeat lookups are fast, and visualises per-episode rating
trajectories with [plotly](https://plotly.com/r/).

> attrakttv is an unofficial fan project and is **not affiliated with,
> endorsed by, or connected to trakt.tv**.

## Installation

``` r
# pak (recommended) — pulls in the {tRakt} GitHub remote automatically
pak::pak("jemus42/attrakttv")

# or remotes
remotes::install_github("jemus42/attrakttv")
```

## Usage

``` r
attrakttv::attrakttv_app()
```

By default the app listens on **<http://127.0.0.1:7842>** and opens in
your browser. Override with the `port` argument or the `attrakttv.port`
option.

Cached data is stored in a SQLite database under the
platform-appropriate data directory
(`rappdirs::user_data_dir("attrakttv", <version>)`):

| Platform | Default path                                                 |
|----------|--------------------------------------------------------------|
| macOS    | `~/Library/Application Support/attrakttv/<version>/tRakt.db` |
| Linux    | `~/.local/share/attrakttv/<version>/tRakt.db`                |
| Windows  | `%LOCALAPPDATA%/attrakttv/attrakttv/<version>/tRakt.db`      |

Set the `trakt_db_path` environment variable to override the directory.

### API credentials

- **trakt.tv:** not required for normal read-only use — `{tRakt}` ships
  with a scrambled fallback client_secret since 0.17.0, so basic data
  retrieval works out of the box. Set `trakt_client_id` /
  `trakt_client_secret` in your `~/.Renviron` if you want your own
  quota.
- **fanart.tv:** required to fetch poster images. Sign up at
  <https://fanart.tv/get-an-api-key/>, then set `fanarttv_api_key` in
  your `~/.Renviron`. See `attrakttv.env-sample` for the variable names.

Sourcing the included `attrakttv.env-sample` instead of `~/.Renviron`
also works — it is recognised by `Sys.getenv()` when copied to
`.Renviron` in the working directory.

## Features

- Search any TV show by title and see per-show metadata, season summary,
  and per-episode ratings.
- Caches results to SQLite so subsequent lookups are instant.
- Built with [{bslib}](https://rstudio.github.io/bslib/) on Bootstrap 5
  with light/dark mode toggle.
- Stable URL per show via query params (e.g. `?show=counterpart`).

## Development

``` sh
make install      # pak::local_install
make launch       # launch the installed app on port 7842
make launch-dev   # devtools::load_all + run against /tmp/attrakttv-smoke-db
make document     # devtools::document
make check        # devtools::check
```

`make launch-dev` uses a throwaway SQLite db at
`/tmp/attrakttv-smoke-db` so it never pollutes your real cache.

## Deployment

The included `Dockerfile` / `docker-compose.yml` predate the current
revival and are pending an overhaul — treat them as starting points
only. The legacy `.httr-oauth` PIN dance described in older docs is no
longer required (see API credentials above).

## Code of Conduct

Please note that the ‘attrakttv’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
