# attrakttv 0.1.0.9000 (development version)

* **Facelift.** Migrated from `shinythemes::shinytheme('flatly')` (Bootstrap 3)
  to `bslib::page_navbar()` with `bs_theme(version = 5, brand = TRUE)`. New
  `_brand.yml` defines the trakt-aligned red accent and Inter/JetBrains Mono
  typography; the rest of the palette comes from bslib's "shiny" preset which
  natively supports light/dark color modes.
* Added a light/dark mode toggle (`bslib::input_dark_mode()`) in the navbar.
* Restructured the show overview into a `bslib::card` with a `value_box` row
  for Rating / Votes / Episodes / Runtime. Replaces the old `kableExtra` table.
* Replaced the `Plot` / `Table` `tabsetPanel` with `navset_card_underline()`.
* Added a footer disclaiming any affiliation with trakt.tv.
* Dropped `kableExtra` and `shinythemes` from `Imports`; added `bslib`,
  `bsicons`, `brand.yml`.
* Switched DT tables to `style = "auto"` so they pick up Bootstrap 5 styling.
* `cache_add_episodes()` now normalises seasons and episodes tibbles via the
  new `cleanup_seasons()` / `cleanup_episodes()` helpers, so the on-disk
  schema for those tables is stable across tRakt version drift (previously
  any change in `seasons_summary()` columns would break appends to an
  existing cache).
* `attrakttv_app()` now binds to a fixed port (`7842` by default) so local
  testing always lands on the same URL. Override with the `port` argument
  or the `attrakttv.port` option.
* Bumped version to dev (`0.1.0.9000`), which also changes the default
  `cache_db_path()` to `~/Library/Application Support/attrakttv/0.1.0.9000/`
  on macOS — older caches at `0.1.0/` can be deleted.

# attrakttv 0.1.0

Still working out the basic functionality regarding the database and the app itself.

* Make `cache_db_path()` smarter about the default db location via `rappdirs::user_data_dir()`.
* Added `Dockerfile` and `docker-compose.yml` for easier deployment.
* Added a `NEWS.md` file to track changes to the package.
