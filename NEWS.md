# attrakttv 0.1.0.9000 (development version)

* Live trakt search in the show dropdown. As you type, the selectize
  control fetches the top trakt matches (debounced 250 ms) and merges
  them alongside the locally-cached shows, prefixed `trakt:<id>` until
  picked. Disambiguates between e.g. "Silo (2023)" and "Sí lo digo
  (2022)" without needing to guess the right year. Picking a `trakt:`
  result caches the show and re-renders the dropdown with the regular
  `cache:` entry. Freeform "type-then-enter" still works as a fallback
  if JS is off or the user submits before the search returns.
* Plot controls per card header: the Episodes plot gets an **x-axis**
  toggle (`Episode #` vs `Air date` — the date axis shows real hiatuses
  as gaps, useful for shows with long breaks) and a **y-axis** toggle
  (`Auto` / `0+` / `1–10`); the Seasons boxplot gets the same y toggle.
  Both navset cards now expose bslib's `full_screen` expand button, and
  the resting plot heights bump up (episodes 420 → 500 px, seasons
  320 → 400 px) for less cramped defaults.
* Deployable as a single-container Docker stack: the rewritten
  `Dockerfile` (rocker/r-ver + pak, no shiny-server) and
  `docker-compose.yml` (bind-mount `./data/`, port
  `127.0.0.1:7842:3838`, `.env` for secrets) follow the horst
  one-folder-per-service convention. `attrakttv_app()` gained a
  `host` argument so the container can bind `0.0.0.0` while local
  dev keeps the `127.0.0.1` default. The README's "Deployment"
  section documents the full flow including the Caddy stanza for
  `attrakttv.jemu.name`.
* New: per-season boxplot in the Seasons view (alongside the existing Table
  tab) shows the distribution of episode ratings per season. Designed to
  scale from 1-season shows up to long-runners (35+ seasons); x-axis labels
  rotate when the season count gets dense.
* Wired up trakt's current logomark assets:
  `trakt-logomark.svg` shows as a small accent in the intro card, and the
  mono variant overlays the episodes rating plot (previously a stale
  `trakt-icon-black.png` reference that no longer existed).
* Refreshed the README: drops the stale `.httr-oauth` PIN-dance setup
  instructions, updates the default cache path to the `rappdirs` location,
  notes the fixed local port (7842), and documents the new `make` targets.
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
