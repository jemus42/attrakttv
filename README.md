
<!-- README.md is generated from README.Rmd. Please edit that file -->

# attrakttv

<!-- badges: start -->

<!-- badges: end -->

Just a packaged shiny app relying on
[{tRakt}](https://github.com/jemus42/tRakt) to get data out of
<https://trakt.tv> and display it nicely.

You can find it currently deployed at
[attrakttv.tadaa-data.de](https://attrakttv.tadaa-data.de/).

Please open an issue if you encounter crashes.

## Installation

You can install the latest version of attrakttv from GitHub:

``` r
remotes::install("jemus42/attrakttv")
```

## Usage (Local)

The app needs both credentials for the **trakt.tv** and the
**fanart.tv** API, see `attrakttv.env-sample` for reference. Plug in
your keys and either rename the file to `.Renviron` in the current
working directory or copy these values to your user/global `.Renviron`.

Then you can call `attrakttv::attrakttv_app()` and try to add a show,
which will then hopefully open a browser window displaying a trakt.tv
PIN for you to enter at the R console.  
You can also achieve this by calling any data-retrieving function from
the `{tRakt}` package.

The the downloaded show data is stored in a SQLite database located
under `~/.local/attrakttv/db/tRakt.db` by default, but you can change
that value via the aforementioned configuration file.

## Usage (Docker)

Since this thing requires API keys for both <https://trakt.tv> and
<https://fanart.tv>, you’ll need to create accounts and get credentials
for those first.  
Copy the file via `cp attrakttv.env-sample attrakttv.env` and plug in
your values.

Next you’ll need to create a `{httr}` OAuth2 token. The easiest way is
to call some function from `{tRakt}` that uses the trakt.tv API
(e.g. `tRakt::search_query("Counterpart")`) which will then prompt you
for an authentication PIN and open a corresponding browser window. Once
set up, the token should be located at `.httr-oauth`.

Once both `attrakttv.env` and `.httr-oauth` you can run

    docker build -t attrakttv .
    docker run --rm -p 3838:3838 --run-as=shiny attrakttv

And the app will be running at `localhost:3838`.

Note that currently the SQLite database is ephemeral.  
You can use `docker-compose` with the included `docker-compose.yml`
which will put the database in a volume called `trakt_db`. This is
mapped to a local volume you have to define via environment variable set
in `.env`, so `cp .env-sample .env` and plug in your desired path (which
**must exist**).

Regarding the volume thing: [This was
helpful](http://blog.code4hire.com/2018/06/define-named-volume-with-host-mount-in-the-docker-compose-file/)

## Code of Conduct

Please note that the ‘attrakttv’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
