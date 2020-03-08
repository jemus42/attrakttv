
<!-- README.md is generated from README.Rmd. Please edit that file -->

# attrakttv

<!-- badges: start -->

<!-- badges: end -->

Just a packaged shiny app relying on
[{tRakt}](https://github.com/jemus42/tRakt) to get data out of
<https://trakt.tv> and display it nicely.

## Installation

You can install the latest version of attrakttv from GitHub:

``` r
remotes::install("jemus42/attrakttv")
```

## Docker

Since this thing requires API keys for both <https://trakt.tv> and
<https://fanart.tv>, you’ll need to create accounts and get credentials
for those first.  
Then plug them into `attrakttv.env-sample` and rename it to
`attrakttv.env`.

Next you’ll need to create a `{httr}` OAuth2 token. The easiest way is
to call some function from `{tRakt}` that uses the trakt.tv API
(e.g. `tRakt::search_query("Counterpart")`) which will then prompt you
for an authentication PIN and open a corresponding browser window. Once
set up, the token should be located at `.httr-oauth`.

Once both `attrakttv.env` and `.httr-oauth` you can run

    docker build -t attrakttv .

And the app will be running at `localhost:3838`.

Note that currently the SQLite database is ephemeral - I’m working on
it. Should just be a volumes thing.

## Code of Conduct

Please note that the ‘attrakttv’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
