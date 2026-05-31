library(bslib)
library(bsicons)

intro_card <- card(
  id = "intro-card",
  class = "intro-card",
  card_body(
    div(
      class = "intro-card-body",
      img(
        src = "img/trakt-logomark.svg",
        class = "intro-logo",
        alt = "trakt logomark",
        height = "44"
      ),
      div(
        class = "intro-text",
        p(class = "lead mb-2",
          "attrakttv lets you quickly inspect TV show ratings on a per-episode basis,",
          " powered by ", a(href = "https://trakt.tv", "trakt.tv"), " data."
        ),
        p(class = "text-muted small mb-0",
          "Pick a cached show from the dropdown, or type any title to look it up.",
          " Source is on ", a(href = "https://github.com/jemus42/attrakttv", "GitHub"), "."
        )
      )
    )
  )
)

app_theme <- bs_theme(
  version = 5,
  brand = TRUE,
  preset = "shiny"
)

# Cache-bust local assets with the file mtime so browsers pick up updates
# without a manual Shift+Reload. Shiny doesn't emit a Cache-Control header,
# so plain `?v=` is enough to defeat the validation cache. The URL path is
# relative to inst/app/www/ (which is what Shiny serves from the root).
asset_url <- function(url_path) {
  abs <- system.file(file.path("app", "www", url_path), package = "attrakttv")
  v <- if (nzchar(abs)) as.integer(file.info(abs)$mtime) else 0L
  sprintf("%s?v=%d", url_path, v)
}

app_head <- tags$head(
  tags$link(rel = "shortcut icon", href = "favicon.png"),
  tags$script(src = asset_url("js/proxy-click.js"), type = "application/javascript"),
  tags$script(src = asset_url("js/selectize-search.js"), type = "application/javascript"),
  tags$link(href = asset_url("css/tRakt.css"), rel = "stylesheet"),
  tags$noscript(p(img(
    src = "//analytics.tadaa-data.de/matomo.php?idsite=22&amp;rec=1",
    style = "border:0;",
    alt = ""
  )))
)

shinyUI(tagList(
  app_head,
  page_navbar(
    title = "attrakttv",
    id = "main_nav",
    theme = app_theme,
    window_title = "attrakttv",
    navbar_options = navbar_options(collapsible = TRUE, underline = TRUE),
    fillable = FALSE,

    nav_panel(
      title = "Show Data",
      icon = bs_icon("graph-up"),
      useShinyjs(),

      intro_card,

      div(
        class = "control-bar",
        tagAppendAttributes(
          selectizeInput(
            width = "100%",
            inputId = "shows_cached", label = NULL,
            choices = show_ids, selected = "",
            options = list(
              create = TRUE,
              placeholder = "Pick a show – if it's not listed yet I'll look it up",
              maxOptions = 50,
              maxItems = 1
            )
          ),
          `data-proxy-click` = "get_show"
        ),
        # Proxy only, never displayed
        hidden(actionButton(
          inputId = "get_show",
          label = " Do the thing with the stuff",
          width = "100%"
        ))
      ),

      hidden(uiOutput("show_overview")),

      hidden(
        div(
          id = "season_container",
          class = "section-block",
          navset_card_underline(
            id = "season_tabset",
            title = "Seasons",
            selected = "Table",
            full_screen = TRUE,
            nav_panel(
              title = "Table",
              icon = bs_icon("table"),
              DT::DTOutput(outputId = "show_seasons_table", width = "100%")
            ),
            nav_panel(
              title = "Plot",
              icon = bs_icon("box"),
              plotlyOutput("plotly_seasons", height = "400px")
            ),
            nav_spacer(),
            nav_item(
              div(
                class = "plot-axis-controls",
                radioButtons(
                  inputId = "se_y",
                  label = "y:",
                  choices = c("Auto" = "auto", "0+" = "tozero", "1–10" = "fixed"),
                  selected = "auto",
                  inline = TRUE
                )
              )
            )
          )
        )
      ),

      hidden(
        div(
          id = "episodes_container",
          class = "section-block",
          navset_card_underline(
            id = "episode_tabset",
            title = "Episodes",
            selected = "Plot",
            full_screen = TRUE,
            nav_panel(
              title = "Plot",
              icon = bs_icon("graph-up"),
              plotlyOutput("plotly_episodes", height = "500px")
            ),
            nav_panel(
              title = "Table",
              icon = bs_icon("table"),
              DT::DTOutput(outputId = "show_episodes_table", width = "100%")
            ),
            nav_spacer(),
            nav_item(
              div(
                class = "plot-axis-controls",
                radioButtons(
                  inputId = "ep_x",
                  label = "x:",
                  choices = c("Episode #" = "abs", "Air date" = "date"),
                  selected = "abs",
                  inline = TRUE
                ),
                radioButtons(
                  inputId = "ep_y",
                  label = "y:",
                  choices = c("Auto" = "auto", "0+" = "tozero", "1–10" = "fixed"),
                  selected = "auto",
                  inline = TRUE
                )
              )
            )
          )
        )
      )
    ),

    nav_panel(
      title = "Changelog",
      icon = bs_icon("journal-text"),
      card(
        card_body(
          includeMarkdown(system.file("NEWS.md", package = "attrakttv"))
        )
      )
    ),

    nav_spacer(),
    nav_item(input_dark_mode(id = "color_mode", mode = "dark")),

    footer = tags$footer(
      class = "app-footer text-center small text-muted py-3",
      "attrakttv is an unofficial fan project and is ",
      tags$strong("not affiliated with, endorsed by, or connected to trakt.tv"),
      ". · ",
      a("Source on GitHub", href = "https://github.com/jemus42/attrakttv"),
      " · ",
      glue("v{utils::packageVersion('attrakttv')}")
    )
  )
))
