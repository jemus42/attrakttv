intro_text <- wellPanel(
  id = "intro-wellpanel",
  img(src = "img/trakt-tall-red-black.svg", width = 100,
      id = "intro-trakt-logo", style = "float: right; margin-left: 10px; margin-bottom: 10px;"),
  p(
    class = "lead",
    HTML("This is <code>attrakttv</code>, for now at least (naming things is hard).")
  ),
  p("It's a complete overhaul of ",
    tags$a(href = "https://trakt.jemu.name", "trakt.jemu.name"),
    ", and while not feature complete yet, it's at least a lot faster and less terrible."
  ),
  p("You can look up TV show data from ", a(href = "https://trakt.tv", "trakt.tv"),
    "and get data and plots for seasons and episodes."
  ),
  p("The source is available on ", a(href = "https://github.com/jemus42/attrakttv", "GitHub")),
  h3("How do you even stuff?"),
  p("Select a show from the dropdown menu, or look up a new one by, well, entering a new thing.")
)

# Image next to dropdown item seems… unhelpful
# dropdown_item_render <- I("{
#   option: function(item, escape) {
#     return '<div>' + escape(item.label) +
#     '<img src=\"img/trakt-icon-black.svg\" width=20 style=\"float: right;\"/></div>'
#   }
# }")

shinyUI(
  navbarPage(
    title = app_title, # tags$a(href = "/", app_title),
    theme = shinythemes::shinytheme("flatly"),
    collapsible = TRUE,
    # Main view ----
    tabPanel("Main",
      icon = icon("tasks"),
      tags$head(
        #tags$meta(name = "google-site-verification", content = "fbD3_htmdCUtmrjbI1dAZbsS0sN-T10_U3xAN7W791Y"),
        tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
        tags$script(src = "js/matomo.js", type = "application/javascript"),
        tags$script(src = "js/proxy-click.js", type = "application/javascript"),
        tags$link(href = "css/tRakt.css", rel = "stylesheet"),
        tags$noscript(p(img(
          src = "//analytics.tadaa-data.de/matomo.php?idsite=22&amp;rec=1",
          style = "border:0;",
          alt = ""))
        )
      ),
      # shinythemes::themeSelector(),

      # Intro text ----
      intro_text,

      # Control panel ----
      wellPanel(
        fluidRow(style = "margin-top: 15px;",
          column(
            10, offset = 1,
            #h3(icon("search"), "Show Selection"),
            tagAppendAttributes(
              selectizeInput(width = "100%",
                inputId = "shows_cached", label = NULL,
                choices = show_ids, selected = "",
                options = list(
                  create = TRUE,
                  placeholder = "Pick a show – if it's not listed yet I'll look it up",
                  maxOptions = 50,
                  maxItems = 1
                  # render = dropdown_item_render
                )
              ),
              `data-proxy-click` = "get_show"
            ),
            # Not displayed (ever)
            # Only used as a proxy
            hidden(actionButton(
              inputId = "get_show",
              label = " Do the thing with the stuff",
              width = "100%"
            ))
          )
        )
      ),
      # Show information ----
      hidden(
        htmlOutput("show_overview")
      ),

      # Season information ----
      hidden(
        fluidRow(id = "season_container", class = "collapse",
          hr(),
          h2(
            tags$a(href = "#season_table", `data-toggle` = "collapse", "Seasons"),
            style = "text-align: center;"
          ),
          hr(),
          column(id = "season_table",
            12, class = "seasons_table_column",
            DT::DTOutput(outputId = "show_seasons_table", width = "100%")
            )
          )
      ),
      # Episode information ----
      hidden(
        fluidRow(id = "episodes_container", class = "collapse",
          hr(),
          h2(
            tags$a(href = "#episodes_table", `data-toggle` = "collapse", "Episodes"),
            style = "text-align: center;"
          ),
          hr(),
          tabsetPanel(type = "pills", id = "episode_tabset", selected = "Plot",
            tabPanel(title = "Table", icon = icon("table"),  id = "episodes_table_tab",
              column(id = "episodes_table",
                     width = 12, class = "episodes_table_column",
                     DT::DTOutput(outputId = "show_episodes_table", width = "100%")
              )
            ),
            tabPanel(title = "Plot", icon = icon("chart-line"), id = "episodes_plot_tab",
              column(id = "episodes_plot",
                     width = 12, class = "episodes_table_column",
                     plotlyOutput("plotly_episodes", height = "500px")
              )
            )
          )
        )
      )
    ),
    # Footer ----
    footer = div(
      hr(),
      hidden(column(12, id = "debug_info",
        h3("Debug info")
      )
    )),
    # Didn't know where else to put it, but this one's a biggie
    useShinyjs()
  ) # end of navbarPage
) # End of shinyUI
