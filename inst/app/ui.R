intro_text <- wellPanel(
  id = "intro-wellpanel",
  p(
    class = "lead",
    HTML("This is <code>attrakttv</code>, for now at least (naming things is hard).")
  ),
  p("It's a complete overhaul of ",
    tags$a(href = "https://trakt.jemu.name", "trakt.jemu.name"),
    ", and while not feature complete yet, it's at least a lot faster and less terrible."
  ),
  p("You can look up TV show data from ", a(href = "https://trakt.tv", "trakt.tv"),
    "and get data on seasons and episodes. Soonish there'll be plots. Hopefully pretty plots."
  ),
  p("The source is available on ", a(href = "https://github.com/jemus42/attrakttv", "GitHub")),
  h3("How do you even stuff?"),
  p("Select a show from the dropdown menu, or look up a new one by, well, entering a new thing.")
)

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
        fluidRow(
          column(
            10, offset = 1,
            #h3(icon("search"), "Show Selection"),
            tagAppendAttributes(
              selectizeInput(
                inputId = "shows_cached", label = NULL,
                choices = show_ids, selected = "",
                options = list(
                  create = TRUE,
                  placeholder = "Pick a show – if it's not listed yet I'll look it up",
                  maxOptions = 25,
                  maxItems = 1
                )
              ),
              `data-proxy-click` = "get_show"
            ),
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
        div(id = "show_overview", htmlOutput("show_overview"))
      ),

      # Episode information ----
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
            DT::DTOutput(outputId = "show_seasons_table", width = "97%")
            )
          )
      ),
      hidden(
        fluidRow(id = "episodes_container", class = "collapse",
          hr(),
          h2(
            tags$a(href = "#episodes_table", `data-toggle` = "collapse", "Episodes"),
            style = "text-align: center;"
          ),          hr(),
          column(id = "episodes_table",
           width = 12, class = "episodes_table_column",
           DT::DTOutput(outputId = "show_episodes_table", width = "97%")
          )
        )
      )
    ),
    # Footer ----
    footer = hr(),
    # Didn't know where else to put it, but this one's a biggie
    useShinyjs()
  ) # end of navbarPage
) # End of shinyUI
