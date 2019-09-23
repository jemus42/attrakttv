shinyUI(
  navbarPage(
    title = app_title,
    theme = shinythemes::shinytheme("flatly"),
    collapsible = TRUE,
    # Main view ----
    tabPanel("Main",
      icon = icon("tasks"),
      tags$head(
        #tags$meta(name = "google-site-verification", content = "fbD3_htmdCUtmrjbI1dAZbsS0sN-T10_U3xAN7W791Y"),
        tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
        tags$script(src = "js/proxy-click.js", type = "application/javascript"),
        tags$link(href = "css/tRakt.css", rel = "stylesheet")
        #includeHTML("html/piwik.html")
      ),

      # Intro text ----
      wellPanel(id = "intro-wellpanel", includeMarkdown("text/intro.md")),

      # Control panel ----
      wellPanel(
        fluidRow(
          column(
            10, offset = 1,
            #h3(icon("search"), "Show Selection"),
            tagAppendAttributes(
              selectizeInput(
                inputId = "shows_cached", label = NULL,
                choices = show_ids, selected = NULL,
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
        fluidRow(id = "season_container",
          hr(),
          h3("Seasons", style = "text-align: center;"),
          column(
            12,
            DT::DTOutput(outputId = "show_season_table")
            )
          )
      ),
      hidden(
        fluidRow(id = "episodes_container",
          hr(),
          h3("Episodes", style = "text-align: center;"),
          column(
           12,
           DT::DTOutput(outputId = "show_episodes_table")
          )
        )
      )
    ),
    # Footer ----
    footer = fluidRow(
      hr()
    ),
    # Didn't know where else to put it, but this one's a biggie
    useShinyjs()
  ) # end of navbarPage
) # End of shinyUI
