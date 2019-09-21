#### Shiny UI ####
shinyUI(
  navbarPage(
    title = app_title, inverse = FALSE, theme = shinytheme("flatly"),
    #### Main view ####
    tabPanel("Main",
      icon = icon("tasks"),
      tags$head(
        tags$meta(name = "google-site-verification", content = "fbD3_htmdCUtmrjbI1dAZbsS0sN-T10_U3xAN7W791Y"),
        includeHTML("html/proxy-click-js.html"),
        includeCSS("css/tRakt.css")
        #includeHTML("html/piwik.html")
      ),

      #### Episode information ####
      ## Show this when the actionButton was not clicked yet, marking the 'inactive' state
      # Used to be a clunky conditionalPanel, now just use shinyjs
      wellPanel(id = "intro-wellpanel", includeMarkdown("text/intro.md")),
      ## Show this only when the actionButton was clicked, marking the 'active' state

      hidden(
        wellPanel(id = "show_overview", htmlOutput("show_overview"))
      ),
      
      hidden(
        hr(),
        tabsetPanel(
          id = "episode_info", selected = "tab_plot"#,
          # tabPanel(
          #   title = "Plot", value = "tab_plot", icon = icon("bar-chart-o"),
          #   plotlyOutput(outputId = "episodeplot")
          # ),
          # tabPanel(
          #   title = "Episodes", value = "tab_data_episodes", icon = icon("table"),
          #   DT::dataTableOutput(outputId = "table_episodes")
          # ),
          # tabPanel(
          #   title = "Seasons", value = "tab_data_seasons", icon = icon("table"),
          #   DT::dataTableOutput(outputId = "table_seasons")
          # )
        )
      ),

      hr(),

      #### Control panel ####
      wellPanel(
        fluidRow(
          column(
            8, offset = 2,
            h3(icon("search"), "Show Selection"),
            # tagAppendAttributes(
            #   textInput(
            #     inputId = "show_searchbox",
            #     label = "Search a new show",
            #     value = "",
            #     placeholder = "Something like \"the simpsons\" should do"
            #   ),
            #   `data-proxy-click` = "get_show"
            # ),
            tagAppendAttributes(
              selectizeInput(
                inputId = "shows_cached", label = "Select from cache or enter a new one to search",
                choices = NULL, selected = NULL, 
                options = list(
                  create = TRUE,
                  placeholder = "Shows people lookup up before",
                  maxOptions = 50,
                  maxItems = 1
                )
              ),
              `data-proxy-click` = "get_show"
            ),
            actionButton(inputId = "get_show", label = "PLOTERIZZLE", icon = icon("play"))
          )
        )
      ),
      hr(),
      fluidRow(
        column(10, offset = 1, includeMarkdown("text/footer.md"))
      )
    ),
    tabPanel(
      title = "About", icon = icon("github"), href = "https://github.com/jemus42"
    ),
    # Didn't know where else to put it, but this one's a biggie
    useShinyjs()
  ) # end of navbarPage
) # End of shinyUI