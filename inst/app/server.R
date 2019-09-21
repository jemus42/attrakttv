#### Shiny Server ####
shinyServer(function(input, output, session) {

  # Caching observer ----
  observe({
    cached_shows <- cache_shows_tbl %>%
      collect() %>%
      arrange(desc(rating))

    show_ids <- paste0("cache:", cached_shows$show_id)
    names(show_ids) <- as.character(glue("{cached_shows$title} ({cached_shows$year})"))

    updateSelectizeInput(
      session, "shows_cached", choices = show_ids, selected = sample(show_ids, 1)
    )
  })

  # Query string observer ----
  observe({
    query <- getQueryString(session)
    query_slug <- query[['show']]

    if (!is.null(query_slug)) {
      show_tmp <- cache_shows_tbl %>% filter(slug == query_slug) %>% collect()
      show_id <- show_tmp$show_id

      if (!identical(show_id, character(1)) & !is.null(show_id)) {
        updateSelectizeInput(
          session, "shows_cached", selected = glue("cache:{show_id}")
        )
        click("get_show")
      }
    }
  })

  # Show info reactiveEvent ----
  show_info <- eventReactive(input$get_show, {

    if (stringr::str_detect(input$shows_cached, "^cache:")) {
      # cli_alert_info("cached show detected {input$shows_cached}")

      input_show <- input$shows_cached %>%
        stringr::str_extract(., "\\d+")

    } else {
      input_show <- input$shows_cached %>%
        stringr::str_remove(., "^cache:") %>%
        cache_add_show(cache_db_con = cache_db_con)

      # cli_alert_info("input_show after caching attempt is {input_show}")
    }

    if (is.null(input_show)) {
      return(NULL)
    }

    show_tmp <- cache_shows_tbl %>% filter(show_id == input_show)
    updateQueryString(glue("?show={pull(show_tmp, slug)}"), mode = "push", session = session)

    if (!is_already_cached("posters", input_show, cache_db_con)) {
      tibble(show_id = input_show, show_poster = get_fanart_poster(pull(show_tmp, tvdb))) %>%
        cache_add_data("posters", ., cache_db_con = cache_db_con)
    }

    show_tmp %>%
      left_join(
        cache_posters_tbl %>%
          select(show_id, show_poster),
        by = "show_id"
      ) %>%
      collect() %>%
      mutate(
        show_poster = if_else(show_poster == "", "https://dump.jemu.name/poster-blank.jpg", show_poster)
      )
  })

  # Show overview output ----
  output$show_overview <- renderUI({
    show <- show_info()
    # cat("show_name renderUI", show$title, "\n")

    # cli_alert_info("show status {show$status}")

    if (!is.null(show)) {
      tags$div(
        h2(
          a(href = glue("https://trakt.tv/shows/{show$slug}"), show$title),
          tags$small(show$status)
        ),
        wellPanel(
        fluidRow(
          column(
            2,
            class = "hidden-xs",
            tags$figure(
              img(
                src = show$show_poster,
                class = "img-responsive img-rounded"
              )
            )
          ),
          column(
            10,
            p(stringr::str_trunc(show$overview, 500, "right")),
            p(
              glue("Show rating: {round(show$rating, 1)} based on {show$votes} votes")
            )
          )
        )
      )
      )
    } else {
      fluidRow(
        column(
          10, offset = 1,
          h2("Nothing found :("),
          p("Try entering the show title, but like... try harder.")
        )
      )
    }

  })


  # get_show observer ----
  observeEvent(input$get_show, {
    # cat(input$shows_cached, "\n")

    if (input$get_show > 0) {
      # cat("input$get_show is", input$get_show, "\n")
      hide(id = "intro-wellpanel")
      shinyjs::show(id = "show_overview")
    }
  })
})
