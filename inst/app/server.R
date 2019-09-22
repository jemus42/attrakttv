#### Shiny Server ####
shinyServer(function(input, output, session) {

  # Caching observer ----
  observe({
    cached_shows <- cache_shows_tbl %>%
      collect() %>%
      filter(rating >= 7, votes >= 1000) %>%
      sample_frac(1)

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
      tibble(
        show_id = input_show,
        show_poster = get_fanart_poster(pull(show_tmp, tvdb))
      ) %>%
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
        show_poster = if_else(show_poster == "", "img/poster-blank.jpg", show_poster)
      )
  })

  # Show seasons reactive ----
  show_seasons <- eventReactive(input$get_show, {

    current_show <- show_info()
    current_show_id <- current_show$show_id

    current_show_seasons <- cache_seasons_tbl %>%
      filter(show_id == current_show_id) %>%
      collect()

    current_show_seasons
  })

  # Show overview output ----
  output$show_overview <- renderUI({
    show <- show_info()
    show_seasons <- show_seasons()
    cli_alert("renderUI: show_overview")

    # Early return for no result
    if (is.null(show)) {
      res <- fluidRow(
        column(
          10, offset = 1,
          h2("Nothing found :("),
          p("Try entering the show title, but like... try harder.")
        )
      )
      return(res)
    }

    summary_table <- show %>%
      select(rating, votes, episodes = aired_episodes, runtime, network, country) %>%
      mutate(
        rating = round(rating, 1),
        country = country_label(country),
        runtime = glue("{runtime}min")
      ) %>%
      mutate_if(is.na, ~ "N/A") %>%
      rename_all(str_to_title) %>%
      knitr::kable(format = "html") %>%
      kableExtra::kable_styling(
        full_width = FALSE, font_size = 18, position = "left",
        bootstrap_options = c("responsive")
      ) %>%
      HTML()

    # Otherwise, do a thing
    tags$div(
      h2(
        a(href = glue("https://trakt.tv/shows/{show$slug}"),
          glue("{show$title} ({show$year})")),
        br(),
        tags$small(stringr::str_to_title(show$status))
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
            p(class = "lead", stringr::str_trunc(show$overview, 200, "right")),
            summary_table
          )
        )
      )
    )
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

  # User search logging?
  observeEvent(input$get_show, {

    if ((input$get_show %% 2) == 0) return(NULL)

    res <- tibble(
      time = as.numeric(lubridate::now(tzone = "UTC")),
      request = input$shows_cached
    )
    cli_alert("Caching request")
    check_cache_table("requests", res, cache_db_con)
    RSQLite::dbWriteTable(cache_db_con, "requests", res, append = TRUE)
  })
})
