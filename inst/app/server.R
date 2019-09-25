# options(shiny.reactlog = TRUE)
# library(reactlog)


shinyServer(function(input, output, session) {

  # Caching observer ----
  # observe(label = "Cache initializer", {
  #   cached_shows <- cache_shows_tbl %>%
  #     collect() %>%
  #     filter(rating >= 7, votes >= 1000) %>%
  #     sample_frac(1)
  #
  #   show_ids <- paste0("cache:", cached_shows$show_id)
  #   names(show_ids) <- as.character(glue("{cached_shows$title} ({cached_shows$year})"))
  #
  #   updateSelectizeInput(
  #     session, "shows_cached", choices = show_ids, selected = sample(show_ids, 1)
  #   )
  # })

  observeEvent(input$shows_cached, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if (input$shows_cached != "") click("get_show")
  })

  # Query string observer ----
  observe(label = "Query string updater", {

    query <- getQueryString(session)
    query_slug <- query[['show']]

    if (!is.null(query_slug)) {
      show_tmp <- cache_shows_tbl %>% filter(slug == query_slug) %>% collect()
      show_id <- show_tmp$show_id

      if (!identical(show_id, character(1)) & !is.null(show_id)) {
        updateSelectizeInput(
          session, "shows_cached", selected = glue("cache:{show_id}")
        )
        # click("get_show")
      }
    }

    query_debug <- query[['debug']]

     if (identical(query_debug, "true")) {
       shinyjs::show("debug_info")
     }

  })

  # show_info() reactiveEvent ----
  show_info <- eventReactive(input$shows_cached, label = "show_info()", {

    query_slug <- getQueryString(session)[['show']] %||% ""
    cli_alert_info("query_slug {query_slug}")

    if (stringr::str_detect(input$shows_cached, "^cache:")) {
      # cli_alert_info("cached show detected {input$shows_cached}")

      input_show <- input$shows_cached %>%
        stringr::str_extract(., "\\d+")

    } else if (input$shows_cached != "") {
      # cli_alert("Adding to cache")
      input_show <- input$shows_cached %>%
        stringr::str_remove(., "^cache:") %>%
        cache_add_show(cache_db_con = cache_db_con)

      if (is.null(input_show)) return(NULL)

      # cli_alert_info("input_show after caching attempt is {input_show}")
    } else if (!is.null(query_slug)) {
      input_show <- convert_ids(slug = query_slug, cache_db_con = cache_db_con)
    } else {
      return(NULL)
    }

    # cli_alert_warning("input_show {input_show}")
    show_tmp <- cache_shows_tbl %>% filter(show_id == input_show)
    # cli_alert_info("pull(show_tmp, slug) {pull(show_tmp, slug)}")

    if (!identical(query_slug, pull(show_tmp, slug))) {
      updateQueryString(glue("?show={pull(show_tmp, slug)}"), mode = "push", session = session)
    }

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

  # show_seasons() ----
  show_seasons <- eventReactive(show_info(), label = "show_seasons()", {

    # cli_alert_info("Making show_seasons()")

    current_show <- show_info()
    current_show_id <- current_show$show_id

    if (!is_already_cached("seasons", current_show_id, cache_db_con)) {
      cli_alert_success("Adding {current_show_id} episodes to cache")
      cache_add_episodes(show_id = current_show_id, replace = FALSE, cache_db_con)
    }

    current_show_episodes <- cache_episodes_tbl %>%
      filter(show_id == current_show_id) %>%
      collect() %>%
      group_by(season) %>%
      summarize(
        mean_rating = weighted.mean(rating, w = votes, na.rm = TRUE),
        sum_votes = sum(votes),
        last_aired = max(first_aired)
      )

    current_show_seasons <- cache_seasons_tbl %>%
      filter(show_id == current_show_id) %>%
      collect() %>%
      left_join(current_show_episodes, by = "season") %>%
      transmute(
        season = as.character(season),
        title = title,
        aired_total = if_else(
          aired_episodes < episode_count,
          glue("{aired_episodes} (of {episode_count} total)"),
          as.character(aired_episodes)
        ),
        rating = round(rating, 1),
        mean_rating = round(mean_rating, 1),
        votes = votes,
        sum_votes = sum_votes,
        first_aired = unix_date(first_aired),
        last_aired = unix_date(last_aired)
      )

    current_show_seasons
  }, ignoreNULL = TRUE)


  # show_episodes() ----
  show_episodes <- eventReactive(show_info(), label = "show_episodes()", {

    show_seasons()

    # cli_alert_info("Making show_episodes()")

    current_show <- show_info()
    current_show_id <- current_show$show_id

    # if (!is_already_cached("episodes", current_show_id, cache_db_con)) {
    #   cli_alert_success("Adding {current_show_id} episodes to cache")
    #   cache_add_episodes(show_id = current_show_id, replace = FALSE, cache_db_con)
    # }

    current_show_episodes <- cache_episodes_tbl %>%
      filter(show_id == current_show_id) %>%
      collect() %>%
      mutate(
        season_episode = sprintf("s%02de%02d", season, episode),
        first_aired = unix_date(first_aired),
        season = as.character(season)
      ) %>%
      filter(
        first_aired <= lubridate::today()
      )

    current_show_episodes
  }, ignoreNULL = TRUE)

  # show_overview renderUI  ----
  output$show_overview <- renderUI({
    # input$shows_cached
    # show <- isolate(show_info())
    show <- show_info()
    # show_seasons <- show_seasons()
    # cli_alert("renderUI: show_overview")

    # Early return for no result
    if (is.null(show)) {
      res <- fluidRow(
        column(
          10, offset = 1,
          h2("Nothing found :("),
          p("Try entering the show title, but like... try harder.",
            style = "text-align: center;")
        )
      )
      return(res)
    }

    summary_table <- show %>%
      select(rating, votes, episodes = aired_episodes, runtime) %>%
      mutate(
        rating = round(rating, 1),
        runtime = glue("{runtime}min"),
        rating = glue("{rating} - “{rating_label(rating)}”")
      ) %>%
      mutate_if(is.na, ~ "N/A") %>%
      rename_all(str_to_title) %>%
      kable(format = "html", escape = FALSE) %>%
      kable_styling(
        full_width = FALSE, font_size = 18, position = "center",
        bootstrap_options = c("responsive")
      ) %>%
      HTML()

    # links_table <- show %>%
    #   select(slug, tvdb, imdb, tmdb) %>%
    #   transmute(
    #     trakt.tv = glue("https://trakt.tv/shows/{slug}"),
    #     tvdb = glue("https://www.thetvdb.com/?id={tvdb}&tab=series"),
    #     IMDb = glue("https://www.imdb.com/title/{imdb}/"),
    #     TMDB = glue("https://www.themoviedb.org/tv/{tmdb}")
    #   ) %>%
    #   gather(Site, url) %>%
    #   transmute(` ` = cell_spec(Site, link = url)) %>%
    #   t() %>%
    #   kable(escape = FALSE) %>%
    #   kable_styling(full_width = FALSE) %>%
    #   HTML()

    # Otherwise, do a thing
    tags$div(
      h2(
        a(href = glue("https://trakt.tv/shows/{show$slug}"),
          glue("{show$title} ({show$year})")),
        br(),
        tags$small(
          HTML(glue("{bullet} {country_label(show$country)} {bullet}
               {language_label(show$language)} {bullet}
               {show$network} {bullet} {str_to_title(show$status)} {bullet}"))
        )
      ),
      wellPanel(
        fluidRow(
          column(
            2,
            class = "hidden-xs",
            tags$figure(
              img(
                src = show$show_poster,
                class = "img-responsive img-rounded show-poster",
                style = "max-height: 250px;"
              ),
              tags$figcaption(
                p(tags$a("fanart.tv", href = "https://fanart.tv/",
                         style = "color: #2C3E50; text-decoration: none;"),
                  class = "small", style = "padding-left: 5px;"
                )
              )
            )
          ),
          column(
            9, offset = -1,
            p(class = "lead",
              if_else(!is.na(show$overview),
                      stringr::str_trunc(show$overview, 300, "right"),
                      "No overview available ¯\\_(ツ)_/¯")
            ),
            summary_table
          )
        )
      )
    )
  })

  # DT: Seasons ----
  output$show_seasons_table <- DT::renderDT({
    seasons <- show_seasons() %>%
      select(-season)

    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, colspan = 1, "Name"),
          th(rowspan = 2, colspan = 1, "Episodes"),
          th(colspan = 2, "Ratings", class = "second-header", id = "secondhead"),
          th(colspan = 2, "Votes", class = "second-header", id = "secondhead"),
          th(colspan = 2, "Aired", class = "second-header", id = "secondhead")
        ),
        tr(
          th("Season"), th("Episode (mean)"),
          th("Season"), th("Episodes"),
          th("Started"), th("Ended")
        )
      )
    ))

    datatable(
      data = seasons,
      container = sketch,
      # colnames = c(
      #   "Name" = "title",
      #   "Season Rating" = "rating",
      #   "Episode Rating (mean)" = "mean_rating",
      #   "Season Votes" = "votes",
      #   "Episode Votes" = "sum_votes",
      #   "Episodes (Aired)" = "aired_total",
      #   "First Aired" = "first_aired",
      #   "Last Aired" = "last_aired"
      #   ),
      rownames = FALSE, style = "bootstrap",
      fillContainer = FALSE,
      options = list(
        dom = "ptl",
        pageLength = 15,
        autoWidth = FALSE,
        #scrollY = 400,
        #scroller = TRUE,
        #deferRender = TRUE,
        scrollCollapse = TRUE,
        lengthMenu = list(c(15, 30, -1), c("15", "30", "All"))
      ),
      extensions = "Responsive"
    )
  })

  # DT: Episodes ----
  output$show_episodes_table <- DT::renderDT({
    episodes <- show_episodes()

    episodes %>%
      transmute(
        season_episode = season_episode,
        title = title,
        rating = round(rating, 1),
        votes = votes,
        comment_count = comment_count,
        first_aired = first_aired
      ) %>%
      datatable(
        colnames = c(
          "Season/Episode" = "season_episode",
          "Name" = "title",
          "Rating" = "rating",
          "Votes" = "votes",
          "Comments" = "comment_count",
          "First Aired" = "first_aired"
        ),
        rownames = FALSE, style = "bootstrap",
        filter = list(position = "top", clear = TRUE, plain = TRUE),
        fillContainer = FALSE,
        options = list(
          dom = "t",
          autoWidth = FALSE,
          pageLength = -1,
          scrollY = 500,
          scroller = TRUE,
          deferRender = TRUE,
          scrollCollapse = TRUE
          #lengthMenu = list(c(25, 50, -1), c("25", "50", "All"))
        ),
        extensions = "Responsive"
      )
  })

  # plotly: Episodes ----
  output$plotly_episodes <- renderPlotly({
    episodes <- show_episodes()
    seasons <- show_seasons() %>%
      select(season, season_title = title)

    cli_alert_info("Doing the plotly")
#
#     glimpse(seasons)
#     glimpse(episodes)

    if (length(unique(seasons$season)) > 1) {
      episodes <- lm(
        rating ~ episode * season - episode - 1,
        weights = votes, data = episodes
      ) %>%
        broom::augment() %>%
        select(.fitted_season = .fitted, episode, season) %>%
        left_join(
          episodes,
          by = c("episode", "season")
        )
    } else {
      episodes <- lm(
        rating ~ episode,
        weights = votes, data = episodes
      ) %>%
        broom::augment() %>%
        select(.fitted_season = .fitted, episode) %>%
        left_join(
          episodes,
          by = c("episode")
        )
    }

    # glimpse(episodes)
    # glimpse(seasons)

    episodes <- episodes %>%
      left_join(
        seasons,
        by = "season"
      ) %>%
      arrange(first_aired) %>%
      mutate(
        episode_abs = seq_along(first_aired),
        season_title = factor(
          season_title, levels = rev(unique(season_title)), ordered = TRUE
        )
      ) %>%
      make_hoverinfo()


    # plot_ly ----
    plot_ly(
      data = episodes,
      x = ~episode_abs, y = ~rating, color = ~season_title
    ) %>%
    add_markers(
      type = "scattergl", mode = "markers",
      stroke = I("black"),
      alpha = .75, size = 5, name = ~season_title,
      legendgroup = ~season,
      text = ~hovertext,
      hoverinfo = "text"
    ) %>%
    add_lines(
      y = ~.fitted_season, type = "lines", size = I(3),
      line = list(dash = "dash"),
      legendgroup = ~season,
      showlegend = FALSE,
      hoverinfo = "skip"
    ) %>%
    layout(
      xaxis = list(
        title = "Episode #",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "Rating (1-10)"
      ),
      showlegend = nrow(seasons) <= 15,
      legend = list(
        orientation = "h",
        x = 0, y = 100
      ),
      images = list(
        list(
          source = "img/trakt-icon-black.png",
          xref = "paper",
          yref = "paper",
          x = 0,
          y = 1,
          sizex = 0.1,
          sizey = 0.1,
          opacity = 0.5
        )
      )
    ) %>%
      config(
        staticPlot = FALSE, displayModeBar = TRUE,
        editable = FALSE, sendData = FALSE, displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          "toImage",
          "sendDataToCloud",
          "editInChartStudio",
          "hoverCompareCartesian",
          "hoverClosestCartesian",
          "select2d",
          "lasso2d",
          # "zoom2d",
          "zoomIn2d", "zoomOut2d",
          "resetViews", "resetScale2d",
          "toggleSpikelines"
        )
      )


    # p <- current_show_episodes %>%
    #   mutate(season = factor(season)) %>%
    #   ggplot(aes(x = episode, y = rating, fill = season, color = season)) +
    #   geom_point(size = 3, shape = 21, stroke = .4, color = "black") +
    #   geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
    #   theme_minimal() +
    #   labs(x = "Episode #", y = "Rating (1-10)")
    #
    # ggplotly(p)

  })


  # Startup toggles ----
  observeEvent(input$get_show, once = TRUE, label = "Hide Intro", {
    # cat(input$shows_cached, "\n")

    if (input$get_show > 0) {
      # cat("input$get_show is", input$get_show, "\n")
      hide(id = "intro-wellpanel", anim = TRUE, animType = "slide", time = 1)
      shinyjs::show(id = "show_overview", anim = TRUE, animType = "slide", time = 2)
      shinyjs::show(id = "season_container", anim = TRUE, animType = "slide", time = 2)
      shinyjs::show(id = "episodes_container", anim = TRUE, animType = "slide", time = 2)
    }
  })

  # Request log ----
  observeEvent(show_info(), label = "Log requests", {

    if (isolate(input$get_show) == 0) return(NULL)
    if (isolate(input$shows_cached) == "") return(NULL)

    res <- tibble(
      time = as.numeric(lubridate::now(tzone = "UTC")),
      request = isolate(input$shows_cached)
    )
    cli_alert("Logging request")
    check_cache_table("requests", res, cache_db_con)
    RSQLite::dbWriteTable(cache_db_con, "requests", res, append = TRUE)
  })
})
