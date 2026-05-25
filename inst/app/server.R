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
      show_tmp <- cache_shows_tbl() %>% filter(slug == query_slug) %>% collect()
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
    # cli_alert_info("query_slug {query_slug}")

    if (stringr::str_detect(input$shows_cached, "^cache:")) {
      # cli_alert_info("cached show detected {input$shows_cached}")

      input_show <- input$shows_cached %>%
        stringr::str_extract(., "\\d+")

    } else if (input$shows_cached != "") {
      input_show <- input$shows_cached %>%
        stringr::str_remove(., "^cache:") %>%
        cache_add_show(cache_db_con = cache_db_con)

      if (is.null(input_show)) return(NULL)

      # Refresh the dropdown so the freeform query gets replaced with the
      # resolved "Title (Year)" label keyed by cache id.
      shows <- cache_shows_tbl() %>% collect()
      choices <- c("", setNames(
        paste0("cache:", shows$show_id),
        as.character(glue("{shows$title} ({shows$year})"))
      ))
      updateSelectizeInput(
        session, "shows_cached",
        choices = choices,
        selected = paste0("cache:", input_show)
      )
    } else if (!is.null(query_slug)) {
      input_show <- convert_ids(slug = query_slug, cache_db_con = cache_db_con)
    } else {
      return(NULL)
    }

    # cli_alert_warning("input_show {input_show}")
    show_tmp <- cache_shows_tbl() %>% filter(show_id == input_show)
    cli_alert_info("{lubridate::now('UTC')} - Current show: {pull(show_tmp, slug)}")

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
        cache_posters_tbl() %>%
          select(show_id, show_poster),
        by = "show_id"
      ) %>%
      collect() %>%
      mutate(
        show_poster = if_else(show_poster == "", "img/poster-blank.jpg", show_poster)
      )
  }, ignoreInit = TRUE)

  # show_seasons() ----
  show_seasons <- eventReactive(show_info(), label = "show_seasons()", {

    # cli_alert_info("Making show_seasons()")

    current_show <- show_info()
    current_show_id <- current_show$show_id

    if (current_show$aired_episodes == 0) {
      return(NULL)
    }

    if (!is_already_cached("seasons", current_show_id, cache_db_con)) {
      cli_alert_success("Adding {current_show_id} episodes to cache")
      cache_add_episodes(show_id = current_show_id, replace = FALSE, cache_db_con)
    }

    current_show_episodes <- cache_episodes_tbl() %>%
      filter(show_id == current_show_id) %>%
      collect() %>%
      group_by(season) %>%
      summarize(
        mean_rating = weighted.mean(rating, w = votes, na.rm = TRUE),
        sum_votes = sum(votes),
        last_aired = max(first_aired, na.rm = TRUE)
      )

    current_show_seasons <- cache_seasons_tbl() %>%
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

    if (current_show$aired_episodes == 0) {
      return(NULL)
    }

    # if (!is_already_cached("episodes", current_show_id, cache_db_con)) {
    #   cli_alert_success("Adding {current_show_id} episodes to cache")
    #   cache_add_episodes(show_id = current_show_id, replace = FALSE, cache_db_con)
    # }

    current_show_episodes <- cache_episodes_tbl() %>%
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
    show <- show_info()

    # Early return for no result
    if (is.null(show)) {
      return(card(
        class = "border-warning",
        card_body(
          h4("Nothing found :("),
          p("Try entering the show title, but like… try harder.")
        )
      ))
    }

    safe_overview <- if (!is.na(show$overview)) {
      stringr::str_trunc(show$overview, 380, "right")
    } else {
      "No overview available ¯\\_(ツ)_/¯"
    }

    rating_rounded <- round(show$rating, 1)

    card(
      class = "show-overview",
      card_header(
        class = "show-header",
        a(
          href = glue("https://trakt.tv/shows/{show$slug}"),
          class = "show-title-link",
          h3(class = "mb-1", glue("{show$title} ({show$year})"))
        ),
        div(
          class = "show-meta text-muted small",
          HTML(glue(
            "{country_label(show$country)} · {language_label(show$language)} · ",
            "{show$network} · {str_to_title(show$status)}"
          ))
        )
      ),
      card_body(
        layout_columns(
          col_widths = c(3, 9),
          class = "g-3",
          tags$figure(
            class = "show-poster-frame mb-0",
            img(
              src = show$show_poster,
              class = "img-fluid rounded show-poster",
              alt = glue("Poster for {show$title}")
            ),
            tags$figcaption(
              class = "small text-muted mt-1",
              "Poster via ",
              tags$a("fanart.tv", href = "https://fanart.tv/")
            )
          ),
          div(
            p(class = "lead", safe_overview),
            layout_column_wrap(
              width = "180px",
              fill = FALSE,
              class = "mt-3",
              value_box(
                title = "Rating",
                value = rating_rounded,
                showcase = bs_icon("star-fill"),
                theme = "primary",
                p(rating_label(rating_rounded), class = "mb-0 small")
              ),
              value_box(
                title = "Votes",
                value = format(show$votes, big.mark = ","),
                showcase = bs_icon("people-fill"),
                theme = "secondary"
              ),
              value_box(
                title = "Episodes",
                value = show$aired_episodes,
                showcase = bs_icon("collection-play"),
                theme = "secondary"
              ),
              value_box(
                title = "Runtime",
                value = glue("{show$runtime}m"),
                showcase = bs_icon("clock"),
                theme = "secondary"
              )
            )
          )
        )
      )
    )
  })

  # DT: Seasons ----
  output$show_seasons_table <- DT::renderDT({
    seasons <- show_seasons()

    if (is.null(seasons)) {
      return(NULL)
    }

    seasons <- seasons %>% select(-season)

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
      rownames = FALSE, style = "auto",
      fillContainer = FALSE,
      options = list(
        dom = "lt",
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

    if (is.null(episodes)) {
      return(NULL)
    }

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
        rownames = FALSE, style = "auto",
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

    if (is.null(episodes)) {
      return(NULL)
    }

    seasons <- show_seasons() %>%
      select(season, season_title = title)

    # cli_alert_info("Doing the plotly")

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
          source = "img/trakt-logomark-mono-light.svg",
          xref = "paper",
          yref = "paper",
          x = 0.01,
          y = 0.98,
          sizex = 0.08,
          sizey = 0.08,
          opacity = 0.4
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


  })

  # plotly: Seasons (boxplot of episode ratings per season) ----
  output$plotly_seasons <- renderPlotly({
    episodes <- show_episodes()

    if (is.null(episodes) || nrow(episodes) == 0) {
      return(NULL)
    }

    seasons_n <- length(unique(episodes$season))
    show_outliers <- nrow(episodes) <= 250  # too crowded for huge shows
    # For shows with one season, force boxpoints so the box is meaningful
    # (otherwise a single box with just quartile bars is uninformative).
    box_points <- if (seasons_n == 1) "all" else if (show_outliers) "outliers" else FALSE

    # Stable categorical x order: numeric season ascending, displayed as "S<n>"
    season_order <- sort(unique(as.numeric(episodes$season)))
    episodes <- episodes %>%
      mutate(season_label = factor(
        paste0("S", season),
        levels = paste0("S", season_order),
        ordered = TRUE
      ))

    plot_ly(
      data = episodes,
      x = ~season_label,
      y = ~rating,
      type = "box",
      boxpoints = box_points,
      jitter = 0.3,
      pointpos = 0,
      marker = list(size = 4, opacity = 0.6),
      line = list(width = 1.2),
      fillcolor = "rgba(237, 28, 36, 0.18)",
      color = I("#ED1C24"),
      hovertemplate = paste0(
        "%{x}<br>",
        "Rating: %{y:.1f}<extra></extra>"
      )
    ) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(
          title = "Season",
          tickangle = if (seasons_n > 12) -45 else 0,
          automargin = TRUE
        ),
        yaxis = list(
          title = "Episode rating",
          zeroline = FALSE,
          rangemode = "normal"
        ),
        margin = list(l = 50, r = 10, t = 10, b = 40)
      ) %>%
      config(
        displaylogo = FALSE,
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list(
          "toImage", "sendDataToCloud", "editInChartStudio",
          "select2d", "lasso2d",
          "zoomIn2d", "zoomOut2d",
          "resetViews", "resetScale2d", "toggleSpikelines"
        )
      )
  })


  # Startup toggles ----
  observeEvent(input$get_show, once = TRUE, label = "Hide Intro", {
    # cat(input$shows_cached, "\n")

    if (input$get_show > 0) {
      # cat("input$get_show is", input$get_show, "\n")
      hide(id = "intro-card", anim = TRUE, animType = "slide", time = 1)
      shinyjs::show(id = "show_overview", anim = TRUE, animType = "slide", time = 2)

      if (!is.null(show_seasons())) {
        shinyjs::show(id = "season_container", anim = TRUE, animType = "slide", time = 2)
        shinyjs::show(id = "episodes_container", anim = TRUE, animType = "slide", time = 2)
      }
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
    # cli_alert("Logging request")
    # check_cache_table("requests", res, cache_db_con)
    res <- RSQLite::dbWriteTable(cache_db_con, "requests", res, append = TRUE)
  })
})
