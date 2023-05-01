

# Define server logic required to draw a histogram

shinyServer(function(session, input, output) {
  
  grab_season = eventReactive(input$choose_season, {
    return (input$season)
  })
  
  output$display_games = renderUI({
    if(is.null(grab_season())) {
      return (NULL)
    }
    
    # Collect data from game management file (team names, dates, games)
    possible_games = list()
    for(i in 1:nrow(game_management)) {
      if(game_management$Year[[i]] == grab_season()) {
          opposing_team = game_management$Opposing_Team[[i]] 
          date = paste(game_management$Year[[i]],game_management$Month[[i]],game_management$Day[[i]],sep="-")
          csv_name = paste(opposing_team, date, sep = "_")
          possible_games[[i]] = csv_name
      }
    }
    
    possible_games_paths = possible_games
    for(i in 1:length(possible_games_paths)) {
      possible_games_paths[[i]] = paste(possible_games_paths[[i]],'.csv',sep='')
    }
    
    game_names = possible_games
    this_output = list()
    this_output[[1]] = selectInput("game", 
                                   "Game: ", 
                                   game_names)
    this_output[[2]] = actionButton("choose_game", "Choose Game")
    return (this_output)
  })
  
  
 
  
  # GAME SELECTION
  
  # identify which game to grab the athlete list for
  game_id = eventReactive(input$choose_game, {
    
    opponent = str_split(input$game, "_")[[1]][1]
    
    game_id = game_management %>% 
      filter(Opposing_Team == opponent) %>% 
      select(Game_ID) %>% 
      as.numeric()
    
    return (game_id)
  })
  
  output$game_loaded = renderTable({
    if(is.null(df()))
    {
      return(NULL)
    }
    output$finished_df = renderText({"Ready to Visualize!"})
    head(df())
    #meaning we are ready to download the data
  })
  
  
  ###### GETTING GAME DATA
  
  # grab the cleaned data for the game input by the user from the CleanedGames directory of .csv files 
  df <- eventReactive(input$choose_game, {
    game_id = game_id()
    if(is.null(game_id)) {
      return (NULL)
    }
    
    # combine the user's input with the appropriate path for the CleanedGames directory
    path = paste("CleanedGames/", input$game, sep='')
    
    # grab the appropriate .csv for the game selected 
    datause = read_csv(paste(path, '.csv', sep=''), col_names = TRUE)
    
    datause = datause %>% mutate(Player_Number = as.factor(Player_Number))
    
    return(datause)
  })
  
  # MAIN PLOT ATHLETE SELECTION
  
  output$athlete_select = renderUI({
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    if(is.null(game_id)) {
      return (NULL)
    }
    
    players_list = possible_players[game_id][[1]]
    
    selectInput("athlete",
                "Which Athlete: ",
                players_list,
                multiple = TRUE,
                selected = "1" )
  })
  
  # MAIN PLOT SELECT / UNSELECT ALL BUTTON
  
  # activate the server logic once the user clicks the 'Select / Unselect All' button
  observeEvent(input$select_all_athletes, {
    game_id = game_id()
    if(is.null(game_id)) {
      return (NULL)
    }
    
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    players_list = possible_players[game_id][[1]]
    
    # update the athlete selection based on the number of times the user has clicked the button
    if(input$select_all_athletes %% 2 == 1){
      # update the athlete selection to select all
      updateSelectInput(session, 
                        "athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = players_list
      )
    } else{
      # update the athlete selection to unselect all
      updateSelectInput(session,
                        "athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = NULL
      )
    }
  })
  
  # MAIN PLOT TIME SLIDER / FILTERING
  
  output$time_slider = renderUI({
    game_id = game_id()
    if(is.null(game_id)) {
      return (NULL)
    }
    
    # grab the data frame from the reactive element
    df_to_viz = df()
    
    # display the time filtering slider
    sliderInput(
      "range",
      label = "Time Range:",
      min = lubridate::origin + days(1),
      max = lubridate::origin + days(1) + minutes(ceiling(max(df_to_viz$Seconds) / 60)),
      value = c(lubridate::origin + days(1), 
                lubridate::origin + days(1) + minutes(ceiling(max(df_to_viz$Seconds) / 60))),
      step = 1,
      timeFormat = "%H:%M:%S",
      timezone = "+0000",
      ticks = FALSE)
  })
  
  # MAIN PLOT DATA / DATAFRAMES
  
  # filter game data reactively based on time slider (range) and flip inputs
  filtered_data <- reactive({
    if(is.null(input$range)) {
      return (NULL)
    }
    
    # extract the dataframe from the reactive variable
    time_data = df()
    
    hours = as.integer(substr(input$range[1], 12, 13)) * 3600
    minutes = as.integer(substr(input$range[1], 15, 16)) * 60
    start_seconds = hours + minutes + as.integer(substr(input$range[1], 18, 19))

    # if the start is put all the way to the left of the slider it throws an error, this handles/fixes the error
    if(input$range[1] == 86400) {
      start_seconds = 0
    }

    # grab the end time (seconds) as specified by the user with the slider
    hours = as.integer(substr(input$range[2], 12, 13)) * 3600
    minutes = as.integer(substr(input$range[2], 15, 16)) * 60
    end_seconds = hours + minutes + as.integer(substr(input$range[2], 18, 19))

    time_data = time_data %>%
      filter(Seconds >= start_seconds & Seconds <= end_seconds)
    
    # flip
    if(input$flip_data) {
      time_data = time_data %>% 
        mutate(Lat = Lat * -1,
               Long = Long * -1)
    }
    
    return (time_data)
  })
  
  # filter the data further based on the athletes selected by the user
  filtered_data_athlete = reactive({
    filtered_data = filtered_data()
    
    if(is.null(filtered_data)) {
      return (NULL)
    }
    
    filtered_data = filtered_data %>%
      filter(p_id %in% input$athlete)
    
    return (filtered_data)
  })
  
  var_name = reactive({
     
    var = case_when(
    input$var == "Player ID" ~ "Player_Number",
    input$var == "Velocity" ~ "Velocity",
    input$var == "Acceleration" ~ "Acceleration",
    input$var == "Positional Density" ~ "Odometer",
    input$var == "Time Elapsed" ~ "Seconds"
    )
    
    return (var)
  })
  
  # button to download data
  output$download_player_viz <- downloadHandler(
    filename = function() {
      filename = "path_visual"
      paste(filename,'.csv', sep='')
    },
    content = function(file) {
      write.csv(filtered_data_athlete(), file)
    }
  )
  
  # MAIN PLOT VISUALIZATION
  
  # reactive visual (evaluation is dependent on plotly_output's two triggers)
  field_display = reactive({
    filtered_data_athlete = filtered_data_athlete()
    
    if(is.null(filtered_data_athlete)) {
      return (NULL)
    }
    
    # grab the start and end points of each player's paths 
    # we will mark both locations in the visualization (start = triangle, end = square)
    start_points = filtered_data_athlete %>%
      filter(Seconds == min(Seconds))
    end_points = filtered_data_athlete %>%
      filter(Seconds == max(Seconds))

    #set up the lines for the soccer field
    soccer_field_lines = filtered_data_athlete %>% 
      ggplot() +
      geom_rect(aes(xmin = -60, xmax = 60, ymin = -37.5, ymax = 37.5), color = "black", fill = "white") +
      geom_segment(aes(x = 0, y = -37.5, xend = 0, yend = 37.5)) +
      geom_rect(aes(xmin = -60, xmax = -42, ymin = -22, ymax = 22), color = "black", fill = "white") +
      geom_rect(aes(xmin = 60, xmax = 42, ymin = -22, ymax = 22), color = "black", fill = "white") +
      geom_rect(aes(xmin = -60, xmax = -54, ymin = -10, ymax = 10), color = "black", fill = "white") +
      geom_rect(aes(xmin = 60, xmax = 54, ymin = -10, ymax = 10), color = "black", fill = "white") +
      theme_bw() +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      removeGrid(x = TRUE, y = TRUE) +
      scale_x_continuous(name="", limits=c(-70, 70)) +
      scale_y_continuous(name="", limits=c(-50, 50))
    
    # grab the variable to display from the user input
    var_name = var_name()
    
    # if we are only displaying the player paths, simply use the p_id to color the paths
    if(var_name == "Player_Number") {
      plot = soccer_field_lines +
        geom_path(data = filtered_data_athlete, alpha = 0.8,
                  mapping = aes(x = Lat, y = Long, col = Player_Number)) +
        geom_point(data = start_points, size = 3, shape = 17, alpha = 0.8,
                   mapping = aes(x = Lat, y = Long, col = Player_Number)) +
        geom_point(data = end_points, size = 3, alpha = 0.8, shape = 15,
                   mapping = aes(x = Lat, y = Long, col = Player_Number)) +
        scale_color_discrete("Player") +
        theme(legend.position="none")
    } else if(var_name == "Odometer") { # display positional density
      plot <- soccer_field_lines +
        stat_density2d(aes(x = Lat, y = Long, fill = after_stat(level), col = Player_Number),
                       geom = "polygon", alpha = 0.5)
      
      
    } else { 
      plot = soccer_field_lines +
        geom_point(data = filtered_data_athlete, size = 0.1, alpha = 0.8,
                   mapping = aes(x = Lat, y = Long, col = .data[[var_name]], group = Player_Number, fill = Player_Number)) +
        geom_point(data = start_points, size = 3, shape = 24, alpha = 0.8,
                   mapping = aes(x = Lat, y = Long, col = .data[[var_name]], group = Player_Number, fill = Player_Number)) +
        geom_point(data = end_points, size = 3, alpha = 0.8, shape = 22,
                   mapping = aes(x = Lat, y = Long, col = .data[[var_name]], group = Player_Number, fill = Player_Number)) +
        scale_color_viridis_c(option = "turbo") +
        guides(fill = guide_legend(override.aes = list(size = 4)))
    }
    
    # remove the legend for 'time elapsed'
    if(var_name == "Seconds") {
      plot = plot +
        theme(legend.position = 'none')
    }
    
    # check if user wants facets
    if(input$facet) {
      # grab the dimensions of the window
      window_width = input$dimension[1]
      window_height = input$dimension[2]
      
      # multiply the window width by 2/3 to get the width of the visualization space
      viz_width = window_width * 2 / 3
      
      # subtract 200 from the window height to account for space for the other UI components
      viz_height = window_height - 200
      
      facet_cols = find_facet_cols(length(input$athlete), viz_width, viz_height)
      
      plot = plot +
        facet_wrap(~ Player_Number, ncol = facet_cols) 
    }
    
    plot
  })
  
  plotly_field_display = eventReactive(c(input$display, input$hide_sidebar), {
    if ((input$hide_sidebar == 0) & (input$display == 0)) {
      return (NULL)
    }
    
    plot = field_display()
    
    if(is.null(plot)) {
      return (NULL)
    }
    
    ggplotly(plot, height = input$dimension[2] - 200)
  })
  
  # converts the ggplot visual to a plotly field and displays it in the main panel
  output$plotly_field = renderPlotly({
    plotly_field_display()
  }) 
  
  # MAIN PLOT INFO (output to the user) 
  
  # # displays the start and end time of the visualization (based on the user's input on the time slider)
  # output$time_info = renderText({
  #   if(is.null(input$range)) {
  #     return ("Start time (Triangle): NA, End time (Square): NA")
  #   }
  #   
  #   # handling weird RShiny issue with the time slider
  #   if(input$range[1] == 86400) {
  #     paste0("Start time (Triangle): 0",
  #            " End Time (Square): ", as.integer(substr(input$range[2], 12, 13)) * 3600 + 
  #              as.integer(substr(input$range[2], 15, 16)) * 60 + as.integer(substr(input$range[2], 18, 19)))
  #   }
  #   else { 
  #     paste0("Start time (Triangle): ", as.integer(substr(input$range[1], 12, 13)) * 3600 + 
  #              as.integer(substr(input$range[1], 15, 16)) * 60 + as.integer(substr(input$range[1], 18, 19)),
  #            " End Time (Square): ", as.integer(substr(input$range[2], 12, 13)) * 3600 + 
  #              as.integer(substr(input$range[2], 15, 16)) * 60 + as.integer(substr(input$range[2], 18, 19)))
  #   }
  # })
  
  # HIDE / SHOW SIDEBAR OPTION FOR MAIN PLOT
  
  # activate the server logic once the user clicks the 'Hide Sidebar' button
  observeEvent(input$hide_sidebar, {
    if(input$hide_sidebar %% 2 == 1){
      # hide the sidebar of user input
      hideElement(
        selector = "#player_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('player_viz_main_panel', 'col-sm-8')
      addCssClass('player_viz_main_panel', 'col-sm-12')
    } else{
      # show the sidebar again
      showElement(
        selector = "#player_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('player_viz_main_panel', 'col-sm-12')
      addCssClass('player_viz_main_panel', 'col-sm-8')
    }
  })
  
  # PLAYER ANALYSIS ATHLETE SELECTION
  
  output$analysis_athlete_filter = renderUI({
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    players_list = possible_players[game_id][[1]]
    
    selectInput("analysis_athlete",
                "Which Athlete: ",
                players_list,
                multiple = TRUE,
                selected = "1"
    )
  })
  
  # PLAYER ANALYSIS SELECT / UNSELECT ALL BUTTON
  
  # activate the server logic once the user clicks the 'Select / Unselect All' button
  observeEvent(input$analysis_select_all_athletes, {
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    players_list = possible_players[game_id][[1]]
    
    # update the athlete selection based on the number of times the user has clicked the button
    if(input$analysis_select_all_athletes %% 2 == 1){
      # update the athlete selection to select all
      updateSelectInput(session, 
                        "analysis_athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = players_list
      )
    } else{
      # update the athlete selection to unselect all
      updateSelectInput(session,
                        "analysis_athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = NULL
      )
    }
  })
  
  # PLAYER ANALYSIS DATAFRAME
  
  # filter the analysis data based on the athlete selection input
  analysis_df <- reactive({
    # grab the original dataframe from the reactive variable 
    analysis_df = df()
   
    # use the sidebar input to identify which players to include in the visualization
    analysis_df = analysis_df %>%
      filter(p_id %in% input$analysis_athlete)
    
    # return the filtered dataframe
    return(analysis_df)
  })
  
  # PLAYER ANALYSIS VISUALIZATION
  
  # reactive visual with two triggers: display button and hide/show sidebar button (resize)
  analysis_plot = reactive({
    # grab the analysis data
    analysis_df = analysis_df()
    
    # grab the maximum number of minutes in the game
    max_min = analysis_df %>%
      mutate(game_minutes = Seconds / 60) %>%
      summarise(max_min = max(game_minutes)) %>%
      as.numeric()
    
    # switch statement which selects which variable to analyze based on the user's input on the dropdown menu
    var_name = case_when(
      input$analysis_var == "Velocity" ~ "Velocity",
      input$analysis_var == "Acceleration" ~ "Acceleration",
      input$analysis_var == "Distance Travelled" ~ "Odometer"
    )
    
    # if we are analyzing the total distance travelled we need to create a grid of line graphs
    
    if(var_name == "Odometer") {
      # create a line graph tracking odometer over time for each player
      plot = analysis_df %>% 
        mutate(game_minutes = Seconds / 60) %>% 
        ggplot(mapping = aes(x = game_minutes, y = Odometer)) +
        geom_line(mapping = aes(color = Player_Number)) +
        facet_wrap(~ Player_Number, labeller = 'label_both') +
        scale_x_continuous(breaks = seq(0, max_min, by = 10)) +
        scale_color_viridis_d(option = "turbo") +
        labs(x = "Game Time (min)", y = "Total Distance Travelled") +
        theme_bw() +
        theme(panel.grid.minor.x = element_blank(), legend.position = "none")
      
    } else { # otherwise we need to create a plot of box-and-whisker plots
      # check if violin plot
      if(input$player_violin) {
        plot = analysis_df %>% 
          ggplot(mapping = aes(x = Player_Number, y = .data[[var_name]])) +
          geom_violin(mapping = aes(color = Player_Number, fill = Player_Number), alpha = 0.4) +
          scale_fill_viridis_d(option = "turbo") +
          scale_color_viridis_d(option = "turbo") +
          labs(x = "Player Id", y = var_name) +
          theme_bw() +
          theme(legend.position = "none")
      }
      else
      {
        plot = analysis_df %>% 
          ggplot(mapping = aes(x = Player_Number, y = .data[[var_name]])) +
          geom_boxplot(mapping = aes(color = Player_Number, fill = Player_Number), alpha = 0.4) +
          scale_fill_viridis_d(option = "turbo") +
          scale_color_viridis_d(option = "turbo") +
          labs(x = "Player Id", y = var_name) +
          theme_bw() +
          theme(legend.position = "none")
      }
    }
    plot
  })
  
  # reactively trigger the creation of the plot 
  plotly_analysis_display = eventReactive(c(input$hide_analysis_sidebar, input$analysis_display), {
    if ((input$hide_analysis_sidebar == 0) & (input$analysis_display == 0)) {
      return (NULL)
    }
    
    # generate the ggplot visual
    plot = analysis_plot()
    
    if(is.null(plot)) {
      return (NULL)
    }
    
    # convert visual to plotly
    ggplotly(plot, height = input$dimension[2] - 200)
  })

  # display plotly visual  
  output$analysis_viz = renderPlotly({
    plotly_analysis_display()
  })
  
  # button to download data
  output$download_player_analysis <- downloadHandler(
    filename = function() {
      filename = "player_analysis"
      paste(filename,'.csv', sep='')
    },
    content = function(file) {
      write.csv(analysis_df(), file)
    }
  )
  
  # HIDE / SHOW SIDEBAR OPTION FOR PLAYER ANALYSIS
  
  # activate the server logic once the user clicks the 'Hide Sidebar' button
  observeEvent(input$hide_analysis_sidebar, {
    if(input$hide_analysis_sidebar %% 2 == 1){
      # hide the sidebar of user input
      hideElement(
        selector = "#analysis_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('analysis_viz_main_panel', 'col-sm-8')
      addCssClass('analysis_viz_main_panel', 'col-sm-12')
    } else{
      # show the sidebar again
      showElement(
        selector = "#analysis_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('analysis_viz_main_panel', 'col-sm-12')
      addCssClass('analysis_viz_main_panel', 'col-sm-8')
    }
  })

  # INTERVAL ANALYSIS ATHLETE SELECTION
  
  output$interval_analysis_athlete_filter = renderUI({
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    players_list = possible_players[game_id][[1]]
    
    selectInput("interval_analysis_athlete",
                "Which Athlete: ",
                players_list,
                multiple = TRUE,
                selected = "1"
    )
  })
  
  # INTERVAL ANALYSIS SELECT / UNSELECT ALL BUTTON
  
  # activate the server logic once the user clicks the 'Select / Unselect All' button
  observeEvent(input$interval_analysis_select_all_athletes, {
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    players_list = possible_players[game_id][[1]]
    
    # update the athlete selection based on the number of times the user has clicked the button
    if(input$interval_analysis_select_all_athletes %% 2 == 1){
      # update the athlete selection to select all
      updateSelectInput(session, 
                        "interval_analysis_athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = players_list
      )
    } else{
      # update the athlete selection to unselect all
      updateSelectInput(session,
                        "interval_analysis_athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = NULL
      )
    }
  })
  
  # INTERVAL ANALYSIS DATAFRAME
  
  interval_analysis_df <- reactive({
    # grab the original dataframe from the reactive variable 
    interval_analysis_df = df()
    
    if(is.null(interval_analysis_df)) {
      return (NULL)
    }
    
    # use the sidebar input to identify which players to include in the visualization
    interval_analysis_df = interval_analysis_df %>%
      filter(p_id %in% input$interval_analysis_athlete)
    
    # return the filtered dataframe
    return(interval_analysis_df)
  })
  
  # INTERVAL ANALYSIS VISUALIZATION
  
  # reactive visual 
  interval_analysis_plot = reactive({
    # grab the interval analysis data
    interval_analysis_df = interval_analysis_df()
    
    if(is.null(interval_analysis_df)) {
      return (NULL)
    }
    
    # switch statement which selects which variable to analyze based on the user's input on the dropdown menu 
    var_name = case_when(
      input$interval_analysis_var == "Velocity" ~ "Velocity",
      input$interval_analysis_var == "Acceleration" ~ "Acceleration",
      input$interval_analysis_var == "Distance Travelled" ~ "Odometer"
    )
    
    # grab the interval size/length (make sure to cast/convert to numeric)
    interval_size = input$interval_size %>% as.numeric()
    
    # check the user input to determine which variable to put on the x-axis and which to facet by
    if(input$interval_facet_var == "Player Id") {
      facet_var = "Player_Number"
      x_var = "Minutes"
      x_axis_title = "Time Interval (min)"
    } else {
      facet_var = "Minutes"
      x_var = "Player_Number"
      x_axis_title = "Player Id"
    }
    
    # if we are analyzing the total distance travelled we need to create a grid of line graphs
    if(var_name == "Odometer") {
      # create a line graph tracking odometer over time for each player
      plot = interval_analysis_df %>% 
        mutate(game_minutes = Seconds / 60,
               Minutes = cut(game_minutes,
                             breaks = make_minute_breaks(game_minutes, interval_size),
                             include.lowest = TRUE)) %>% 
        group_by(Minutes, Player_Number) %>% 
        mutate(start_distance = min(Odometer),
               finish_distance = max(Odometer),
               start_minutes = min(game_minutes)) %>% 
        ungroup() %>% 
        mutate(rel_distance = Odometer - start_distance,
               rel_minutes = game_minutes - start_minutes) %>% 
        ggplot(mapping = aes(x = rel_minutes, y = rel_distance)) +
        geom_line(mapping = aes(group = interaction(Player_Number, Minutes), color = .data[[x_var]])) +
        facet_wrap(~ .data[[facet_var]]) +
        scale_color_viridis_d(option = "turbo", x_axis_title) +
        labs(y = paste('distance run (m) within ', interval_size, ' minute interval', sep = ''), 
             x = paste('relative time (min) within ', interval_size, ' minute interval', sep = '')) +
        theme_bw() +
        theme(legend.position = 'top', panel.grid.minor.x = element_blank()) +
        guides(color = guide_legend(nrow = 2))
    } else { # otherwise we need to create a grid of plots of box-and-whisker plots
      plot = interval_analysis_df %>%
        mutate(game_minutes = Seconds / 60,
               Minutes = cut(game_minutes,
                             breaks = make_minute_breaks(game_minutes, interval_size),
                             include.lowest = TRUE)) %>% 
        ggplot(mapping = aes(x = .data[[x_var]], y = .data[[var_name]])) +
        geom_boxplot(mapping = aes(fill = .data[[x_var]], color = .data[[x_var]]), alpha = 0.4) +
        facet_wrap(~ .data[[facet_var]]) +
        scale_color_viridis_d(option = "turbo") +
        scale_fill_viridis_d(option = "turbo") +
        labs(x = x_axis_title, y = var_name) +
        theme_bw() +
        theme(legend.position = "none") 
      
      # if time interval is on the x_axis we need to tilt the labels
      if (x_var == "Minutes") {
        plot = plot + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      }
    }
    
    # return the visual
    return(plot)
  })
  
  # reactively trigger the creation of the plot 
  plotly_interval_analysis_display = eventReactive(c(input$hide_interval_analysis_sidebar, input$interval_analysis_display), {
    if ((input$hide_interval_analysis_sidebar == 0) & (input$interval_analysis_display == 0)) {
      return (NULL)
    }
    
    # generate the ggplot visual
    plot = interval_analysis_plot()
    
    if(is.null(plot)) {
      return (NULL)
    }
    
    # convert visual to plotly
    ggplotly(plot, height = input$dimension[2] - 200)
  })
  
  # display plotly visual  
  output$interval_analysis_viz = renderPlotly({
    plotly_interval_analysis_display()
  })
  
  output$download_player_analysis_interval <- downloadHandler(
    filename = function() {
      filename = "player_analysis_interval"
      paste(filename,'.csv', sep='')
    },
    content = function(file) {
      write.csv(interval_analysis_df(), file)
    }
  )
  
  # HIDE / SHOW SIDEBAR OPTION FOR INTERVAL ANALYSIS
  
  # activate the server logic once the user clicks the 'Hide Sidebar' button
  observeEvent(input$hide_interval_analysis_sidebar, {
    if(input$hide_interval_analysis_sidebar %% 2 == 1){
      # hide the sidebar of user input
      hideElement(
        selector = "#interval_analysis_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('interval_analysis_viz_main_panel', 'col-sm-8')
      addCssClass('interval_analysis_viz_main_panel', 'col-sm-12')
    } else{
      # show the sidebar again
      showElement(
        selector = "#interval_analysis_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('interval_analysis_viz_main_panel', 'col-sm-12')
      addCssClass('interval_analysis_viz_main_panel', 'col-sm-8')
    }
  })
  
  # BURST PLOT ATHLETE SELECTION
  
  output$burst_athlete_filter = renderUI({
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    players_list = possible_players[game_id][[1]]
    
    selectInput("burst_athlete",
                "Which Athlete: ",
                players_list,
                multiple = TRUE,
                selected = "1"
    )
  })
  
  # BURST PLOT SELECT / UNSELECT ALL BUTTON
  
  # activate the server logic once the user clicks the 'Select / Unselect All' button
  observeEvent(input$burst_select_all_athletes, {
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    players_list = possible_players[game_id][[1]]
    
    # update the athlete selection based on the number of times the user has clicked the button
    if(input$burst_select_all_athletes %% 2 == 1){
      # update the athlete selection to select all
      updateSelectInput(session, 
                        "burst_athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = players_list
      )
    } else{
      # update the athlete selection to unselect all
      updateSelectInput(session,
                        "burst_athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = NULL
      )
    }
  })
  
  # BURST PLOT TIME SLIDER / FILTERING
  
  output$burst_time_filter = renderUI({
    # grab the data frame from the reactive element
    df_to_viz = df()
    
    # display the time filtering slider
    sliderInput(
      "burst_range",
      label = "Time Range:",
      min = lubridate::origin + days(1),
      max = lubridate::origin + days(1) + minutes(ceiling(max(df_to_viz$Seconds) / 60)),
      value = c(lubridate::origin + days(1), 
                lubridate::origin + days(1) + minutes(ceiling(max(df_to_viz$Seconds) / 60))),
      step = 1,
      timeFormat = "%H:%M:%S",
      timezone = "+0000",
      ticks = FALSE
    )
  })
  
  # SPRINT BURST VELOCITY SLIDER / THRESHOLD
  
  output$burst_velocity_slider = renderUI({
    # grab the data frame from the reactive element
    df_to_viz = df()
    
    # grab the max velocity to display (round down the tenths place) 
    max_velo = floor(max(df_to_viz$Velocity) * 10) / 10
    
    # display the time filtering slider
    sliderInput(
      "velocity_threshold",
      label = "Velocity Threshold:",
      min = 0,
      max = max_velo,
      value = 12,
      step = 0.1,
      ticks = FALSE
    )
  })
  
  # SPRINT BURST VARIABLE FILTER SLIDER / THRESHOLD
  
  output$burst_filter_slider = renderUI({
    filter_var = input$burst_filter_var
    
    # check whether the user wants to filter or not
    if(is.null(filter_var)) {
      return (NULL)
    }
    
    this_output = list()
    
    if("Distance Travelled" %in% filter_var)
    {
      this_output[[1]] = sliderInput(
        "burst_filter_threshold_dist",
        label = paste("Distance Travelled", " Threshold:", sep = ''),
        min = 0,
        max = 60,
        value = 30,
        step = 0.1,
        ticks = FALSE
      )
      if("Duration" %in% filter_var)
      {
        this_output[[2]] = sliderInput(
          "burst_filter_threshold_duration",
          label = paste("Duration", " Threshold:", sep = ''),
          min = 0,
          max = 20,
          value = 5,
          step = 0.1,
          ticks = FALSE
        )
      }
    }
    else if("Duration" %in% filter_var)
    {
      this_output[[1]] = sliderInput(
        "burst_filter_threshold_duration",
        label = paste("Duration", " Threshold:", sep = ''),
        min = 0,
        max = 20,
        value = 5,
        step = 0.1,
        ticks = FALSE
      )
    }
    return (this_output)
  })
  
  # BURST PLOTS DATAFRAMES
  
  # filter by athlete, time, and flip option
  burst_filtered_df = reactive({
    # extract the original dataframe from the reactive variable
    burst_data = df()
    
    if(is.null(input$burst_range)) {
      return (NULL)
    }

    # filter the df to only include athletes selected 
    burst_data = burst_data %>%
      filter(p_id %in% input$burst_athlete)
    
    # grab the start time (seconds) as specified by the user with the slider 
    hours = as.integer(substr(input$burst_range[1], 12, 13)) * 3600
    minutes = as.integer(substr(input$burst_range[1], 15, 16)) * 60
    start_seconds = hours + minutes + as.integer(substr(input$burst_range[1], 18, 19))
    
    # if the start is put all the way to the left of the slider it throws an error, this handles/fixes the error
    if(input$burst_range[1] == 86400) {
      start_seconds = 0
    }
    
    # grab the end time (seconds) as specified by the user with the slider
    hours = as.integer(substr(input$burst_range[2], 12, 13)) * 3600
    minutes = as.integer(substr(input$burst_range[2], 15, 16)) * 60
    end_seconds = hours + minutes + as.integer(substr(input$burst_range[2], 18, 19))
    
    # filter the dataframe based on the user input time range
    burst_data = burst_data %>% 
      filter(Seconds >= start_seconds & Seconds <= end_seconds)
    
    # flip
    if(input$flip_data_burst) {
      burst_data = burst_data %>% 
        mutate(Lat = Lat * -1,
               Long = Long * -1)
    }
    
    # return the new dataset
    return (burst_data)
  })
  
  # create/identify the sprint bursts
  sprint_burst_df = reactive({
    # extract the filtered dataframe from the reactive variable
    # filtered by selected players and time slider / filter
    burst_data = burst_filtered_df()
    
    # grab all sprint bursts using the velocity threshold input by the user
    sprint_burst_df = grab_sprint_bursts(burst_data, input$velocity_threshold)
    
    # get additional filter variables
    filter_var = input$burst_filter_var
    
    # check for further filtering, as specified by the user
    if (!is.null(filter_var)) {
      # calculate the aggregate statistic (i.e. duration or distance travelled)
      # then filter to only keep burst ids with values above the threshold
      
      if("Distance Travelled" %in% filter_var)
      {
        filtered_burst_ids = sprint_burst_df %>% 
          group_by(p_id, burst_id) %>% 
          summarise(var_diff = max(Odometer) - min(Odometer),
                    .groups = 'drop') %>%
          filter(var_diff > input$burst_filter_threshold_dist) %>%
          select(p_id, burst_id)
      }
      if("Duration" %in% filter_var)
      {
        filtered_burst_ids = sprint_burst_df %>% 
          group_by(p_id, burst_id) %>% 
          summarise(var_diff = max(Seconds) - min(Seconds),
                    .groups = 'drop') %>%
          filter(var_diff > input$burst_filter_threshold_duration) %>%
          select(p_id, burst_id)
      }
      
      # filter the non-aggregated data based on the filtered aggregate list of player and burst ids
      # by performing a merge (inner join) on the two matching columns
      sprint_burst_df = merge(sprint_burst_df, filtered_burst_ids, by = c("p_id", "burst_id"))
    }
    
    # return the new dataset
    return (sprint_burst_df)
  })
  
  # SPRINT BURST VISUALIZATION
  
  # reactive visual 
  burst_plot = reactive({
    # grab the sprint burst data
    sprint_burst_df = sprint_burst_df()
    
    # set up the soccer field
    soccer_field_lines = sprint_burst_df %>% 
      ggplot() +
      geom_rect(aes(xmin = -60, xmax = 60, ymin = -37.5, ymax = 37.5), color = "black", fill = "white") +
      geom_segment(aes(x = 0, y = -37.5, xend = 0, yend = 37.5)) +
      geom_rect(aes(xmin = -60, xmax = -42, ymin = -22, ymax = 22), color = "black", fill = "white") +
      geom_rect(aes(xmin = 60, xmax = 42, ymin = -22, ymax = 22), color = "black", fill = "white") +
      geom_rect(aes(xmin = -60, xmax = -54, ymin = -10, ymax = 10), color = "black", fill = "white") +
      geom_rect(aes(xmin = 60, xmax = 54, ymin = -10, ymax = 10), color = "black", fill = "white") +
      theme_bw() +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      removeGrid(x = TRUE, y = TRUE) +
      scale_x_continuous(name="", limits=c(-70, 70)) +
      scale_y_continuous(name="", limits=c(-50, 50)) 
    
    # grab the start and end times of each burst from the df
    sprint_starts = sprint_burst_df %>%
      group_by(Player_Number, burst_id) %>%
      mutate(start = min(Seconds)) %>%
      filter(Seconds == start)
    
    sprint_ends = sprint_burst_df %>%
      group_by(Player_Number, burst_id) %>%
      mutate(end = max(Seconds)) %>%
      filter(Seconds == end)
    
    # check how many athletes the user has selected 
    num_athletes = length(input$burst_athlete)
    
    plot = soccer_field_lines + 
      geom_point(data = sprint_starts, size = 3, shape = 24, 
                  mapping = aes(x = Lat, y = Long, col = burst_id, group = Player_Number, fill = Player_Number)) +
      geom_point(data = sprint_burst_df, size = 0.1, 
                 mapping = aes(x = Lat, y = Long, col = burst_id, group = Player_Number))  +
      geom_point(data = sprint_ends, size = 3, shape = 22, 
                 mapping = aes(x = Lat, y = Long, col = burst_id, group = Player_Number, fill = Player_Number))  +
      scale_color_viridis_c(option = "turbo")
    
    # check for faceting
    if(input$facet_burst) {
      # grab the dimensions of the window
      window_width = input$dimension[1]
      window_height = input$dimension[2]
      
      # multiply the window width by 2/3 to get the width of the visualization space
      viz_width = window_width * 2 / 3
      
      # subtract 200 from the window height to account for space for the other UI components
      viz_height = window_height - 200
      
      facet_cols = find_facet_cols(length(input$burst_athlete), viz_width, viz_height)
      
      plot = plot +
        facet_wrap(~ Player_Number, ncol = facet_cols) 
    }
    
    # return the visual
    return(plot)
  })
  
  # reactively trigger the creation of the plot 
  plotly_burst_display = eventReactive(c(input$hide_burst_sidebar, input$burst_display), {
    if ((input$hide_burst_sidebar == 0) & (input$burst_display == 0)) {
      return (NULL)
    }
    
    # generate the ggplot visual
    plot = burst_plot()
    
    if(is.null(plot)) {
      return (NULL)
    }
    
    # convert visual to plotly
    ggplotly(plot, height = input$dimension[2] - 200)
  })
  
  # convert to plotly and output
  output$burst_viz = renderPlotly({
    plotly_burst_display()
  })
  
  output$download_burst <- downloadHandler(
    filename = function() {
      filename = "sprint_burst_visual"
      paste(filename,'.csv', sep='')
    },
    content = function(file) {
      write.csv(sprint_burst_df(), file)
    }
  )
  
  # BURST PLOT / VISUALIZATION INFO (output to the user) 
  
  # # displays the start and end time of the visualization (based on the user's input on the time slider)
  # output$burst_time_info = renderText({
  #   # null error / warning when first loading the app / page 
  #   if(is.null(input$burst_range)) {
  #     return ("Start time (Triangle): NA, End time (Square): NA")
  #   }
  #   
  #   # handling weird RShiny issue with the time slider
  #   if(input$burst_range[1] == 86400) {
  #     paste0("Start time (Triangle): 0",
  #            " End Time (Square): ", as.integer(substr(input$burst_range[2], 12, 13)) * 3600 + 
  #              as.integer(substr(input$burst_range[2], 15, 16)) * 60 + as.integer(substr(input$burst_range[2], 18, 19)))
  #   }
  #   else { 
  #     paste0("Start time (Triangle): ", as.integer(substr(input$burst_range[1], 12, 13)) * 3600 + 
  #              as.integer(substr(input$burst_range[1], 15, 16)) * 60 + as.integer(substr(input$burst_range[1], 18, 19)),
  #            " End Time (Square): ", as.integer(substr(input$burst_range[2], 12, 13)) * 3600 + 
  #              as.integer(substr(input$burst_range[2], 15, 16)) * 60 + as.integer(substr(input$burst_range[2], 18, 19)))
  #   }
  # })
  
  # HIDE / SHOW SIDEBAR FOR SPRINT BURST PLOT
  
  # activate the server logic once the user clicks the 'Hide Sidebar' button
  observeEvent(input$hide_burst_sidebar, {
    if(input$hide_burst_sidebar %% 2 == 1){
      # hide the sidebar of user input
      hideElement(
        selector = "#burst_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('burst_viz_main_panel', 'col-sm-8')
      addCssClass('burst_viz_main_panel', 'col-sm-12')
    } else{
      # show the sidebar again
      showElement(
        selector = "#burst_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('burst_viz_main_panel', 'col-sm-12')
      addCssClass('burst_viz_main_panel', 'col-sm-8')
    }
  })
  
  # SPRINT BURST ANALYSIS SECTION
  
  # SPRINT BURST ANALYSIS ATHLETE SELECTION
  
  output$burst_analysis_athlete_filter = renderUI({
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    players_list = possible_players[game_id][[1]]
    
    selectInput("burst_analysis_athlete",
                "Which Athlete: ",
                players_list,
                multiple = TRUE,
                selected = "1"
    )
  })
  
  # SPRINT BURST ANALYSIS SELECT / UNSELECT ALL BUTTON
  
  # activate the server logic once the user clicks the 'Select / Unselect All' button
  observeEvent(input$burst_analysis_select_all_athletes, {
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    players_list = possible_players[game_id][[1]]
    
    # update the athlete selection based on the number of times the user has clicked the button
    if(input$burst_analysis_select_all_athletes %% 2 == 1){
      # update the athlete selection to select all
      updateSelectInput(session, 
                        "burst_analysis_athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = players_list
      )
    } else{
      # update the athlete selection to unselect all
      updateSelectInput(session,
                        "burst_analysis_athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = NULL
      )
    }
  })
  
  # SPRINT BURST ANALYSIS VELOCITY SLIDER / THRESHOLD
  
  output$burst_analysis_velocity_slider = renderUI({
    # grab the data frame from the reactive element
    df_to_viz = df()
    # grab the max velocity to display (round down the tenths place) 
    max_velo = floor(max(df_to_viz$Velocity) * 10) / 10
    # display the slider
    sliderInput(
      "analysis_velocity_threshold",
      label = "Velocity Threshold:",
      min = 0,
      max = max_velo,
      value = 12,
      step = 0.1,
      ticks = FALSE
    )
  })
  
  # SPRINT BURST ANALYSIS VARIABLE FILTER SLIDER / THRESHOLD
  
  output$burst_analysis_filter_slider = renderUI({
    filter_var = input$burst_analysis_filter_var
    
    # check whether the user wants to filter or not
    if(is.null(filter_var)) {
      return (NULL)
    }
    
    this_output = list()
    
    if("Distance Travelled" %in% filter_var)
    {
      this_output[[1]] = sliderInput(
        "burst_analysis_filter_threshold_dist",
        label = paste("Distance Travelled", " Threshold:", sep = ''),
        min = 0,
        max = 60,
        value = 30,
        step = 0.1,
        ticks = FALSE
      )
      if("Duration" %in% filter_var)
      {
        this_output[[2]] = sliderInput(
          "burst_analysis_filter_threshold_duration",
          label = paste("Duration", " Threshold:", sep = ''),
          min = 0,
          max = 20,
          value = 5,
          step = 0.1,
          ticks = FALSE
        )
      }
    }
    else if("Duration" %in% filter_var)
    {
      this_output[[1]] = sliderInput(
        "burst_analysis_filter_threshold_duration",
        label = paste("Duration", " Threshold:", sep = ''),
        min = 0,
        max = 20,
        value = 5,
        step = 0.1,
        ticks = FALSE
      )
    }
    return (this_output)
  })
  
  # SPRINT BURST ANALYSIS DATAFRAME
  
  burst_analysis_data <- reactive({
    # grab the original dataframe from the reactive variable 
    burst_analysis_data = df()
    
    # use the sidebar input to identify which players to include in the visualization
    burst_analysis_data = burst_analysis_data %>%
      filter(p_id %in% input$burst_analysis_athlete)
    
    # return the filtered dataframe
    return(burst_analysis_data)
  })
  
  burst_analysis_df = reactive({
    # extract the filtered dataframe from the reactive variable
    # filtered by selected players
    burst_analysis_data = burst_analysis_data()
    
    # grab all sprint bursts using the velocity threshold input by the user
    burst_analysis_df = grab_sprint_bursts(burst_analysis_data, input$analysis_velocity_threshold)
    
    # switch statement that selects which variable to filter by based on user input 
    filter_var = input$burst_analysis_filter_var
    
    # check for further filtering, as specified by the user
    
    if (!is.null(filter_var)) {
      # calculate the aggregate statistic (i.e. duration or distance travelled)
      # then filter to only keep burst ids with values above the threshold
      
      if("Distance Travelled" %in% filter_var)
      {
        filtered_burst_ids = burst_analysis_df %>% 
          group_by(p_id, burst_id) %>% 
          summarise(var_diff = max(Odometer) - min(Odometer),
                    .groups = 'drop') %>%
          filter(var_diff > input$burst_analysis_filter_threshold_dist) %>%
          select(p_id, burst_id)
      }
      if("Duration" %in% filter_var)
      {
        filtered_burst_ids = burst_analysis_df %>% 
          group_by(p_id, burst_id) %>% 
          summarise(var_diff = max(Seconds) - min(Seconds),
                    .groups = 'drop') %>%
          filter(var_diff > input$burst_analysis_filter_threshold_duration) %>%
          select(p_id, burst_id)
      }
      
      # filter the non-aggregated data based on the filtered aggregate list of player and burst ids
      # by performing a merge (inner join) on the two matching columns
      burst_analysis_df = merge(burst_analysis_df, filtered_burst_ids, by = c("p_id", "burst_id"))
    }
    
    # return the new dataset
    return (burst_analysis_df)
  })
  
  # SPRINT BURST ANALYSIS VISUALIZATION
  
  # reactive visual
  burst_analysis_plot = reactive({
    # grab the burst analysis data
    burst_analysis_df = burst_analysis_df()
    
    # switch statement which selects which variable to analyze based on the user's input on the dropdown menu 
    var_name = case_when(
      input$burst_analysis_var == "Velocity" ~ "Velocity",
      input$burst_analysis_var == "Acceleration" ~ "Acceleration",
      input$burst_analysis_var == "Count" ~ "Count",
      input$burst_analysis_var == "Distance Travelled" ~ "Odometer",
      input$burst_analysis_var == "Duration" ~ "Seconds"
    )
    
    if(var_name == "Count") {
      # create a bar graph showing the number of sprint bursts for each player
      plot = burst_analysis_df %>% 
        count(Player_Number, burst_id) %>% 
        ggplot(mapping = aes(x = Player_Number, fill = Player_Number)) +
        geom_bar() +
        scale_fill_viridis_d(option = "turbo") +
        labs(x = "Player Id", y = "Count") +
        theme_bw() +
        theme(legend.position = "none") 
    } else if(var_name == "Odometer" | var_name == "Seconds") { # create a plot of box-and-whisker plots
      # showing the distribution of the distance or duration of the sprints
      if(input$burst_entire_violin) {
        plot = burst_analysis_df %>% 
          group_by(Player_Number, burst_id) %>% 
          summarise(var_diff = max(.data[[var_name]]) - min(.data[[var_name]]),
                    .groups = 'drop') %>% 
          ggplot(mapping = aes(x = Player_Number, y = var_diff, 
                               col = Player_Number, fill = Player_Number)) +
          geom_violin(alpha = 0.4) +
          scale_fill_viridis_d(option = "turbo") +
          scale_color_viridis_d(option = "turbo") +
          labs(y = paste('Distribution of ', input$burst_analysis_var, sep = ''), 
               x = "Player Id") +
          theme_bw() +
          theme(legend.position = "none")
      }
      else{
        plot = burst_analysis_df %>% 
          group_by(Player_Number, burst_id) %>% 
          summarise(var_diff = max(.data[[var_name]]) - min(.data[[var_name]]),
                    .groups = 'drop') %>% 
          ggplot(mapping = aes(x = Player_Number, y = var_diff, 
                               col = Player_Number, fill = Player_Number)) +
          geom_boxplot(alpha = 0.4) +
          scale_fill_viridis_d(option = "turbo") +
          scale_color_viridis_d(option = "turbo") +
          labs(y = paste('Distribution of ', input$burst_analysis_var, sep = ''), 
               x = "Player Id") +
          theme_bw() +
          theme(legend.position = "none")
      }
      
    } else { # otherwise create a plot of box-and-whisker plots 
      # showing the distribution of average velocity or acceleration for the sprints
      
      if(input$burst_entire_violin) {
        plot = burst_analysis_df %>% 
          group_by(Player_Number, burst_id) %>% 
          summarise(var_avg = mean(.data[[var_name]]),
                    .groups = 'drop') %>% 
          ggplot(mapping = aes(x = Player_Number, y = var_avg, 
                               col = Player_Number, fill = Player_Number)) +
          geom_violin(alpha = 0.4) +
          scale_fill_viridis_d(option = "turbo") +
          scale_color_viridis_d(option = "turbo") +
          labs(y = paste('Average ', var_name, ' Within Each Sprint', sep = ''), 
               x = "Player Id") +
          theme_bw() +
          theme(legend.position = "none")
      }
      else
      {
        plot = burst_analysis_df %>% 
          group_by(Player_Number, burst_id) %>% 
          summarise(var_avg = mean(.data[[var_name]]),
                    .groups = 'drop') %>% 
          ggplot(mapping = aes(x = Player_Number, y = var_avg, 
                               col = Player_Number, fill = Player_Number)) +
          geom_boxplot(alpha = 0.4) +
          scale_fill_viridis_d(option = "turbo") +
          scale_color_viridis_d(option = "turbo") +
          labs(y = paste('Average ', var_name, ' Within Each Sprint', sep = ''), 
               x = "Player Id") +
          theme_bw() +
          theme(legend.position = "none")
      }
      
     
    }
    
    # return the visual
    return(plot)
  })
  
  # reactively trigger the creation of the plot 
  plotly_burst_analysis_display = eventReactive(c(input$hide_burst_analysis_sidebar, input$burst_analysis_display), {
    if ((input$hide_burst_analysis_sidebar == 0) & (input$burst_analysis_display == 0)) {
      return (NULL)
    }
    
    # generate the ggplot visual
    plot = burst_analysis_plot()
    
    if(is.null(plot)) {
      return (NULL)
    }
    
    # convert visual to plotly
    ggplotly(plot, height = input$dimension[2] - 200)
  })
  
  # render plot for burst analysis entire game
  output$burst_analysis_viz = renderPlotly({
    plotly_burst_analysis_display()
  })
  
  output$download_burst_analysis <- downloadHandler(
    filename = function() {
      filename = "sprint_burst_analysis"
      paste(filename,'.csv', sep='')
    },
    content = function(file) {
      write.csv(burst_analysis_df(), file)
    }
  )
  
  # HIDE / SHOW SIDEBAR OPTION FOR BURST ANALYSIS
  
  # activate the server logic once the user clicks the 'Hide Sidebar' button
  observeEvent(input$hide_burst_analysis_sidebar, {
    if(input$hide_burst_analysis_sidebar %% 2 == 1){
      # hide the sidebar of user input
      hideElement(
        selector = "#burst_analysis_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('burst_analysis_viz_main_panel', 'col-sm-8')
      addCssClass('burst_analysis_viz_main_panel', 'col-sm-12')
    } else{
      # show the sidebar again
      showElement(
        selector = "#burst_analysis_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('burst_analysis_viz_main_panel', 'col-sm-12')
      addCssClass('burst_analysis_viz_main_panel', 'col-sm-8')
    }
  })
  
  # SPRINT BURST INTERVAL ANALYSIS SECTION
  
  # SPRINT BURST INTERVAL ANALYSIS ATHLETE SELECTION
  
  output$burst_interval_analysis_athlete_filter = renderUI({
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    players_list = possible_players[game_id][[1]]
    
    selectInput("burst_interval_analysis_athlete",
                "Which Athlete: ",
                players_list,
                multiple = TRUE,
                selected = "1"
    )
  })
  
  # SPRINT BURST INTERVAL ANALYSIS SELECT / UNSELECT ALL BUTTON
  
  # activate the server logic once the user clicks the 'Select / Unselect All' button
  observeEvent(input$burst_interval_analysis_select_all_athletes, {
    # grab the list of available players for the game chosen by the user 
    game_id = game_id()
    players_list = possible_players[game_id][[1]]
    
    # update the athlete selection based on the number of times the user has clicked the button
    if(input$burst_interval_analysis_select_all_athletes %% 2 == 1){
      # update the athlete selection to select all
      updateSelectInput(session, 
                        "burst_interval_analysis_athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = players_list
      )
    } else{
      # update the athlete selection to unselect all
      updateSelectInput(session,
                        "burst_interval_analysis_athlete",
                        label = "Which Athlete: ",
                        choices = players_list,
                        selected = NULL
      )
    }
  })
  
  # SPRINT BURST INTERVAL ANALYSIS VELOCITY SLIDER / THRESHOLD
  
  output$burst_interval_analysis_velocity_slider = renderUI({
    # grab the data frame from the reactive element
    df_to_viz = df()
    
    # grab the max velocity to display (round down the tenths place) 
    max_velo = floor(max(df_to_viz$Velocity) * 10) / 10
    
    # display the time filtering slider
    sliderInput(
      "interval_analysis_velocity_threshold",
      label = "Velocity Threshold:",
      min = 0,
      max = max_velo,
      value = 12,
      step = 0.1,
      ticks = FALSE
    )
  })
  
  # SPRINT BURST INTERVAL ANALYSIS VARIABLE FILTER SLIDER / THRESHOLD
  
  output$burst_interval_analysis_filter_slider = renderUI({
    filter_var = input$burst_interval_analysis_filter_var
    
    # check whether the user wants to filter or not
    if(is.null(filter_var)) {
      return (NULL)
    }
    
    this_output = list()
    
    if("Distance Travelled" %in% filter_var)
    {
      this_output[[1]] = sliderInput(
        "burst_interval_analysis_filter_threshold_dist",
        label = paste("Distance Travelled", " Threshold:", sep = ''),
        min = 0,
        max = 60,
        value = 30,
        step = 0.1,
        ticks = FALSE
      )
      if("Duration" %in% filter_var)
      {
        this_output[[2]] = sliderInput(
          "burst_interval_analysis_filter_threshold_duration",
          label = paste("Duration", " Threshold:", sep = ''),
          min = 0,
          max = 20,
          value = 5,
          step = 0.1,
          ticks = FALSE
        )
      }
    }
    else if("Duration" %in% filter_var)
    {
      this_output[[1]] = sliderInput(
        "burst_analysis_filter_threshold_duration",
        label = paste("Duration", " Threshold:", sep = ''),
        min = 0,
        max = 20,
        value = 5,
        step = 0.1,
        ticks = FALSE
      )
    }
    return (this_output)
  })
  
  # SPRINT BURST INTERVAL ANALYSIS DATA / DATAFRAME
  
  burst_interval_analysis_data <- reactive({
    # grab the original dataframe from the reactive variable 
    burst_interval_analysis_data = df()
    
    # use the sidebar input to identify which players to include in the visualization
    burst_interval_analysis_data = burst_interval_analysis_data %>%
      filter(p_id %in% input$burst_interval_analysis_athlete)
    
    # return the filtered dataframe
    return(burst_interval_analysis_data)
  })
  
  burst_interval_analysis_df = reactive({
    # extract the filtered dataframe from the reactive variable
    # filtered by selected players
    burst_interval_analysis_data = burst_interval_analysis_data()
    
    # grab all sprint bursts using the velocity threshold input by the user
    burst_interval_analysis_df = grab_sprint_bursts(burst_interval_analysis_data, 
                                                    input$interval_analysis_velocity_threshold)
    
    # switch statement that selects which variable to filter by based on user input 
    filter_var = input$burst_interval_analysis_filter_var
    
    if (!is.null(filter_var)) {
      # calculate the aggregate statistic (i.e. duration or distance travelled)
      # then filter to only keep burst ids with values above the threshold
      
      if("Distance Travelled" %in% filter_var)
      {
        filtered_burst_ids = burst_interval_analysis_df %>% 
          group_by(p_id, burst_id) %>% 
          summarise(var_diff = max(Odometer) - min(Odometer),
                    .groups = 'drop') %>%
          filter(var_diff > input$burst_interval_analysis_filter_threshold_dist) %>%
          select(p_id, burst_id)
      }
      if("Duration" %in% filter_var)
      {
        filtered_burst_ids = burst_interval_analysis_df %>% 
          group_by(p_id, burst_id) %>% 
          summarise(var_diff = max(Seconds) - min(Seconds),
                    .groups = 'drop') %>%
          filter(var_diff > input$burst_interval_analysis_filter_threshold_duration) %>%
          select(p_id, burst_id)
      }
      
      # filter the non-aggregated data based on the filtered aggregate list of player and burst ids
      # by performing a merge (inner join) on the two matching columns
      burst_interval_analysis_df = merge(burst_interval_analysis_df, filtered_burst_ids, by = c("p_id", "burst_id"))
    }
    
    # return the new dataset
    return (burst_interval_analysis_df)
  })
  
  # SPRINT BURST INTERVAL ANALYSIS VISUALIZATION
  
  # reactive visual 
  burst_interval_analysis_plot = reactive({
    # grab the burst analysis data
    burst_interval_analysis_df = burst_interval_analysis_df()
    
    # grab the interval size/length (make sure to cast/convert to numeric)
    burst_interval_size = input$burst_interval_size %>% as.numeric()
    
    # grab the user input for the variable/statistic to display
    var_name = case_when(
      input$burst_interval_analysis_var == "Velocity" ~ "Velocity",
      input$burst_interval_analysis_var == "Acceleration" ~ "Acceleration",
      input$burst_interval_analysis_var == "Count" ~ "Count",
      input$burst_interval_analysis_var == "Distance Travelled" ~ "Odometer",
      input$burst_interval_analysis_var == "Duration" ~ "Seconds"
    )
    
    # check the user input to determine which variable to put on the x-axis and which to facet by
    if(input$burst_interval_facet_var == "Player Id") {
      facet_var = "Player_Number"
      x_var = "Minutes"
      x_axis_title = "Time Interval (min)"
    } else {
      facet_var = "Minutes"
      x_var = "Player_Number"
      x_axis_title = "Player Id"
    }
    
    if(var_name == "Count") {
      # create a bar graph showing the number of sprint bursts for each player
      plot = burst_interval_analysis_df %>%
        mutate(game_minutes = Seconds / 60,
               Minutes = cut(game_minutes,
                             breaks = make_minute_breaks(c(0, max(game_minutes)), burst_interval_size),
                             include.lowest = TRUE)) %>%
        count(Player_Number, Minutes, burst_id) %>% 
        ggplot(mapping = aes(x = .data[[x_var]], fill = .data[[x_var]])) +
        geom_bar() +
        facet_wrap(~ .data[[facet_var]]) +
        scale_fill_viridis_d(option = "turbo") +
        labs(x = x_axis_title, y = "Count") +
        theme_bw() +
        theme(legend.position = "none")
    } else if(var_name == "Odometer" | var_name == "Seconds") { # create a plot of box-and-whisker plots
      # showing the distribution of the distance or duration of the sprints
      
      if(input$burst_interval_violin) {
        plot = burst_interval_analysis_df %>% 
          mutate(game_minutes = Seconds / 60,
                 Minutes = cut(game_minutes,
                               breaks = make_minute_breaks(c(0, max(game_minutes)), burst_interval_size),
                               include.lowest = TRUE)) %>%
          group_by(Player_Number, Minutes, burst_id) %>% 
          summarise(var_diff = max(.data[[var_name]]) - min(.data[[var_name]]),
                    .groups = 'drop') %>% 
          ggplot(mapping = aes(x = .data[[x_var]], y = var_diff, 
                               col = .data[[x_var]], fill = .data[[x_var]])) +
          geom_violin(alpha = 0.4) +
          facet_wrap(~ .data[[facet_var]]) +
          scale_fill_viridis_d(option = "turbo") +
          scale_color_viridis_d(option = "turbo") +
          labs(y = paste('Distribution of ', input$burst_interval_analysis_var, sep = ''), 
               x = x_axis_title) +
          theme_bw() +
          theme(legend.position = "none")
      }
      else{
        plot = burst_interval_analysis_df %>% 
          mutate(game_minutes = Seconds / 60,
                 Minutes = cut(game_minutes,
                               breaks = make_minute_breaks(c(0, max(game_minutes)), burst_interval_size),
                               include.lowest = TRUE)) %>%
          group_by(Player_Number, Minutes, burst_id) %>% 
          summarise(var_diff = max(.data[[var_name]]) - min(.data[[var_name]]),
                    .groups = 'drop') %>% 
          ggplot(mapping = aes(x = .data[[x_var]], y = var_diff, 
                               col = .data[[x_var]], fill = .data[[x_var]])) +
          geom_boxplot(alpha = 0.4) +
          facet_wrap(~ .data[[facet_var]]) +
          scale_fill_viridis_d(option = "turbo") +
          scale_color_viridis_d(option = "turbo") +
          labs(y = paste('Distribution of ', input$burst_interval_analysis_var, sep = ''), 
               x = x_axis_title) +
          theme_bw() +
          theme(legend.position = "none")
      }
      
      
    } else { # otherwise we need to create a plot of box-and-whisker plots
      # check if violin plot
      if(input$burst_interval_violin) {
        plot = burst_interval_analysis_df %>% 
          mutate(game_minutes = Seconds / 60,
                 Minutes = cut(game_minutes,
                               breaks = make_minute_breaks(c(0, max(game_minutes)), burst_interval_size),
                               include.lowest = TRUE)) %>%
          group_by(Player_Number, Minutes, burst_id) %>% 
          summarise(var_avg = mean(.data[[var_name]]),
                    .groups = 'drop') %>% 
          ggplot(mapping = aes(x = .data[[x_var]], y = var_avg, 
                               col = .data[[x_var]], fill = .data[[x_var]])) +
          geom_violin(alpha = 0.4) +
          facet_wrap(~ .data[[facet_var]]) +
          scale_fill_viridis_d(option = "turbo") +
          scale_color_viridis_d(option = "turbo") +
          labs(y = paste('Average ', var_name, ' Within Each Sprint', sep = ''), 
               x = x_axis_title) +
          theme_bw() +
          theme(legend.position = "none")
      }
      else
      {
        plot = burst_interval_analysis_df %>% 
          mutate(game_minutes = Seconds / 60,
                 Minutes = cut(game_minutes,
                               breaks = make_minute_breaks(c(0, max(game_minutes)), burst_interval_size),
                               include.lowest = TRUE)) %>%
          group_by(Player_Number, Minutes, burst_id) %>% 
          summarise(var_avg = mean(.data[[var_name]]),
                    .groups = 'drop') %>% 
          ggplot(mapping = aes(x = .data[[x_var]], y = var_avg, 
                               col = .data[[x_var]], fill = .data[[x_var]])) +
          geom_boxplot(alpha = 0.4) +
          facet_wrap(~ .data[[facet_var]]) +
          scale_fill_viridis_d(option = "turbo") +
          scale_color_viridis_d(option = "turbo") +
          labs(y = paste('Average ', var_name, ' Within Each Sprint', sep = ''), 
               x = x_axis_title) +
          theme_bw() +
          theme(legend.position = "none")
      }
    }
    
    # if time interval is on the x_axis we need to tilt the labels
    if (x_var == "Minutes") {
      plot = plot + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    }
    
    # return the visual
    return(plot)
  })
  
  # reactively trigger the creation of the plot 
  plotly_burst_interval_analysis_display = eventReactive(c(input$hide_burst_interval_analysis_sidebar, input$burst_interval_analysis_display), {
    if ((input$hide_burst_interval_analysis_sidebar == 0) & (input$burst_interval_analysis_display == 0)) {
      return (NULL)
    }
    
    # generate the ggplot visual
    plot = burst_interval_analysis_plot()
    
    if(is.null(plot)) {
      return (NULL)
    }
    
    # convert visual to plotly
    ggplotly(plot, height = input$dimension[2] - 200)
  })
  
  # output the plot/visualization for game interval burst analysis
  output$burst_interval_analysis_viz = renderPlotly({
    plotly_burst_interval_analysis_display()
  })
  
  output$download_burst_analysis_interval <- downloadHandler(
    filename = function() {
      filename = "sprint_burst_analysis_interval"
      paste(filename,'.csv', sep='')
    },
    content = function(file) {
      write.csv(burst_interval_analysis_df(), file)
    }
  )
  
  # HIDE / SHOW SIDEBAR OPTION FOR BURST INTERVAL ANALYSIS
  
  # activate the server logic once the user clicks the 'Hide Sidebar' button
  observeEvent(input$hide_burst_interval_analysis_sidebar, {
    if(input$hide_burst_interval_analysis_sidebar %% 2 == 1){
      # hide the sidebar of user input
      hideElement(
        selector = "#burst_interval_analysis_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('burst_interval_analysis_viz_main_panel', 'col-sm-8')
      addCssClass('burst_interval_analysis_viz_main_panel', 'col-sm-12')
    } else{
      # show the sidebar again
      showElement(
        selector = "#burst_interval_analysis_viz_sidebar"
      )
      
      # resize the main panel with the plot visuals
      removeCssClass('burst_interval_analysis_viz_main_panel', 'col-sm-12')
      addCssClass('burst_interval_analysis_viz_main_panel', 'col-sm-8')
    }
  })
})