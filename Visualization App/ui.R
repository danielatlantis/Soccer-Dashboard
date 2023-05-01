

# Define UI for application that draws a histogram
shinyUI(
  # navbar page
  navbarPage('University of Pittsburgh Soccer',
             # grab the page dimensions
             tags$head(tags$script('
                            var dimension = [0, 0];
                            // on connection grab the dimensions
                            $(document).on("shiny:connected", function(e) {
                              dimension[0] = window.innerWidth;
                              dimension[1] = window.innerHeight;
                              Shiny.onInputChange("dimension", dimension);
                            });
                            // grab the new dimensions whenever the user resizes the window
                            $(window).resize(function(e) {
                              dimension[0] = window.innerWidth;
                              dimension[1] = window.innerHeight;
                              Shiny.onInputChange("dimension", dimension);
                            });
             ')),
             tabPanel('Welcome!',
                      # activate shinyjs package
                      useShinyjs(),
                      # activate shinybrowser package
                      detect(),
                      includeMarkdown('welcome_page.md')
             ),
             tabPanel('Game Selection', 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("season",
                                      "Seasons: ",
                                      years
                          ),
                          actionButton("choose_season", "Choose Season"),
                          uiOutput("display_games")
                        ),
                        mainPanel(
                          uiOutput("game_loaded")
                        )
                      )
             ),
             tabPanel('Player Visualization',
                      # use a sidebar layout
                      sidebarLayout(
                        sidebarPanel(
                          # id for jQuery selection
                          id = 'player_viz_sidebar',
                          h4("Visualize Based on Players, Time Interval, and Variables"),
                          # display athlete selection options
                          uiOutput("athlete_select"),
                          # select all athletes option
                          actionButton("select_all_athletes", "Select / Unselect All"),
                          # display time filter input option
                          uiOutput("time_slider"),
                          # Select variable to filter by
                          selectInput("var",
                                      "Which variable would you like to display: ",
                                      c("Player ID", "Velocity", "Acceleration", "Positional Density", "Time Elapsed"),
                                      selected = "None"
                          ),
                          checkboxInput("facet", "Player's displayed on Different fields?", FALSE),
                          # Flip Data
                          checkboxInput("flip_data", "Flip Data Orientation?", FALSE),
                          # display field
                          actionButton("display", "Display Field"),
                        ),
                        mainPanel(
                          # hide the sidebar to make visuals larger
                          actionButton("hide_sidebar", "Show / Hide Sidebar"),
                          
                          downloadButton("download_player_viz", "Download Data"),
                          # id for jQuery selection
                          id = 'player_viz_main_panel',
                          # Displays soccer field 
                          plotlyOutput("plotly_field") %>% withSpinner(type = 8, color.background = 'white', color = "#03b1fc", size = 1),
                          #verbatimTextOutput("time_info")
                          
                          
                        )
                      )
             ),
             tabPanel('Player Analysis',
                      tabsetPanel(type = 'pills',
                                  tabPanel('Entire Game',
                                           sidebarLayout(
                                             sidebarPanel(
                                               # id for jQuery selection
                                               id = 'analysis_viz_sidebar',
                                               h4("User Input Options"),
                                               # display athlete selection options
                                               uiOutput("analysis_athlete_filter"),
                                               # select all athletes option
                                               actionButton("analysis_select_all_athletes", "Select / Unselect All"),
                                               # select variable to analyze / display
                                               selectInput("analysis_var",
                                                           "Which variable would you like to analyze: ",
                                                           c("Velocity", "Acceleration", "Distance Travelled"),
                                                           selected = "None"
                                               ),
                                               # Violin Plot
                                               checkboxInput("player_violin", "Use Violin Plot?", FALSE),
                                               # button to trigger display
                                               actionButton("analysis_display", "Display Visual")
                                             ),
                                             mainPanel(
                                               # hide the sidebar to make visuals larger
                                               actionButton("hide_analysis_sidebar", "Show / Hide Sidebar"),
                                               
                                               downloadButton("download_player_analysis", "Download Data"),
                                               # id for jQuery selection
                                               id = 'analysis_viz_main_panel',
                                               # displays visualization 
                                               plotlyOutput("analysis_viz") %>% withSpinner(type = 8, color.background = 'white', color = "#03b1fc", size = 1)
                                             )
                                           )
                                  ),
                                  tabPanel('Game Intervals',
                                           sidebarLayout(
                                             sidebarPanel(
                                               # id for jQuery selection
                                               id = 'interval_analysis_viz_sidebar',
                                               h4("User Input Options"),
                                               # display athlete selection options
                                               uiOutput("interval_analysis_athlete_filter"),
                                               # select all athletes option
                                               actionButton("interval_analysis_select_all_athletes", "Select / Unselect All"),
                                               # select variable to analyze / display
                                               selectInput("interval_analysis_var",
                                                           "Which variable would you like to analyze: ",
                                                           c("Velocity", "Acceleration", "Distance Travelled"),
                                                           selected = "None"
                                               ),
                                               # select size / length of time intervals
                                               selectInput("interval_size",
                                                           "Select the desired length/size of the time intervals: ",
                                                           c('5', '10', '15', '20', '25', '30', '35', '40', '45'),
                                                           selected = '10'
                                               ),
                                               # select which variable to facet by 
                                               radioButtons(
                                                 "interval_facet_var",
                                                 "Which variable would you like to facet the visualization by:",
                                                 choices = c("Player Id", "Time Interval"),
                                                 selected = "Player Id"
                                               ),
                                               # button to trigger display
                                               actionButton("interval_analysis_display", "Display Visual")
                                             ),
                                             mainPanel(
                                               # hide the sidebar to make visuals larger
                                               actionButton("hide_interval_analysis_sidebar", "Show / Hide Sidebar"),
                                               
                                               downloadButton("download_player_analysis_interval", "Download Data"),
                                               # id for jQuery selection
                                               id = 'interval_analysis_viz_main_panel',
                                               # displays visualization 
                                               plotlyOutput("interval_analysis_viz") %>% withSpinner(type = 8, color.background = 'white', color = "#03b1fc", size = 1)
                                             )
                                           )
                                  )
                      )
             ),
             tabPanel('Sprint Burst Visualization',
                       sidebarLayout(
                         sidebarPanel(
                           # id for jQuery selection
                           id = 'burst_viz_sidebar',
                           # header / title
                           h4("User Input Options"),
                           # display athlete selection options
                           uiOutput("burst_athlete_filter"),
                           # select all athletes option
                           actionButton("burst_select_all_athletes", "Select / Unselect All"),
                           # display time filter input option
                           uiOutput("burst_time_filter"),
                           # display velocity threshold input option
                           uiOutput("burst_velocity_slider"),
                           # select variable to filter further by
                           selectInput("burst_filter_var",
                                       "Which additional variable would you like to filter by: ",
                                       c( "Distance Travelled", "Duration"),
                                       selected = "None",
                                       multiple = TRUE
                           ),
                           # display filter threshold for variable of user's choice
                           uiOutput("burst_filter_slider"),
                           #display facets
                           checkboxInput("facet_burst", "Player's displayed on Different fields?", FALSE),
                           #flip data
                           checkboxInput("flip_data_burst", "Flip Data Orientation?", FALSE),
                           # click to display the visual
                           actionButton("burst_display", "Display Field")
                         ),
                         mainPanel(
                           # hide the sidebar to make visuals larger
                           actionButton("hide_burst_sidebar", "Hide / Show Sidebar"),
                           
                           downloadButton("download_burst", "Download Data"),
                           # id for jQuery selection
                           id = 'burst_viz_main_panel',
                           # Displays soccer field 
                           plotlyOutput("burst_viz") %>% withSpinner(type = 8, color.background = 'white', color = "#03b1fc", size = 1),
                           # Displays clicked on data
                           #verbatimTextOutput("burst_time_info")
                        )
                      )
             ),
             tabPanel("Sprint Burst Analysis",
                      tabsetPanel(type = 'pills',
                                  tabPanel('Entire Game',
                                           sidebarLayout(
                                             sidebarPanel(
                                               # id for jQuery selection
                                               id = 'burst_analysis_viz_sidebar',
                                               h4("User Input Options"),
                                               # display athlete selection options
                                               uiOutput("burst_analysis_athlete_filter"),
                                               # select all athletes option
                                               actionButton("burst_analysis_select_all_athletes", "Select / Unselect All"),
                                               # select variable to analyze / display
                                               selectInput("burst_analysis_var",
                                                           "Which variable would you like to analyze: ",
                                                           c("Count", "Velocity", "Acceleration", "Distance Travelled", "Duration"),
                                                           selected = "Velocity"
                                               ),
                                               # display velocity threshold input option
                                               uiOutput("burst_analysis_velocity_slider"),
                                               # select variable to filter further by
                                               selectInput("burst_analysis_filter_var",
                                                           "Which additional variable would you like to filter by: ",
                                                           c( "Distance Travelled", "Duration"),
                                                           selected = "None",
                                                           multiple = TRUE
                                               ),
                                               # display filter threshold for variable of user's choice
                                               uiOutput("burst_analysis_filter_slider"),
                                               # button to trigger display
                                               # Violin Plot
                                               checkboxInput("burst_entire_violin", "Use Violin Plot?", FALSE),
                                               actionButton("burst_analysis_display", "Display Visual")
                                             ),
                                             mainPanel(
                                               # hide the sidebar to make visuals larger
                                               actionButton("hide_burst_analysis_sidebar", "Show / Hide Sidebar"),
                                               downloadButton("download_burst_analysis", "Download Data"),
                                               # id for jQuery selection
                                               id = 'burst_analysis_viz_main_panel',
                                               # displays visualization 
                                               plotlyOutput("burst_analysis_viz") %>% withSpinner(type = 8, color.background = 'white', color = "#03b1fc", size = 1)
                                             )
                                           )
                                  ),
                                  tabPanel('Game Intervals',
                                           sidebarLayout(
                                             sidebarPanel(
                                               # id for jQuery selection
                                               id = 'burst_interval_analysis_viz_sidebar',
                                               h4("User Input Options"),
                                               # display athlete selection options
                                               uiOutput("burst_interval_analysis_athlete_filter"),
                                               # select all athletes option
                                               actionButton("burst_interval_analysis_select_all_athletes", "Select / Unselect All"),
                                               # select variable to analyze / display
                                               selectInput("burst_interval_analysis_var",
                                                           "Which statistic would you like to analyze: ",
                                                           c("Count", "Velocity", "Acceleration", "Distance Travelled", "Duration"),
                                                           selected = "None"
                                               ),
                                               # select size / length of time intervals
                                               selectInput("burst_interval_size",
                                                           "Select the desired length/size of the time intervals: ",
                                                           c('5', '10', '15', '20', '25', '30', '35', '40', '45'),
                                                           selected = '10'
                                               ),
                                               # select which variable to facet by 
                                               radioButtons(
                                                 "burst_interval_facet_var",
                                                 "Which variable would you like to facet the visualization by:",
                                                 choices = c("Player Id", "Time Interval"),
                                                 selected = "Player Id"
                                               ),
                                               # display velocity threshold input option
                                               uiOutput("burst_interval_analysis_velocity_slider"),
                                               # select variable to filter further by
                                               selectInput("burst_interval_analysis_filter_var",
                                                           "Which additional variable would you like to filter by: ",
                                                           c( "Distance Travelled", "Duration"),
                                                           selected = "None",
                                                           multiple = TRUE
                                               ),
                                               # display filter threshold for variable of user's choice
                                               uiOutput("burst_interval_analysis_filter_slider"),
                                               # button to trigger display
                                               # Violin Plot
                                               checkboxInput("burst_interval_violin", "Use Violin Plot?", FALSE),
                                               actionButton("burst_interval_analysis_display", "Display Visual")
                                             ),
                                             mainPanel(
                                               # hide the sidebar to make visuals larger
                                               actionButton("hide_burst_interval_analysis_sidebar", "Show / Hide Sidebar"),
                                               downloadButton("download_burst_analysis_interval", "Download Data"),
                                               # id for jQuery selection
                                               id = 'burst_interval_analysis_viz_main_panel',
                                               # displays visualization 
                                               plotlyOutput("burst_interval_analysis_viz") %>% withSpinner(type = 8, color.background = 'white', color = "#03b1fc", size = 1)
                                             )
                                           )
                                  )
                      )
             ),
             tabPanel('Description',
                      includeMarkdown('description.md')
                      )
             )
  )
