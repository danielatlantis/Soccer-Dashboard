library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  # navbar page
  navbarPage('University of Pittsburgh Soccer',
             tabPanel('Welcome!',
                      includeMarkdown('welcome_page.md')),
             tabPanel('Player Data',
                      sidebarLayout(
                        sidebarPanel(
                          h4("Data Input"),
                          fileInput("game_file", "Choose a file to be imported",
                                    multiple = TRUE,
                                    accept = ".csv"),
                          selectInput("loc",
                                      "Location: ",
                                      possible_loc_name,
                                      selected = "None"),
                          uiOutput("no_saved_coords"),
                          
                          textInput("shortCollegeName", "Short College Name for Opponent", "ex: Pitt, Cleveland-State"),
                          textInput("fullCollegeName", "Full College Name for Opponent", "ex: University of Pittsburgh"),
                          textInput("city", "Field City", ""),
                          textInput("state", "Field State", "ex: PA"),
                          numericInput("year", "Year", ""),
                          numericInput("month", "Month", "ex: 01 for January"),
                          numericInput("day", "Day", ""),
                          actionButton("done", "Rotate Data!"),
                          ),
                        mainPanel(
                          tableOutput("file_contents"),
                          verbatimTextOutput("finished_upload"),
                          tableOutput("transformed_content"),
                          verbatimTextOutput("finished_df"),
                          verbatimTextOutput("athlete"),
                          downloadButton("download", "Download Data"),
                        )
                        )
                      ),
             tabPanel('Description',
                      includeMarkdown('description.md')
             )
                      )
  )


