library(shiny)
library(sportyR)
library(tidyverse)
library(plotly)
library(ggrepel)
library(shinyWidgets)
library(lubridate)
library(ggExtra)
library(shinyjs)
library(shinybrowser)
library(shinycssloaders)
library(ggforce)

setwd("..")
game_management = read_csv("Data_Management/Game_Management.csv", col_names = TRUE)
locations <- read_csv("Data_Management/Locations.csv", col_names = TRUE)

seasons = c()
for(i in 1:nrow(game_management)) {
  seasons = append(seasons,game_management$Year[[i]])
}

#get all years, but only include each one once
years = paste(unique(seasons), collapse = ", ")

#surround athletes with quotes

possible_players = list()
# Collect data from game management file (players)
for(i in 1:nrow(game_management)) {
  players = game_management$Players
  possible_players[[i]] = substr(players[i], 2, nchar(players[i])-1)
  possible_players[i] = str_split(possible_players[i], ",")
}

# SPRINT BURST FUNCTIONS

# Grab All Sprint Bursts for a Given Velocity Threshold

grab_sprint_bursts = function(df, velocity_threshold) {
  # filter the dataframe to only include times where the player is running at a speed above the velocity threshold
  sprint_burst_df = df %>% 
    filter(Velocity > velocity_threshold) %>% 
    group_by(p_id) %>% 
    mutate(lag_seconds = lag(Seconds)) %>% 
    mutate(diff_seconds = round(Seconds - lag_seconds, 2)) %>% 
    mutate(check_seq = ifelse(diff_seconds > 0.1,
                              1,
                              0)) %>% 
    mutate(burst_id0 = check_seq) %>% 
    mutate(burst_id0 = ifelse(is.na(burst_id0), 1, burst_id0)) %>% 
    mutate(burst_id = cumsum(burst_id0)) %>%
    ungroup()
  
  return(sprint_burst_df)
}

make_minute_breaks <- function(time, time_interval) {
  # grab the range of times in the data (i.e. range(Seconds))
  time_range <- time %>% range()
  
  # if there are no values outside the evenly spaced intervals, simply return the sequence
  if( time_range[2] %% time_interval == 0 ) {
    return( seq(time_range[1], time_range[2], by = time_interval) )
  } else { # otherwise create an extra bin for the "left over" values
    round_max <- time_range[2] - (time_range[2] %% time_interval)
    return( c( seq(time_range[1], round_max, by = time_interval), time_range[2]) )
  }
}

# IDENTIFY FACET LAYOUT FOR VISUALS BASED ON WINDOW SIZE

find_facet_cols = function(num_viz, browser_width, browser_height) {
  num_cols = 1
  indiv_viz_height = browser_height / num_viz
  indiv_viz_width = browser_width
  indiv_viz_aspect_ratio = indiv_viz_height / indiv_viz_width
  
  while (indiv_viz_aspect_ratio < 0.5) {
    num_cols = num_cols + 1
    viz_per_col = num_viz / num_cols
    
    indiv_viz_height = browser_height / viz_per_col
    indiv_viz_width = browser_width / num_cols
    indiv_viz_aspect_ratio = indiv_viz_height / indiv_viz_width
  }
  
  return(num_cols)
}

# SPORTY R SET UP

# variable to hold the sportyR geom_soccer object
soccer_field_geom = geom_soccer("ncaa", color_updates = list(
  plot_background = NULL,
  offensive_half_pitch = "#FFFFFF",
  defensive_half_pitch = "#FFFFFF",
  pitch_apron = "#FFFFFF",
  touchline = "#000000",
  goal_line = "#000000",
  corner_arc = "#000000",
  halfway_line = "#000000",
  center_circle = "#000000",
  center_mark = "#000000",
  penalty_box = "#000000",
  goal_box = "#000000",
  penalty_mark = "#000000",
  corner_defensive_mark = "#000000",
  goal = "#000000")
)

