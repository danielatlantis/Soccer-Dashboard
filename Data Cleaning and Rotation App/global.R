library(shiny)
library(tidyverse)

setwd("..")
### read in the game info 
game_management = read_csv("Data_Management/Game_Management.csv", col_names = TRUE)
### read in the location info for the fields
locations <- read_csv("Data_Management/Locations.csv", col_names = TRUE)
### read in the rotation info for the fields
rotate_info <- readr::read_csv('Data_Management/field_rotate_info.csv',col_names = TRUE)

possible_locs = list()

for(i in 1:nrow(locations))
{
  loc_id = locations$Location_ID[[i]] 
  address = locations$Full_Address[[i]] 
  loc_name = paste(loc_id, address, sep = ". ")
  possible_locs[[i]] = loc_name
}
possible_locs[[i+1]] = "None"

possible_loc_name = possible_locs

### define a function which projects the Lat/Long coordinates to
### a 2D system based on the field the game is located at
make_player_2d_coords <- function(players_df, a_field)
{
  # define the rotation point as the point with the lowest Lat
  # and convert lat/long to radians
  players_df %>% 
    mutate(lambda = Longitude * pi / 180,
           phi = Latitude * pi / 180) %>% 
    mutate(x = (lambda - a_field$lon_ref) * cos(0.5*(a_field$lat_ref + phi)),
           y = phi - a_field$lat_ref) %>% 
    # radius of the earth is 6371 km
    mutate(x = 6371 * x, y = 6371 * y) %>% 
    # convert to yards
    mutate(x = x * 1093.6133, y = y * 1093.6133)
}

### define a function which rotates the player 2D projected coordinates
### based on the field the game is located at
rotate_player_2d <- function(players_df, a_field)
{
  
  if(a_field$clock_type == 'counter clockwise' ){
    #rotate counter clockwise
    players_df <- players_df %>%
      mutate(rotate_x = x * cos( a_field$rotate_radian ) - y * sin( a_field$rotate_radian ),
             rotate_y = x * sin( a_field$rotate_radian ) + y * cos( a_field$rotate_radian ))
    
  }
  else {
    # rotate clockwise
    players_df <- players_df %>%
      mutate(rotate_x = x * cos( a_field$rotate_radian ) + y * sin( a_field$rotate_radian ),
             rotate_y = -x * sin( a_field$rotate_radian ) + y * cos( a_field$rotate_radian ))
  }
  
  # subtract out the rotated center point
  players_df <- players_df %>% 
    mutate(new_x = rotate_x - a_field$rotated_mid_x, 
           new_y = rotate_y - a_field$rotated_mid_y) %>%
    mutate(Lat = new_x,
           Long = new_y)
}