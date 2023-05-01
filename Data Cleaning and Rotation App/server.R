library(shiny)
library(shinyFiles)
library(tidyverse)
library(proxy)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  setwd("..")
  possible_loc = reactive(input$loc)
  
  output$no_saved_coords = renderUI({
    this_output = list()
    if(possible_loc() == 'None')
    {
      #get all information needed about game to rotate and store game information in tables
      this_output[[1]] = numericInput("corner1_lat", "Corner Point (Lat)", 0)
      this_output[[2]] = numericInput("corner1_long", "Corner Point (Long)", 0)
      this_output[[3]] = numericInput("corner2_lat", "Corner Point (Lat)", 0)
      this_output[[4]] = numericInput("corner2_long", "Corner Point (Long)", 0)
      this_output[[5]] = numericInput("corner3_lat", "Corner Point (Lat)", 0)
      this_output[[6]] = numericInput("corner3_long", "Corner Point (Long)", 0)
      this_output[[7]] = numericInput("corner4_lat", "Corner Point (Lat)", 0)
      this_output[[8]] = numericInput("corner4_long", "Corner Point (Long)", 0)
      this_output[[9]] = textInput("fullFieldName", "Field Name", "")
      return (this_output)
    }
    else{
      ### we have rotate info saved and we will get it later
      return (NULL)
    }
  })
  
  # show a few rows of files that user wants to rotate
  output$file_contents = renderTable({
    if(is.null(input$game_file))
      return(NULL)
    
    file_list = list()
    
    # get list of files the the user inputs
    for(i in 1:length(input$game_file[, 1])){
      file_list[[i]] = read.csv(input$game_file[[i, "datapath"]], skip = 8)
    }
    # once create a list of all files that are inputted, we are ready to rotate
    output$finished_upload = renderText({"Ready to Rotate!"})
    head(file_list[[1]])
  })
  
  # these eventReactives get the input of all info user enters for a new game we dont have field information for
  corner1_lat = eventReactive(input$done, {
    input$corner1_lat
  })
  
  corner1_long = eventReactive(input$done, {
    input$corner1_long
  })
  
  corner2_lat = eventReactive(input$done, {
    input$corner2_lat
  })
  
  corner2_long = eventReactive(input$done, {
    input$corner2_long
  })
  
  corner3_lat = eventReactive(input$done, {
    input$corner3_lat
  })
  
  corner3_long = eventReactive(input$done, {
    input$corner3_long
  })
  
  corner4_lat = eventReactive(input$done, {
    input$corner4_lat
  })
  
  corner4_long = eventReactive(input$done, {
    input$corner4_long
  }) 
  
  shortCollegeName = eventReactive(input$done, {
    input$shortCollegeName
  }) 
  
  fullFieldName = eventReactive(input$done, {
    input$fullFieldName
  }) 
  
  fullCollegeName = eventReactive(input$done, {
    input$fullCollegeName
  }) 
  
  city = eventReactive(input$done, {
    input$city
  })
  
  state = eventReactive(input$done, {
    input$state
  })
  
  day = eventReactive(input$done, {
    input$day
  })
  
  month = eventReactive(input$done, {
    input$month
  })
  
  year = eventReactive(input$done, {
    input$year
  })
  
  # this event reactive creates a new dataframe that contains all the player files rotated and combined into one dataframe
  rotated_binded_df = eventReactive(input$done, {
    if(is.null(input$game_file))
    {
      return(NULL)
    }
    file_list = list()
    
    loc_id = 0 # this will be changed, depending on if we have the location info saved or not
    # if we have loc saved, use saved coordinates, if no loc saved, get inputs from user
    if(possible_loc() == 'None')# we do not have the location info, we need to get it from user
    {
      
      # write new location to location file
      locations[nrow(locations) + 1,] <- list(nrow(locations) + 1, shortCollegeName(), fullFieldName(), fullCollegeName(),city(),state(),corner1_lat(), corner1_long(),corner2_lat(),corner2_long(), corner3_lat(),corner3_long(), corner4_lat(),corner4_long())
      
      locations %>% 
        readr::write_csv('Data_Management/Locations.csv', col_names = TRUE)
      
      ### make the field data TIDY
      tidy_fields <- locations %>% 
        select(Location_ID, College_Loc, Full_Address, Full_College, City, State,
               starts_with('Corner')) %>% 
        pivot_longer(starts_with('Corner')) %>% 
        tidyr::separate(name,
                        c("corner_word", "latlong"),
                        sep = "_") %>% 
        mutate(corner_id = stringr::str_extract(corner_word, '\\d+')) %>% 
        mutate(corner_id = as.numeric(corner_id)) %>% 
        select(-corner_word) %>% 
        pivot_wider(id_cols = c("Location_ID", "College_Loc", "Full_Address",
                                "Full_College", 'City', 'State', 'corner_id'),
                    names_from = 'latlong',
                    values_from = 'value') %>% 
        rename(Latitude = Lat, Longitude = Long)
      
      ### save the tidy field information
      tidy_fields %>% 
        readr::write_csv('Data_Management/tidy_fields.csv', col_names = TRUE)
      
      
      ### apply to all fields
      rotated_tidy_fields <- tidy_fields %>% 
        select(Location_ID, Longitude, Latitude, corner_id) %>% 
        mutate(field_id = Location_ID) %>% 
        group_by(field_id) %>% 
        tidyr::nest() %>% 
        mutate(data_2d = purrr::map(data, make_2d_coords),
               data_rotate = purrr::map(data_2d, make_2d_horizontal)) %>% 
        select(field_id, data_rotate) %>% 
        tidyr::unnest(cols = c(data_rotate)) %>% 
        ungroup()
      
      ### the information required to rotate each field
      field_rotate_info <- rotated_tidy_fields %>% 
        distinct(Location_ID, clock_type, rotate_radian, rotate_degree,
                 lat_ref, lon_ref, rotated_mid_x, rotated_mid_y)
      
      ### save the rotated field information
      field_rotate_info %>% 
        readr::write_csv('field_rotation/field_rotate_info.csv',
                         col_names = TRUE)
      
      # set loc id for the rotation, it is the last row in the location file since we just added it
      loc_id = nrow(location)
      
      # get the rotate info again after writing to it
      rotate_info <- readr::read_csv('Data_Management/field_rotate_info.csv',col_names = TRUE)
      
    }
    else #already have the coords
    {
      # get loc id from user input
      loc_id = as.numeric(str_split(possible_loc() , ". ", 2)[[1]][1])
    }
    
    athlete_string = c()  # used to get athlete number from each of the files
    
    # go through each of the files and rotate each one separately, then combine them all together
    # we combine them all together so it is easier to visualize in the other app
    for(i in 1:length(input$game_file[, 1]))
    {
      file_list[[i]] = read.csv(input$game_file[[i, "datapath"]], skip = 8)
      
      ### get the rotation information for this game's field
      rotate_use <- rotate_info %>% 
        filter(Location_ID == loc_id)
      
      # rotate the data depending on the field location, this makes the fields horizantal so we can graph them easier  
      file_list[[i]] = file_list[[i]] %>% 
        make_player_2d_coords(rotate_use) %>% 
        rotate_player_2d(rotate_use) 
      
      # getting the names of files so we can check which file corresponds to which player and which half it is
      file_name <-basename(input$game_file[[i, "name"]])
      
      length_file_name = nchar(file_name)
      athlete_num <- as.integer(substr(file_name,unlist(gregexpr('Athlete ', file_name))[1] + 8, length_file_name - 3))
      
      athlete_string = append(athlete_string, athlete_num)
      
      # need to update the seconds in each file based on which half it is, so each half does not restart at 0 seconds, so need to add a new column that represents which half of the game this file represents
      if(TRUE %in% grepl('2OT', file_name, fixed = TRUE))
      {
        file_list[[i]] =  file_list[[i]] %>% mutate (p_id = athlete_num[1])
        file_list[[i]] =  file_list[[i]] %>% mutate (Half = as.integer(4))
      }
      else if(TRUE %in% grepl('FIRST HALF', file_name, fixed = TRUE))
      {
        file_list[[i]] =  file_list[[i]] %>% mutate (p_id = athlete_num[1])
        file_list[[i]] =  file_list[[i]] %>% mutate (Half =  as.integer(1))
      }
      else if(TRUE %in% grepl('SECOND HALF', file_name, fixed = TRUE))
      {
        file_list[[i]] =  file_list[[i]] %>% mutate (p_id = athlete_num[1])
        file_list[[i]] =  file_list[[i]] %>% mutate (Half =  as.integer(2))
      }
      else # first ot
      {
        file_list[[i]] =  file_list[[i]] %>% mutate(p_id = athlete_num[1])
        file_list[[i]] =  file_list[[i]] %>% mutate (Half =  as.integer(3))
      }
    }
    # end for loop, done rotating each file
    
    # bind all files into one big file
    the_big_file <- bind_rows(file_list)
    
    # editing the seconds depending on which half of the game it is
    half_1 = the_big_file %>% filter(Half == 1)
    half_2 = the_big_file %>% filter(Half == 2)
    half_3 = the_big_file %>% filter(Half == 3)
    
    the_big_file = the_big_file %>% 
      select(Seconds, Velocity, Acceleration, Odometer, Lat, Long, Half, p_id) %>% 
      mutate(Seconds = ifelse(Half == 2, Seconds + max(half_1$Seconds), 
                              ifelse(Half == 3, Seconds + max(half_2$Seconds) + max(half_1$Seconds), 
                                     ifelse(Half == 4, Seconds + max(half_3$Seconds) + max(half_2$Seconds) + max(half_1$Seconds), Seconds))))
    
    Seconds = c()
    Velocity = c()
    Acceleration = c()
    Odometer = c()
    Lat = c()
    Long = c()
    Half = c()
    p_id = c()
    
    df <- data.frame(Seconds,Velocity,Acceleration,Odometer,Lat,Long,Half,p_id)
    
    # fix the odometer for each player's data
    for(player in unique(the_big_file$p_id)) {
      half_1 = the_big_file %>% filter(Half == 1) %>%
        filter(p_id == player)
      half_2 = the_big_file %>% filter(Half == 2) %>%
        filter(p_id == player)
      half_3 = the_big_file %>% filter(Half == 3) %>%
        filter(p_id == player)
      
      # fix odometer reading so total distance ran is cumulative for whole game
      updated_odometer = the_big_file %>%
        filter(p_id == player) %>%
        mutate(Odometer = case_when(Half == 2 ~ Odometer + max(half_1$Odometer), 
                                    Half == 3 ~ Odometer + max(half_2$Odometer) + max(half_1$Odometer),
                                    Half == 4 ~ Odometer + max(half_3$Odometer) + max(half_2$Odometer) + max(half_1$Odometer), 
                                    Half == 1 ~ Odometer))
      df = rbind(df, updated_odometer)
    }
    
    # filter out data not on field
    df = df %>% filter(Lat <= 62 | Lat >= -62)
    df = df %>% filter(Long <= 40 | Long >= -40)
    
    
    #get all athletes, but only include each one once
    athletes = paste(unique(athlete_string), collapse = ", ")
    #surround athletes with quotes
    athletes = paste("\"", athletes, sep = "")
    athletes = paste( athletes, "\"",sep = "")
    # write new game to game management file, need to copy in players later, the loc_id is nrow(locations) because we have already inserted the new location to the file
    game_management[nrow(game_management) + 1,] <- list(nrow(game_management) + 1, shortCollegeName(), loc_id,athletes, year(),month(),day())
    game_management %>% 
      readr::write_csv('Data_Management/Game_Management.csv', col_names = TRUE)
    
    #return data frame
    if(is.null(input$game_file))
    {
      return(NULL)
    }
    return (df)
  })
  
  # display a few rows of the fully rotated data
  output$transformed_content = renderTable({
    if(is.null(rotated_binded_df()))
    {
      return(NULL)
    }
    output$finished_df = renderText({"Ready to Download!"})
    head(rotated_binded_df())
    #meaning we are ready to download the data
  })
  
  # button to download data
  output$download <- downloadHandler(
    filename = function() {
      filename = paste(shortCollegeName(),year(),sep = "_")
      filename = paste(filename,month(),day(),sep = "-")
      paste(filename,'.csv', sep='')
    },
    content = function(file) {
      write.csv(rotated_binded_df(), file)
    }
  )
})