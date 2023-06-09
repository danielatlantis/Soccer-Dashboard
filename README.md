# Soccer-Dashboard

## Introduction

This was a project tasked to my data science team, where our client wanted a system to visualize player's paths on a virtual field and analyze different facets of the game such as their velocity, total disance travelled, and what we called "sprint bursts" which were sprints that took place above a specfic speed threshold and/or over a certain distance. This project consists of two RStudio applications: the main application, and the supplemental data application. The main application is a multi-feature tool designed to help visualize and analyze data from soccer games and includes tools that can be used to identify and analyze sprint bursts throughout the games for each player based on a number of criteria. The data application is designed to restructure the raw data files provided by the tracking software and format them to be compatible with the main application software. The formatted data can then be downloaded to the user’s system and stored appropriately for future use.

## Getting started

First, when importing new data into the system, open the R Shiny Data App. This can be done by opening the ui.R, server.R, or global.R files located in the “R Shiny Data App” folder in RStudio and running them. Prior to running the files, a few packages may need to be installed in RStudio (this will only need to be done once) and these packages can be found at the top of the global.R file. Once the application is open, a welcome page will be displayed with a more detailed step-by-step instruction manual on how to use the data application. The process for opening the main application is the same as the data application. Open up one of ui.R, server.R, or global.R located in the “RShiny App” folder in RStudio. Then ensure that all needed libraries are downloaded and press run to open the application.

## Example Images from Application

![Player Path](Documentation/player_paths.png)

![Sprint Bursts](Documentation/sprint_bursts.png)
