# TCX Data Viewer

## Overview

TCX Data Viewer is a Shiny application that allows users to upload and visualize data from Training Center XML (TCX) files. This app is particularly useful for athletes and fitness enthusiasts who want to analyze their workout data, including GPS tracks and heart rate information.

## Features

1. **File Upload**: Users can upload one or multiple TCX files.
2. **Interactive Map**: Displays GPS tracks of the uploaded activities on a Leaflet map.
3. **Heart Rate Visualization**: Shows a plot of heart rate over time for all uploaded activities.
4. **Activity Summary**: Provides key information about each uploaded file, including:
   - Start date and time
   - Duration
   - Maximum heart rate

## How to Use

1. Launch the application.
2. Use the file input in the sidebar to upload one or more TCX files.
3. The app will process the files and display:
   - A summary of each file's information in the sidebar
   - GPS tracks on the map in the main panel
   - A heart rate plot for all activities in the main panel

## Technical Details

This application is built using R and the following key libraries:
- shiny: For creating the web application
- leaflet: For rendering interactive maps
- xml2: For parsing TCX files
- lubridate: For handling date-time data
- ggplot2: For creating the heart rate plot

The app uses a color-coding system to distinguish between multiple uploaded files on both the map and the heart rate plot.

## Installation

To run this application locally:

1. Make sure you have R installed on your system.
2. Install the required packages:
   ```R
   install.packages(c("shiny", "leaflet", "xml2", "lubridate", "ggplot2"))
3. Save the provided R script as app.R.

Run the application in R:

shiny::runApp("path/to/app.R")
Notes
The application supports multiple file uploads, allowing for comparison between different activities.
Error handling is implemented to manage issues with file processing.
The map shows start and end points for each activity, along with the full GPS track.
The heart rate plot allows for easy comparison of heart rate data across multiple activities.

Or, you could just go [here](https://sayersr.github.io/2024_scgis_tcx_webr/) for the Shinylive version.