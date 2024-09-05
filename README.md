# TCX Data Viewer

## Overview

TCX Data Viewer is a Shiny application that allows users to upload and visualize data from Training Center XML (TCX) files. This app is particularly useful for athletes and fitness enthusiasts who want to analyze their workout data, including GPS tracks and heart rate information.

## Features

1. **File Upload**: Users can upload one or multiple TCX files.
2. **Interactive Map**: Displays GPS tracks of the uploaded activities on a Leaflet map.
3. **Heart Rate Visualization**: Shows an interactive plot of heart rate over time for all uploaded activities.
4. **Activity Summary**: Provides key information about each uploaded file, including:
   - Start date and time
   - Duration
   - Maximum heart rate
5. **Interactive Data Exploration**: Hovering over the heart rate plot updates the map with the corresponding location.

## How to Use

1. Launch the application.
2. Use the file input in the sidebar to upload one or more TCX files.
3. The app will process the files and display:
   - A summary of each file's information in the sidebar
   - GPS tracks on the map in the main panel
   - An interactive heart rate plot for all activities in the main panel
4. Hover over the heart rate plot to see the corresponding location on the map.

## Technical Details

This application is built using R and the following key libraries:
- shiny: For creating the web application
- leaflet: For rendering interactive maps
- xml2: For parsing TCX files
- lubridate: For handling date-time data
- plotly: For creating the interactive heart rate plot
- dplyr and tidyr: For data manipulation

The app uses a colorblind-friendly color-coding system to distinguish between multiple uploaded files on both the map and the heart rate plot.

## Installation

To run this application locally:

1. Make sure you have R installed on your system.
2. Install the required packages:
   ```R
   install.packages(c("shiny", "leaflet", "xml2", "lubridate", "plotly", "dplyr", "tidyr"))
   ```
3. Save the provided R script as `app.R`.
4. Run the application in R:
   ```R
   shiny::runApp("path/to/app.R")
   ```

## Notes

- The application supports multiple file uploads, allowing for comparison between different activities.
- Error handling is implemented to manage issues with file processing.
- The map shows start and end points for each activity, along with the full GPS track.
- The heart rate plot allows for easy comparison of heart rate data across multiple activities.
- Hovering over the heart rate plot updates the map with the corresponding location, enabling interactive exploration of the data.
- A debug output is available, showing details about the processed data.

## Shinylive Version

For a web-based version of this application, visit [https://sayersr.github.io/2024_scgis_tcx_webr/](https://sayersr.github.io/2024_scgis_tcx_webr/). This version uses Shinylive to run the R Shiny application directly in your web browser.