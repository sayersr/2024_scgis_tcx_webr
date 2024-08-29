library(shiny)
library(leaflet)
library(XML)
library(lubridate)
library(ggplot2)

# Define contrasting colors
CONTRASTING_COLORS <- c("#FF0000", "#00FF00", "#0000FF", "#FF00FF", "#00FFFF", "#FFFF00", "#800000", "#008000", "#000080", "#800080")

ui <- fluidPage(
  titlePanel("TCX Data Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("tcx_files", "Upload TCX file(s)", accept = ".tcx", multiple = TRUE),
      uiOutput("file_info")
    ),
    mainPanel(
      leafletOutput("map", height = "400px"),
      plotOutput("heart_rate_plot", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  tcx_data <- reactiveVal(list())
  
  observeEvent(input$tcx_files, {
    req(input$tcx_files)
    processed_data <- list()
    
    for (i in seq_along(input$tcx_files$datapath)) {
      file <- input$tcx_files$datapath[i]
      filename <- input$tcx_files$name[i]
      
      tryCatch({
        tcx <- xmlParse(file)
        
        # Extract trackpoints
        trackpoints <- xpathApply(tcx, "//ns:Trackpoint", namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"))
        
        # Extract data
        times <- as.POSIXct(xpathSApply(tcx, "//ns:Time", xmlValue, namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
        lats <- as.numeric(xpathSApply(tcx, "//ns:LatitudeDegrees", xmlValue, namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")))
        lons <- as.numeric(xpathSApply(tcx, "//ns:LongitudeDegrees", xmlValue, namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")))
        heart_rates <- as.numeric(xpathSApply(tcx, "//ns:HeartRateBpm/ns:Value", xmlValue, namespaces = c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")))
        
        # Calculate duration in seconds
        duration_seconds <- as.numeric(difftime(max(times), min(times), units = "secs"))
        
        processed_data[[filename]] <- list(
          times = times,
          lats = lats,
          lons = lons,
          heart_rates = heart_rates,
          start_time = min(times),
          duration = duration_seconds,
          max_hr = max(heart_rates),
          color = CONTRASTING_COLORS[(i - 1) %% length(CONTRASTING_COLORS) + 1]
        )
      }, error = function(e) {
        processed_data[[filename]] <- list(error = paste("Error processing TCX data:", e$message))
      })
    }
    
    tcx_data(processed_data)
  })
  
  output$file_info <- renderUI({
    data <- tcx_data()
    if (length(data) == 0) {
      return(p("Please upload TCX file(s)."))
    }
    
    info_html <- "<h3>Data Information:</h3>"
    for (filename in names(data)) {
      file_data <- data[[filename]]
      if ("error" %in% names(file_data)) {
        info_html <- paste0(info_html, sprintf("<p><strong>%s:</strong> Error: %s</p>", filename, file_data$error))
      } else {
        # Format duration as HH:MM:SS
        duration_formatted <- sprintf("%02d:%02d:%02d", 
                                      file_data$duration %/% 3600, 
                                      (file_data$duration %% 3600) %/% 60, 
                                      file_data$duration %% 60)
        
        info_html <- paste0(info_html, sprintf(
          "<p><strong>%s:</strong><br>Date: %s<br>Duration: %s<br>Maximum Heart Rate: %d bpm</p>",
          filename,
          format(file_data$start_time, "%Y-%m-%d %H:%M:%S"),
          duration_formatted,
          file_data$max_hr
        ))
      }
    }
    
    HTML(info_html)
  })
  
  output$map <- renderLeaflet({
    data <- tcx_data()
    if (length(data) == 0) {
      return(NULL)
    }
    
    m <- leaflet() %>% addTiles()
    
    for (filename in names(data)) {
      file_data <- data[[filename]]
      if (!"error" %in% names(file_data)) {
        m <- m %>%
          addPolylines(
            lat = file_data$lats,
            lng = file_data$lons,
            color = file_data$color,
            weight = 2.5,
            opacity = 0.8,
            popup = filename
          ) %>%
          addCircleMarkers(
            lat = file_data$lats[1],
            lng = file_data$lons[1],
            radius = 6,
            color = "green",
            fillOpacity = 1,
            popup = paste("Start -", filename)
          ) %>%
          addCircleMarkers(
            lat = file_data$lats[length(file_data$lats)],
            lng = file_data$lons[length(file_data$lons)],
            radius = 6,
            color = "red",
            fillOpacity = 1,
            popup = paste("End -", filename)
          )
      }
    }
    
    m
  })
  
  output$heart_rate_plot <- renderPlot({
    data <- tcx_data()
    if (length(data) == 0) {
      return(NULL)
    }
    
    plot_data <- data.frame()
    
    for (filename in names(data)) {
      file_data <- data[[filename]]
      if (!"error" %in% names(file_data)) {
        time_diff <- as.numeric(difftime(file_data$times, file_data$times[1], units = "secs"))
        plot_data <- rbind(plot_data, data.frame(
          time = time_diff,
          heart_rate = file_data$heart_rates,
          filename = filename,
          color = file_data$color
        ))
      }
    }
    
    ggplot(plot_data, aes(x = time / 60, y = heart_rate, color = filename)) +
      geom_line() +
      scale_color_manual(values = setNames(plot_data$color, plot_data$filename)) +
      labs(x = "Time (minutes)", y = "Heart Rate (bpm)", title = "Heart Rate Over Time") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)
