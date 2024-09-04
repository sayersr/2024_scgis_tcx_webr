library(shiny)
library(leaflet)
library(xml2)
library(lubridate)
library(plotly)
library(dplyr)
library(tidyr)

# Define colorblind-friendly colors
COLORBLIND_FRIENDLY_COLORS <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

convert_tcx <- function(tcx_content) {
  tcx <- read_xml(tcx_content)
  ns <- c(ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")
  
  times <- as.POSIXct(xml_text(xml_find_all(tcx, "//ns:Time", ns = ns)), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  lats <- as.numeric(xml_text(xml_find_all(tcx, "//ns:LatitudeDegrees", ns = ns)))
  lons <- as.numeric(xml_text(xml_find_all(tcx, "//ns:LongitudeDegrees", ns = ns)))
  heart_rates <- as.numeric(xml_text(xml_find_all(tcx, "//ns:HeartRateBpm/ns:Value", ns = ns)))
  
  points <- data.frame(lat = lats, lon = lons)
  start_time <- min(times)
  duration <- as.numeric(difftime(max(times), min(times), units = "secs"))
  
  list(
    points = points,
    heart_rates = heart_rates,
    start_time = start_time,
    duration = duration,
    timestamps = times
  )
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet@1.7.1/dist/leaflet.css"),
    tags$script(src = "https://unpkg.com/leaflet@1.7.1/dist/leaflet.js"),
    tags$script(src = "https://cdn.plot.ly/plotly-latest.min.js")
  ),
  titlePanel("webR TCX Demo"),
  sidebarLayout(
    sidebarPanel(
      fileInput("tcx_files", "Upload TCX file(s)", accept = ".tcx", multiple = TRUE),
      uiOutput("file_info")
    ),
    mainPanel(
      div(
        style = "display: flex; flex-direction: column; gap: 10px;",
        leafletOutput("map", height = "400px"),
        uiOutput("heart_rate_plot_ui")
      ),
      verbatimTextOutput("debug_info")
    )
  )
)

server <- function(input, output, session) {
  tcx_data <- reactiveVal(list())
  timeline_data <- reactiveVal(data.frame())
  
  observeEvent(input$tcx_files, {
    req(input$tcx_files)
    processed_data <- list()
    all_data <- list()
    
    for (i in seq_along(input$tcx_files$datapath)) {
      file <- input$tcx_files$datapath[i]
      filename <- input$tcx_files$name[i]
      
      tryCatch({
        tcx_content <- readLines(file, warn = FALSE)
        tcx_content <- paste(tcx_content, collapse = "\n")
        result <- convert_tcx(tcx_content)
        
        color <- COLORBLIND_FRIENDLY_COLORS[(i - 1) %% length(COLORBLIND_FRIENDLY_COLORS) + 1]
        processed_data[[filename]] <- c(
          result,
          list(
            max_hr = max(result$heart_rates, na.rm = TRUE),
            color = color
          )
        )
        
        df <- data.frame(
          timestamp = result$timestamps,
          heart_rate = result$heart_rates,
          lat = result$points$lat,
          lon = result$points$lon,
          file = filename,
          color = color
        )
        df$elapsed_time <- as.numeric(difftime(df$timestamp, min(df$timestamp), units = "secs")) / 60
        all_data[[i]] <- df
      }, error = function(e) {
        processed_data[[filename]] <- list(error = paste("Error processing TCX data:", e$message))
      })
    }
    
    tcx_data(processed_data)
    
    if (length(all_data) > 0) {
      combined_data <- do.call(rbind, all_data)
      timeline_data(combined_data)
    }
  })
  
  output$file_info <- renderUI({
    data <- tcx_data()
    if (length(data) == 0) {
      return(p("Please upload TCX file(s)."))
    }
    
    info_html <- "<h3>Run Data:</h3>"
    for (filename in names(data)) {
      file_data <- data[[filename]]
      if ("error" %in% names(file_data)) {
        info_html <- paste0(info_html, sprintf("<p><strong>%s:</strong> Error: %s</p>", filename, file_data$error))
      } else {
        duration_formatted <- sprintf("%02d:%02d:%02d", 
                                      file_data$duration %/% 3600, 
                                      (file_data$duration %% 3600) %/% 60, 
                                      file_data$duration %% 60)
        
        info_html <- paste0(info_html, sprintf(
          "<p><strong>%s:</strong><br>Date: %s<br>Duration: %s<br>Max Heart Rate: %d bpm</p>",
          filename,
          format(file_data$start_time, "%Y-%m-%d"),
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
    
    all_points <- do.call(rbind, lapply(data, function(x) x$points))
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Change to a grayscale basemap
      fitBounds(
        lng1 = min(all_points$lon, na.rm = TRUE),
        lat1 = min(all_points$lat, na.rm = TRUE),
        lng2 = max(all_points$lon, na.rm = TRUE),
        lat2 = max(all_points$lat, na.rm = TRUE)
      )
    
    for (filename in names(data)) {
      file_data <- data[[filename]]
      if (!"error" %in% names(file_data)) {
        m <- m %>%
          addPolylines(
            lat = file_data$points$lat,
            lng = file_data$points$lon,
            color = file_data$color,
            weight = 3,
            opacity = 0.8,
            popup = filename
          ) %>%
          addCircleMarkers(
            lat = file_data$points$lat[1],
            lng = file_data$points$lon[1],
            radius = 6,
            color = "black",
            fillColor = file_data$color,
            fillOpacity = 1,
            popup = paste("Start -", filename)
          ) %>%
          addCircleMarkers(
            lat = file_data$points$lat[nrow(file_data$points)],
            lng = file_data$points$lon[nrow(file_data$points)],
            radius = 6,
            color = "black",
            fillColor = file_data$color,
            fillOpacity = 1,
            popup = paste("End -", filename)
          )
      }
    }
    
    m
  })
  
  output$heart_rate_plot_ui <- renderUI({
    if (nrow(timeline_data()) > 0) {
      plotlyOutput("heart_rate_plot", height = "300px")
    }
  })
  
  output$heart_rate_plot <- renderPlotly({
    data_info <- timeline_data()
    req(nrow(data_info) > 0)
    
    plot_data <- plot_ly()
    
    for (file_name in unique(data_info$file)) {
      file_data <- data_info %>% 
        filter(file == file_name) %>% 
        drop_na(heart_rate, elapsed_time)
      
      if (nrow(file_data) > 0) {
        plot_data <- plot_data %>% add_trace(
          x = ~elapsed_time,
          y = ~heart_rate,
          type = 'scatter',
          mode = 'lines',
          name = file_name,
          line = list(color = file_data$color[1]),
          hoverinfo = 'text',
          text = ~sprintf("File: %s<br>Time: %.2f min<br>Heart Rate: %d bpm", file, elapsed_time, heart_rate),
          data = file_data
        )
      }
    }
    
    max_time <- max(data_info$elapsed_time, na.rm = TRUE)
    min_hr <- min(data_info$heart_rate, na.rm = TRUE)
    max_hr <- max(data_info$heart_rate, na.rm = TRUE)
    
    plot_data %>% 
      layout(
        title = "Heart Rate Over Time",
        xaxis = list(
          title = "Time Elapsed (minutes)",
          range = c(0, max_time)
        ),
        yaxis = list(
          title = "Heart Rate (bpm)",
          range = c(min_hr * 0.9, max_hr * 1.1)
        ),
        hovermode = "closest",
        legend = list(orientation = "h", y = 1.02, yanchor = "bottom", x = 1, xanchor = "right")
      ) %>%
      event_register("plotly_hover") %>%
      config(displayModeBar = FALSE)
  })
  
  observeEvent(event_data("plotly_hover"), {
    hover_data <- event_data("plotly_hover")
    
    if (!is.null(hover_data) && !is.null(hover_data$x)) {
      hover_time <- hover_data$x
      data_info <- timeline_data()
      
      if (nrow(data_info) > 0) {
        tryCatch({
          current_data <- data_info %>%
            group_by(file) %>%
            filter(abs(elapsed_time - hover_time) == min(abs(elapsed_time - hover_time))) %>%
            slice(1) %>%
            ungroup()
          
          if (nrow(current_data) > 0) {
            leafletProxy("map") %>%
              clearMarkers() %>%
              addCircleMarkers(
                data = current_data,
                lat = ~lat,
                lng = ~lon,
                radius = 8,
                color = ~color,
                fillColor = ~color,
                fillOpacity = 0.8,
                popup = ~sprintf("File: %s<br>Heart Rate: %d bpm<br>Time: %.2f min", file, heart_rate, elapsed_time)
              )
          }
        }, error = function(e) {
          print(paste("Error updating map:", e$message))
        })
      }
    }
  })
  
  output$debug_info <- renderPrint({
    data_info <- timeline_data()
    if (nrow(data_info) == 0) {
      return("No data available for debugging")
    }
    
    cat("Total rows:", nrow(data_info), "\n")
    cat("Columns:", paste(colnames(data_info), collapse = ", "), "\n")
    cat("Heart rate range:", min(data_info$heart_rate, na.rm = TRUE), "-", max(data_info$heart_rate, na.rm = TRUE), "bpm\n")
    cat("Time range:", min(data_info$elapsed_time, na.rm = TRUE), "-", max(data_info$elapsed_time, na.rm = TRUE), "minutes\n")
    
    cat("\nSample data (first 5 rows):\n")
    print(head(data_info, 5))
  })
}

shinyApp(ui = ui, server = server)