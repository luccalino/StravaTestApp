# Loading packages (install if not yet installed)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, osmdata, dplyr, ggplot2, sf, raster, rgdal, yaml, 
               httr, jsonlite, Thermimage, shinycssloaders, shinybusy, rStrava)

# Construct UI
ui <- fluidPage(
  
  titlePanel("Strava Test"),
  
  sidebarPanel(
    tags$div(class="header", checked = NA,
             tags$p('Hello, this is a very simple test app')
    ),
    br(),
    textInput("app_name", "Application name", value = "", width = NULL, placeholder = NULL),
    textInput("id", "Client ID", value = "", width = NULL, placeholder = NULL),
    textInput("secret", "Client secret", value = "", width = NULL, placeholder = NULL),
    textInput("place", "Place", value = "Adelboden, Switzerland", width = NULL, placeholder = NULL),
    actionButton("gd","Get data & visualize!", icon("arrow-circle-down"), 
                 style = "color: #fff; background-color: #fc4c02; border-color: #2e6da4")
  ),
  
  mainPanel(
    withSpinner(plotOutput(outputId = 'plot1', width = 250, height = 250),
                color = "#fc4c02",
                type = 7),
  )
  
)

# Construct server
server <- function(input, output) {
  
  observeEvent(input$gd, {
    show_modal_gif(
      src = "https://media.giphy.com/media/kUTME7ABmhYg5J3psM/giphy.gif",
      text = "Getting activity data. This can take a while..."
    )
    
    # Create the authentication token
    stoken <- httr::config(token = strava_oauth(input$app_name, 
                                                input$id, 
                                                input$secret, 
                                                app_scope = "activity:read_all"))

    # Get activities
    my_acts <- get_activity_list(stoken)
    data <- as.data.frame(compile_activities(my_acts)) 
    
    # Subset only activities with coordinates
    data <- subset(data, !is.na(start_latlng1) | 
                     !is.na(start_latlng2))

    # Convert coordinates from WGS 84 to LV05  
    coordinates(data) <- c("start_latlng2", "start_latlng1")
    proj4string(data) <- CRS("+init=epsg:4326") # WGS 84
    CRS.new <- CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
    data <- spTransform(data, CRS.new)
    data <- as.data.frame(data)
    
    # Define region (lat/lon as polygone) 
    poly_bb <- getbb(place_name = input$place)
    poly_bb_flipped <- poly_bb
    poly_bb_flipped[2,1] <- poly_bb[1,2]
    poly_bb_flipped[1,2] <- poly_bb[2,1]
    colnames(poly_bb_flipped) <- c("x","y")
    rownames(poly_bb_flipped) <- c("min","max")
    poly_bb_flipped <- data.frame(poly_bb_flipped)
    coordinates(poly_bb_flipped) <- c("x", "y")
    proj4string(poly_bb_flipped) <- CRS("+init=epsg:4326") 
    poly_bb_flipped <- spTransform(poly_bb_flipped, CRS.new)
    poly_bb_flipped <- as.matrix(as.data.frame(poly_bb_flipped))
    poly_bb_flipped[1,1] <- poly_bb_flipped[1,1] # X dimension
    poly_bb_flipped[2,1] <- poly_bb_flipped[2,1] # X dimension
    poly_bb_flipped[1,2] <- poly_bb_flipped[1,2] # Y dimension
    poly_bb_flipped[2,2] <- poly_bb_flipped[2,2] # Y dimension
    poly_bb_4326 <- data.frame(poly_bb_flipped)
    coordinates(poly_bb_4326) <- c("x", "y")
    proj4string(poly_bb_4326) <- CRS.new 
    poly_bb_4326 <- spTransform(poly_bb_4326, "+proj=longlat +datum=WGS84 +no_defs")
    poly_bb_4326 <- as.matrix(as.data.frame(poly_bb_4326))
    
    # Subset only data within region (defined above)
    data <- subset(data, start_latlng2 >= poly_bb_flipped[1,1] & 
                     start_latlng2 <= poly_bb_flipped[2,1] &
                     start_latlng1 >= poly_bb_flipped[1,2] & 
                     start_latlng1 <= poly_bb_flipped[2,2])
    
    # Subset rides and runs
    data <- subset(data, type == "Run" | 
                     type == "Ride")
    
    # Get polylines 
    map_list <- list()
    
    for (i in 1:nrow(data)) {
      this_activity <- get_activity(id = data$id[i], stoken = stoken)
      this_polyline <- this_activity$map$summary_polyline
      decoded_polyline <- gepaf::decodePolyline(this_polyline)
      decoded_polyline$id <- this_activity$id
      map_list[[i]] <- decoded_polyline
      print(i) # Print progress in console
    }
    
    # Stack geocoded data
    polyline_data <- do.call(rbind, map_list)
    
    # Change projection
    coordinates(polyline_data) <- c("lon", "lat")
    proj4string(polyline_data) <- CRS("+init=epsg:4326")
    polyline_data <- spTransform(polyline_data, CRS.new)
    polyline_data <- as.data.frame(polyline_data)
    
    remove_modal_gif()
    
    show_modal_gif(
      src = "https://media.giphy.com/media/kUTME7ABmhYg5J3psM/giphy.gif",
      text = "Activity data check.\nGetting features. This can take a while..."
    )
    
    remove_modal_gif()
    
    output$plot1 <- renderPlot(width = function() 750, 
                               height = function() 1000, 
                               res = 95, 
        
        {
        
        ## Plot dimension
        xdim <- c(poly_bb_flipped[1,1], poly_bb_flipped[2,1])
        ydim <- c(poly_bb_flipped[1,2], poly_bb_flipped[2,2])
        
        ## Plot
        p <- ggplot() +
          geom_path(data = polyline_data, aes(x = lon, y = lat, group = id), 
                    color = "red3", alpha = 0.5, size = 0.175, lineend = "round", show.legend = FALSE) +
          coord_sf(xlim = xdim, ylim = ydim) + 
          theme_void()
        
        p
        
        })
  
    })

  }

# Bring user interface and server together
shinyApp(ui = ui, server = server)