
# file_name <- "C:/Users/Tonio/Downloads/Afternoon_Ride (2).fit"
# file_name <- "biketrainr-master/data/01_04_2022_Evening_Ride.fit"
# records <- load_strava(file_name)
create_dygraph_data <- function(records){
  track_raw <- records
  # has_na <- is.na(track_raw) %>% sum
  # if(has_na) stop("NA in data")
  
  track_raw$lat <- track_raw$position_lat
  track_raw$lon <- track_raw$position_long
  if(!is.null(track_raw$heart_rate)) track_raw$heart_rate %<>% as.numeric()
  
  # todo: why raw?
  track <- track_raw
  track$distance %<>% round
  # todo could refactor here.
  
  if(is.null(track$enhanced_altitude)) track$enhanced_altitude <- track$altitude
  if(!is.null(track$enhanced_altitude)) track$enhanced_altitude %<>% round(digits = 1) # if there is no altitude - e.g. indoor training

  idx <- which("heart_rate" == names(track) | "enhanced_altitude" == names(track) | 
              "speed" == names(track) | "power" == names(track) | "cadence" == names(track))
  qxts <- xts(track[, idx], order.by = track$time)
  return(list(qxts = qxts, track_raw = track))
}

# First: Handover workouts - will only be handovered if there is an etry otherwise it would not be called
# Then render the selecter which workout should be shown. Default will be the first one.
# Then call the rest.

modServer <- function(id, workouts, global) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # print("inside module")
      # print("selected_workout")
      # print(selected_workout)
      # 
      # observe({
      #   global$selected_workout <- selected_workout  
      # })
      
      hr_max <- 190
      hr_lit <- c(124, 146, 174)
      
      ns <- NS(id)
      
      output$select_workout_for_leaflet <- renderUI({
        selectInput(ns("select_workout"), "Select Workout:", 1:length(workouts), selected = global$selected_workout)
      })
      
      #input$select_workout, 
      observeEvent(input$select_workout, { 
        sel <- as.numeric(input$select_workout)
        req(sel <= length(workouts))
        records <- workouts[[sel]]$records

        print(head(records))
        print(sum(is.na(records)))
        out <- create_dygraph_data(records)
        qxts <- out$qxts
        track_raw <- out$track_raw
        hr_max <- 190
        hr_lit <- c(124, 146, 174)
        
        n <- track_raw %>% nrow
        global$track <- track_raw
        global$track_raw <- track_raw
        global$qxts <- qxts
        global$keep_time <- rep(TRUE, n)
        global$heart_range <- 1:n
        global$trigger_dygraph_update <- rnorm(1)
        
      })
      
      # onevent("mousemove", "msvr", function(event) print(event$offsetX))
      
      observeEvent(input$trackmap_bounds, {
        if(Sys.time() > global$map_updated_at + 1){
          global$map_updated_at <- Sys.time()
        }
      })
      
      observeEvent(input$trackmap_bounds, {
        req(!is.null(input$trackmap_bounds))
        global$trackmap_bounds <- input$trackmap_bounds
      })
      
      # it has to have an update from graph window without having had a previous update from the map.
      # one way would be to enforce there is a time window (e.g. x seconds) in which another update is forbidden.
      # x has to be large enough to avoid having circular updates (leaflet and dygraph triggering each other), but
      # small enough to allow user to make fast updates. --> Trying with 1 second.
      observeEvent(input$dygraph_date_window, {
        global$dy_updated_at <- Sys.time()
      })
      
      
      observeEvent(global$dy_updated_at, {
        req(global$track_raw)
        dy_triggered <- global$dy_updated_at + 1 >= Sys.time()
        if(dy_triggered){
          req(global$sub_track$position_lat)
          # print(input$dygraph_date_window)
          from = req(input$dygraph_date_window[[1]])
          to = req(input$dygraph_date_window[[2]])
          from <- strptime(from, "%Y-%m-%dT%H:%M:%OSZ") + 2*3600
          to <- strptime(to, "%Y-%m-%dT%H:%M:%OSZ") + 2*3600
          
          start <- which.min(abs(global$track_raw$time - from))
          end <- which.min(abs(global$track_raw$time - to))
          global$heart_range <- start:end
          
          global$sub_track <- global$track_raw[global$heart_range, ]
          lats <- global$sub_track$position_lat %>% range
          longs <- global$sub_track$position_long %>% range
          leafletProxy("trackmap", session) %>%
            fitBounds(lng1 = longs[1], lng2 = longs[2], lat1 = lats[1], lat2 = lats[2])
          # global$dy_updated_at <- FALSE
        }else{
          print("njet")
        }
        
      })
      
      observeEvent(c(global$dy_updated_at, global$heart_range), {
        # req(global$dy_updated_at == TRUE)
        # print(global$dy_updated_at)
        
        # todo: this is doubled, see above?
        from = req(input$dygraph_date_window[[1]])
        to = req(input$dygraph_date_window[[2]])
        from <- strptime(from, "%Y-%m-%dT%H:%M:%OSZ") + 2*3600
        to <- strptime(to, "%Y-%m-%dT%H:%M:%OSZ") + 2*3600
        
        start <- which.min(abs(global$track_raw$time - from))
        end <- which.min(abs(global$track_raw$time - to))
        global$heart_range <- start:end
        
        is_subset <- length(global$track_raw$timestamp) != length(start:end)
        if(is_subset){
          global$sub_track <- global$track[global$heart_range, ]
        }else{
          global$sub_track <- NULL
        }
      })
      
      target_icon <- icons(
        iconUrl = "https://cdn.icon-icons.com/icons2/817/PNG/512/thefreeforty_target_icon-icons.com_66342.png",
        iconWidth = 20, iconHeight = 20
      )
      
      cycle_icon <- icons(
        iconUrl = "https://w7.pngwing.com/pngs/873/319/png-transparent-bicycle-safety-cycling-cyclist-icon-text-bicycle-logo.png",
        iconWidth = 20, iconHeight = 20
      )
      
      observeEvent(c(global$dy_updated_at, input$choose_seq), {
        
        global$keep_time
        req(input$choose_seq)
        req(global$track_raw)
        req(global$track_raw$lon)
        req(global$track_raw$lat)
        print("inside observeEvent dy_upd")
        
        if(!is.null(global$sub_seq)){
          global$sub_track <- global$track_raw[global$sub_seq[[input$choose_seq]], ]

          # global$dy_track <- track_raw[1:length(track_raw$timestamp) %in% global$sub_seq[[input$choose_seq]], ] might need this
        }
        just_sec_updated <- global$seq_updated_at + 1 >= Sys.time()
        
        leafletProxy("trackmap", session) %>%
          removeShape("init_poly") %>%
          removeShape("add_poly_base") %>%
          removeShape("add_poly_sub") %>%
          addPolylines(data = global$track, lng = ~lon, lat = ~lat, layerId = "add_poly_base")
        
        if(!is.null(global$sub_seq)){
          leafletProxy("trackmap", session) %>%
            addPolylines(data = global$sub_track, lng = ~lon, lat = ~lat, color = "green", layerId = "add_poly_sub")
        }
        
      })
      
      output$trackmap = renderLeaflet({
        # global$trigger_map_update
        global$track
        isolate({
          # print("global$dy_updated_at223")
          req(global$track$lon)
          req(global$track$lat)
          print("anyway")
          
          lon <- global$track$lon %>% range(na.rm = TRUE)
          lat <- global$track$lat %>% range(na.rm = TRUE)
          m <- leaflet() %>% addTiles()
          
          finish <- c(tail(global$track$lon, 1), tail(global$track$lat, 1))
          m %>% fitBounds(lon[1], lat[1], lon[2], lat[2]) %>%
            addPolylines(data = global$track, lng = ~lon, lat = ~lat, layerId = "init_poly") %>%
            addMarkers(lng = finish[1], lat = finish[2], icon = target_icon)
          
        })
      })
      
      observeEvent(input$mouse, {
        req(input$mouse)
        tr <- global$track[input$mouse$x, ]
        req(tr$position_long)
        req(tr$position_lat)
        leafletProxy("trackmap", session) %>%
          removeMarker(layerId = "current_pos") %>%
          addMarkers(lng = tr$position_long, lat = tr$position_lat, layerId = "current_pos", icon = cycle_icon)
        
      })
      
      output$create_seq <- renderUI({
        req(global$keep_time)
        ww <- global$keep_time %>% which
        end_first_seq <- which(diff(ww) != 1)
        
        if(length(end_first_seq)){
          start_sec_seq <- end_first_seq + 1
          first_seq <- ww[1:end_first_seq]
          second_seq <- ww[start_sec_seq:length(ww)]
          
          global$sub_seq <- list(
            Hinweg = first_seq,
            Rueckweg = second_seq,
            Beide = ww
          )
          
          radioButtons("choose_seq", "Choose sequences", c("Hinweg", "Rueckweg", "Beide"), selected = "Beide")
        }else{
          global$sub_seq <- NULL
        }
      })
      
      observe({
        input$choose_seq
        isolate({global$seq_updated_at <- Sys.time()})
      })
      
      observeEvent(c(global$trigger_dygraph_update, global$map_updated_at), {
        # print(global$map_updated_at)
        map_is_init <- !is.null(global$trackmap_bounds)
        req(map_is_init)
        req(global$track_raw$lat)
        req(global$track_raw$lon)
        
        global$trigger_dygraph_update
        # if update was less than a second ago - now is smaller/equal to then + 1
        manual_update <- global$map_updated_at +1 >= Sys.time()
        # print(global$map_updated_at)
        # print(Sys.time())
        # print(global$map_updated_at + 1 >= Sys.time())
        # print("manual_update")
        # print(manual_update)
        #   # req(manual_update == TRUE)
        isolate({
          # print("dss")
          bounds <- global$trackmap_bounds
          # print(bounds)
          global$keep_time <- global$track_raw$lat < bounds$north & global$track_raw$lat > bounds$south &
            global$track_raw$lon < bounds$east & global$track_raw$lon > bounds$west
        })
      })
      
      observe({
        input$choose_seq
        input$dygraph_date_window
        
        global$just_map_updated <- global$map_updated_at +1 >= Sys.time()
        # print("yeah")
        # print(global$map_updated_at + 1)
        # print(Sys.time())
        # print(global$just_map_updated)
        req(global$seq_updated_at)
        
        just_sec_updated <- global$seq_updated_at + 1 >= Sys.time()
        dy_triggered <- global$dy_updated_at + 1 >= Sys.time()
        if(global$just_map_updated & !dy_triggered){
          # print("need dygraph update")
          global$trigger_dygraph_update <- rnorm(1)
        }
      })
      
      observe({
        req(global$track_raw)
        global$dy_track <- global$track_raw[global$keep_time, ]
      })
      
      output$dygraphs <- renderUI({
        
        has_heart_rate <- !is.null(global$qxts$heart_rate)
        has_altitude <- !is.null(global$qxts$altitude)
        has_power <- !is.null(global$qxts$power)
        has_cadence <- !is.null(global$qxts$cadence)
        has_speed <- !is.null(global$qxts$speed)
        
        out <- tagList()
        if(has_power) out <- tagList(out, dygraphOutput(ns("dy_watt"), height = "100px"))
        if(has_altitude) out <- tagList(out, dygraphOutput(ns("dy_altitude"), height = "100px"))
        if(has_heart_rate) out <- tagList(out, dygraphOutput(ns("dy_heart_rate"), height = "100px"))
        if(has_cadence) out <- tagList(out, dygraphOutput(ns("dy_cadence"), height = "100px"))
        if(has_speed) out <- tagList(out, dygraphOutput(ns("dy_speed"), height = "100px"))
        
        out
        
      })
      
      observeEvent(global$qxts, {

        isolate({
          req(global$dy_track)
          print("inside render dygraph") 
          track <- global$dy_track
          track$speed %<>% as.numeric

          valueFormatter <- paste0("function(v, opts, seriesName, dygraph, row) {
              Shiny.onInputChange('", session$ns("mouse"),"', {'x': row, 'val': v})
            	return v;
            }")

          range <- c(0, max(c(global$qxts$heart_rate, global$qxts$power)))
          has_heart_rate <- !is.null(global$qxts$heart_rate)
          
          if(has_heart_rate){
            
            # %>% dyRangeSelector()
            
            output$dy_heart_rate <- renderDygraph({
              graph <- dygraph(global$qxts$heart_rate)
              graph %<>%
                dyAxis("y", label = "Heart rate", valueFormatter = valueFormatter) %>% 
                dyCrosshair(direction = "vertical") %>% 
                dySeries("heart_rate", axis = 'y',  fillGraph = TRUE, color = "red", strokeWidth = 1) %>% 
                dyLimit(hr_lit[1], color = "blue") %>%
                dyLimit(hr_lit[2], color = "blue") %>%
                dyLimit(hr_lit[3], color = "blue") 
            })
            
            # do not need it again: 
            valueFormatter <- NULL
          }

          if(!is.null(global$qxts$speed)){
            speed <- global$qxts$speed
            
            output$dy_speed <- renderDygraph({
              graph <- dygraph(global$qxts$speed)
              graph %<>% dyAxis("y", label = "speed", valueFormatter = valueFormatter, valueRange = c(0, max(speed))) %>%
                dyCrosshair(direction = "vertical") %>% 
                dySeries("speed", axis = 'y',  fillGraph = TRUE, color = "yellow", strokeWidth = 1)
            })
          }
          
          if(!is.null(global$qxts$cadence)){
            cadence <- global$qxts$cadence
            
            output$dy_cadence <- renderDygraph({
              graph <- dygraph(global$qxts$cadence)
              graph %<>% dyAxis("y", label = "cadence", valueFormatter = valueFormatter, valueRange = c(0, max(max(cadence), 120))) %>%
                dyCrosshair(direction = "vertical") %>% 
                dySeries("cadence", axis = 'y',  fillGraph = TRUE, color = "purple", strokeWidth = 1)
            })
          }
          
                    
          if(!is.null(global$qxts$enhanced_altitude)){
            altitude <- global$qxts$enhanced_altitude
            range <- c(min(altitude), max(altitude, 300))
            
            output$dy_altitude <- renderDygraph({
              graph <- dygraph(global$qxts$enhanced_altitude)
              graph %<>% dyAxis("y", label = "altitude", valueFormatter = valueFormatter, valueRange = range) %>%
                dyCrosshair(direction = "vertical") %>% 
                dySeries("enhanced_altitude", axis = 'y',  fillGraph = TRUE, color = "brown", strokeWidth = 1)
            })
          }
          
          if(!is.null(global$qxts$power)){
            output$dy_watt <- renderDygraph({
              graph <- dygraph(global$qxts$power)
              graph %<>% dyAxis("y", label = "watt") %>%
                dyCrosshair(direction = "vertical") %>% 
                dySeries("power", axis = 'y',  fillGraph = TRUE, color = "blue", strokeWidth = 1) %>% 
                dyLimit(140, color = "blue") %>%
                dyLimit(215, color = "blue")
              #   dySeries("watt", axis = ('y2'), color = "black", strokeWidth = 1, strokePattern = "dashed")
            })
          }

        })
        
      })
      
      output$averages <- renderUI({
        req(global$heart_range)
        track_subset <- global$track_raw[global$heart_range, ]
        
        has_watt <- !is.null(track_subset$power)
        has_heart_rate <- !is.null(track_subset$heart_rate)
        has_speed <- !is.null(track_subset$speed) # should be included, but for stability of the app
        
        average_watt <- NULL
        average_heart_rate <- NULL
        average_speed <- NULL        
        if(has_watt) average_watt <- div(strong("Average power: "), track_subset$power %>% mean %>% round, "watts", inline = TRUE)
        if(has_heart_rate) average_heart_rate <- div(strong("Average heart rate: "), track_subset$heart_rate %>% mean %>% round, "bpm", inline = TRUE)
        if(has_speed) average_speed <- div(strong("Average speed: "), track_subset$speed %>% mean %>% round(digits = 2), "km/h", inline = TRUE)
        
        tagList(
          average_watt,
          average_heart_rate,
          average_speed
        )

      })
      
      output$from <- renderText({
        date_time <- strptime(req(input$dygraph_date_window[[1]]), "%Y-%m-%dT%H:%M:%OSZ") + 2*3600
        strftime(date_time, "%H:%M:%OS")
      })
      
      output$to <- renderText({
        date_time <- strptime(req(input$dygraph_date_window[[2]]), "%Y-%m-%dT%H:%M:%OSZ") + 2*3600
        strftime(date_time, "%H:%M:%OS")
      })
      
      output$clicked <- renderText({
        strftime(req(input$dygraph_date_window[[1]]))
      })
      
      output$point <- renderText({
        paste0('X = ', strftime(req(input$dygraph_click$x_closest_point)),
               '; Y = ', req(input$dygraph_click$y_closest_point))
      })
      
    })
}


modUI <- function(id, label = "CSV file") {
  
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        div(strong("From: "), textOutput(ns("from"), inline = TRUE)),
        div(strong("To: "), textOutput(ns("to"), inline = TRUE)),
        div(strong("Date: "), textOutput(ns("clicked"), inline = TRUE)),
        uiOutput(ns("averages")),
        br(),
        uiOutput(ns("create_seq")),
        br(),
        helpText("Click and drag to zoom in (double click to zoom back out).")
      ),
      mainPanel(
        uiOutput(ns("select_workout_for_leaflet")),
        uiOutput(ns("dygraphs")),
        leafletOutput(ns('trackmap'))
      )
    )
  )
}


