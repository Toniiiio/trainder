create_dygraph_data <- function(records){
  track_raw <- records
  track_raw$lat <- track_raw$position_lat
  track_raw$lon <- track_raw$position_long
  track_raw$heart_rate %<>% as.numeric()
  track <- track_raw
  track$distance %<>% round
  if(is.null(track$enhanced_altitude)) track$enhanced_altitude <- track$altitude
  idx <- which("heart_rate" == names(track) | "enhanced_altitude" == names(track) | 
              "speed" == names(track) | "power" == names(track))
  qxts <- xts(track[, idx], order.by = track$time)
  has_na <- is.na(track_raw) %>% sum
  if(has_na) stop("NA in data")
  return(list(qxts = qxts, track_raw = track_raw))
}

# First: Handover workouts - will only be handovered if there is an etry otherwise it would not be called
# Then render the selecter which workout should be shown. Default will be the first one.
# Then call the rest.

modServer <- function(id, workouts) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      hr_max <- 190
      hr_lit <- c(124, 146, 162)
      
      ns <- NS(id)
      
      output$select_workout_for_leaflet <- renderUI({
        selectInput(ns("select_workout"), "Select Workout:", 1:length(workouts))
      })
      
      observeEvent(input$select_workout, {
        global$workout_nr <- as.numeric(input$select_workout)
      })
      
      global <- reactiveValues(track = NULL, workout_nr = 1, track_raw = NULL, keep_time = NULL, heart_range = NULL,
                               map_updated_at = Sys.time() + 3,
                               dy_updated_at = Sys.time() + 3, sub_seq = NULL, just_map_updated = FALSE,
                               seq_updated_at = Sys.time() + 3)
      
      #input$select_workout, 
      observeEvent(global$workout_nr, { 
        print("workouts")
        print(length(workouts))
        print("global$workout_nr")
        print(global$workout_nr)
        req(global$workout_nr <= length(workouts))
        records <- workouts[[global$workout_nr]]$records
        print("head(records)")
        print(head(records))
        
        out <- create_dygraph_data(records)
        qxts <- out$qxts
        track_raw <- out$track_raw
        hr_max <- 190
        hr_lit <- c(124, 146, 162)
        
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
        req(global$track_raw)
        req(global$track_raw$lon)
        req(global$track_raw$lat)
        
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
          print(global$keep_time %>% sum)
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
      
      
      output$dygraph <- renderDygraph({
        
        global$trigger_dygraph_update
        # print("keep time")
        isolate({
          track <- global$dy_track
          # print(str(track))
          track$speed %<>% as.numeric
          # df <- data.frame(x = track$time, y = track$speed) # toberemoved
          
          print(head(global$qxts))
          graph <- dygraph(global$qxts, main = "Bike ride")
          

          range <- c(0, max(c(qxts$heart_rate, qxts$power)))
          if(!is.null(global$qxts$heart_rate)){
            graph %<>%
              dyAxis("y", label = "Heart rate", valueRange = range, 
                     valueFormatter = paste0("function(v, opts, seriesName, dygraph, row) {
              Shiny.onInputChange('", session$ns("mouse"),"', {'x': row, 'val': v})
            	return v;
            }")) %>% 
              dyLimit(hr_lit[1], color = "blue") %>%
              dyLimit(hr_lit[2], color = "blue") %>%
              dyLimit(hr_lit[3], color = "blue") %>%
              dyRangeSelector()
          }
          
          print("global$qxts$enhanced_altitude")
          print(head(global$qxts$enhanced_altitude))
          if(!is.null(global$qxts$enhanced_altitude)){
            print("xx")
            graph %<>%
              # dyAxis("y2", label = "altitude", independentTicks = TRUE) %>%
              dySeries("enhanced_altitude", axis = ('y2'), fillGraph = TRUE, color = "grey", strokeWidth = 1)
          }
          
          if(!is.null(global$qxts$watt)){
            graph %<>%
              dyAxis("y2", label = "watt", independentTicks = TRUE, valueRange = range) %>%
              dySeries("watt", axis = ('y2'), color = "black", strokeWidth = 1, strokePattern = "dashed")
          }
          
          graph

        })
        
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
        br(),
        uiOutput(ns("create_seq")),
        br(),
        helpText("Click and drag to zoom in (double click to zoom back out).")
      ),
      mainPanel(
        uiOutput(ns("select_workout_for_leaflet")),
        div(id = "msvr", dygraphOutput(ns("dygraph"))),
        leafletOutput(ns('trackmap'))
      )
    )
  )
}

