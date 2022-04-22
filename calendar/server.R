credentials <- data.frame(
  user = c("shiny", "shinymanager"), # mandatory
  password = c("shiny", "12345"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

data <- data.frame(
  calendarId = numeric(),
  title = character(),
  body = character(),
  recurrenceRule = character(),
  start = character(),
  end = character(),
  category = character(),
  location = character(),
  bgColor = character(),
  color = character(),
  borderColor = character(),
  date_diff = character()
)

shinyInput <- function(FUN, len, ids, ...) {
  inputs <- character(len)
  for (nr in seq_len(len)) {
    inputs[nr] <- as.character(FUN(ids[nr], ...))
  }
  inputs
}


options(scipen = 999)

server <- function(input, output, session) {
  
  global <- reactiveValues(data = data, calendar_updated = TRUE, current_date = Sys.Date(), sportler = NULL)
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  ############### ADD TEMPLATE
  
  observeEvent(input$choose_template, {
    
    showModal(
      modalDialog(
        selectInput("template", label = "Choose template:", choices = c("week_vo2_regular", "month_vo2_block")),
        dateInput("template_start_date", "Start date:", value = Sys.Date(), min = Sys.Date()),
        h4("Description:"), br(), 
        h6("Target: Vo2, Ratio: 90/10, Dauer: 1 Woche"),
        footer = tagList(
          actionButton('add_template', 'Add'),
          modalButton('cancel')
        )
      )  
    )
  })
  
  observeEvent(input$add_template, {
    removeModal()
    
    name <- paste0("template_", input$template)
    data <- get(name)
    template_start_date <- input$template_start_date
    data$start <- template_start_date + data$date_diff
    data$end <- template_start_date + data$date_diff
    print("go")
    for(nr in seq(nrow(data))){
      cal_proxy_add("my_calendar", list(start = data[nr, ]$start, end = data[nr, ]$end, 
                                        isAllDay = FALSE, category = "time", title = data[nr, ]$title))
    }
    global$data <- rbind(data, global$data)
  })
  
  ################ TABSET PANEL: WORKOUT TABLE
  
  df <- reactiveValues(data = data.frame(
    
    Name = c('Dilbert', 'Alice', 'Wally', 'Ashok', 'Dogbert'),
    Motivation = c(62, 73, 3, 99, 52),
    Actions = shinyInput(actionButton, 5, 'button_', label = "Fire", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
    stringsAsFactors = FALSE,
    row.names = 1:5
  ))
  
  output$workout_details <- renderDataTable({
    print("renderDataTable")
    
    df <- data.frame(cyclist1()$workout_details)
    
    req(df)
    req(nrow(df) > 0)

    print("df")    
    print(df)

    df$Sport <- "Radfahrt"
    df$TSS <- round(df$TSS)
    df$IF <- round(df$IF, 2)
    df$NP <- round(df$NP)
    df$weekday <- weekdays(df$date)
    df$delete <- shinyInput(actionButton, nrow(df), paste0('del_', df$id), label = "Delete", onclick = 'Shiny.onInputChange(\"delete_workout\",  {id: this.id, random: Math.random()})')
    df$edit <- shinyInput(actionButton, nrow(df), 'button_', label = "Edit", onclick = 'Shiny.onInputChange(\"edit_workout\",  this.id)')
    df$link <- shinyInput(actionButton, nrow(df), 'button_', label = "View", onclick = 'Shiny.onInputChange(\"view_workout\",  this.id)')
    df$km <- round(df$distance, 2)
    df$altitude <- round(df$altitude)
    df$title <- "Bike ride"
    df[, c("id", "title", "weekday", "date", "duration", "km", "altitude", "TSS", "IF", "NP", "delete", "edit", "link")]
  }, options = list(
    pageLength = 10
  ), escape = FALSE)
  
  
  observeEvent(input$delete_workout, {
    id_raw <- input$delete_workout$id
    print("id_raw")
    print(id_raw)
    id <- gsub(pattern = "del_", replacement = "", id_raw)
    print("idd")
    print(id)
    cyclist1()$del_wd_entries(id)
    #cal_proxy_delete("my_calendar", input$my_calendar_delete)
    # print(cyclist1()$workout_details)
  })
  
  observeEvent(res_auth$user, {
    
    req(res_auth$user)
    global$user_name <- res_auth$user
    global$is_admin <- res_auth$admin
    
    file_name <- paste0(global$user_name, ".RData")
    user_data_files <- list.files("user_data")
    
    has_file <- file_name %in% user_data_files
    print(user_data_files)
    
    source("sportler.R")
    if(has_file){
      
      global$sportler = readRDS(file = paste0("user_data/", global$user_name, ".RData"))
      
    }else{
      
      global$sportler <- list(name = global$user_name)
      # cyclist1() <- cyclist1$new()
      saveRDS(object = global$sportler, file = paste0("user_data/", global$user_name, ".RData"))
      
    }
    
  })
    
  cyclist1 <- cyclist$new()$reactive()
  
  observeEvent(input$file1, {
    if(!is.null(input$file1)){
      cyclist1()$set_file_names(input$file1$datapath)
      # path <- "biketrainr-master/data/"
      # sportler$cyclist1$file_names <- file.path(path, list.files(path))
      cyclist1()$upload_workouts()
      print("upload_Workouts works")
      print(cyclist1()$workout_details)
      file_name <- paste0("user_data/", global$user_name, ".RData")
      saveRDS(object = global$sportler, file = file_name)
      
      # file_name <- "user_data/shiny.RData"
      # xx <- readRDS(file = file_name)
      # print(xx$cyclist1$workout_details)
    }
     
  })
  
  # lets app crash in combination with shinymanager - authentication???
  # session$onSessionEnded(function() {
      # saveRDS(object = global$sportler, file = paste0("user_data/", global$user_name, ".RData"))
  #   stopApp()
  # })
  
  session$onFlushed( function(){
    shinyjs::runjs(js_current_date)
  }, once = TRUE)
  

  observe({
    
    clicked_text <- input$clicked_data$val
    print(input$clicked_data)
    req(clicked_text)
    
    global$click_date <- clicked_text %>% 
      gsub(pattern = " |\n", replacement = "") %>% 
      as.numeric
    
    dataModal <- function(failed = FALSE){
      modalDialog(
        div(style = ";background-color: #e9f5f8;",
            
            tags$h2('Choose your exercise:'),
            radioButtons("sport_type", "Choose one:", inline = TRUE,
                         choiceNames = list(
                           icon("bicycle"), 
                           icon("running"), 
                           icon("skiing-nordic"),
                           icon("swimming"),
                           icon("dumbbell")
                         ),
                         choiceValues = list(
                           "bike", "run", "ski", "swim", "dumbbell"
                         )
            ),
            selectInput("template", "Choose from template:",
                        c("4_4min_HIT", "3_13_30_15_HIT", "2h_LIT")),
            
            output$das <- renderUI({
              
              sel = gen_session_details(input$template)
              
              tagList(
                textInput('title', 'Title', value = sel$title),
                selectInput("session_type", "Choose session type:",
                            c("HIT", "LIT", "VLamax", "Other"), selected = sel$type),
                numericInput('duration', 'Duration in minutes', sel$duration, min = 1, max = 600),
                textAreaInput("description", "Description", sel$description) #, width = "300px"
              )
              
              })
            ),
            
            footer = tagList(
              actionButton('add_workout', 'Submit'),
              modalButton('cancel')
            )
      )
    }
    
    #sun
    #cloud-rain
    #cloud
    isolate({
      out <- showModal(
        dataModal()
      )
    })
    
  })
  
  
  ##### Update / stored data - when dragged
  
  observeEvent(input$my_calendar_update, {
    update <- input$my_calendar_update
    id <- update$schedule$id
    global$data[global$data$id == id, "start"] <- update$changes$start %>% as.Date()
    global$data[global$data$id == id, "end"] <- update$changes$end %>% as.Date()
  })

  output$my_calendar <- renderCalendar({
    cal <- calendar(data = data, 
                    defaultDate = Sys.Date(), # global$current_date --> lets the adding of new schedules by click fail 
                    navigation = TRUE, useDetailPopup = FALSE,
                    view = input$view,
                    useNavigation = TRUE,
                    isReadOnly = FALSE,
                    useCreationPopup = FALSE
    ) %>% 
      cal_events(
        clickSchedule = JS( # required for custom popup of schedule clicks
          "function(event) {",
          "Shiny.setInputValue('calendar_id_click', {schedule: event.schedule, id: event.schedule.id, x: event.event.clientX, y: event.event.clientY});",
          "}"
        )
      )
  })
  
  
  observeEvent(input$my_calendar_add, {
    
    dataModal <- function(failed = FALSE){
      modalDialog(
        div(style = ";background-color: #e9f5f8;",

            fluidRow(
              column(width = 3,
                tags$h2('Choose your exercise:')
              )
            ),
            fluidRow(
              column(width = 3,
                     radioButtons("sport_type", "Choose activity:", inline = TRUE,
                                  choiceNames = list(
                                    icon("bicycle"), 
                                    icon("running"), 
                                    icon("skiing-nordic"),
                                    icon("swimming"),
                                    icon("dumbbell")
                                  ),
                                  choiceValues = list(
                                    "bike", "run", "ski", "swim", "dumbbell"
                                  )
                     )
              ),
              column(width = 3,
                     output$dasss <- renderUI({
                       sel = gen_session_details(input$template)
                      textInput('title', 'Title', value = sel$title)
                     })
              ),
              column(width = 3,
                     selectInput("template", "Session from template:",
                                 c("4_4min_HIT", "3_13_30_15_HIT", "2h_LIT"))                     
              ),
              output$dass <- renderUI({
                sel = gen_session_details(input$template)
                column(width = 3,
                       selectInput("session_type", "Choose session type:",
                                   c("HIT", "HIT_EB", "HIT_IB", "LIT", "VLamax", "Other"), selected = sel$type)
                )
              })
            ),

            output$das <- renderUI({
              
              sel = gen_session_details(input$template)
              
              tagList(
                output$dass <- renderUI({
                  sel = gen_session_details(input$template)
                  intervall_details <- numericInput('duration', 'Duration in minutes', sel$duration, min = 1, max = 600)
                  if(input$session_type == "HIT_EB"){
                    intervall_details <- tagList(
                      fluidRow(h4("Intervall details:")),
                      fluidRow(
                        column(width = 2,
                               numericInput("reps", label = "Reps:", value = 4, min = 2, max = 12)
                        ),
                        column(width = 2,
                               numericInput("duration", label = "Duration in min.:", value = 4, min = 2, max = 12),
                        ),
                        column(width = 2,
                               numericInput("break_ratio", label = "Break ratio:", value = 0.5, min = 0.1, max = 2, ),
                        ),
                        column(width = 2,
                               numericInput("watt_hit", label = "Intensity Watts:", value = 300, min = 100, max = 600)                           
                        ),
                        column(width = 2,
                               numericInput("watt_break", label = "Break Watts:", value = 130, min = 50, max = 300)                           
                        )
                      )
                    )
                  }
                }),
                fluidRow(
                  column(width = 6,
                       textAreaInput("description", "Description", sel$description, width = "100%", height = "180px")
                  )
                )
              )
              
            })
        ),
        
        footer = tagList(
          actionButton('add_workout', 'Submit'),
          modalButton('cancel')
        )
      )
    }
    
    #sun
    #cloud-rain
    #cloud
    isolate({
      out <- showModal(
        dataModal()
      )
    })
    
  })
  

  observeEvent(input$my_calendar_update, {
    str(input$my_calendar_update)
    cal_proxy_update("my_calendar", input$my_calendar_update)
  })
  
  # observeEvent(input$my_calendar_delete, {
  #   str(input$my_calendar_delete)
  #   cal_proxy_delete("my_calendar", input$my_calendar_delete)
  # })
  
  
  observeEvent(input$add_workout, {
    print("xxxxxxxx")
    removeModal()
    global$name <- input$name
    global$state <- input$state
    
    type <- gsub(pattern = "HIT_", replacement = "", input$session_type)
    duration <<- input$duration
    repitition <<- input$reps
    break_size <<- input$break_size
    watt_hit <<- input$watt_hit
    watt_break <<- input$watt_break
    # could also handover  reactiveValues (global)
    ff <- create_workout(
                   type = "eb", 
                   duration = duration, # cant hand over input$duration directly? 
                   repetition = repitition, #input$reps 
                   break_size = break_size, #input$break_ratio 
                   watt_hit = watt_hit, #input$watt_hit 
                   watt_break = watt_break #input$watt_break
    )

    ####### NEEEED
    #### to add rest of add_meta function here.
    # But probably first a design discussion.
    # About ... ellipsis usage and modules, because
    #code gets too big
    
    
    add <- data.frame(
      calendarId = max(global$data$calendarId) + 1,
      title = "dummy",
      body = "",
      recurrenceRule = "",
      start = "",
      end = "",
      category = "time",
      location = NA,
      bgColor = NA,
      color = NA,
      borderColor = NA,
      date_diff = NA,
      id = max(global$data$id) + 1,
      type = NA
    )
    
    global$current_date <- global$current_dates[input$clicked_data$nr + 1]

    add$type <- ifelse(is.null(input$session_type), "template", input$session_type)
    title <- ifelse(is.null(input$title), "template", input$title)
    add$title <- title #paste(input$sport_type, input$duration, "min")
    
    added <- input$my_calendar_add
  
    add$start <- added$start %>% as.Date()
    add$end <- added$end %>% as.Date()

    print("adddd")
    isolate(global$data <- rbind(add, global$data))

    
    cal_proxy_add("my_calendar", list(start = input$my_calendar_add$start, end = input$my_calendar_add$end, 
                                      isAllDay = FALSE, category = "time", title = input$title))
    
  })
  
  observeEvent(req(global$uploaded), {
    duration <- global$uploaded$meta$duration # or better take amount of units?
    planned <- 120
    green <- abs(duration - planned) / planned < 0.5
    
    workout_day <- global$uploaded$meta$date
    global$day_to_mark <- which(global$current_dates == workout_day) - 1
    
    global$calendar_updated <- TRUE
    Sys.sleep(0.5)

    records <- global$uploaded$records
    has_power <- !is.null(records$power)
    
    if(has_power){
      secs <- length(records$power)
      NP <- calc_NP(watts = records$power)
      TSS <- calc_tss(FTP = 270, NP = NP, sec = secs)
    }
    
  })
  
  
  #### Delete entries
  
  observeEvent(input$schedule_delete, {
    
    removeModal()
  
    sched <- input$calendar_id_click$schedule
    print(sched)
    to_delete <- list(id = sched$id, title = sched$title, 
                      location = sched$location, start = sched$start$`_date`, end = sched$end$`_date`, 
                      isAllDay = sched$isAllDay, category = sched$category, calendarId = sched$calendarId)
    # print(dput(to_delete))
    # print(identical(to_delete, input$mycalendar_delete))
    
    del_idx <- which(global$data$calendarId == sched$id)
    global$data <- global$data[-del_idx, ]

    cal_proxy_delete("my_calendar", to_delete)
  })
  
  
  ######## START: CUSTOM SCHEDULE CLICK POPUP #############
  
  observeEvent(input$calendar_id_click, {
    
    removeUI(selector = "#custom_popup")
    id <- as.numeric(input$calendar_id_click$id)
    
    # Get the appropriate line clicked
    sched <- global$data[global$data$id == id, ]
    
    dataModal <- function(failed = FALSE) {
      
      df <- data.frame(planned = c(100, 60, "", 100, 30, 5), actual = c(89, 58, 0.8, 98, 28, 3))
      rownames(df) <- c("TSS", "Duration", "IF", "kcal", "carbs", "fat")
      
      output$hottable <- renderRHandsontable({
        rhandsontable(df, format = '0a')
      })
      
      modalDialog(
        tabsetPanel(
          tabPanel("Summary",         
                   rHandsontableOutput("hottable"),
                   popup[[global$data$type[id]]]$tf,
                   tags$b("Ernährung: (Vor Einheit)"),
                   popup[[global$data$type[id]]]$nutr_before,
                   tags$b("Ernährung: (während Einheit)"),
                   popup[[global$data$type[id]]]$nutr_during,
                   tags$b("Ernährung: (Nach Einheit)"),
                   popup[[global$data$type[id]]]$nutr_after,
                   br(),
                   div(popup[[global$data$type[id]]]$meta),
                   br(),
                   tags$b("FAQ:"),
                   popup[[global$data$type[id]]]$faq
          ),
          tabPanel("Charts", h4("a"))
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("schedule_delete", label = "Delete"),
          actionButton(inputId = "ok", label = "Save & Close")
        ),
        size = "l"
      )
    }
    
    showModal(dataModal())

  })
  
  observeEvent(input$close_calendar_panel, {
    removeUI(selector = "#custom_popup")
  })
  
  observeEvent(input$ok, {
    removeModal()
  })
  
  rv <- reactiveValues(id = NULL, status = NULL)
  observeEvent(input$status, {
    rv$id <- input$calendar_id_click$id
    rv$status <- input$status
  })
  
  ######## END: CUSTOM SCHEDULE CLICK POPUP #############
  
  
  observe({
    req(input$my_calendar_dates)
    dates <- lapply(input$my_calendar_dates, as.Date)
    global$current_dates <- seq(from = dates$start, to = dates$end, "days")
  })
  
  
  ### MARKING GREEN
  observe({
    req(global$current_dates)
    workout_day <- as.Date("2022-02-05")
    day_to_mark <- which(global$current_dates == workout_day) - 1
    shinyjs::runjs(js_mark_dates(day_to_mark, color = "green"))
  })
  
  
}
