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


server <- function(input, output, session) {
  
  global <- reactiveValues(data = data, calendar_updated = TRUE, current_date = Sys.Date())
  
  # observe({
  #   print("xxx")
  #   shinyjs::runjs(js_mark_dates(Sys.Date(), color = "green"))
  #   print("updated_calendar")
  #   global$data
  #   print(input$updated_calendar)
  # }, priority = -1)
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  observeEvent(c(global$data, global$current_date),{
    print("dsss")
    shinyjs::runjs(js_current_date)
  })
  
  # lets app crash in combination with shinymanager - authentication
  # session$onSessionEnded(function() {
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
  
  
  # output$my_calendar <- renderCalendar({
  #   calendar(global$data, navigation = TRUE, defaultDate = global$current_date, 
  #            useDetailPopup = FALSE, view = input$view) %>% 
      # cal_events(
      #   clickSchedule = JS( # required for custom popup of schedule clicks
      #     "function(event) {",
      #     "Shiny.setInputValue('calendar_id_click', {id: event.schedule.id, x: event.event.clientX, y: event.event.clientY});",
      #     "}"
      #   )
      # )
  # })
  
  
  
  output$my_calendar <- renderCalendar({
    cal <- calendar(data = cal_demo_data(), #global$data
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
  
  observeEvent(input$add_workout, {
    
    removeModal()
    
    cal_proxy_add("my_calendar", list(start = input$my_calendar_add$start, end = input$my_calendar_add$end, 
                                      isAllDay = FALSE, category = "time", title = input$title))
    
  })
  
  observeEvent(input$my_calendar_update, {
    str(input$my_calendar_update)
    cal_proxy_update("my_calendar", input$my_calendar_update)
  })
  
  observeEvent(input$my_calendar_delete, {
    str(input$my_calendar_delete)
    cal_proxy_delete("my_calendar", input$my_calendar_delete)
  })
  
  
  observeEvent(req(input$add_workout), { #, input$document_clicked
    removeModal()
    global$name <- input$name
    global$state <- input$state
    
    add <- data.frame(
      calendarId = 1,
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
      id = nrow(global$data) + 1,
      type = NA
    )
    
    global$current_date <- global$current_dates[input$clicked_data$nr + 1]

    add$type <- ifelse(is.null(input$session_type), "template", input$session_type)
    title <- ifelse(is.null(input$title), "template", input$title)
    add$title <- title #paste(input$sport_type, input$duration, "min")
    req(global$click_date)
    add$start <- global$current_date %>% as.Date()
    add$end <- global$current_date %>% as.Date()

    # isolate(global$data <- rbind(add, global$data))
    cal_proxy_add("my_calendar", list(start = "2022-04-11T12:30:00+02:00", end = "2022-04-11T13:30:00+02:00", 
                                      isAllDay = FALSE, category = "time"))
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
    
    to_delete <- list(id = sched$id, title = sched$title, 
                      location = sched$location, start = sched$start$`_date`, end = sched$end$`_date`, 
                      isAllDay = sched$isAllDay, category = sched$category, calendarId = sched$calendarId)
    # print(dput(to_delete))
    # print(identical(to_delete, input$mycalendar_delete))
    cal_proxy_delete("my_calendar", to_delete)
  })
  
  
  ######## START: CUSTOM SCHEDULE CLICK POPUP #############
  
  observeEvent(input$calendar_id_click, {
    
    print(input$calendar_id_click)
    
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
        )
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
    # print("global$current_dates")
    # print(global$current_dates)
  })
  
  
  ### MARKING GREEN
  observe({
    req(global$current_dates)
    workout_day <- as.Date("2022-02-05")
    day_to_mark <- which(global$current_dates == workout_day) - 1
    
    shinyjs::runjs(js_mark_dates(day_to_mark, color = "green"))
  })
  
  
}
