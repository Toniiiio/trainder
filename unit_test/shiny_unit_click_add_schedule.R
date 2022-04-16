library(shiny)
library(shinyjs)
library(toastui)

ui <- fluidPage(
  useShinyjs(),
  tags$h2("Add, Update and Delete schedule interactively"),
  
  tags$p(
    "Click on the calendar to create a new schedule",
    "then you will be able to edit or delete it."
  ),
  
  calendarOutput("my_calendar")
)

server <- function(input, output) {
  
  output$my_calendar <- renderCalendar({
    cal <- calendar(data = cal_demo_data(),
      defaultDate = Sys.Date(),
      navigation = TRUE,
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
    cal_proxy_update("my_calendar", input$my_calendar_update)
  })
  
  # observe({
  #   print(input$calendar_id_click)
  #   print(input$schedule$`__fe_id`)
  #   print(input$schedule$id)
  # })
  
  observeEvent(input$my_calendar_delete, {
    print(dput(input$my_calendar_delete))
    sched <- input$calendar_id_click$schedule
    
    to_delete <- list(id = sched$id, title = sched$title, 
                      location = sched$location, start = sched$start$`_date`, end = sched$end$`_date`, 
                      isAllDay = sched$isAllDay, category = sched$category, calendarId = sched$calendarId)
    print(dput(to_delete))
    print(identical(to_delete, input$mycalendar_delete))
    cal_proxy_delete("my_calendar", to_delete)
  })

}

runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
