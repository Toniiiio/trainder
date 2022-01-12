library(shiny)
library(shinyjs)
library(toastui)

# follow up: https://cfss.uchicago.edu/setup/git-with-rstudio/
# https://github.com/settings/profile

source("C:/R/data.R")
start_date <- as.Date("2022-01-03")
block_template_vo2_1$start <- block_template_vo2_1$date_diff + start_date
block_template_vo2_1$end <- block_template_vo2_1$date_diff + start_date


gen_session_details <- function(title){
  
  out <- list(
    "4_4min_HIT" = data.frame(
      title = "4_4min_HIT",
      duration = 60,
      description = "Vortag: Kohlenhydratspeicher im Vorfeld auffüllen.
                   Verpflegung: Kohlenhydratreich.
                   Ablauf: 10min Aufwärmen per Stufen, 4min @300W, 2min Pause @140W. Wiederhole 4 Mal." 
    ),
    "3_13_30_15_HIT" = data.frame(
      title = "3_13_30_15_HIT",
      duration = 60,
      description = "Vortag: Kohlenhydratspeicher im Vorfeld auffüllen.
                   Verpflegung: Kohlenhydratreich.
                   Ablauf: 10min Aufwärmen per Stufen, 30sec @330W, 15sec Pause @140W. Wiederhole 13 Mal." 
    ),
    "2h_LIT" = data.frame(
      title = "2h_LIT",
      duration = 120,
      description = "Ggf. ersten 60min nüchtern. Ggf. Koffeein/Teein zuführen. Slow Carb zuführen." 
    )
  )
  
  out[[title]]
  
}


ui <- fluidPage(
  tags$h2("calendar shiny example"),
  useShinyjs(),
  
  # tags$head(),
  fluidRow(
    column(
      width = 12,
      calendarOutput("my_calendar")
    )
  )
)

data <- data.frame(
  calendarId = integer(),
  title = character(),
  body = character(),
  recurrenceRule = character(),
  start = character(),
  end = character(),         
  category = character(),
  location = character(),
  bgColor = character(),
  color = character(),
  borderColor = character()
)

# data <- cal_demo_data()[1, ]
data <- block_template_vo2_1
data$id <- seq_len(nrow(data))


js_current_date <- "
var elements = document.getElementsByClassName('tui-full-calendar-weekday-grid-line  tui-full-calendar-near-month-day');
for (var i = 0; i < elements.length; i++) {
  elements[i].addEventListener('click', function(el) {
    var target = el.target;
    Shiny.onInputChange('clicked_data', {val: target.textContent, rand: Math.random()});
  });
}


var elements = document.getElementsByClassName('tui-full-calendar-month-week-item');
for (var i = 0; i < elements.length; i++) {
  elements[i].addEventListener('click', function(el) {
    var target = el.target;
    while (target.className != 'tui-full-calendar-month-week-item') {
      target = target.parentElement
    }
    Shiny.onInputChange('row_idx', [...target.parentElement.children].indexOf(target))
  });
}
"


server <- function(input, output, session) {
  
  global <- reactiveValues(data = data)
  #//var rows = document.getElementById('my_calendar').getElementsByClassName('tui-full-calendar-month-week-item');
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  session$onFlushed( function(){
    shinyjs::runjs(js_current_date)
  }, once = FALSE )
  
  
  observe({
    
    clicked_text <- input$clicked_data$val
    req(clicked_text)
    
    global$click_date <- clicked_text %>% 
      gsub(pattern = " |\n", replacement = "") %>% 
      as.numeric
    
    isolate({
      out <- showModal(modalDialog(
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
            numericInput('duration', 'Duration in minutes', sel$duration, min = 1, max = 600),
            textAreaInput("description", "Description", sel$description) #, width = "300px"
          )
          
        }),
        
        footer = tagList(
          actionButton('submit', 'Submit'),
          modalButton('cancel')
        )
      ))
    })
    
  })
  
  observeEvent(input$submit, {
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
      id = nrow(global$data) + 1
    )
    
    add$title <- input$title #paste(input$sport_type, input$duration, "min")
    add$start <- paste0("2022-01-", rep(0, 2 - nchar(global$click_date)), global$click_date) %>% as.Date
    add$end <- paste0("2022-01-", rep(0, 2 - nchar(global$click_date)), global$click_date) %>% as.Date
    isolate(global$data <- rbind(add, global$data))
    print(global$data %>% dput)
  })
  
  output$my_calendar <- renderCalendar({
    calendar(global$data, navigation = TRUE, useDetailPopup = FALSE) %>% 
      cal_events(
        clickSchedule = JS(
          "function(event) {", 
          "Shiny.setInputValue('calendar_id_click', {id: event.schedule.id, x: event.event.clientX, y: event.event.clientY});", 
          "}"
        )
      )
  })
  
  observeEvent(input$calendar_id_click, {
    removeUI(selector = "#custom_popup")
    id <- as.numeric(input$calendar_id_click$id)
    # Get the appropriate line clicked
    sched <- global$data[global$data$id == id, ]
    
    insertUI(
      selector = "body",
      ui = absolutePanel(
        id = "custom_popup",
        top = input$calendar_id_click$y,
        left = input$calendar_id_click$x, 
        draggable = FALSE,
        width = "300px",
        tags$div(
          style = "width: 250px; position: relative; background: #FFF; padding: 10px; box-shadow: 0px 0.2em 0.4em rgb(0, 0, 0, 0.8); border-radius: 5px;",
          actionLink(
            inputId = "close_calendar_panel", 
            label = NULL, icon = icon("close"), 
            style = "position: absolute; top: 5px; right: 5px;"
          ),
          tags$br(),
          tags$div(
            style = "text-align: center;",
            tags$p(
              global$data$title[id]
            )
          )
        )
      )
    )
  })
  
  observeEvent(input$close_calendar_panel, {
    removeUI(selector = "#custom_popup")
  })
  
  rv <- reactiveValues(id = NULL, status = NULL)
  observeEvent(input$status, {
    rv$id <- input$calendar_id_click$id
    rv$status <- input$status
  })
  
  observe({
    req(!is.na(input$row_idx))
    req(input$row_idx)
    start_month <- format(as.Date(input$my_calendar_dates$start),"%m")
    current_month <- format(as.Date(input$my_calendar_dates$current),"%m")
    end_month <- format(as.Date(input$my_calendar_dates$end),"%m")
    row_nr = input$row_idx
    val = gsub(input$clicked_data$val, pattern = "\n| ", replacement = "") %>% as.numeric
    req(!is.na(val))
    if(row_nr %in% c(0, 1) & val > 16){
      month = start_month
    }else if(row_nr %in% c(4, 5) & val < 16){
      month = end_month
    }else{
      month = current_month
    }
    print(month)
    print(val)
  })
  
  output$click <- renderPrint({
    input$my_calendar_click
  })
  
}

runApp(shinyApp(ui, server), launch.browser = TRUE)
