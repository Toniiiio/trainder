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

#.find('.tui-full-calendar-month-week-item').index()
# alert(e.closest('div').find('.tui-full-calendar-month-week-item'));
server <- function(input, output, session) {
  
  global <- reactiveValues(data = data)
  #//var rows = document.getElementById('my_calendar').getElementsByClassName('tui-full-calendar-month-week-item');
  session$onFlushed(function() shinyjs::runjs("
  var elements = document.getElementById('my_calendar').getElementsByTagName('*');
  for(var nr = 0, len = elements.length; nr < len; nr++) {
    if(elements[nr].classList.contains('tui-full-calendar-weekday-grid-line')){
      elements[nr].onclick = function (e) {
        Shiny.onInputChange('clicked_data', {val: e.target.textContent, rand: Math.random()});
      }
    }
  }
  "), once = FALSE)
  
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
                     )),
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
      borderColor = NA
    )
    
    
    add$title <- input$title #paste(input$sport_type, input$duration, "min")
    add$start <- paste0("2022-01-", rep(0, 2 - nchar(global$click_date)), global$click_date)
    add$end <- paste0("2022-01-", rep(0, 2 - nchar(global$click_date)), global$click_date)
    isolate(global$data <- rbind(add, global$data))
    print(global$data %>% dput)
  })
  
  output$my_calendar <- renderCalendar({
    calendar(global$data, navigation = TRUE)
  })
  
  output$dates <- renderPrint({
    input$my_calendar_dates
  })
  
  output$click <- renderPrint({
    input$my_calendar_click
  })
  
}

runApp(shinyApp(ui, server), launch.browser = TRUE)
