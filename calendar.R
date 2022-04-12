library(shiny)
library(shinyjs)
library(toastui)
library(waiter)
library(shinycssloaders)

# options(shiny.error = browser)

# follow up: https://cfss.uchicago.edu/setup/git-with-rstudio/
# https://github.com/settings/profile

source("data.R")
source("load_strava.R")
start_date <- as.Date("2022-02-01")
block_template_vo2_1$start <- block_template_vo2_1$date_diff + start_date
block_template_vo2_1$end <- block_template_vo2_1$date_diff + start_date
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

gen_session_details <- function(title){
  
  out <- list(
    "4_4min_HIT" = data.frame(
      title = "4_4min_HIT",
      duration = 60,
      type = "HIT",
      description = "Vortag: Kohlenhydratspeicher im Vorfeld auffüllen.
                   Verpflegung: Kohlenhydratreich.
                   Ablauf: 10min Aufwärmen per Stufen, 4min @300W, 2min Pause @140W. Wiederhole 4 Mal." 
    ),
    "3_13_30_15_HIT" = data.frame(
      title = "3_13_30_15_HIT",
      duration = 60,
      type = "HIT",
      description = "Vortag: Kohlenhydratspeicher im Vorfeld auffüllen.
                   Verpflegung: Kohlenhydratreich.
                   Ablauf: 10min Aufwärmen per Stufen, 30sec @330W, 15sec Pause @140W. Wiederhole 13 Mal." 
    ),
    "2h_LIT" = data.frame(
      title = "2h_LIT",
      type = "LIT",
      duration = 120,
      description = "Ggf. ersten 60min nüchtern. Ggf. Koffeein/Teein zuführen. Slow Carb zuführen." 
    )
  )
  
  out[[title]]
  
}


ui <- fluidPage(
  tags$h4("Training calendar"),
  useShinyjs(),
  useWaitress(),
  
  # tags$head(),
  fluidRow(
    column(width = 1,
           checkboxInput("show_food", "Show food:", value = FALSE)
    ),
    column(width = 2,
           selectInput("view", label = "View:", choices = c("month", "week", "day"))
    ),
    column(width = 2,
           fileInput("file1", "Choose  File",
                     multiple = FALSE,
                     accept = ".fit")
    )
  ),
  fluidRow(
    withSpinner(calendarOutput("my_calendar"), type = 2)
  )
)

# data <- data.frame(
#   calendarId = integer(),
#   title = character(),
#   body = character(),
#   recurrenceRule = character(),
#   start = character(),
#   end = character(),         
#   category = character(),
#   location = character(),
#   bgColor = character(),
#   color = character(),
#   borderColor = character()
# )

# data <- cal_demo_data()[1, ]
data <- block_template_vo2_1
data$id <- seq_len(nrow(data))
data$type <- ifelse(grepl(pattern = "HIT", data$title), "HIT", "LIT")

data2 <- data
data2$title <- ifelse(data2$type == "HIT", "High Carb essen", no = "Ggf. Low Carb essen")
data2$start <- data2$start - 1
data2$end <- data2$end - 1

js_mark_dates <- function(date_nr, color = "green"){paste0("
var elements = document.getElementsByClassName('tui-full-calendar-weekday-grid-line  tui-full-calendar-near-month-day');
var elements2 = document.getElementsByClassName('tui-full-calendar-weekday-grid-header');

for (var nr = 0; nr < elements.length; nr++) {
  elements[nr].addEventListener('click', function(el) {
    var target = el.target;
    Shiny.onInputChange('clicked_data', {val: target.textContent, rand: Math.random()});
  });
  if(nr == ", date_nr,"){
    elements[nr].style['background-color'] = '", color,"'
    
    container = elements2[nr].getElementsByTagName('span')[0]
    container.style.textAlign= 'center';
    container.style.width = '240px'
    var div = document.createElement('div');
    var htmlString = '<i class=\"fa fa-sun\" role=\"presentation\" aria-label=\"sun icon\"></i>'
    //var htmlString = '<i class=\"fa fa-cloud\" role=\"presentation\" aria-label=\"cloud icon\"></i>'
    div.innerHTML = htmlString.trim();
    div.style.float = 'right'
    div.style.marginRight = '5px';
    div.style.marginTop = '5px';
    container.appendChild(div);
    
  }
  console.log(2)
}")}


js_current_date <- "
var elements_title = document.getElementsByClassName('tui-full-calendar-weekday-schedule-title');
for (var i = 0; i < elements_title.length; i++) {
  container = elements_title[i].parentNode
  container.style.textAlign= 'center';
  elements_title[i].style.float = 'left'
  var div = document.createElement('div');
  var htmlString = '<i class=\"fa fa-bicycle\" role=\"presentation\" aria-label=\"bicycle icon\"></i>'
  
  div.innerHTML = htmlString.trim();
  div.style.float = 'left'
  div.style.marginLeft = '2px';
  container.appendChild(div);
};

// get date
var elements = document.getElementsByClassName('tui-full-calendar-weekday-grid-line  tui-full-calendar-near-month-day');
for (var i = 0; i < elements.length; i++) {
  elements[i].addEventListener('click', function(el) {
    var target = el.target;
    Shiny.onInputChange('clicked_data', {val: target.textContent, rand: Math.random()});
  });
}

// get month
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
  
  global <- reactiveValues(data = data, calendar_updated = TRUE)
  #//var rows = document.getElementById('my_calendar').getElementsByClassName('tui-full-calendar-month-week-item');
  
  
  observe({
    print(input$user_data)
  })
  
  observeEvent(input$file1, {
    print(input$file1)
    if(!is.null(input$file1)){
      global$calendar_updated <- FALSE
      global$uploaded <- parse_strava(input$file1$datapath)
    }
    # 
  })
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  session$onFlushed( function(){
    shinyjs::runjs(js_current_date)
  }, once = TRUE)
  
  observe({
    if(input$show_food){
      global$data <- rbind(data, data2)      
    }else{
      global$data <- data
    }
  })
  
  
  observe({
    
    clicked_text <- input$clicked_data$val
    req(clicked_text)
    
    global$click_date <- clicked_text %>% 
      gsub(pattern = " |\n", replacement = "") %>% 
      as.numeric
    
    #sun
    #cloud-rain
    #cloud
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
            selectInput("session_type", "Choose session type:",
                        c("HIT", "LIT", "VLamax", "Other"), selected = sel$type),
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
  
  observeEvent(req(input$submit), { #, input$document_clicked
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
    
    add$type <- ifelse(is.null(input$session_type), "template", input$session_type)
    title <- ifelse(is.null(input$title), "template", input$title)
    add$title <- title #paste(input$sport_type, input$duration, "min")
    req(global$click_date)
    add$start <- paste0("2022-01-", rep(0, 2 - nchar(global$click_date)), global$click_date) %>% as.Date
    add$end <- paste0("2022-01-", rep(0, 2 - nchar(global$click_date)), global$click_date) %>% as.Date
    print("a")
    print(str(add))
    print(str(global$data))
    isolate(global$data <- rbind(add, global$data))
    print(global$data %>% dput)
  })
  
  observeEvent(req(global$uploaded), {
    duration <- global$uploaded$meta$duration
    planned <- 120
    green <- abs(duration - planned) / planned < 0.5
    
    workout_day <- global$uploaded$meta$date
    global$day_to_mark <- which(global$current_dates == workout_day) - 1
    
    global$calendar_updated <- TRUE
    Sys.sleep(0.5)
    print("green")
    print(green)
  })
  
  output$my_calendar <- renderCalendar({
    
    req(global$calendar_updated == TRUE)
    if(!is.null(global$day_to_mark)){
      print(global$day_to_mark)
      shinyjs::runjs(js_mark_dates(5, color = "green"))
      print("dingggg")
    }
    
    calendar(global$data, navigation = TRUE, useDetailPopup = FALSE, view = input$view) %>% 
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
    
    popup <- list()
    popup$LIT <- list()
    popup$LIT$nutr_before <- tags$div("Ggf. Nüchtern")
    popup$LIT$nutr_during <- tags$div("Wasser")
    popup$LIT$nutr_after <- tags$div("Recovery Shake (Eiweiß) + ggf. Carbs, wenn CarbSpeicher aufgefüllt werden sollen.")
    popup$LIT$tf <- c()
    popup$LIT$watt_table <- tagList(tags$b("Werte: "), br(), c("Bleibe zwischen 130-200 Watt."), br())
    popup$LIT$faq <- tags$ul(
      tags$li(tags$a("Kann ich härter fahren?", href = "https://www.google.de", target = "_blank")),
      tags$li(tags$a("Sollte ich ein Warmup machen?", href = "https://www.google.de", target = "_blank")),
      tags$li(tags$a("Muss ich das Ausfahren machen?", href = "https://www.google.de", target = "_blank"))
    )

    popup$HIT <- list()
    popup$HIT$nutr_before <- tags$div("High Carb ")
    popup$HIT$nutr_during <- tags$div("Carbs: 60-90g ")
    popup$HIT$nutr_after <- tags$div("Recovery Shake (Eiweiß) + ggf. Carb wenn CarbSpeicher aufgefüllt werden sollen.")
    popup$HIT$tf <- tagList(tags$b("Trittfrequenz: "), tags$div("TF: 90-110 U/min"))
    popup$HIT$faq <- tags$ul(
      tags$li(tags$a("Kann ich härter fahren?", href = "https://www.google.de", target = "_blank")),
      tags$li(tags$a("Muss ich das Warmup machen?", href = "https://www.google.de", target = "_blank")),
      tags$li(tags$a("Muss ich das Ausfahren machen?", href = "https://www.google.de", target = "_blank"))
    )

    dataModal <- function(failed = FALSE) {
      
      df <- data.frame(planned = c(100, 60, "", 100, 30, 5), actual = c(89, 58, 0.8, 98, 28, 3))
      rownames(df) <- c("TSS", "Duration", "IF", "kcal", "carbs", "fat")
      
      output$hottable <- renderRHandsontable({
        rhandsontable(df, format = '0a')
      })
      
      id = 1
      
      modalDialog(
        tabsetPanel(
          tabPanel("Summary",         rHandsontableOutput("hottable"),
                   
                   popup[[global$data$type[id]]]$tf,
                   tags$b("Ernährung: (Vor Einheit)"),
                   popup[[global$data$type[id]]]$nutr_before,
                   tags$b("Ernährung: (während Einheit)"),
                   popup[[global$data$type[id]]]$nutr_during,
                   tags$b("Ernährung: (Nach Einheit)"),
                   popup[[global$data$type[id]]]$nutr_after,
                   br(),
                   div(popup[[global$data$type[id]]]$meta, id = "aaa", onclick = "alert(2)"),
                   br(),
                   tags$b("FAQ:"),
                   popup[[global$data$type[id]]]$faq)
        ),
        tabPanel("Charts", h4("a")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = "ok", label = "Save & Close")
        )
      )
    }
    
    showModal(dataModal())
    
    
    # shinyjs::runjs(paste0("
    #   console.log(99)
    #   container = document.getElementsByTagName('body')[0]
    #   console.log(21)
    #   var htmlString = \"", popup, "\"
    #   var div = document.createElement('div');
    #   div.innerHTML = htmlString.trim();
    #   div.style.float = 'right'
    #   div.style.marginRight = '5px';
    #   div.style.marginTop = '5px';
    #   container.appendChild(div);
    #   console.log(22)
    #   console.log(23);
    #   div.addEventListener('input', function() {
    #     alert('input event fired');
    #   }, false);
    # "))
    
    
    
  })
  
  # document.getElementById("editor").addEventListener("input", function() {
  #   console.log("input event fired");
  # }, false);
  
  onevent(event = "input", id = "user_time2", print("text"))
  
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

  
  observe({
    req(input$my_calendar_dates)
    dates <- lapply(input$my_calendar_dates, as.Date)
    global$current_dates <- seq(from = dates$start, to = dates$end, "days")
  })
  
  observe({
    req(global$current_dates)
    workout_day <- as.Date("2022-02-05")
    day_to_mark <- which(global$current_dates == workout_day) - 1
    
    shinyjs::runjs(js_mark_dates(day_to_mark, color = "green"))
  })
  
  
  observe({

    # req(!is.na(input$row_idx))
    # req(input$row_idx)
    # start_month <- format(as.Date(input$my_calendar_dates$start),"%m")
    # current_month <- format(as.Date(input$my_calendar_dates$current),"%m")
    # end_month <- format(as.Date(input$my_calendar_dates$end),"%m")
    # row_nr = input$row_idx
    # val = gsub(input$clicked_data$val, pattern = "\n| ", replacement = "") %>% as.numeric
    # req(!is.na(val))
    # if(row_nr %in% c(0, 1) & val > 16){
    #   month = start_month
    # }else if(row_nr %in% c(4, 5) & val < 16){
    #   month = end_month
    # }else{
    #   month = current_month
    # }
    # print(month)
    # print(val)
  })
  
  output$click <- renderPrint({
    input$my_calendar_click
  })
  
}

runApp(shinyApp(ui, server), launch.browser = TRUE)
