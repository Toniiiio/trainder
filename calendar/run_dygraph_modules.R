library(dygraphs)
library(magrittr)
library(datasets)
library(shiny)
library(shinyjs)
library(xts)
library(leaflet)


# source("R/gen_data.R")
# source("R/gen_energy_data.R")


source("calendar/dygraph_modules.R")

ui <- shinyUI(fluidPage(
  useShinyjs(),
  tabsetPanel(id = "panel_tab",
    tabPanel("Plot", actionButton("upload", "upload")),
    tabPanel("Summary", "yo"),
    tabPanel("Table", uiOutput("mod_workout_view"))
  )
))


server <- function(input, output, session) {
  global2 <- reactiveValues(records = NULL)
  
  observeEvent(input$upload, {
    file_name <- "biketrainr-master/data/02_04_2022_LIT.fit"
    source("load_strava.R")  
    global2$records <- load_strava(file_name = file_name)
  })
  
  #records <- mtcars#createRecords(id = "try2")
  observe({
    
    if(!is.null(global2$records)){
      out <- modUI("try", "try")
      workout <- list(records = global2$records, meta = NULL)
      mod <- modServer(id = "try", workouts = list(workout, workout))
    }else{
      out <- h5(
        tags$a("Upload", href ="javascript:Shiny.setInputValue('switch_panel', Math.random());"), 
        " a workout to use this view!"
      )
      
    }
    
    output$mod_workout_view <- renderUI({
      out
    })
    
  })
  
  observeEvent(input$switch_panel, {
    updateTabsetPanel(session = session, inputId = "panel_tab", selected = "Plot")
  })
  

}

shinyApp(ui, server)
