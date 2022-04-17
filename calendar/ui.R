ui <- fluidPage(
  tags$h4("Training calendar"),
  useShinyjs(),
  useWaitress(),
  
  tabsetPanel(
    tabPanel("calendar", 
             fluidRow(
               column(width = 1,
                      checkboxInput("show_food", "Show food:", value = FALSE)
               ),
               column(width = 2,
                      selectInput("view", label = "View:", choices = c("month", "week", "day"))
               ),
               column(width = 2,
                      fileInput("file1", "Choose  File",
                                multiple = TRUE,
                                accept = ".fit")
               )
             ),
             fluidRow(
               withSpinner(
                 column(width = 10, calendarOutput("my_calendar")),
                 type = 2
               ),
               uiOutput("calendar_sidebar")
             )
    ),
    tabPanel("Workout",
             DT::dataTableOutput("workout_details", width = "50%")
    )
  )
  
)

