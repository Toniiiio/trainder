
ui <- fluidPage(
  tags$style(
    type = 'text/css',
    '.modal-dialog { width: fit-content !important; }
    .dygraph-axis-label-x:nth-of-type(-n+1) {display: none !important; }'
  ),
  tags$h4("Training calendar"),
  useShinyjs(),
  useWaitress(),
  
  tabsetPanel(id = "panel_tab",
    tabPanel("calendar", 
             fluidRow(
               column(width = 1,
                      checkboxInput("show_food", "Show food:", value = FALSE)
               ),
               column(width = 2,
                      selectInput("view", label = "View:", choices = c("month", "week", "day"))
               ),
               column(width = 2,
                      actionButton("choose_template", "Choose template")
               ),
               column(width = 2,
                      fileInput("file1", "Choose  File",
                                multiple = TRUE,
                                accept = ".fit")
               ),
               column(width = 2,
                      checkboxInput("stitch", "stitch files")
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
    tabPanel("All Workouts",
             uiOutput("workout_details")
    ),
    tabPanel("Workout View",
             uiOutput("mod_workout_view")
    )
  )
  
)

