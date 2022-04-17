ui <- fluidPage(
  tags$h2("My secure application"),
  verbatimTextOutput("auth_output"),
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
                     multiple = TRUE,
                     accept = ".fit")
    )
  ),
  fluidRow(
    withSpinner(
      column(width = 10, calendarOutput("my_calendar")),
      type = 2),
    uiOutput("calendar_sidebar")
  )
  
)