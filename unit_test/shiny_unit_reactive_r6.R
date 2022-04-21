
library(shiny)
Person <- R6::R6Class(
  "Person",
  private = list(
    reactiveDep = NULL
  ),
  public = list(
    workout_details = NULL,
    initialize = function() {
      private$reactiveDep <- reactiveVal(0)
    },
    reactive = function() {
      reactive({
        private$reactiveDep()
        self
      })
    },
    changewd = function(x) {
      private$reactiveDep(isolate(private$reactiveDep()) + 1)
      self$workout_details <- x
    }
  )
)

library(shiny)

ui <- fluidPage(
  actionButton("chg", "chg"),
  numericInput("num", "num", 2),
  dataTableOutput("dt")
)

server <- function(input, output, session) {
  
  p <- Person$new()$reactive()

  observeEvent(input$chg,{
    p()$changewd(input$num)
  })
  
  observeEvent(p(), {
    print(p()$workout_details)
    print("nowwww")
  })
  
  output$dt <- renderDataTable({
    data.frame(p()$workout_details)
  })
  
}

shinyApp(ui, server)


