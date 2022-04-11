library(rhandsontable)
library(shiny)

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

df <- data.frame(planned = c(100, 60, "", 100, 30, 5), actual = c(89, 58, 0.8, 98, 28, 3))
rownames(df) <- c("TSS", "Duration", "IF", "kcal", "carbs", "fat")
df



shinyApp(
  ui = basicPage(
    actionButton("show", "Show modal dialog")
  ),
  
  server = function(input, output) {
    
    dataModal <- function(failed = FALSE) {
      
      output$hottable <- renderRHandsontable({
        rhandsontable(df, format = '0a')
      })
      id = 1
      
      modalDialog(

        rHandsontableOutput("hottable"),
        
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
          popup[[global$data$type[id]]]$faq,

        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      )
    }
    
    # Show modal when button is clicked.
    observeEvent(input$show, {
      showModal(dataModal())
    })
    
    observeEvent(input$ok, {
        print(hot_to_r(input$hottable))
        removeModal()
    })
    
  }
)

