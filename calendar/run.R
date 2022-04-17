library(shiny)
library(shinyjs)
library(toastui)
library(waiter)
library(shinycssloaders)
library(rhandsontable)
library(shinymanager)


source("calendar/load_data.R", encoding = "UTF-8")
source("calendar/ui.R", encoding = "UTF-8")
source("calendar/server.R", encoding = "UTF-8")

ui <- secure_app(ui)

runApp(shinyApp(ui, server), launch.browser = TRUE)

