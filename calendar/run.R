library(shiny)
library(shinyjs)
library(shinyjqui)
library(toastui)
library(waiter)
library(shinycssloaders)
library(rhandsontable)
library(shinymanager)
library(DT)
library(xts)
library(logging)

options(shiny.error = browser)

source("calendar/dygraph_modules.R")
source("sportler.R")
source("load_strava.R")
source("calendar/analyse_energy.R")
source("calendar/gen_energy_data.R")
source("create_watt_table.R")
source("calendar/load_data.R", encoding = "UTF-8")
source("calendar/ui.R", encoding = "UTF-8")
source("calendar/server.R", encoding = "UTF-8")
source("nutritionals.R", encoding = "UTF-8")

ui <- secure_app(ui)

runApp(shinyApp(ui, server), launch.browser = TRUE, port = 9999)


