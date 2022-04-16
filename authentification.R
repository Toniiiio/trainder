# authentification

# Overview: https://towardsdatascience.com/r-shiny-authentication-incl-demo-app-a599b86c54f7
# https://paul.rbind.io/shinyauthr/ - long time no github update - password stored in dataframe? 342 stars
# https://datastorm-open.github.io/shinymanager/ password encryped and stored in encrypted sqlite
# is on cran. github update - 302 stars


credentials <- data.frame(
  user = c("shiny", "shinymanager"), # mandatory
  password = c("azerty", "12345"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

library(shiny)
library(shinymanager)

# Wrap your UI with secure_app
ui <- secure_app(ui)


shinyApp(ui, server)