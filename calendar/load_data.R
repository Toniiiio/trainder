source("data.R")
source("load_strava.R")
source("calendar/load_functions.R")
# source("key_performance_chart.R")
source("calendar/js.R")

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

start_date <- as.Date("2022-02-01")
template_month_vo2_block$start <- template_month_vo2_block$date_diff + start_date
template_month_vo2_block$end <- template_month_vo2_block$date_diff + start_date
template_month_vo2_block$id <- seq_len(nrow(template_month_vo2_block))
template_month_vo2_block$calendarId <- seq_len(nrow(template_month_vo2_block))
template_month_vo2_block$type <- ifelse(grepl(pattern = "HIT", template_month_vo2_block$title), "HIT", "LIT")



template_week_vo2_regular
template_week_vo2_regular$start <- template_week_vo2_regular$date_diff + start_date
template_week_vo2_regular$end <- template_week_vo2_regular$date_diff + start_date
template_week_vo2_regular$id <- seq_len(nrow(template_week_vo2_regular))
template_week_vo2_regular$calendarId <- seq_len(nrow(template_week_vo2_regular))
template_week_vo2_regular$type <- ifelse(grepl(pattern = "HIT", template_week_vo2_regular$title), "HIT", "LIT")

template_week_vo2_regular


#data <- template_month_vo2_block

# data2 <- data
# data2$title <- ifelse(data2$type == "HIT", "High Carb essen", no = "Ggf. Low Carb essen")
# data2$start <- data2$start - 1
# data2$end <- data2$end - 1

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
