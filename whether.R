url <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/frankfurt?unitGroup=metric&key=YOUR_API_KEY&contentType=json"
download.file(url, destfile = "aa.json")


api = "0e77aaafab9c0918d2bc7a6e69d567f3"
url <- paste0("http://api.openweathermap.org/data/2.5/forecast?q=Washington&cnt=16&appid=", api)
# url <- paste0("http://api.openweathermap.org/data/2.5/forecast?id=524901&appid=", api)
download.file(url, destfile = "aa.json")

xx <- jsonlite::fromJSON("aa.json")
xx$list

weather <- cbind(
  xx$list$dt_txt,
  do.call(rbind, xx$list$weather)
)
weather

