library(httr)
library(tidyr)
library(jsonlite)
library (plyr)
library(data.table)

getStock <- function(f, t) {
  query = list(region = "US", lang = "en", symbol = "NBEV", from = toString(as.numeric(as.POSIXct(f))), to = toString(as.numeric(as.POSIXct(t))), events = "div", interval = "1d")
  res = GET("https://apidojo-yahoo-finance-v1.p.rapidapi.com/stock/get-histories", add_headers("X-RapidAPI-Host" = "apidojo-yahoo-finance-v1.p.rapidapi.com", "X-RapidAPI-Key" = "d83c0bf332mshff17063a7353c72p1c419cjsn43de5782fd8d"), query = query)
  c = content(res, "text")
  c = fromJSON(c)
  t <- as.Date(as.POSIXct(c$chart$result$timestamp[[1]], origin="1970-01-01"))
  l <- c$chart$result$indicators$quote[[1]]$low[[1]]
  v <- c$chart$result$indicators$quote[[1]]$volume[[1]]
  cl <- c$chart$result$indicators$quote[[1]]$close[[1]]
  o <- c$chart$result$indicators$quote[[1]]$open[[1]]
  h <- c$chart$result$indicators$quote[[1]]$high[[1]]
  df <- data.frame(t, l, v, cl, o, h)
  colnames(df) <- c("timestamp", "low", "volume", "close", "open", "high")
  return(df)
}

