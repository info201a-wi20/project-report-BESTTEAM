library(httr)
library(tidyr)
library(dplyr)
library(jsonlite)


# Usage: f: from date "yyyy-mm-dd", t: to date "yyyy-mm-dd", country_code:
# country code of country ("US", "CN")
getStock <- function(f, t, country_code) {
  query = list(region = country_code, lang = "en", symbol = "NBEV", from = toString(as.numeric(as.POSIXct(f))), to = toString(as.numeric(as.POSIXct(t))), events = "div", interval = "1d")
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
  colnames(df) <- c("Date", "low", "volume", "close", "open", "high")
  return(df)
}

getVirus <- function() {
  path <- paste0(getwd(), "/data/cov_data/covid_19_data.csv")
  c <- read.csv(path, stringsAsFactors = FALSE)
  c <- c[-1]
  names <- colnames(c)
  names[1] <- "Date"
  colnames(c) <- names
  return(c)
}







