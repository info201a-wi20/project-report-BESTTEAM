library("tidyr")
library("dplyr")
library("tibble")
library("haven")
library(help = "datasets")
library("ggplot2")
library("wbstats")
library("maps")

source(paste0(getwd(), "/GetData.R"))

# Sample of sources
path <- paste0(getwd(), "/daily_csv.csv")
natural_gas <- read.csv(path, stringsAsFactors = FALSE)
ng <- natural_gas[5794:5813,]
ng$Date <- as.Date(ng$Date)
stock_sample <- getStock("2020-01-22", "2020-02-02", "US") %>% left_join(ng, by = "Date") %>% top_n(3)
virus_sample <- getVirus() %>% select(Date, Province.State, Country.Region, Confirmed, Deaths, Recovered) %>% top_n(3)



# Death Rate vs Survival Rate

virus <- getVirus()
processed <- virus %>% group_by(Date) %>%
  summarise(sum_confirmed = sum(Confirmed),
            sum_death = sum(Deaths),
            sum_recovered = sum(Recovered)) %>%
  mutate(death_rate = sum_death * 100 / sum_confirmed,
         recover_rate = sum_recovered * 100 / sum_confirmed)

processed_China <- virus %>% filter(Country.Region == "Mainland China") %>%
  group_by(Date) %>%
  summarise(sum_confirmed = sum(Confirmed),
            sum_death = sum(Deaths),
            sum_recovered = sum(Recovered)) %>%
  mutate(death_rate = sum_death * 100 / sum_confirmed,
         recover_rate = sum_recovered * 100 / sum_confirmed)

ggplot(data = processed) +
  geom_line(mapping = aes(x = Date, y = death_rate, group = 1, colour = "Death")) +
  geom_line(mapping = aes(x = Date, y = recover_rate, group = 1, colour = "Recover")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Death vs. Recovered",
    x = "Date",
    y = "Rate",
    color = "Legend"
  ) + scale_color_manual(values = c("Death" = "red", "Recover" = "green"))



# Q1: Based on the data set, 
# does the number of coronovirus pneumonia cases confirmed 
# have no effect on the price of natural gas?
path <- paste0(getwd(), "/daily_csv.csv")
natural_gas <- read.csv(path, stringsAsFactors = FALSE)
ng <- natural_gas[5794:5813,]
path1 <- paste0(getwd(), "/confirmed.dta")
confirmed1 <- read_dta(path1)
confirmed <- rename(confirmed1, 
                    "1/22/20" = "v6",
                    "1/23/20" = "v7",
                    "1/24/20" = "v8",
                    "1/25/20" = "v9",
                    "1/26/20" = "v10",
                    "1/27/20" = "v11",
                    "1/28/20" = "v12",
                    "1/29/20" = "v13",
                    "1/30/20" = "v14",
                    "1/31/20" = "v15",
                    "2/1/20" = "v16",
                    "2/2/20" = "v17",
                    "2/3/20" = "v18",
                    "2/4/20" = "v19",
                    "2/5/20" = "v20",
                    "2/6/20" = "v21",
                    "2/7/20" = "v22",
                    "2/8/20" = "v23",
                    "2/9/20" = "v24",
                    "2/10/20" = "v25",
                    "2/11/20" = "v26",
                    "2/12/20" = "v27",
                    "2/13/20" = "v28",
                    "2/14/20" = "v29",
                    "2/15/20" = "v30",
                    "2/16/20" = "v31",
                    "2/17/20" = "v32",
                    "2/18/20" = "v33",
                    "2/19/20" = "v34",
                    "2/20/20" = "v35")
summary(confirmed)
x <- rbind(confirmed, c("Mean", "7.303","8.592","12.38","18.87","27.87","38.51","73.39",
                        "81.13","108.3","130.62","158.4","220.88","261.59","314.37",
                        "363.6","405.5","452.53","488.43","528.3","562.7","589.5","595.0",
                        "794.3", "880.1","908.3","937.2","964.0","988.7", "989", "989.3"))

mean <- x %>% 
  filter(regions == "Mean") %>% 
  gather(key = "Date",
         value = "mean_cases") 

meancases <- mean[-1,]
ng$Date <- as.Date(ng$Date)
meancases$Date <- as.Date(meancases$Date, format = "%m/%d/%y")
joined <- left_join(ng, meancases, by = "Date")

reg <- ggplot(data = joined, mapping = aes(x = as.numeric(reorder(mean_cases, Price)), y = Price))+
  geom_point(color = "blue")+
  labs(title = "Regression of natural gas prices and confirmed cases", 
       x = "number of confirmed cases", 
       y = "price")+
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  theme_bw()

model <- lm(Price ~ as.numeric(mean_cases), data = joined)
print(model)

correlation <- cor(as.numeric(joined$Price), as.numeric(joined$mean_cases))



# Q2: Based on the data set, 
# what is the effect of the number of coronavirus pneumonia cases
#   confirmed have on the volume of the stock trade of [company]? 
stock_df <- getStock("2020-01-22", "2020-02-20", "CN")

virus_df <- read.csv("data/cov_data/time_series_covid_19_confirmed.csv", 
                     stringsAsFactors = FALSE)
stock_df <- stock_df %>% select(Date, volume)
virus_df <- virus_df %>% filter(Country.Region == "Mainland China")

drops <- c("Province.State", "Country.Region", "Lat", "Long")
virus_df <- virus_df[ , !(names(virus_df) %in% drops)]
mean <- virus_df %>% 
  summarise_all("mean")

virus_new <-data.frame(c(mean)) %>% 
  gather(key = Date, value = Confirmed_cases) %>% select(Confirmed_cases)
#date_insert <- c(stock_df %>% pull(Date)) #  length 21

virus_new <- virus_new[-c(4, 5, 6, 11, 12, 18, 19, 22, 23), ]

new_date_frame <- data.frame(stock_df, virus_new)

reg_volume <- ggplot(data = new_date_frame, mapping = aes(x = as.numeric(reorder(volume, virus_new)), y = volume))+
  geom_point(color = "red") +
  labs(title = "Regression of volume amount and confirmed cases", 
       x = "Number of confirmed cases", 
       y = "Volume") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_bw()

correlation_volume <- cor(as.numeric(new_date_frame$virus_new), as.numeric(new_date_frame$volume)) # 0.5493214


#What is the comparison of the recover rate among all countries/regions?

virus <- getVirus()
#View(virus)

map_gg <- map_data("world") 
map_gg <- mutate(map_gg, iso3c = iso.alpha(map_gg$region, n = 3))
#View(map_gg)
virus[virus$Country.Region == "Mainland China", "Country.Region"] = "China"
virus[virus$Country.Region == "Hong Kong", "Country.Region"] = "China"
virus[virus$Country.Region == "Macau", "Country.Region"] = "China"
virus[virus$Country.Region == "Taiwan", "Country.Region"] = "China"
virus <- virus%>%
  group_by(Country.Region)%>%
  summarize(sum_confirm = sum(Confirmed),
            sum_recover = sum(Recovered),
            recover_rate = 100 * sum_recover/sum_confirm)

virus <- mutate(virus, iso3c = iso.alpha(virus$Country.Region, n = 3))
comparision <- left_join(map_gg, virus, by = "iso3c")

recover_compare <- ggplot(data = comparision) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = recover_rate)) +
  labs(title = "Recovery rate of Cononavirus among all countries/regions") +
  coord_quickmap() +
  theme_void()



