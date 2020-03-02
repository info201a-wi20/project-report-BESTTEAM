#What is the comparison of the death rate among all countries/regions?
source("GetData.R")
library("dplyr")
library("ggplot2")
library("wbstats")
library("maps")

virus <- getVirus()
View(virus)

map_gg <- map_data("world") 
map_gg <- mutate(map_gg, iso3c = iso.alpha(map_gg$region, n = 3))
View(map_gg)
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
