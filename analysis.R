library("ggplot2")

source(paste0(getwd(), "/GetData.R"))


# Death Rate vs Survival Rate

virus <- getVirus()
processed <- v %>% group_by(Date) %>%
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

