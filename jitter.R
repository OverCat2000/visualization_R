jitter = homicide %>%
  filter(`Vict Age` != 0) %>%
  filter(!is.na(race))

set.seed(1234)
sample = sample_n(jitter, 1000)

ggplot(jitter, aes(x = `Vict Age`, y = race, color = `Vict Sex`)) +
  geom_jitter(width = 0.3, height = 0.4) +
  labs(x = "victime age", y = "race", title = "age of homicide victims categorized by race and gender") +
  theme_dark()

areaCrime = homicide %>%
  group_by(stdTime, `AREA NAME`) %>%
  tally()

areaCrime = as.data.frame(areaCrime)
areaCrime

areaCrime = areaCrime %>%
  mutate(MA = rollmean(n, 10, fill = NA))

areaCrime

ggplot(areaCrime, aes(x = as.Date(stdTime), y = n)) + 
  facet_wrap(~areaCrime$`AREA NAME`, ncol = 7, nrow = 3) +
  geom_line(color = "red", linewidth = 1) + 
  labs(x = "date", y = "crimes", title = "time series by area for homicide") +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))

length(unique(areaCrime$`AREA NAME`))
 