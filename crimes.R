library(lubridate)
library(zoo)


crimeTS = as.data.frame(table(df$stdTime))
crimeTS$year = year(as.Date(crimeTS$Var1))
crimeTS = crimeTS %>%
  mutate(MA = rollmean(Freq, 20, fill = NA))

ggplot(crimeTS, aes(x = as.Date(Var1), y = MA)) + 
  geom_line(color = "Brown", linewidth = 1) + 
  labs(x = "date", y = "crimes", title = "time series by year") +
  theme_grey()


#shoplifting
shopliftingTS = as.data.frame(table(shoplifting$stdTime))
shopliftingTS$year = year(as.Date(shopliftingTS$Var1))
shopliftingTS = shopliftingTS%>%
  mutate(MA = rollmean(Freq, 20, fill = NA))
shopliftingTS

#burglary
burglaryTS = as.data.frame(table(burglary$stdTime))
burglaryTS$year = year(as.Date(burglaryTS$Var1))
burglaryTS = burglaryTS%>%
  mutate(MA = rollmean(Freq, 20, fill = NA))

#arson
arsonTS = as.data.frame(table(arson$stdTime))
arsonTS$year = year(as.Date(arsonTS$Var1))
arsonTS = arsonTS%>%
  mutate(MA = rollmean(Freq, 20, fill = NA))

assaultTS = as.data.frame(table(assault$stdTime))
assaultTS$year = year(as.Date(assaultTS$Var1))
assaultTS = assaultTS%>%
  mutate(MA = rollmean(Freq, 20, fill = NA))

vehicleTS = as.data.frame(table(vehicle$stdTime))
vehicle$year = year(as.Date(vehicleTS$Var1))
vehicleTS = vehicleTS%>%
  mutate(MA = rollmean(Freq, 20, fill = NA))

theftTS = as.data.frame(table(theft$stdTime))
theftTS$year = year(as.Date(theftTS$Var1))
theftTS = theftTS%>%
  mutate(MA = rollmean(Freq, 20, fill = NA))

pickpoketingTS = as.data.frame(table(pickpoketing$stdTime))
pickpoketingTS$year = year(as.Date(pickpoketingTS$Var1))
pickpoketingTS = pickpoketingTS%>%
  mutate(MA = rollmean(Freq, 20, fill = NA))

robberyTS = as.data.frame(table(robbery$stdTime))
robberyTS$year = year(as.Date(robberyTS$Var1))
robberyTS = robberyTS%>%
  mutate(MA = rollmean(Freq, 20, fill = NA))

batteryTS = as.data.frame(table(battery$stdTime))
batteryTS$year = year(as.Date(batteryTS$Var1))
batteryTS = batteryTS%>%
  mutate(MA = rollmean(Freq, 20, fill = NA))


ggplot() +
  geom_line(shopliftingTS, mapping = aes(x = as.Date(Var1), y = MA, color = "shop lifting")) +
  geom_line(burglaryTS, mapping = aes(x = as.Date(Var1), y = MA, colour = "burglary")) +
  geom_line(assaultTS, mapping = aes(x = as.Date(Var1), y = MA, colour = "assault")) +
  geom_line(arsonTS, mapping = aes(x = as.Date(Var1), y = MA, color = "arson")) +
  geom_line(vehicleTS, mapping = aes(x = as.Date(Var1), y = MA, , color = "vehicle theft")) +
  geom_line(theftTS, mapping = aes(x = as.Date(Var1), y = MA, , color = "identity theft")) +
  geom_line(pickpoketingTS, mapping = aes(x = as.Date(Var1), y = MA, color = "pickpoketing")) +
  geom_line(batteryTS, mapping = aes(x = as.Date(Var1), y = MA, color = "battery")) +
  geom_line(robberyTS, mapping = aes(x = as.Date(Var1), y = MA, color = "robbery")) +
  labs(x = "time", y = "number of crime", title = "time series of crimes by type") +
  scale_colour_manual("", 
                      breaks = c("shop lifting", "burglary", "assault", "arson", "vehicle theft",
                                 "identity theft", "pickpoketing", "battery", "robbery"),
                      values = c("#86592d", "#006600", "blue", "#ffaa00", "#ffff00","#00b300", "#00ffff",
                                 "#ff33bb", "#ff3300")) +
  
  
  theme_grey()


homicideTS = as.data.frame(table(homicide$stdTime))
homicideTS
homicideTS = homicideTS %>%
  mutate(MA = rollmean(Freq, 20, fill = NA))

homicideTS

ggplot(homicideTS, aes(x = as.Date(Var1), y = MA)) +
  geom_line(color = "red") +
  labs(x = "time", y = "homicide count", title = "time series of homicide") +
  theme_dark()



  ggplot(arsonTS, aes(x = as.Date(Var1), y = Freq)) + 
  geom_line(color = "coral") + 
  facet_wrap(~year, scales = "free") + 
  labs(x = "date", y = "arsons", title = "time series by year for arsons") +
  theme_grey()

ggplot(arson, aes(x = month)) +
  geom_bar(fill = "lightyellow") +
  labs(title = "arson occured in months") +
  theme_dark() +
  theme(axis.text.x = element_text(size = 10))

ggplot(shopliftingTS, aes(x = as.Date(Var1), y = Freq)) + 
  geom_line(color = "coral") + 
  facet_wrap(~year, scales = "free") + 
  labs(x = "date", y = "shopliftings", title = "time series by year for shopliftings") +
  theme_grey()

ggplot(shoplifting, aes(x = time)) +
  geom_bar(fill = "lightpink") +
  labs(title = "shoplifting occured time") +
  theme_dark() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))

ggplot(shoplifting, aes(x = month)) +
  geom_bar(fill = "lightyellow") +
  labs(title = "shoplifting occured in months") +
  theme_dark() +
  theme(axis.text.x = element_text(size = 10))

ggplot(burglaryTS, aes(x = as.Date(Var1), y = Freq)) + 
  geom_line(color = "coral") + 
  facet_wrap(~year, scales = "free") + 
  labs(x = "date", y = "burglarys", title = "time series by year for burglarys") +
  theme_grey()

ggplot(burglary, aes(x = time)) +
  geom_bar(fill = "ivory") +
  labs(title = "burglarys occured time") +
  theme_dark() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))






View(df)
  
  



