library(lubridate)

df$stdTime = as.Date(df$`DATE OCC`, format = "%m/%d/%Y")
crimeTS = as.data.frame(table(df$stdTime))
crimeTS$year = year(as.Date(crimeTS$Var1))
crimeTS

ggplot(crimeTS, aes(x = as.Date(Var1), y = Freq)) + 
  geom_line(color = "coral") + 
  facet_wrap(~year, scales = "free") + 
  labs(x = "date", y = "crimes", title = "time series by year") +
  theme_grey()

#shoplifting
shopliftingTS = as.data.frame(table(shoplifting$stdTime))
shopliftingTS$year = year(as.Date(shopliftingTS$Var1))
shopliftingTS

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

#burglary
burglaryTS = as.data.frame(table(burglary$stdTime))
burglaryTS$year = year(as.Date(burglaryTS$Var1))
burglaryTS

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

#arson
arsonTS = as.data.frame(table(arson$stdTime))
arsonTS$year = year(as.Date(arsonTS$Var1))
arsonTS

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


  
  



