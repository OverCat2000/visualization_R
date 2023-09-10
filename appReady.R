library(dplyr)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(zoo)
library(tidyverse)

ggmap::register_google(key = "AIzaSyCKb1WLoI5A3Lme4MveEOSujqPl4E5LfPw", write = TRUE)

act = unique(myCrime$crimeType)
act

map = function(para) {
  get_googlemap(center = "los Angeles") %>% 
    ggmap() +
    geom_point(data = myCrime %>%
                 filter(crimeType == para) %>%
                 filter(LON != 0 & LAT != 0),
               aes(x = LON, y = LAT), color = "tomato") +
    labs(title = "arson crime map of los angeles")
}

map(act[6])

test =myCrime %>%
  group_by(stdTime, crimeType) %>%
  tally()
test = as.data.frame(test)

beta = test %>%
  group_by(crimeType) %>%
  mutate(MA = rollmean(n, 20, fill = NA))
beta = as.data.frame(beta)

ggplot(beta, mapping = aes(x = as.Date(stdTime), y = MA, col = crimeType)) +
         geom_line()

# max = max(as.Date(arsonTS$Var1))
# min = min(as.Date(arsonTS$Var1))
# arsontime = seq.Date(min, max, "day")
# length(arsonTS$Var1)
# length(arsontime$arsontime)
# arsonTS
# arsonTS[c(1, 4)]
# arsontime = as.data.frame(arsontime)
# dim(arsontime)
# 
# marson = merge(arsonTS[c(1, 2)], arsontime, by.x = "Var1", by.y = "arsontime", all.y = T)
# dim(marson)
# marson[is.na(marson)] 
# sum(is.na(marson))

max = max(as.Date(myCrime$stdTime))
min = min(as.Date(myCrime$stdTime))




time = function(crm) {
 temp = beta %>%
   filter(crimeType == crm)
 tempPlot = xts(temp$MA, temp$stdTime)
 return(tempPlot)
}

for (i in 3:9) {
  mer = merge(mer, time(act[i]))
}

maxCord = function(LON) {
  -max(abs(LON))
}



LONcoord = myCrime %>%
  group_by(`AREA NAME`) %>%
  filter(LON != 0 & LAT != 0) %>%
  summarise(LO = maxCord(LON))

LATcoord = myCrime %>%
  group_by(`AREA NAME`) %>%
  filter(LON != 0 & LAT != 0) %>%
  summarise(LA = -maxCord(LAT))

coord = merge(LONcoord, LATcoord)

coord = as.data.frame(coord)

as.numeric(coord[coord$`AREA NAME` == area[2], ][2])

coord





