library(ggmap)
library(ggpubr)

conTable = as.data.frame(table(myCrime$crimeType, myCrime$`AREA NAME`))
ggballoonplot(conTable, fill = "value") +
  gradient_fill("blue") +
  labs(x = "crime", y = "area", title = "balloonplot of crimes with respect to area")+
  theme_dark()

timeCrime = as.data.frame(table(myCrime$crimeType, myCrime$time))
timeCrime
ggballoonplot(timeCrime, fill = "Freq") +
  gradient_fill("red")+
  # scale_fill_gradientn(colors = c("#ffcccc", "#ff8080", "#ff4d4d", "#ff0000")) +
  labs(x = "crime", y = "time", title = "balloonplot of crimes with respect to time")+
  theme_dark()


ggmap::register_google(key = "AIzaSyCKb1WLoI5A3Lme4MveEOSujqPl4E5LfPw", write = TRUE)


get_googlemap(center = "los Angeles") %>% 
  ggmap() +
  geom_point(data = arson, aes(x = LON, y = LAT), color = "tomato") +
  labs(title = "arson crime map of los angeles")

get_googlemap(center = "los Angeles") %>% 
  ggmap() +
  geom_point(data = homicide, aes(x = LON, y = LAT), color = "tomato", alpha = 0.3) +
  labs(title = "homicide crime map of los angeles")


street77 = assault %>%
  filter(`AREA NAME` == "77th Street")
lon = max(abs(street77$LON))
lat = max(abs(street77$LAT))

View(street77)

get_googlemap(center = c(x = -118.30, y = 33.97), zoom = 13) %>%
  ggmap() +
  geom_point(data = street77, aes(x = LON, y = LAT), color = "lightcoral")

Hollywood = vehicle %>%
  filter(`AREA NAME` == "Hollywood")
lon = max(abs(Hollywood$LON))
lat = max(abs(Hollywood$LAT))
lon
# Hollywood = myCrime %>%
#     filter(`AREA NAME` == "Hollywood")

get_googlemap(center = c(x = -118.35, y = 34.12), zoom = 13) %>%
  ggmap() +
  geom_point(data = Hollywood, aes(x = LON, y = LAT), color = "tomato") +
  labs(title = "vehicle stolen in Hollywood")

get_googlemap(center = "los Angeles") %>% 
  ggmap() +
  geom_point(data = sample_n(myCrime, 10000), aes(x = LON, y = LAT, color = crimeType), alpha = 0.3) +
  geom_point(data = areas, mapping = aes(x = LON_name, y = LAT_name), shape = 18, size = 3) +
  geom_text(data = areas, mapping = aes(x = LON_name, y = LAT_name, label = `AREA NAME`), vjust = -1, color = "black", size = 4) +
  scale_color_brewer(palette= "Oranges")

# p + geom_point(data = areas, aes(x = LON_name, y = LAT_name), shape = 17, size = 5) +
  # geom_text(aes(label = areas$`AREA NAME`))

get_googlemap(center = "los Angeles") %>% 
  ggmap() +
  geom_point(data = areas, mapping = aes(x = LON_name, y = LAT_name), shape = 17, size = 5) +
  geom_text(data = areas, mapping = aes(x = LON_name, y = LAT_name, label = `AREA NAME`), vjust = -1, color = "red")
  

areas = myCrime %>%
  group_by(`AREA NAME`) %>%
  filter(LON != 0 & LAT != 0) %>%
  summarise_at(vars(LON, LAT), list(name = mean))
View(areas)

  










    
    
    
    
