crime = table(df$`Crm Cd Desc`)
names(sort(crime, decreasing = T))[1:20]
names(crime)



battery = df%>%
  filter(str_detect(`Crm Cd Desc`, "BATTERY"))
table(battery$`Crm Cd Desc`)
battery = battery %>%
  mutate(crimeType = "battery")


theft = df%>%
  filter(str_detect(`Crm Cd Desc`, "THEFT OF IDENTITY")) %>%
  mutate(crimeType = "identity theft")
table(theft$`Crm Cd Desc`)

shoplifting = df%>%
  filter(str_detect(`Crm Cd Desc`, "SHOPLIFTING")) %>%
  mutate(crimeType = "shoplifting")
table(shoplifting$`Crm Cd Desc`)

pickpoketing = df%>%
  filter(str_detect(`Crm Cd Desc`, "PICKPOCKET") | str_detect(`Crm Cd Desc`, "PURSE")) %>%
  mutate(crimeType = "pickpoketing")
table(pickpoketing$`Crm Cd Desc`)

vehicle = df%>%
  filter(str_detect(`Crm Cd Desc`, "VEHICLE") & str_detect(`Crm Cd Desc`, "STOLEN")) %>%
  mutate(crimeType = "vehicle theft")
table(vehicle$`Crm Cd Desc`)

robbery = df%>%
  filter(str_detect(`Crm Cd Desc`, "ROBBERY")) %>%
  mutate(crimeType = "robbery")
table(robbery$`Crm Cd Desc`)

burglary = df%>%
  filter(str_detect(`Crm Cd Desc`, "BURGLARY")) %>%
  mutate(crimeType = "burglary")
table(burglary$`Crm Cd Desc`)

assault = df%>%
  filter(str_detect(`Crm Cd Desc`, "ASSAULT WITH DEADLY WEAPON") | str_detect(`Crm Cd Desc`, "INTIMATE")) %>%
  mutate(crimeType = "assault")
table(assault$`Crm Cd Desc`)

arson = df%>%
  filter(str_detect(`Crm Cd Desc`, "ARSON")) %>%
  mutate(crimeType = "arson")
table(arson$`Crm Cd Desc`)

myCrime = rbind(battery, shoplifting, assault, vehicle, pickpoketing, arson, burglary, robbery, theft)
tcrime = as.data.frame(table(myCrime$crimeType))
tcrime

homicide = df%>%filter(`Crm Cd Desc` == "CRIMINAL HOMICIDE")

ggplot(tcrime, aes(y = reorder(Var1, Freq), x = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(y = "crime type", title = "types of crime") + 
  theme_dark()

ggplot(data = subset(myCrime, !is.na(`Vict Sex`) & myCrime$`Vict Sex` != "H"), aes(y = crimeType, fill = `Vict Sex`)) +
  geom_bar(position = "fill") +
  labs(x = "percentage", y = "crime type", title = "victime percentage by gender by crime type")

View(myCrime)

