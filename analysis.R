library(ggplot2)
library(dplyr)
library(plyr)
library(stringr)

attach(Crime_Data_from_2020_to_Present)

colnames(Crime_Data_from_2020_to_Present)

df = Crime_Data_from_2020_to_Present
df


descent = c('A', 'B', 'C', 'D', 'F', 'G', 'H', 'I',
            'J', 'K', 'L', 'O', 'P', 'S', 'U', 'V', 
            'W', 'X', 'Z')

race = c("asian", "black", "chinese", "cambodian",
         "filipino", "guamannina", "hispanic", "american indian",
         "japanese", "korean", "laotian", "other",
         "pacific", "samoan", "hawaiian", "vietnamese",
         "white", "unknown", "asian indina")

length(descent)
length(race)

df$race = mapvalues(df$`Vict Descent`, from = descent,
                    to = race)

df[c('race', 'Vict Descent')]

###################race of the victims#####################
ggplot(df, aes(x = race)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

######################areas where crime took place###################
ggplot(df, aes(x = `AREA NAME`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

################structures where crime took place#########################
place = table(factor(df$`Premis Desc`))
place = sort(place, decreasing = T)
topPlace = place[1:20]
topPlaceN = names(topPlace)
topPlaceN

filterPlace = df%>%filter(`Premis Desc` %in% topPlaceN)

ggplot(filterPlace, aes(x = `Premis Desc`)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 5, angle = 75, hjust = 1))

###############################crimes################################
crime = table(df$`Crm Cd Desc`)
crime = sort(crime, decreasing = T)
abcCrime = sort(names(crime))
write.table(abcCrime, "crime.txt")
topCrime = crime[1:20]
topCrimeN = names(topCrime)
topCrime

filterCrime = df%>%filter(`Crm Cd Desc` %in% topCrimeN)

ggplot(filterCrime, aes(x = `Crm Cd Desc`)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))

####################################weapons used##################
weapon = table(df$`Weapon Desc`)
weapon = sort(weapon, decreasing = T)
topWeapon = weapon[1:20]
topWeaponN = names(topWeapon)
topWeaponN

filterWeapon = df%>%filter(`Weapon Desc` %in% topWeaponN)

ggplot(filterWeapon, aes(x = `Weapon Desc`)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))

#########################time#############################################
df$time = format(round(as.POSIXct(df$`TIME OCC`, format = "%H%M"), units = "hours"), format = "%H:%M")
df$time

ggplot(df, aes(x = time)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))

#######################################period##############################
df$period = as.Date(df$`Date Rptd`, format = "%m/%d/%Y") - as.Date(df$`DATE OCC`, format = "%m/%d/%Y")
bool = ifelse(df$period < 30, F, T)
verPeriod = df$period[bool]
verPeriod

ggplot(as.data.frame(verPeriod), aes(x = verPeriod)) +
  geom_histogram()

###################################month##################################
df$month = factor(month.abb[as.integer(format(as.POSIXct(df$`DATE OCC`, format = "%m/%d/%y"), format = "%m"))], levels = month.abb)
df[c("DATE OCC", "month")]

ggplot(df, aes(x = month)) +
  geom_bar()




#############################################################################

#########################criminal homicide##############################
homicide = df%>%filter(`Crm Cd Desc` == "CRIMINAL HOMICIDE")

ggplot(homicide, aes(x = `AREA NAME`)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))

ggplot(homicide, aes(x = `Weapon Desc`)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))

ggplot(homicide, aes(x = `Vict Sex`)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))

ggplot(homicide, aes(x = `Vict Age`)) +
  geom_histogram()

ggplot(homicide, aes(x = time)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))

ggplot(homicide, aes(x = month)) +
  geom_bar()


ggplot(homicide, aes(x = race)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))



#############################shoplifting####################################
shoplifting = df%>%
  filter(str_detect(`Crm Cd Desc`, "SHOPLIFTING"))

unique(shoplifting$`Crm Cd Desc`)

ggplot(shoplifting, aes(x = month)) +
  geom_bar()

ggplot(shoplifting, aes(x = `AREA NAME`)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))

ggplot(shoplifting, aes(x = time)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))
























  


  








