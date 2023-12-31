library(dplyr)
library(ggplot2)
library(plyr)
library(stringr)
library(gridExtra)

df = Crime_Data_from_2020_to_Present
dft = sample_n(df, 1000)

#creating year column
df$year = as.integer(format(as.POSIXct(df$`DATE OCC`, 
                                       format = "%m/%d/%Y"),
                            format = "%Y")
                     )

ggplot(data = subset(df, year != 2023), aes(x = year)) +
  geom_bar(fill = "lightblue") +
  theme_dark() +
  labs(title = "crime count by year")

#//disabled

# #dataframes by year
# df2020 = df%>%
#   filter(year == 2020)
# df2021 = df%>%
#   filter(year == 2021)
# df2022 = df%>%
#   filter(year == 2022)
# df2023 = df%>%
#   filter(year == 2023)

#disabled//

#creating month column
df$month = factor(month.abb[as.integer(format(as.POSIXct(df$`DATE OCC`, 
                                                         format = "%m/%d/%Y"), 
                                              format = "%m")
                                       )
                            ], 
                  levels = month.abb)

ggplot(data = subset(df, year != 2023), aes(x = as.factor(year), fill = month)) +
  geom_bar(position = "dodge") +
  labs(x = "year", title = "crime count by year by month") +
  theme_dark()

#creating weekday column
days = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
df$weekDay =  factor(weekdays(as.Date(df$`DATE OCC`, format = "%m/%d/%y")), levels = days)

ggplot(data = subset(df, year != 2023), aes(x = weekDay)) +
  geom_bar()

#rounding time to the hour
df$time = format(round(as.POSIXct(df$`TIME OCC`, format = "%H%M"), units = "hours"), format = "%H:%M")

df$ctime = format(as.POSIXct(df$`TIME OCC`, format = "%H%M"), format = "%H:%M")

df[, c("ctime", "TIME OCC")]

ggplot(df, aes(x = time)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 75, hjust = 1))



#recoding victim descent
descent = c('A', 'B', 'C', 'D', 'F', 'G', 'H', 'I',
            'J', 'K', 'L', 'O', 'P', 'S', 'U', 'V', 
            'W', 'X', 'Z')
race = c("asian", "black", "chinese", "cambodian",
         "filipino", "guamannina", "hispanic", "american indian",
         "japanese", "korean", "laotian", "other",
         "pacific", "samoan", "hawaiian", "vietnamese",
         "white", "unknown", "asian indina")

df$race = mapvalues(df$`Vict Descent`, from = descent, to = race)
table(df$race)
people = table(df$race)
people = as.data.frame(people[people > 15000])
people
total = sum(people$Freq)

ggplot(data= people, aes(x = "",y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(x = "race", y = "count", title = "victims by race") +
  geom_text(aes(label = paste0(round(Freq/total*100, 2), "%")), position = position_stack(vjust= 0.5)) +
  theme_void()


df$stdTime = as.Date(df$`DATE OCC`, format = "%m/%d/%Y")

p1 = ggplot(data = df, aes(y = `Vict Age`)) +
  geom_boxplot(color = "coral", fill = "cadetblue") +
  theme_grey()
p2 = ggplot(data = subset(df, `Vict Age`!= 0), aes(x = `Vict Age`)) +
  geom_histogram(fill = "cadetblue", color = "coral") +
  theme_grey()
grid.arrange(p1, p2, nrow = 1)


View(df)



  
  
  
  
  
  
  



