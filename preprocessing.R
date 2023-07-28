library(dplyr)
library(ggplot2)

df = Crime_Data_from_2020_to_Present
dft = sample_n(df, 1000)

#-----CREATING_YEAR_COLUMN-----
df$year = as.integer(format(as.POSIXct(df$`DATE OCC`, 
                                       format = "%m/%d/%Y"),
                            format = "%Y")
                     )

ggplot(data = subset(df, year != 2023), aes(x = year)) +
  geom_bar()

# #dataframes by year
# df2020 = df%>%
#   filter(year == 2020)
# df2021 = df%>%
#   filter(year == 2021)
# df2022 = df%>%
#   filter(year == 2022)
# df2023 = df%>%
#   filter(year == 2023)

#-----CREATING_MONTH_COLUMN-----
df$month = factor(month.abb[as.integer(format(as.POSIXct(df$`DATE OCC`, 
                                                         format = "%m/%d/%Y"), 
                                              format = "%m")
                                       )
                            ], 
                  levels = month.abb)

ggplot(data = subset(df, year != 2023), aes(x = as.factor(year), fill = month)) +
  geom_bar(position = "dodge")

#-----CREATING_WEEK_DAY_COLUMN-----
df$weekDay =  weekdays(as.Date(df$`DATE OCC`, format = "%m/%d/%y"))

ggplot(data = subset(df, year != 2023), aes(x = weekDay)) +
  geom_bar()
 
View(df) 
  
  
  
  
  
  
  



