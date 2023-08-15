library(ggplot2)
library(dplyr)

attach(bank_marketing)
df = bank_marketing

table(job)
barplot(sort(table(job), decreasing = T))
df

df1 = as.data.frame(table(job))
df1
ggplot(data = df1, aes(x = reorder(job, +Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, size = 10), axis.title.y = element_text(angle = 0))


percentageDf = df%>%
  group_by(marital)%>%
  count()%>%
  mutate(percentage = n/nrow(bank_marketing)*100)

percentageDf


ggplot(percentageDf, aes(x = marital, y = percentage)) +
  geom_bar(stat = "identity")

barplot(table(y, marital), beside = F)

ggplot(df, aes(x = education, fill = y)) +
  geom_bar(position = "fill")

ggplot(df, aes(x = age)) +
  geom_dotplot(binwidth = 0.1)

ggplot(df, aes(x = age)) +
  geom_histogram(fill = "tomato", color = "green", binwidth = 1)

ggplot(df, aes(x = y, y = age)) +
  geom_boxplot()














  
  
  
  
  
  
  
  
   
  