attach(bank_marketing)

barplot(sort(table(job), decreasing = T), cex.names = 1, cex.axis = 1)

library(ggplot2)

ggplot(data = bank_marketing, mapping = aes(x = job)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45))

p = ggplot(data = subset(bank_marketing, !is.na(marital)), mapping = aes(x = marital)) +
  geom_bar() +
  coord_flip()
p


df_marital = as.data.frame(table(marital))
df_marital

p1 = ggplot(df_marital, aes(x = reorder(marital, -Freq), y = Freq)) +
  geom_bar(stat = "identity")
p1

library(dplyr)

perc_marital = bank_marketing%>%group_by(marital)%>%count()%>%mutate(percentage = n/nrow(bank_marketing)*100)
perc_marital

ggplot(perc_marital, aes(x = marital, y = percentage)) +
  geom_bar(stat = "identity")

ggplot(perc_marital, aes(x = reorder(marital, -percentage),y = percentage)) +
  geom_bar(stat = "identity")

table(marital, education)

marital_y = bank_marketing%>%group_by(y, education)%>%count()
marital_y

barplot(table(y, education), beside = T)

ggplot(bank_marketing, aes(x = education, fill = y)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 30))

ggplot(data = subset(bank_marketing, !is.na(education)), aes(x = education, fill = y)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 30))

ggplot(data = subset(bank_marketing, !is.na(education)), aes(x = education, fill = y)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 30))

ggplot(bank_marketing, aes(x = age)) +
  geom_dotplot(binwidth = 1)


ggplot(bank_marketing, aes(x = age)) +
  geom_histogram(fill = "red", color = "navy")

ggplot(bank_marketing, aes(x = age)) +
  geom_density(fill = "red", color = "navy")

ggplot(bank_marketing, aes(y = age)) +
  geom_boxplot(fill = "tomato", color = "hotpink")

ggplot(bank_marketing, aes(y = age, x = y)) +
  geom_boxplot(fill = "coral", color = "magenta")

ggplot(bank_marketing, aes(x = cons.price.idx, y = cons.conf.idx)) +
  geom_point(color = "navy") +
  geom_smooth()





