library(dplyr)

#11 

red = read.csv("winequality-red.csv", sep=";")
white = read.csv("winequality-white.csv", sep=";")

red["type"] <- "red"
white["type"] <- "white"

wine <- bind_rows(red, white) 

#12

wine[,grep("dioxide", colnames(wine))]
wine %>% select(contains("dioxide"))

#13

s = wine %>% filter(density > mean(wine$density)) %>% arrange(-volatile.acidity)
head(s, 1)

s = wine %>% filter(density > mean(wine$density))
s[which.max(s$volatile.acidity), ]

#14

wine2 <- wine %>% mutate(dioxide_ratio =  free.sulfur.dioxide / total.sulfur.dioxide)
wine2

#15

s = wine2 %>% filter(quality == 5) %>% arrange(fixed.acidity)
bind_rows(head(s, 1)[c("fixed.acidity","volatile.acidity")], tail(s, 1)[c("fixed.acidity","volatile.acidity")]) %>% arrange(-fixed.acidity)

#16

aggregate(residual.sugar ~ quality < 6, wine2, mean)
aggregate(residual.sugar ~ quality >= 6 & quality < 7, wine2, mean)
aggregate(residual.sugar ~ quality >= 7, wine2, mean)

# other way
tapply(wine2$residual.sugar, wine2$quality < 6, mean)
tapply(wine2$residual.sugar, wine2$quality >= 6 & wine2$quality < 7, mean)
tapply(wine2$residual.sugar, wine2$quality >= 7, mean)

#17

wine3 <- wine2 %>% mutate(qlabel = case_when(
  quality < 6 ~ "L",
  quality >= 6 & quality < 7 ~ "M",
  quality >= 7 ~ "H"))
wine3

#18

wine3 %>% group_by(qlabel) %>% summarise(mean_pH = mean(pH), mean_citric.acid = mean(citric.acid))

#19

# row count for validation
list(all = nrow(wine3), red = nrow(subset(wine3, type=="red")), white = nrow(subset(wine3, type=="white")))

wine3.1 <- wine3 %>% group_by(type, qlabel) %>% summarise(count = n())
wine3.1 %>% group_by(type) %>% mutate(type_count = sum(count), ratio = count / sum(count))

#20

wine4 <- wine3 %>% mutate(acid_mean = (volatile.acidity + citric.acid) / 2)
wine4
head(wine4 %>% filter(dioxide_ratio > acid_mean) %>% arrange(-dioxide_ratio), 5)
