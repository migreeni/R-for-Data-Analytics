library(dplyr)

t <- read.csv("titanic.csv")

# 1

head(x <- t %>% select(Name, Sex, Age, Survived), 5)

# 2

cabin <- t %>% 
  select(Cabin) %>% 
  filter(!is.na(Cabin) & Cabin != "")
unique(cabin)

# 3

fare_mean <- t %>% 
  group_by(Survived) %>% 
  summarise(Fare_maen = mean(Fare))
fare_mean

#4

t %>% 
  filter(Age <= 20) %>% 
  group_by(Pclass) %>% 
  summarise(Class_Count = n(), Fare_mean = mean(Fare))

#5

head(th <- t %>%  filter(grepl("th", Name, ignore.case = TRUE)), 5)

# 6

# na value check in Age
table(is.na(t$Age))

t %>%
  filter(!is.na(Age)) %>% # there is na value in Age, so filter na needed
  group_by(Pclass, Sex) %>% 
  summarize(Age_mean = mean(Age)) %>% 
  arrange(desc(Age_mean))

# 7

t %>% 
  filter(Age <= 15) %>% 
  group_by(Pclass) %>% 
  summarise(Total_count = n(), Survived_count = sum(Survived==1)) %>% 
  mutate(Surv_percentage = sprintf("%.2f%%", Survived_count / Total_count * 100)) %>% 
  select(Pclass, Surv_percentage)

#8

head(na.omit(t) %>% filter(!is.na(Name) & !is.na(Age) & !is.na(Sex) & !is.na(Survived)) %>% 
  mutate(Summary = paste(paste("Name:", Name), paste("Age:", Age), paste("Sex:", Sex), paste("Survived:", ifelse(Survived==1, "Yes", "No")), sep = "|")),
  5)

#9

head(na.omit(t)  %>% 
  mutate(Passenger_char = case_when(
    Age >= 30 & Sex == "male" ~ "male_over_30",
    Age < 30 & Sex == "male" ~ "male_under_30",
    Age >= 30 & Sex == "female" ~ "female_over_30",
    Age < 30 & Sex == "female" ~ "female_under_30"
  )) %>% 
  select(Age, Sex, Passenger_char),
  5)

#10

na.omit(t)  %>% 
  mutate(Passenger_char = case_when(
    Age >= 30 & Sex == "male" ~ "male_over_30",
    Age < 30 & Sex == "male" ~ "male_under_30",
    Age >= 30 & Sex == "female" ~ "female_over_30",
    Age < 30 & Sex == "female" ~ "female_under_30"
  )) %>% 
  group_by(Pclass, Passenger_char) %>% 
  summarise(Total_count = n(), Death_count = sum(Survived == 0)) %>% 
  mutate(Death_percentage = sprintf("%.2f%%", Death_count / Total_count * 100)) %>% 
  select(Pclass, Passenger_char, Death_percentage)
