install.packages("dplyr")
library(dplyr)
titanic_file <- read.csv("titanic.csv")
titanic <- as_tibble(titanic_file)

# Q1
data1 <- select(titanic, c(Name,Sex,Age,Survived))
# data1에 titanic 데이터의 Name,Sex,Age,Survived 값만 선택하여 저장
head(data1,5)   #data1의 첫 5행만 출력

# Q2
data2 <- titanic %>% select('Cabin') %>% filter(Cabin!="") %>% unique
# data2에 Cabin 값 중 ""값은 제외하고 유일한 값들만 저장
head(data2, 5)   #data2의 첫 5행만 출력

# Q3
data3 <- titanic %>% group_by(Survived) %>%   #생존 여부에 따라 group_by하고
  summarise(Fare_mean = mean(Fare, na.rm=TRUE))   #결측치를 제거한 Fare값의 평균을 Fare_mean에 저장
head(data3) #data3 출력

# Q4
data4 <- titanic %>% filter(Age<=20) %>% group_by(Pclass) %>%
  #titanic 값 중에 Age가 20세 이하인 승객 중 Pclass에 대하여 group_by
  summarise(Class_count = n(), Fare_mean = mean(Fare, na.rm=TRUE))
#Class count 값에는 승객의 수, Fare_mean에는 결측치를 제외한 Fare의평균을 저장
head(data4 %>% arrange(desc(Pclass)))  #Pclass가 내림차순이 되도록 출력 

# Q5
data5 <- titanic %>% filter (grepl('th',Name, ignore.case=TRUE)) %>% select(PassengerId, Name)
#grepl 함수를 이용해 Name에 'th'가 들어간 행을 data5에 저장. 대소문자 구별하지 않기 위해 ignore.case=TRUE를 해준다.
#그 중에서 PassenferId와 Name 변수만 선택
head(data5, 5)  #data5의 첫 5행만 출력

# Q6 
data6 <- titanic %>% group_by(Pclass, Sex) %>%   # Pclass, Sex에 대해 group_by
  summarise(Age_mean = mean(Age, na.rm=TRUE))
#Age_mean에 결측치를 제외한 Age의 평균값을 저장
head(data6 %>% arrange(desc(Age_mean))) #Age_mean이 내림차순이 되도록 data6 출력

# Q7 
data7 <- titanic %>% group_by(Pclass) %>%   #Pclass에 대해서 group_by
  summarise(Surv_percentage = sprintf("%.2f%%", (sum(Survived==1) / n() * 100)))
  #Surv_percentage에 (Survived==1인 승객 수*100 / 그룹별 전체 승객 수)의 값을 sprintf로 소수점 두자리수까지만 저장하고 뒤에 %를 붙여줌.
head(data7 %>% arrange(Pclass))
#Pclass가 오름차순이 되도록 data7 출력

# Q8  
tit <- titanic %>% filter(!is.na(Name) & !is.na(Age) & !is.na(Sex) & !is.na(Survived))
# tit에 Name, Age, Sex, Survived 변수의 결측치가 있는 행 제거한 데이터 저장
data8 <- tit %>% mutate(Summary = paste0('Name: ', Name, '| Age: ', Age, ' Sex: ', Sex, ' | Survived: ', ifelse(Survived==1, 'Yes', 'NO')))
# Summary 변수에 문제에서 요구하는 형식으로 저장, 이때 Survived는 ifelse를 사용하여 Yes, No 대입 
head(data8['Summary'],5)
# head()를 사용하여 data8 중 Summary변수만 첫 5행을 출력한다.

# Q9  
titanic2 <- titanic %>% filter(!is.na(Sex) & !is.na(Age)) %>%
  # Sex와 Age의 결측치 제거
  mutate(Passenger_char = (ifelse(Age>=30, paste0(Sex,'_over_30'), paste0(Sex,'_under_30'))))
  # 문제의 조건에 맞게 나이에 따라 paste0(sex,'~')를 Passenger_char에 넣어 mutate해준다.
data9 <- titanic2 %>% select(Age, Sex, Passenger_char)
# 위에서 저장하나 titanic2 값들 중 Age, Sex, Passenger_char만 뽑아 data9에 저장 
head(data9, 5)    # data9의 첫 5행만 출력력                                   

# Q10 
data10 <- titanic2 %>% group_by(Pclass, Passenger_char) %>%
  # Q9에서 만든 titanic2에서 Pclass, Passenger_char에 대해 group_by
  summarise(Death_percentage_num = round((sum(Survived==0)*100 / n()),2)) %>%
  # Death_percentage_num에 소수점 2의 자리에서 반올림한 사망율을 저장
  mutate(Death_percentage = paste0(Death_percentage_num, '%')) %>%
  # Death_percentage에는 문제 조건에 맞게 사망율에 %를 합한 문자열 저장
  arrange(desc(Death_percentage_num))
  # Death_percentage_num을 내림차순이 되도록 정렬한 값을 data10에 저장
head(data10 %>% select(Pclass, Passenger_char, Death_percentage))
# data10의 값 중 Pclass, Passenger_char, Death_percentage를 선택하여 출력



# Q11부터
red = read.csv("winequality-red.csv", sep=';')
white = read.csv("winequality-white.csv", sep =';')

# Q11
red_type <- red %>% mutate(type = 'red')
white_type <- white %>% mutate(type = 'white')
# red, white 모두 type을 추가하여 red_type, white_type에 저장
wine <- bind_rows(red_type, white_type)
# red_type과 white_type을 합쳐 wine에 저장
head(wine)  # head()로 wine 출력(해도 되고 안해도 됨.)

# Q12 방법1
head(wine %>% select(contains('dioxide')))
# %>%과 select, contains함수를 사용해 dioxide가 포함된 wine의 열 출력
# Q12 방법2 
head(wine[,grep('dioxide', colnames(wine))])
# wine 열들 중 이름에 dioxide가 들어간 열들 출력

# Q13 
data13 <- wine %>% filter(density > mean(density)) %>% 
  # wine의 값들 중에 density가 density의 평균보다 큰 데이터 선택
  filter(volatile.acidity == max(volatile.acidity))
  # 그 중에서 volatile.acidity가 최대인 데이터
head(data13)  # head()로 data 출력

# Q14 
wine2 <- wine %>% mutate(dioxide_ratio = free.sulfur.dioxide / total.sulfur.dioxide)
# dioxide_ratio=free.sulfur.dioxide / total.sulfur.dioxide인 값을 wine에 추가한 데이터를 wine2에 저장
head(wine2)  # head()로 wine2 출력

# Q15 
data15 <- wine %>% filter(quality==5) %>% 
  # quality가 5인 데이터 중에서
  filter(fixed.acidity == max(fixed.acidity) | fixed.acidity == min(fixed.acidity)) %>%
  #fixed.acidity의 값이 최소이거나 최대일 때
  select(fixed.acidity, volatile.acidity)
  #그 행들만 선택하여 data15에 저장
head(data15 %>% arrange(desc(fixed.acidity)))  #volatile.acidity가 내림차순이 되도록 출력

# Q16 
data16 <- wine %>% 
  # wine 데이터 중에서
  mutate(qual = ifelse(quality<6, 'Low Quality', ifelse(quality<=7, 'Medium Quality', 'High Quality')))
  # ifelse를 두번 사용하여 품질 기준에 맞게 qual에 문자열 저장
data16_q <- data16 %>% group_by(qual) %>% summarise(residual.sugar_average = sum(residual.sugar)/n())
# data16_q에 data16에서 품질기준에 따라 residual.sugar의 평균값을 내고 비교
head(data16_q %>% mutate(rank =rank(residual.sugar_average)))
# 비교를 위해 순위를 매긴 column도 함께 출력

# Q17 
data17 <- data16 %>% mutate(qlabel = case_when(
  quality < 6 ~ "L",
  quality >= 6 & quality < 7 ~ "M",
  quality >= 7 ~ "H"))
#case_when 함수를 써서 품질 기준에 따라 qlabel column을 생성한다.
head(data17)  #head()를 사용하여 data17 출력

# Q18 
data18 <- data17 %>% group_by(qlabel) %>%
  # qlabel 별로 group_by
  summarise(pH_average = mean(pH), citric.acid_average = mean(citric.acid))
  # pH_average와 citric.acid_average에 pH와 citric.acid의 평균값을 저장
head(data18) # head()를 사용하여 data18 출력

# Q19  
data19_1 <- data17 %>% group_by(type, qlabel) %>% summarise(count = n())
#data17에서 type과 qlabel에 대해 goup_by하고 갯수를 센 count 변수를 summarise한다.
data19 <- data19_1 %>% group_by(type) %>% mutate(type_count = sum(count), ratio = count / sum(count))
#red, white 두 type에 대해 group_by 한 후 type_count에는 타입별 총 개수를 입력하여 ratio에는 문제에서 구하고자 하는 비중을 저장 
head(data19)
#head함수로 data19 출력

# Q20 
data20 <- wine2 %>% mutate(acid_mean = (volatile.acidity + citric.acid)/2) %>%
  # Q14에서 만든 wine2 데이터에 aicd_mean을 계산하여 새로운 변수로 추가
  filter(dioxide_ratio > acid_mean)
  # 그 중에서 dioxide_ratio가 acid_mean보다 큰 값만 저장
head(data20, 5)
#data20 중 앞 5행만 출력
