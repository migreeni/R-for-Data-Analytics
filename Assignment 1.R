# Q1
for ( i in c(1:9)) {      #1부터 9단의 시작
  print(paste0(i,'단'))   #보기 편하게 단 별로 나눔
  for ( j in c(1:9)) {    #곱해줄 숫자 1부터 9
    cat(i, "*", j, "=", i*j, "\n")  #3 * 3 = 9 와 같은 형태로 print
  }
}

# Q2
sum=0
a=0     # sum과 a에 초기값 0을 부여해줌
while (sum < 250) {  #누적합이 250 미만이도록 제한
  a <- a+1    # 1 loop마다 a에 1씩 더해줌
  sum <- sum+a  # sum에는 a값을 계속 더해주며 누적합 계산
}
print(paste0('합은 ',sum,'이고, 마지막으로 더해진 수는 ',a,'이다.')) #누적합이 250 미만일 때 누적합과 마지막으로 더해진 값 출력

# Q3
top <- c("후드티", "체크셔츠", "줄무늬 셔츠", "니트", "파자마 상의")
bottom <- c("청바지", "슬랙스", "트레이닝 바지", "면바지", "파자마 하의")
cloth <- data.frame(top, bottom)     # top과 bottom으로 이루어진 cloth 데이터 생성
i<-1
j<-1       # i와 j에 초깃값 1 부여
while ( i <=5 ) {       #i가 5이하일 동안
  while ( j <=5 ) {     #j가 5이하일 동안
    if ( i < 5 ) {      #파자마 상의가 아니고
      if ( j <5 ) {     #파자마 하의가 아니면
        print(paste0(cloth[i,1],"와 ",cloth[j,2]))   #조합 출력
        j <- j+1
      }
      else {            #파자마 하의일 때
        print(paste0(cloth[i,1],"와 ", cloth[5,2],"를 고르셨습니다. 다시 골라주세요!"))
        j <- j+1
      }
    }
    else {              #파자마 상의일 때
      print(paste0(cloth[5,1],"와 ",cloth[j,2],"를 고르셨습니다. 다시 골라주세요!"))
      j <- j+1
    }
  }
  i <- i+1
  j <- 1
}

# Q4 
sum <- 0    #개수를 계산할 sum에 초기값 0
for ( i in c(1:100) ){    #i는 1부터 100까지의 수
  if (i %% 6 ==0) {       #i가 6의 배수일 때
    sum <- sum + 1        #sum에 1을 더해 개수를 더해준다.
  }
}
cat("1부터 100까지 6의 배수는", sum, "개이다.")

# Q5 
# given
x <- c(1:5)
mat <- matrix(0, nrow=5, ncol=5)
# 빈칸 채우기
for (i in x) {
  for (j in x) {
    mat [i, j] <- abs(j-i)       #빈 칸에 abs(j-i)
  }
}
mat

# Q6
for ( i in c(1:5) ) {      #총 5줄
  print(rep("*",i))        #첫 번째 줄은 * 1개, 두 번째 줄은 * 2개...반복
}

# Q7
nums <- c(1:4)         # nums에 숫자 1부터 4를 넣음.
for ( i in nums) {     #nums의 숫자 1부터 4까지
  print(paste0(i,'의 세제곱은 ',i^3))           #3제곱을 해줌.
}

# Q8
vec <- c(0,1,0,1,0,0,0,0,0,1,1,1)
num_zero <- 0
num_one <- 0        # 1과 0의 개수를 나타내줄 변수에 초깃값 0 대입
for ( i in vec) {   # vec의 원소들을 차례 대로
  if ( i == 0 ) num_zero <- num_zero + 1    # i가 0이면 num_zero 값을 1 더해줌
  else num_one <- num_one + 1    #i가 1이면 num_one 값을 1 더해줌
}
print(paste0('0의 개수는 ', num_zero, ' 1의 개수는 ',num_one))

# Q9 
a <- 0         #n-2번째 수를 a로
b <- 1         #n-1번째 수를 b로   
c <- 0         #a+b값을 저장하는 공간으로
repeat {
  print(b)
  c <- a+b     #c에 다음 값을 저장하고
  a <- b
  b <- c       #a와 b를 한 칸씩 미룬다.
  if (b>377) break  #377까지의 수열 출력
}

# Q10
# given
sentence= '데이터사이언스'
wrong_sentence = '테이어사이언스'
sentence_letter = strsplit(sentence, '')
wrong_sentence_letter=strsplit(wrong_sentence,'')

i <- 4
repeat {
  if (substr(sentence_letter, i, i)!=substr(wrong_sentence_letter, i, i)) {
    #각 글자별로 같은지 다른지 판별하고 다르면 아래와 같은 문장 출력
    print(paste0((i+1)/5, '번째 글자가 틀렸습니다. ',sentence,'로 다시 입력하세요.'))
  }
  i <- i+5
  if ( i == (nchar(sentence)+1)*5) break     # i가 끝까지 왔으면 break
}

# Q11 
A <- list(100,-0.009,3,-30,-0.10)
B <- list(-20, 2, -0.9, 0.085, 5)
max_num <- 0          #최댓값을 담을 max_num 선언
for (i in A) {
  for ( j in B ) {    #A와 B의 숫자들을 차례대로
    a <- max(i+j, i-j, j-i, i*j, i/j, j/i)  #6가지의 가능한 수 중 가장 큰 수를 a에 저장
    if ( a > max_num ) {      #a가 전까지의 최댓값보다 크면 
      max_num <- a            #max_num 값을 현재 a 값으로 바꿔줌
    }
  }
}
print(max_num)


# Q12
i <- 0
mult <- 1      # i! = mult 형식으로 나타낼 예정
while ( i<=10 ) {     # i를 0부터 10까지
  if ( i == 0) {      # i가 0일 경우만 예외적으로
    mult <- 1         # 팩토리얼 값에 1을 넣어준다.
  }
  else {
    mult <- mult*i    # 나머지 경우에는 그 전 mult 값에 i를 계속 곱해주어 계산
  }
  print(paste0(i,"! = ",mult))
  i <- i+1
}

# Q13
signal=c('초록','초록','노랑','빨강','노랑','초록')
for (sig in signal) {
  if (sig == '초록') {
    print(paste0(sig,'불입니다. 이동'))
  } else if (sig =='노랑') {                 #빈 칸=else if
    print(paste0(sig,'불입니다. 천천히'))
  } else {
    print(paste0(sig,'불입니다. 정지'))      #빈 칸=정지
  }
}

# Q14 (1)
menus <- c('떡','어묵','소스','떡볶이')
calories <- c(541, 213, 120, NA)
menu_cal <- data.frame(menus, calories)
menu_cal

cal_chk <- c('떡','어묵','소스')
total_cal <- 0

for (menu in cal_chk) {         #빈 칸에 cal_chk
  for (i in 1:nrow(menu_cal)) {
    if(menu_cal[i,1]==menu) {
      cal=menu_cal[i,2]
      total_cal=total_cal+cal
      print(paste0(cal_chk,'의 칼로리는 ', cal)) }}}  # 빈 칸에 cal_chk
print(paste0('떡/어묵/소스 칼로리의 합은 ', total_cal))

# Q14 (2) 
for (i in 1:nrow(menu_cal)) {
  for (j in 1:ncol(menu_cal)) {
    if(is.na(menu_cal[i,j])) {   #빈 칸은 menu_cal[i,j]
       menu_cal[i,j]= total_cal  #빈 칸은 menu_cal[i,j]
    }
  }
}
menu_cal

# Q15 (문제)
student <- c('Annie','Theo','Steve','Hannah')
grade1 <- c(85, 65, 85, 100)
grade2 <- c(90, 75, 90, 90)
grade3 <- c(75, 55, 80, 85)
grade4 <- c(95, 75, 100, 90)

math_grade <- data.frame(name=student, exam1=grade1, exam2=grade2, exam3=grade3, exam4=grade4)
math_grade

# Q15 (1) 
for ( i in 1:nrow(math_grade)) {        # 각 학생별로
  total=0                               # 총 점수 total 선언
  for ( j in 2:ncol(math_grade)) {      # 4개의 수학점수를를
    total <- total+math_grade[i,j]      # 모두 더한 값을 total에 계속해서 저장
  }
  avg=total/4        # 평균은 총점 / 4
  if (avg > 90) {    # 평균이 90점이 넘으면 출력력
    print(paste0('이번 학기 ',math_grade[i,1],'의 평균 점수는 ', avg, '점입니다.'))
  }
}

# Q15 (2) 
for ( j in 2:ncol(math_grade)) {        # 각 시험별로
  total=0                               # 총 점수 total 선언
  for ( i in 1:nrow(math_grade)) {      # 4 학생의 점수를
    total <- total+math_grade[i,j]      # 모두 더한 값을 total에 계속해서 저장
  }
  avg=total/4        # 평균은 총점 / 4
  if (avg < 80) {    # 평균이 80점 미만이면 
    print(paste0(j-1,'번째 시험은 어려웠습니다. '))
  }
}

# Q15 (3) 
for ( i in 1:nrow(math_grade)) {        # 각 시험별로 
  max_score=0                           # 최댓값 max_score 선언
  for ( j in 2:ncol(math_grade)) {      # 4번의 시험 동안
    if ( max_score < math_grade[i,j]) { # 최고 점수를 계속 max_score에 저장
      max_score <- math_grade[i,j]
    }
  }
  if (max_score > 90) {                 # 최고 점수가 90점을 넘기면 출력
    print(paste0('이번 학기 ',math_grade[i,1],'의 최고 점수는 ', max_score, '점입니다.'))
  }
}

# Q16
install.packages('MASS')
library(MASS)
df=Cars93
head(df)
A <- list(df$Manufacturer)
B <- list(df$Price)
tapply(A, B, mean)
B
