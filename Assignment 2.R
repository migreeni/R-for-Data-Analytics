# Q1 
A <- sample(x=1:50, size=20)
# 1에서 50까지 랜덤하게 선택된 20개의 수
ExSearch <- function(x) {
  count <- 0
  # 횟수를 통해 false를 판별하기 위해 count 변수 선언
  for (i in c(1:length(A))) {
    # list A의 길이동안
    if (x == A[i]) {
      # list A에 사용자가 입력한 수가 존재하면
      print("TRUE")
      # TRUE 출력하고
      break
      # for문 break
    }
    else {
      # list A에 사용자가 입력한 수가 존재하지 않으면
      count <- count +1
      # count에 1을 추가해준다.
    }
  }
  if (count == length(A)) {
    # count의 값이 A의 길이와 똑같으면 
    # 모든 A의 값과 사용자가 입력한 수를 비교해보았으나 
    # 존재하지 않는 것을 의미하므로
    print("FALSE")
    # false 출력
  }
}
num <- readline('insert please: ')
# 사용자가 숫자를 입력하도록 하는 함수 readline
num <- as.numeric(num)
# 입력한 값은 문자열로 저장되므로 숫자로 변환
ExSearch(num)

# Q2 
moneycount <- function(x) {
  coin500 <- x%/%500
  # 돈을 500원으로 나눈 몫이 500원의 개수
  coin100 <- (x%%500)%/%100
  # 돈을 500원으로 나눈 나머지를 100원으로 나눈 몫이 100원의 개수
  coin50 <- (x%%100)%/%50
  # 돈을 100원으로 나눈 나머지를 50원으로 나눈 몫이 50원의 개수
  coin10 <- (x%%50)%/%10
  # 돈을 50원으로 나눈 나머지를 10원으로 나눈 몫이 10원의 개수
  return(paste0("500원 ",coin500,"개, 100원 ",coin100, "개, 50원 ",coin50,"개, 10원 ",coin10,"개가 필요합니다."))
  # 출력
}
moneycount(23220)

# Q3 
thisyear_score <- sample(x=0:10, size=5)
# 0에서 10까지 랜덤하게 선택된 5개의 수(올해 개봉한 영화의 평점들)
predict_score <- function(x) {
  avg <- mean(x)
  # mean 함수로 list 올해 영화의 평점의 평균 값을 avg에 저장
  result <- x[5] - avg
  # 마지막 영화 평점에서 평점의 평균을 뺀 값을 result에 저장
  if ( result > 10) {
    # result 값이 10을 넘길 경우 
    result <- 10
    # result 값을 10으로 설정
  }
  else if (result < 0) {
    # result 값이 0보다 작을 경우
    result <- 0
    # result 값을 0으로 설정
  }
  return(result)
  # result 값 반환하여 다음 해 영화 평점 예측 평균으로 사용 
}
predict_score(thisyear_score)

# Q4 
mat <- sample(x=1:30, size=10)
# 10개의 재료의 크기를 랜덤으로 mat에 저장
mult <- sample(x=1:5, size=1)
# 재료에 곱할 랜덤할 숫자를 mult에 저장
faulty_toys <- function(x, z) {
  # 불량품 검출 함수수
 make_toys <- function(x, z) {
   # 장난감 생산 기계 함수
   for (i in c(1:length(x))) {
     # list x의 모든 값들에 대해서 
     x[i] <- x[i]*z
     # z를 곱해준다.
   }
   return(x)
   # 모든 값에 z를 곱해준 list x 반환
}
 x <- make_toys(x, z)
 # x 값에 z를 곱해준 list x를 다시 대입
 count <- 0
 # 불량품의 개수를 세어주는 count 변수 선언
 for (i in c(1:length(x))) {
   # list x의 모든 값에 대해서
   if (x[i] < 10 | x[i] > 30) {
     # 값이 10보다 작고 30보다 크면
     count <- count + 1
     # count 값에 1 더해줌.
   }
 }
 print(paste0('불량품의 개수는 ',count,'개입니다.'))
 # 불량품의 개수 출력
}
faulty_toys(mat, mult)


# Q5 
calculator <- function(x, y, z) {
  if (z == '+') {
    # 연산자가 더하기일 때
    result <- x+y
    # x+y를 결과로 대입
    print(paste0(x,z,y,'의 결과는 ',result,'입니다.'))
  }
  else if (z == '-') {
    # 연산자가 빼기일 때
    result <- x-y
    # x-y를 결과로 대입
    print(paste0(x,z,y,'의 결과는 ',result,'입니다.'))
  }
  else if (z == '*') {
    # 연산자가 곱하기일 때
    result <- x*y
    # x*y를 결과로 대입
    print(paste0(x,z,y,'의 결과는 ',result,'입니다.'))
  }
  else if (z == '/') {
    # 연산자가 나누기일 때
    quot <- x %/% y
    # x%/%y를 몫으로 저장
    rem <- x %% y
    # x%%y를 나머지로 저장
    print(paste0(x,z,y,'의 몫은 ',quot,'이며, 나머지는 ',rem,'입니다.'))
    # 나눗셈은 몫과 나머지를 분할해서 출력
  }
  else {
    print(paste0(z,'를 잘못 입력하셨습니다.'))
    # 연산자 잘못 입력시 잘못 입력 되었다고 출력
  }
}
calculator(3,4,'/')

# Q6 
fibo <- function(x) {
   a <- 0
   # 0번째 fibonacci 수를 0으로 a에 저장
   b <- 1
   # 1번째 fibonacci 수를 1으로 b에 저장
   count <- 0
   # n번째 fibonacci 수를 출력하도록 count 변수 선언 
   while (count < x-1) {
     # count가 x-1보다 작을 동안 while 문 실행행
     num <- a + b
     # n-1, n-2번째 수를 더한 값을 num에 저장
     a <- b
     # n-1번째 수를 a에 옮겨줌
     b <- num
     # n번째 수를 b에 옮겨줌
     count <- count + 1
     # 한 번 진행했으므로 count에 1을 더해줌
   }
   print(num)
   # fibonacci 수 출력
}
fibo(10)

# Q7
triangle_while <- function(x) {
  # while문으로 직각삼각형 출력하는 함수
  count <- 1
  # 함수에 입력된 수만큼만 층을 출력하도록 count 변수 선언
  while (count <= x) {
    # count 변수가 x이하일 동안
    times <- 1
    # count층일 때 times개의 *을 출력하도록 times 변수 선언
    line <- ''
    # *을 계속해서 추가해줄 문자열 line을 ''로 선언
    while (times <= count) {
      # times가 count 이하일 때
      line <- paste0(line, '*')
      # line 문자열에 총 times횟수만큼 *을 추가해줌
      times <- times + 1
      # times+1을 해줌으로서 *을 추가해준 횟수를 세준다.
    }
    print(line)
    count <- count +1
    # 입력된 층수만큼만 출력하도록 count에 계속 1을 더해줌.
  }
}
triangle_while(10)

triangle_for <- function(x) {
  # for문으로 직각삼각형 출력하는 함수
  for (i in c(1:x)) {
    # x번만큼
    line <- ''
    # *을 계속해서 추가해줄 문자열 line을 ''로 선언
    for (i in c(1:i)) {
      # i번동안
      line <- paste0(line, '*')
      # line에 *을 계속추가해줌
    }
    print(line)
    # 출력
  }
}
triangle_for(10)

# Q8 
AI_rps <- function(x) {
  win <- 0
  # 이긴 횟수를 저장하는 win 변수 선언
  lose <- 0
  # 진 횟수를 저장하는 lose 변수 선언
  draw <- 0
  # 무승부 횟수를 저장하는 draw 변수 선언
  for ( i in c(1:x)) {
    # x번동안
    ai <- sample(x=1:3, size = 1 )
    # ai의 가위, 바위, 보를 sample 함수를 이용해 1, 2, 3 중에 랜덤선택해서 ai 변수에 저장
    yt <- sample(x=1:3, size = 1 )
    # 영탁의 가위, 바위, 보를 sample 함수를 이용해 1, 2, 3 중에 랜덤선택해서 yt 변수에 저장
    # 가위를 1 바위를 2 보를 3으로 생각.
    if (ai == yt) {
      # ai와 yt의 값이 같다면
      print("무승부")
      # 무승부 출력
      draw <- draw +1
      # 무승부 횟수 추가가
    }
    else {
      # ai와 yt의 값이 다르다면
      if (yt == 1) {
        # 영탁이 가위를 냈을 때
        if (ai == 2) {
          # ai가 바위를 내면
          print("패")
          # 패 출력
          lose <- lose + 1
          # 패배 횟수 추가
        }
        else {
          # ai가 보를 내면
          print("승")
          # 승 출력
          win <- win + 1
          # 승리 횟수 추가
        }
      }
      else if (yt == 2) {
        # 영탁이 바위를 냈을 때
        if (ai == 1) {
          # ai가 가위를 내면
          print("승")
          # 승 출력
          win <- win + 1
          # 승리 횟수 추가
        }
        else {
          # ai가 보를 냈을 때
          print("패")
          # 패 출력
          lose <- lose + 1
          # 패배 횟수 추가
        }
      }
      else {
        # 영탁이 보를 냈을 때
        if (ai == 1) {
          # ai가 가위를 내면
          print("패")
          # 패 출력
          lose <- lose + 1
          # 패배 횟수 추가
        }
        else {
          # ai가 바위를 내면
          print("승")
          # 승 출력
          win <- win + 1
          # 승리 횟수 추가
        }
      }
    }
  }
  print(paste0('영탁군은 AI에 대항하여 ', win, '승, ', lose, '패, ', draw, ' 무승부를 기록하였습니다.'))
  # 총 승리, 패배, 무승부 횟수에 대해서 출력
}
AI_rps(10)

# Q9 
nums <- sample(x=1:1000, size=100)
# nums에 1에서 1000까지 랜덤하게 선택된 100개의 수를 저장
Bubble <- function(x) {
  start_time <- Sys.time()
  # 시작 시간을 start_time에 저장
  k = length(nums)
  # nums의 값의 개수를 k에 저장
  for (i in c(1:(k-1))) {
    # k-1번 동안
    for (j in c(1:(k-i))) {
    # k-i까지(그 이후로는 이미 정렬되어 있을 것)
      if (x[j] > x[j+1]) {
        # 앞에 있는 값이 뒤에 있는 값보다 크면
        space <- x[j+1]
        x[j+1] <- x[j]
        x[j] <- space
        # 위 과정을 통해 두 값을 교환해준다.
      }
    }
  }
  end_time <- Sys.time()
  # 종료 시간을 end_time에 저장
  time <- end_time - start_time
  # 총 걸린 시간을 time에 저장
  print(x)
  # 정렬된 리스트 출력
  print(paste0("걸린 시간은 ",time, "초"))
  # 걸린 시간 출력
}
Bubble(nums)

# Q10 
nums <- sample(x=1:1000,size=100)
print(nums)
# 1에서 1000까지 랜덤하게 선택된 100개의 수를 nums에 저장 
quicksort <- function(x) {
  #퀵 정렬해주는 함수
  if(length(x) <= 1) {
    # x의 값이 한 개만 존재하면
    return(x)
    # 그대로 출력
  }
  else {
    # x의 값이 한 개가 아니라면
    pivot <- x[1]
    # pivot을 x의 첫 번째 값으로 함.
    a <- x[x < pivot]
    # a에는 x의 값들 중에 pivot 보다 작은 값들을 저장
    b <- x[x > pivot]
    # b에는 x의 값들 중에 pivot 보다 큰 값들을 저장
    return(c(quicksort(a), pivot, quicksort(b)))
    # pivot을 제외한 양 옆은 재귀함수처럼 반복해서 퀵정렬을 함으로써 
    # 마지막에 정렬된 리스트 x를 출력
  }
}
quicksort(nums)
quicksort_time <- function(x){
  # 퀵 정렬 시간 측정해주는 함수
  start_time <- Sys.time()
  # 시작 시간을 start_time에 저장
  quicksort(x)
  # 위에서 만든 함수로 퀵 정렬 실행
  end_time <- Sys.time()
  # 종료 시간을 end_time에 저장
  print(paste0('걸린시간은 ',(end_time-start_time),'초'))
  # 걸린 시간 출력
}
quicksort_time(nums)
