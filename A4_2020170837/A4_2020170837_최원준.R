# 문제해결에 필요한 package install
install.packages("dplyr")
install.packages("stringr")
install.packages("httr")
install.packages("rvest")

library(dplyr)
library(stringr)
library(httr)
library(rvest)

# books.csv 만들기
###################################################

# 각 변수 선언
title <- NULL  
authors <- NULL 
rating <- NULL   
num_of_rating <- NULL
num_of_reviews <- NULL
text <- NULL 


setwd('C:/Users/migre/OneDrive/데이터분석을 위한 프로그래밍언어/과제')
#csv를 저장할 공간을 선언

url <- 'https://www.goodreads.com/search?utf8=%E2%9C%93&query=harry+potter&search_type=books'
# book.csv에 들어갈 해리포터를 검색한 url을 url 변수에 저장

pages_list <- read_html(url) %>%  
  html_nodes('a[href^="/search?page="]') %>% 
  html_text
# 페이지 밑에 있는 2,3,4...100, next를 문자열로 가져와서 pages_list에 저장

tot <- as.integer(pages_list[length(pages_list)-1])
# 총 페이지 수는 pages_list의 요소들 중 next 전에 있는 숫자
# 그 숫자를 정수형으로 바꿔서 tot 변수에 저장

for (i in c(1:tot)) {
#마지막 페이지까지
  page_url <- paste0("https://www.goodreads.com/search?page=",i,"&qid=V8rsic3zvR&query=harry+potter&tab=books&utf8=%E2%9C%93")
  # for문과 paste0를 활용해 page를 변화시켜 page_url에 저장
  books_list <- read_html(page_url) %>%
    html_nodes('a[href^="/book/show/"]') %>% 
    html_attr('href') %>% unique
  # books_list에는 그 페이지의 책들의 url을 list 형태로 저장
  # href 요소들 중에 url이 두 번씩 겹쳐서 작성되어 있어 unique로 중복된 url들을 제거해줌.
  books_num <- length(books_list)
  # 한 페이지에 있는 책들에 대해 for문을 돌리기 위해 books_num에 그 페이지에 있는 총 책의 개수를 저장
  for (j in c(1:books_num)) {
    # 특정 페이지에 있는 모든 책들에 대해서서
    book_url <- paste0("https://www.goodreads.com", books_list[j])
    # 앞에 빠져 있는 "https://www.goodreads.com" 부분과 books_list의 책 url을 붙여서 하나의 책의 url 생성
    book_url <- gsub("&", "&amp;", book_url)
    # 정확히 왜인지는 모르겠으나 html에 적혀있는 url과 실제 페이지의 url의 다른 부분을 바꿔줌. 
    book_url <- gsub('%20', '+', book_url)
    # 후에 multi_books.csv를 만들 때에 책 제목에 있는 빈 칸을 url에 적합하게 하기 위해 +로 바꿔줌. 
    
    book_content <- read_html(book_url)
    # read_html을 통해 book_url의 요소들을 book_content에 저장
    
    # title (step8)
    title_book <- book_content %>% 
      html_nodes('h1#bookTitle') %>% 
      # h1 node 중에 bookTitle이라는 id를 가진 부분이 제목
      html_text %>% 
      # 텍스트만 추출
      str_trim  # 공백 제거
    title_book <- gsub("  ", " ", title_book)
    # 두 칸짜리 공백은 한 칸으로 바꿔줌
    title <- c(title, title_book)
    # title에 계속해서 저장
    
    # authors (step9)
    authors_book <- book_content %>% 
      html_nodes('div.authorName__container') %>%
      #div class 중에 authorName__contatiner가 author들의 이름
      html_text %>%
      # 텍스트만 추출
      str_trim 
      # 공백 제거
    authors_book <- gsub(",","",authors_book)
    # 이름에 , 가 있으면 제거해줌.
    authors_book <- toString(authors_book)
    # toString을 활용하여 author들을 문자열로 묶어줌.
    # 가운데는 쉼표(,)가 들어감
    authors <- c(authors, authors_book)
    # authors에 계속해서 저장
    
    # rating (step10)
    rating_book <- book_content %>%
      html_nodes('span[itemprop=ratingValue]') %>%
      # span class 에서 itemprop의 이름이 ratingValue인 곳 
      html_text %>% 
      # 텍스트 형식만 추출
      str_trim
      # 공백제거
    rating_book <- as.numeric(rating_book)
    # 문자열로 추출된 평점을 숫자 형식으로 바꿔줌
    rating <- c(rating, rating_book)
    # rating에 계속해서 저장
    
    # num_of_rating (step 11)
    num_of_rating_book <- book_content %>%
      html_nodes('meta[itemprop=ratingCount]') %>%
      # meta class 에서 itemprop의 이름이 ratingCount인 곳
      html_attr('content')
      # 그 중에서 content 부분의 내용만 num_of_rating_book에 저장
    num_of_rating_book <- as.numeric(num_of_rating_book)
    # 문자열로 추출된 평점의 개수를 숫자 형식으로 바꿔줌
    num_of_rating <- c(num_of_rating, num_of_rating_book)
    # num_of_rating에 계속해서 저장
    
    # num_of_reviews (step 12)
    num_of_reviews_book <- book_content %>% 
      html_nodes('meta[itemprop=reviewCount]') %>%
      # meta class 에서 itemprop의 이름이 reviewCount인 곳
      html_attr('content')
      # 그 중에서 content 부분의 내용만 num_of_reviews_book에 저장
    num_of_reviews_book <- as.numeric(num_of_reviews_book)
    # 문자열로 추출된 리뷰의 개수를 숫자 형식으로 바꿔줌
    num_of_reviews <- c(num_of_reviews, num_of_reviews_book)
    # num_of_reviews에 계속해서 저장
    
    # text (step 13)
    text_book <- book_content %>% 
      html_nodes('div.readable.stacked') %>%
      # div class에 readable stacked에 description이 저장되어 있음
      html_text %>% 
      # 텍스트 형식만 추출
      str_trim
      # 공백 제거
    if (length(text_book) == 0) {
      # 만약 description이 적혀 있지 않다면
      text <- c(text, " ")
      # text에 한 칸 공백만 저장
    } else {
      # 만약 description이 적혀 있다면
      text_book <- gsub("\n", "", text_book)
      # 중간에 줄바꿈을 위해 있던 \n 기호들을 모두 제거해주고
      text <- c(text, text_book)
      # 계속해서 text에 저장
    }
  }
}

data_book <- data.frame(title, authors, rating, num_of_rating, num_of_reviews, text)
# 지금까지 저장한 모두 변수들을 data frame 형태로 data_book에 저장
write.csv(data_book, file='books.csv')
# write.csv를 사용하여 아까 선언한 공간에 books.csv 파일을 생성
# 그리고 data_book 데이터를 넣어준다.



# multi_books.csv 만들기
#################################################
# books.csv 만들 때 썼던 코드를 그대로 복사해서 필요한 부분들만 수정하였기 때문에
# 수정한 부분들에 대해서만 주석을 달겠습니다.

keyword <- NULL
# 검색어를 바꾸기 위해 keyword 변수 선언
title <- NULL  
authors <- NULL 
rating <- NULL   
num_of_rating <- NULL
num_of_reviews <- NULL
text <- NULL 

setwd('C:/Users/migre/OneDrive/데이터분석을 위한 프로그래밍언어/과제')

url_base<- 'https://www.goodreads.com/search?utf8=%E2%9C%93&query=&search_type=books'
# 이번에는 url_base에 query값을 아직 작성하지 않은 url을 대입.
# query 값에 검색어를 대입해야 한다.
keywords <- c('harry potter','the chronicles of narnia', 'the lord of the rings')
# 주어진 keyword들을 keywords에 저장

for (k in keywords){
  # keywords에 있는 keyword들에 대해서
  url <- modify_url(url_base, query = list(query = k))
  # modify_url을 활용해 query 부분에 keyword들을 대입
  # 그렇게 해서 생긴 url를 url 변수에 저장
  
  pages_list <- read_html(url) %>%
    html_nodes('a[href^="/search?page="]') %>% 
    html_text
  
  tot <- as.integer(pages_list[length(pages_list)-1])

  for (i in c(1:tot)) {
    name <- gsub(" ", "+", k)
    page_url <- paste0("https://www.goodreads.com/search?page=",i,"&qid=V8rsic3zvR&query=",name,"&tab=books&utf8=%E2%9C%93")
    books_list <- read_html(page_url) %>%
      html_nodes('a[href^="/book/show/"]') %>% 
      html_attr('href') %>% unique
    books_num <- length(books_list)
    for (j in c(1:books_num)) {
      book_url <- paste0("https://www.goodreads.com", books_list[j])
      book_url <- gsub("&", "&amp;", book_url)
      book_url <- gsub('%20', '+', book_url)
      
      book_content <- read_html(book_url)
      
      # keyword
      keyword <- c(keyword, k)
      # keyword 값은 그냥 계속해서 keyword에 저장 
      # 이후부터는 위 books.csv를 구하는 코드와 동일
      
      # title (step8)
      title_book <- book_content %>% 
        html_nodes('h1#bookTitle') %>% 
        html_text %>% 
        str_trim
      title_book <- gsub("  ", " ", title_book)
      title <- c(title, title_book)
      
      # authors (step9)
      authors_book <- book_content %>% 
        html_nodes('div.authorName__container') %>%
        html_text %>% 
        str_trim 
      authors_book <- gsub(",","",authors_book)
      authors_book <- toString(authors_book)
      authors <- c(authors, authors_book)
      
      # rating (step10)
      rating_book <- book_content %>%
        html_nodes('span[itemprop=ratingValue]') %>%
        html_text %>% 
        str_trim
      rating_book <- as.numeric(rating_book)
      rating <- c(rating, rating_book)
      
      # num_of_rating (step 11)
      num_of_rating_book <- book_content %>%
        html_nodes('meta[itemprop=ratingCount]') %>%
        html_attr('content')
      num_of_rating_book <- as.numeric(num_of_rating_book)
      num_of_rating <- c(num_of_rating, num_of_rating_book)
      
      # num_of_reviews (step 12)
      num_of_reviews_book <- book_content %>% 
        html_nodes('meta[itemprop=reviewCount]') %>%
        html_attr('content')
      num_of_reviews_book <- as.numeric(num_of_reviews_book)
      num_of_reviews <- c(num_of_reviews, num_of_reviews_book)
      
      # text (step 13)
      text_book <- book_content %>% html_nodes('div.readable.stacked') %>%
        html_text %>% str_trim
      if (length(text_book) == 0) {
        text <- c(text, " ")
      } else {
        text_book <- gsub("\n", "", text_book)
        text <- c(text, text_book)
      }
    }
  }
}


data_book <- data.frame(keyword, title, authors, rating, num_of_rating, num_of_reviews, text)
# 지금까지 저장한 모두 변수들을 data frame 형태로 data_book에 저장(keyword 포함)
write.csv(data_book, file='multi_books.csv')
# write.csv를 사용하여 아까 선언한 공간에 multi_books.csv 파일을 생성
# 그리고 data_book 데이터를 넣어준다.