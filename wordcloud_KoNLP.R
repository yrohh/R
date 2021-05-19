# 라이브러리 로드.
library(RmecabKo)
library(wordcloud)
library(KoNLP)
library(RColorBrewer)
useSejongDic()

# 텍스트 파일 로드.
setwd("작업 디렉토리")
list.files()
job <- readLines('직업이란.txt', encoding="UTF-8")

# 텍스트 정제.
job <- gsub("’","",job)
job <- gsub("‘","",job)
job <- gsub(',',"",job)
job <- gsub('“',"",job)
job <- gsub('”',"",job)
job <- gsub("것","",job)
job <- gsub("적","",job)
job <- gsub("한","",job)
job <- gsub("수","",job)
job <- gsub("직업","",job)

# 단어 추출.
job <- sapply(job, extractNoun, USE.NAMES = F)
View(job)

# 벡터화.
job <- unlist(job)
View(job)

# 2글자 이상의 단어만 추출.
job <- Filter(function(x) {
  nchar(x)>=2
}, job)
View(job)

# 테이블(단어별 빈도수) 형태로 변환.
write(job, 'job.txt')
job <- read.table('job.txt')
job = table(job)

# 색, 폰트 지정.
display.brewer.all()
windowsFonts(font=windowsFont("NanumGothic"))

# 워드클라우드 생성.
wordcloud(
  names(job),
  freq=job,
  scale=c(5, 0.3),
  rot.per=0.1,
  min.freq=3,
  max.words = 100,
  random.color = T,
  random.order = F,
  colors = brewer.pal(11, "Paired"),
  family="font"
)
