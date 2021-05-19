# 패키지 로드.
library(RmecabKo)
library(wordcloud)
library(KoNLP)
library(RColorBrewer)
useSejongDic()
setwd("작업 디렉토리")

# 파일 불러오기.
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

# 빈도수를 갖는 테이블로 변환.
write(job, 'job.txt')
job <- read.table('job.txt')
job = table(job)

# 파레트 조회 및 워드 클라우드 폰트 지정.
display.brewer.all()
windowsFonts(font=windowsFont("NanumGothic"))

# 워드 클라우드 생성.
wordcloud(
  names(job),
  freq=job,
  scale=c(5, 0.3), # 높은 빈도의 단어와 낮은 빈도의 단어의 글자 크기 차.
  rot.per=0.1, # 90도 회전 비율?
  min.freq=3, # 최소 빈도 수.
  max.words = 100, # 표현할 단어 수.
  random.color = T, 
  random.order = F, # 빈도 수 높은 단어를 중앙으로 배치.
  colors = brewer.pal(11, "Paired"),
  family="font"
)
