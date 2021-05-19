library(RmecabKo)
library(wordcloud)
library(KoNLP)
library(RColorBrewer)
useSejongDic()
setwd("C:/Users/yoonjun/Desktop/")
list.files()
job <- readLines('직업이란.txt', encoding="UTF-8")
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

job <- sapply(job, extractNoun, USE.NAMES = F)
View(job)
job <- unlist(job)
View(job)

job <- Filter(function(x) {
  nchar(x)>=2
}, job)
View(job)

write(job, 'job.txt')
job <- read.table('job.txt')
job = table(job)

display.brewer.all()
windowsFonts(font=windowsFont("NanumGothic"))

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
