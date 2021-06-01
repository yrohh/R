### title : 지하철 시간대별 승하차 인원 가장 많은 역 시각화.
### email : ynjoon@naver.com
# --------------------------------------------------------------------------------------------

### 데이터 로드.
setwd("데이터 디렉토리")
data <- read.csv('data02.csv', stringsAsFactors = F)
head(data)
str(data)
colnames(data) <- as.factor(colnames(data))



### 시간대별 승차 인원 최대값&역 구하기.
max_total <- sapply(data[4:51], max)
max_in <- c()
max_out <- c()
for (i in c(1:24)){
  max_in <- c(max_in, max_total[2*i-1])
  max_out <- c(max_out, max_total[2*i])
}

max_in_sub <- c()
max_out_sub <- c()
for (i in c(1:24)){
  max_in_sub <- c(max_in_sub, data[which(data[,2*i+2]==max_in[i]),3][1]) # 열별로 최대 승차 인원값을 가진 인덱스로 역명 추출.
  max_out_sub <- c(max_out_sub, data[which(data[,2*i+3]==max_out[i]),3][1]) # 열별로 최대 하차 인원값을 가진 인덱스로 역명 추출.
}
max_in_sub
max_out_sub



### 그래프 출력을 위한 데이터프레임 생성 및 편집. ---> 승차.
label <- c(4:27)
label <- as.character(label)
label <- paste(max_in_sub, "(", label, ")", sep="")
df_in <- cbind(max_in, label)
df_in <- data.frame(df_in)
df_in$max_in <- as.numeric(df_in$max_in)



### 막대 그래프 시각화.
library(extrafont) # 나눔고딕 폰트 사용.
fonts()
fonttable()
library(ggplot2)
ggplot(df_in, aes(x=label, y=max_in)) +
  theme_bw() +
  scale_x_discrete(limits=df_in$label) +
  geom_bar(stat="identity", fill='red', alpha=0.5) +
  ggtitle('시간대별 승차 인원이 가장 많은 역') +
  theme(plot.title=element_text(family = "NanumGothic", face = "bold", hjust = 0.5, size = 12)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, size=12, family='NanumGothic')) +
  labs(x="", y="") +
  scale_y_continuous(labels = scales::comma) # y축 표시 지정. 부동소수점 표기를 없앰.



### 그래프 출력을 위한 데이터프레임 생성 및 편집.  ---> 하차.
label <- c(4:27)
label <- as.character(label)
label <- paste(max_out_sub, "(", label, ")", sep="")
df_out <- cbind(max_out, label)
df_out <- data.frame(df_out)
df_out$max_out <- as.numeric(df_out$max_out)



### 막대 그래프 시각화.
ggplot(df_out, aes(x=label, y=max_out)) +
  theme_bw() +
  scale_x_discrete(limits=df_out$label) +
  geom_bar(stat="identity", fill='blue', alpha=0.5) +
  ggtitle('시간대별 하차 인원이 가장 많은 역') +
  theme(plot.title=element_text(family = "NanumGothic", face = "bold", hjust = 0.5, size = 12)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, size=12, family='NanumGothic')) +
  labs(x="", y="") +
  scale_y_continuous(labels = scales::comma) # y축 표시 지정. 부동소수점 표기를 없앰.
