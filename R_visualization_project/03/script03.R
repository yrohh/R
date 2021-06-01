### title : 유무임 승하차 비율 파이 차트 시각화.
### email : ynjoon@naver.com
# --------------------------------------------------------------------------------------------

### 데이터 로드.
setwd("데이터 디렉토리")
data <- read.csv('data03.csv', stringsAsFactors = F)
head(data)
str(data)



### 유무임 승하차 인원 가장 많은 역 도출.
max_total <- sapply(data[2:5], max) # 유무임 승하차 최대값 도출.

max_in_p <- data[which(data[,2]==max_total[1]),1] # 최대 유임승차역.
max_out_p <- data[which(data[,3]==max_total[2]),1] # 최대 유임승차역.
max_in_f <- data[which(data[,4]==max_total[3]),1] # 최대 유임승차역.
max_out_f <- data[which(data[,5]==max_total[4]),1] # 최대 유임승차역.

max_total
max_in_p
max_out_p
max_in_f
max_out_f
subname <- c()
subname <- c(subname, max_in_p, max_out_p, max_in_f, max_out_f)
subname # 유임승차, 유임하차, 무임승차, 무임하차 순으로 역명 벡터 생성.

for (i in c(1:4)){ # 최다 유무임 승하차역 확인.
  print(paste(names(max_total)[i], ':', subname[i], max_total[[i]]))
}



### 홍대입구 2호선 유무임 승하차 비율 시각화.
library(ggplot2)
library(extrafont) # 나눔고딕 폰트 사용.
fonts()
fonttable()

hong <- data[which(data[1]=='홍대입구 2호선'),2:5]
hong_value <- as.numeric(hong) # 벡터화.
hong_group <- names(hong)
df_hong <- data.frame(hong_value, hong_group)
df_hong <- transform(df_hong, hong_group = factor(hong_group, levels=c('유임승차', '유임하차', '무임승차', '무임하차'))) # 파이 차트 범례 목록 편집 위함.

mycols <- c("#14CCC0", "#389993", "#FF1C6A", "#CC14AF") # 파이 차트 색 파레트 지정.

ggplot(df_hong, aes(x="", y=hong_value, fill=hong_group)) +
  theme_void() + # 바깥선 제거.
  ggtitle('홍대입구 2호선 유무임 승하차 비율') + 
  theme(plot.title = element_text(family = "NanumGothic", face = "bold", hjust = 0.5, size = 13)) +
  labs(x="", y="") + # 축 이름 제거.
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=90) + # 파이 차트 모양으로 변환, 시작각 90도.
  theme(legend.title=element_blank(), legend.text = element_text(size = 12)) +
  geom_text(aes(label=paste0(round(hong_value/sum(hong_value)*100,0),"%")), family = "NanumGothic",size=5, position=position_stack((vjust=0.5))) +
  scale_fill_manual(values = mycols) # 파이 차트 색 지정.





### 종로3가 1호선 유무임 승하차 비율 시각화.
jong <- data[which(data[1]=='종로3가 1호선'),2:5]
jong_value <- as.numeric(jong) # 벡터화.
jong_group <- names(jong)
df_jong <- data.frame(jong_value, jong_group)
df_jong <- transform(df_jong, jong_group = factor(jong_group, levels=c('유임승차', '유임하차', '무임승차', '무임하차'))) # 파이 차트 범례 목록 편집 위함.

mycols <- c("#14CCC0", "#389993", "#FF1C6A", "#CC14AF") # 파이 차트 색 파레트 지정.

ggplot(df_jong, aes(x="", y=jong_value, fill=jong_group)) +
  theme_void() + # 바깥선 제거.
  ggtitle('종로3가 1호선 유무임 승하차 비율') + 
  theme(plot.title = element_text(family = "NanumGothic", face = "bold", hjust = 0.5, size = 13)) +
  labs(x="", y="") + # 축 이름 제거.
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + # 파이 차트 모양으로 변환, 시작각 90도.
  theme(legend.title=element_blank(), legend.text = element_text(size = 12)) +
  geom_text(aes(label=paste0(round(jong_value/sum(jong_value)*100,0),"%")), family = "NanumGothic",size=5, position=position_stack((vjust=0.5))) +
  scale_fill_manual(values = mycols) # 파이 차트 색 지정.
