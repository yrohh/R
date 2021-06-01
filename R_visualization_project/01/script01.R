### title : 지하철 시간대별 승하차 인원 추이 시각화.
### email : ynjoon@naver.com
# --------------------------------------------------------------------------------------------

### 데이터 로드.
setwd("데이터 디렉토리")
data <- read.csv('data01.csv', stringsAsFactors = F)
head(data)
str(data)



### 시간대별 승하차 인원합 구하기.
total <- sapply(data[4:51],sum)
t_in <- c() # 시간별 승차 벡터 초기화.
t_out <- c() # 시간별 하차 벡터 초기화.

for (i in 1:24){
  t_in <- cbind(t_in, total[[2*i-1]])
  t_out <- cbind(t_out, total[[2*i]])
}

t_in 
t_out
str(t_in)
str(t_out)
t_in <- t(t_in) # 전치.
t_out <- t(t_out) # 전치.



### 그래프 출력을 위한 데이터프레임 생성 및 편집.
df <- rbind(t_in, t_out)
df <- as.data.frame(df)
group_in <- rep('승차',24)
group_out <- rep('하차',24)
group <- c(group_in,group_out)
df$group <- group
time <- rep(4:27, 2)
df$time <- time
names(df) <- c('count', 'group', 'time')
df



### 그래프 출력.
library(ggplot2)
library(extrafont) # 나눔고딕 폰트 사용.
fonts()
fonttable()

ggplot(df, aes(x=time, y=count, group=group, color=group)) + 
  geom_line(size=1.0) + 
  theme_bw() + # plot 테마 지정.
  ggtitle('지하철 시간대별 승하차 인원 추이') + 
  theme(plot.title = element_text(family = "NanumGothic", face = "bold", hjust = 0.5, size = 12)) +
  labs(x="", y="") +
  theme(legend.position = c(0.9,0.9)) + # 범례 위치 지정.
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(4, 27, 1)) + # x축 지정.
  scale_y_continuous(labels = scales::comma) # y축 표시 지정. 부동소수점 표기를 없앰.
