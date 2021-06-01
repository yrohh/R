### title : 역별 이용 승객 지도 시각화.
### email : ynjoon@naver.com
# --------------------------------------------------------------------------------------------

### 데이터 로드.
setwd("데이터 디렉토리")
data <- read.csv('data04.csv', stringsAsFactors = F)
head(data)
str(data)



### 상위 10개 역만 시각화하기 위한 전처리.
library(dplyr)
data <- data %>%
  arrange(desc(이용승객수)) %>%
  head(10)



# 구글맵 라이브러리 로드 및 키 등록.
library(ggmap)
library(ggplot2)
register_google(key = 'AIzaSyATmkSzLIV1P7813YD5a_WOmi_nx7sr9O8')
data$size_ <- data$이용승객수/median(data$이용승객수) # 이용승객수 값을 그대로 적용할 수 없으므로, 다음과 같이 전처리.


# 구글맵 설정.
cen <- c((max(data$경도)+min(data$경도))/2, (max(data$위도)+min(data$위도))/2) # 지도 중심 좌표 지정.
map <- get_googlemap(center = cen,
                     maptype = 'roadmap',
                     zoom = 12,
                     color='bw') # 


library(extrafont) # 나눔고딕 폰트 사용.
fonts()
fonttable()

gmap <- ggmap(map)
gmap <- gmap + # 포인터 출력.
  geom_point(data = data, aes(x = 경도, y = 위도, size=size_), alpha = 0.5, color = '#FF0033') +
  scale_size_continuous(range=c(1,14))

gmap + geom_text(data=data, # 텍스트 출력.             
                   aes(x=경도,y=위도+0.005), #글씨 위치               
                   size=3.5, #글씨 크기
                   family='NanumGothic',
                   color='#8B00FF',
                   fontface=2,
                   label=data$역명) +
  ggtitle('이용객 최다 10개 지하철역 지도 시각화') +
  theme(plot.title = element_text(family = "NanumGothic", face = "bold", hjust = 0.5, size = 13), 
        legend.title=element_blank(), legend.position = 'none') + # plot 제목, 범례 편집.
  labs(x="", y="") # 축 이름 제거.
        
