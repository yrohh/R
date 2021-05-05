# 패키지 로드.
# install.packages("XML")
library("XML")
library(ggplot2)

# api 설정.
api <-  "API키 입력 전까지의 url 주소"
api_key <- "API키"
returnType <- "xml"
numOfRows <- 100
pageNo <- 1
sidoName <- "%EC%84%9C%EC%9A%B8"
ver <- "1.0"

url <- paste(api,
             "?serviceKey=", api_key,
             "&sidoName=", sidoName,
             "&numOfRows=", numOfRows,
             sep="")
url

# xml parsing.
xmlFile <- xmlParse(url)
# xmlRoot(xmlFile) xml 문서 확인
df <- xmlToDataFrame(getNodeSet(xmlFile, "//items/item")) # items 노드 내 여러 item 노드들을 데이터 프레임으로 변환.
View(df)
df$pm10Value <- as.numeric(df$pm10Value)

# 시각화.
ggplot(data=df, aes(x=reorder(stationName,pm10Value), y=pm10Value), dpi=300) + # reorder로 정렬.
  geom_bar(stat="identity", fill="green") + # stat으로 데이터의 숫자 크기로 출력.
  # theme(axis.text.x=element_text(angle=90)) + # x축 눈금 라벨 회전각도.
  labs(title="서울 각 지역의 미세먼지 농도", x="지역", y="농도") + # 축 라벨과 제목 변경.
  scale_fill_manual(values=rainbow(40)) + # 범례 출력 및 막대 색 지정.
  coord_flip() # 막대 가로 출력.

###

# url 생성_편의상 웹으로 직접 확인한 내용을 가져옴.
url2 <- "http://apis.data.go.kr/B552584/ArpltnInforInqireSvc/getCtprvnRltmMesureDnsty?serviceKey={API키}&returnType=xml&numOfRows=1000&pageNo=1&sidoName=%EC%A0%84%EA%B5%AD&ver=1.0"
xmlFile2 <- xmlParse(url2)
df2 <- xmlToDataFrame(getNodeSet(xmlFile2, "//items/item"))
View(df2)
str(df2)
df2$pm25Value <- as.numeric(df2$pm25Value)

# 한 지역 내 여러 관측소가 존재하므로, 지역 기준 평균값 구하기.
library(dplyr)
df2 <- df2 %>% group_by(sidoName) %>%summarise(mean_pm25Value = mean(pm25Value,na.rm=T))

# 구글맵을 활용한 시각화.
library(ggmap)
register_google(key="구글맵API키")
df2$sidoName
cities <- c('강원도','경기도','경상남도','경상북도','광주시','대구시','대전시','부산시','서울시','세종특별자치시청','울산시','인천시','전라남도',
            '전라북도','제주시','충청남도','충청북도')
gc <- geocode(enc2utf8(cities))
gc
df2 <- cbind(df2,gc)
df2
str(df2)
cen <- as.numeric(geocode(enc2utf8('전라북도'))) # 전라북도를 지도 중심 좌표로 설정.
map <- get_googlemap(center=cen, zoom=7, maptype = 'roadmap') # 구글맵 정보 생성.

ggmap(map) +
  geom_point(data=df2,
             aes(x=lon, y=lat),
             color=rainbow(length(df2$mean_pm25Value)),
             size=df2$mean_pm25Value,
             alpha=0.5)
