library(ggmap)
register_google(key = "API KEY")
map <- get_googlemap(center=c(126.975684, 37.572752),
                     zoom=10,
                     size=c(640,640),
                     maptype = "roadmap")
ggmap(map, extent = 'device')


# 주소 이용 출력.
gc <- geocode(enc2utf8("인창고등학교"))
gc
lonlat <- c(gc$lon, gc$lat)
lonlat
map <- get_googlemap(center=lonlat,
                     zoom=15,
                     size=c(640,640),
                     maptype = "roadmap",
                     marker=gc)
ggmap(map, extent = 'device')


# 단양팔경 지도 위에
guri_TAN <- c("구리시민한강공원","구리 동구릉", "곤충생태관")
guri_TAA <- c("경기도 구리시 왕숙천로 49, (토평동)", "경기도 구리시 동구릉로 197, (인창동)", "경기도 구리시 검배로 200, (수택동)")
guri_TAL <- geocode(enc2utf8(guri_TAA))

df <- data.frame(name=guri_TAN,address=guri_TAA, lon=guri_TAL$lon, lat=guri_TAL$lat)
df

cen <- c(
  (max(df$lon)+min(df$lon))/2,
  (max(df$lat)+min(df$lat))/2
)
cen

map <- get_googlemap(center=cen,
                     zoom=13,
                     size=c(640,640),
                     maptype = "roadmap",
                     markers = guri_TAL)
gmap <- ggmap(map, extent = 'device')
gmap + geom_text(data=df,
                 aes(x=lon, y=lat),
                 size = 5,
                 label=df$name)

# 지진 발생 지역 분포
install.packages('openxlsx')
library(openxlsx)
library(ggplot2)

df <- read.xlsx(file.choose(), sheet =1, startRow =4)
View(df)
df[,6] <- gsub(' N','',df[,6])
df[,7] <- gsub(' E','',df[,7])

df2 <- data.frame(lon=df[,7], lat=df[,6], mag=df[,3])
str(df2)
df2[,1] <- as.numeric(as.character(df2[,1]))
df2[,2] <- as.numeric(as.character(df2[,2]))
str(df2)

cen <- c(
  (max(df2$lon)+min(df2$lon))/2,
  (max(df2$lat)+min(df2$lat))/2
)

map <- get_googlemap(center=cen,
                     zoom=6,
                     size=c(640,640),
                     maptype = "roadmap")
gmap <- ggmap(map, extent = 'device')
gmap + geom_point(data=df2,
                  aes(x=lon,y=lat),
                  color='red',
                  size=df2$mag,
                  alpha=0.5)
