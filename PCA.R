# File-Name:       chapter08.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu) and John Myles White (jmw@johnmyleswhite.com)                                                                    
# Purpose:         
# Data Used:       data/DJI.csv, data/stock_prices.csv
# Packages Used:   ggplot2, lubridate, reshape

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

library('ggplot2')

# First code snippet
prices <- read.csv(file.path('data', 'stock_prices.csv'),
                   stringsAsFactors = FALSE)
View(prices)
class(prices$Date)
prices[1, ]
# Date Stock Close
#1 2011-05-25 DTE 51.12

# Second code snippet
install.packages('lubridate')
library('lubridate')

prices <- transform(prices, Date = ymd(Date))
View(prices)
class(prices$Date)
# Third code snippet
install.packages('reshape')
library('reshape')
head(prices)
date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close') # 행을 Date로 열을 Stock(종목)으로 원소를 value(close(종가))로 하는 행렬 생성.
View(prices)
View(date.stock.matrix)
table(is.na(date.stock.matrix)) # 25개의 결측값 존재.

# Fourth code snippet
prices <- subset(prices, Date != ymd('2002-02-01')) # 거의 모든 종목이 결측값을 갖는 날짜행 제거.
prices <- subset(prices, Stock != 'DDR') # 다른 종목과 달리 다른 날짜에 결측값을 갖는 DDR 종목 제거.
View(prices)
date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close') # 다시 행렬 생성.
View(date.stock.matrix)

# Fifth code snippet
cor.matrix <- cor(date.stock.matrix[, 2:ncol(date.stock.matrix)]) # 날짜열(첫 번째)을 제외한 다른 열간 상관계수 산출.
View(cor.matrix) # 상관행렬.
correlations <- as.numeric(cor.matrix) # 벡터로 변환.
View(correlations)

ggplot(data.frame(Correlation = correlations), # 상관계수가 높은 열의 빈도가 높은 것을 확인 -> PCA 적용에 적합.
  aes(x = Correlation, fill = 1)) +
  geom_density() +
  theme(legend.position = 'none')

# Sixth code snippet
pca <- princomp(date.stock.matrix[, 2:ncol(date.stock.matrix)]) # 상관계수 출력할 때와 같이, 함수만 달리하여 날짜열 제외.

# Seventh code snippet
#Call:
#princomp(x = date.stock.matrix[, 2:ncol(date.stock.matrix)])
#Standard deviations:
#Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7
#29.1001249 20.4403404 12.6726924 11.4636450 8.4963820 8.1969345 5.5438308
#Comp.8 Comp.9 Comp.10 Comp.11 Comp.12 Comp.13 Comp.14
#5.1300931 4.7786752 4.2575099 3.3050931 2.6197715 2.4986181 2.1746125
#Comp.15 Comp.16 Comp.17 Comp.18 Comp.19 Comp.20 Comp.21
#1.9469475 1.8706240 1.6984043 1.6344116 1.2327471 1.1280913 0.9877634
#Comp.22 Comp.23 Comp.24
#0.8583681 0.7390626 0.4347983
#24 variables and 2366 observations.

# Eighth code snippet
principal.component <- pca$loadings[, 1]
principal.component # 주성분의 성분 부하.

# Ninth code snippet
loadings <- as.numeric(principal.component)

ggplot(data.frame(Loading = loadings), # 부하가 골고루 분포된 모습을 확인.
  aes(x = Loading, fill = 1)) +
  geom_density() +
  theme(legend.position = 'none')

# Tenth code snippet
market.index <- predict(pca)[, 1] 
View(market.index)

# Eleventh code snippet
dji.prices <- read.csv(file.path('data', 'DJI.csv'),  # 다우 존스 지수 불러오기.
                       stringsAsFactors = FALSE)
dji.prices <- transform(dji.prices, Date = ymd(Date))
View(dji.prices)

# Twelfth code snippet
dji.prices <- subset(dji.prices, Date > ymd('2001-12-31')) # 데이터가 너무 많으므로 축소.
dji.prices <- subset(dji.prices, Date != ymd('2002-02-01')) # 기존 주식 데이터에서 해당 날짜의 데이터를 제거했으므로 동일하게.

# Thirteenth code snippet
test <- data.frame(close=c(1,2,3,4,5))
test2 <- with(test, rev(close))
test2
dji <- with(dji.prices, rev(Close)) # 이전 날짜의 종가가 앞에 오게끔 반대로 정렬하여 벡터 추출.
View(dji)
dates <- with(dji.prices, rev(Date)) # 이전 날짜가 앞에 오게끔 반대로 정렬하여 벡터 추출.
View(dates)

# Fourteenth code snippet
comparison <- data.frame(Date = dates,
                         MarketIndex = market.index,
                         DJI = dji)

ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) # se는 신뢰구간 시각화 여부.

# Fifteenth code snippet
# comparison <- transform(comparison, MarketIndex = -1 * MarketIndex) # 교재와 달리, 음수 부하로 나타나지 않음.

# Sixteenth code snippet
#ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
#  geom_point() +
#  geom_smooth(method = 'lm', se = F)

# Seventeenth code snippet
View(comparison)
alt.comparison <- melt(comparison, id.vars = 'Date')

names(alt.comparison) <- c('Date', 'Index', 'Price')
View(alt.comparison)
ggplot(alt.comparison, # 값의 크기차가 너무 나기 때문에 제대로 확인할 수가 없음,
       aes(x = Date, y = Price, group = Index, color = Index)) +
  geom_point() +
  geom_line()

# Eighteenth code snippet
comparison <- transform(comparison, MarketIndex = scale(MarketIndex)) # 동일 척도를 갖기 위해 스케일링.
comparison <- transform(comparison, DJI = scale(DJI)) # 동일 척도를 갖기 위해 스케일링.

alt.comparison <- melt(comparison, id.vars = 'Date')

names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) + # 주성분이 DJI와 비슷한 양상을 보임. 
  geom_point() +
  geom_line()