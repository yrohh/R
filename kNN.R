# File-Name:       chapter10.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu) and John Myles White (jmw@johnmyleswhite.com)                                                                    
# Purpose:         
# Data Used:       data/example.csv, data/installations.csv
# Packages Used:   class, reshape

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

# First code snippet
df <- read.csv(file.path('data', 'example_data.csv'))

head(df)
View(df)
# X Y Label
#1 2.373546 5.398106 0
#2 3.183643 4.387974 0
#3 2.164371 5.341120 0
#4 4.595281 3.870637 0
#5 3.329508 6.433024 0
#6 2.179532 6.980400 0

# Second code snippet
distance.matrix <- function(df) # 좌표 데이터로 (유클리드) 거리 행렬 생성하는 함수 생성.
{
  distance <- matrix(rep(NA, nrow(df) ^ 2), nrow = nrow(df)) # 100x100 매트릭스(행렬) 생성.
  
  for (i in 1:nrow(df)) # 하나의 행 접근.
  {
    for (j in 1:nrow(df)) # 선택된 행의 열 차례로 접근.
    {
      distance[i, j] <- sqrt((df[i, 'X'] - df[j, 'X']) ^ 2 + (df[i, 'Y'] - df[j, 'Y']) ^ 2) # 유클리드 거리 공식.
    }
  }

  return(distance) # 거리 행렬 반환.
}



# Third code snippet
k.nearest.neighbors <- function(i, distance, k = 5) # 거리 행렬 데이터에서 각 데이터에 인접한 데이터들을 거리순으로 정렬해 k개 반환하는 함수 생성.
{
  return(order(distance[i, ])[2:(k + 1)]) # 작은 수부터 정렬하게 되면 첫 번째 오는 값(0)은 본인과 본인의 거리이므로 제외.
}

# Fourth code snippet
knn <- function(df, k = 5) # 좌표 데이터를 통해, 각 인덱스의 Label값을 knn 알고리즘으로 예측하는 함수 생성.
{
  distance <- distance.matrix(df) # 위에서 만든 거리 행렬 생성 함수로 유클리드 거리 행렬 생성.
  
  predictions <- rep(NA, nrow(df)) # predictions 라는 이름으로 크기 100의 벡터 생성.
  
  for (i in 1:nrow(df))
  {
    indices <- k.nearest.neighbors(i, distance, k = k) # 각 인덱스에 인접한 k개의 데이터만을 가지는 거리 행렬 반환.
    predictions[i] <- ifelse(mean(df[indices, 'Label']) > 0.5, 1, 0) # 인덱스별 인접한 데이터들의 Label값의 평균으로 예측.
  }
  
  return(predictions) # 예측값이 삽입된 벡터 반환.
}


# Fifth code snippet
df <- transform(df, kNNPredictions = knn(df)) # 위에서 만든 함수 적용.
View(df)
sum(with(df, Label != kNNPredictions))
#[1] 7

nrow(df)
#[1] 100

# Sixth code snippet
rm('knn') # In case you still have our implementation in memory. # 기존에 만들었던 knn 함수 제거.

library('class')

df <- read.csv(file.path('data', 'example_data.csv'))

n <- nrow(df) # n은 100.

set.seed(1) # 샘플링값 항상 동일하게.

indices <- sort(sample(1:n, n * (1 / 2))) # 1부터 100까지의 수 중에서 50개 만큼 비복원 추출. => 학습/테스트 데이터 분류용 인덱스로 사용.
View(indices)

training.x <- df[indices, 1:2] # 학습 데이터 분류, Label 제외.
test.x <- df[-indices, 1:2] # 테스트 데이터 분류, Label 제외.
# head(test.x)

training.y <- df[indices, 3] # 학습 데이터 Label.
test.y <- df[-indices, 3] # 테스트 데이터 Label.

# There's a bug here!
predicted.y <- knn(training.x, test.x, training.y, k = 5) # class 패키지의 knn 함수로 예측.
# 학습 데이터, 테스트 데이터, 학습 데이터의 라벨, k 크기 지정.
# predicted.y

sum(predicted.y != test.y) # 7개의 True값, 즉 7개의 행이 예측값과 기존 결과값이 불일치.
#[1] 7

length(test.y) 
#[1] 50
paste((1-sum(predicted.y != test.y)/length((test.y)))*100, '%', sep='') # 전체 데이터 수가 50개이므로, class 패키지의 knn 함수의 정확도는 해당 데이터 기준, 86%
# Seventh code snippet
logit.model <- glm(Label ~ X + Y, data = df[indices, ]) # 학습 데이터를 이용해 glm 모델 생성.
predictions <- as.numeric(predict(logit.model, newdata = df[-indices, ])>0) # 테스트 데이터에 위에서 만들 모델 적용하여 결과 예측.                                                                            # as.numeric() 함수를 이용해서 조건을 통해 0, 1 변환.
# predictions
sum(predictions != test.y)
#[1] 16

# Eighth code snippet
installations <- read.csv(file.path('data', 'installations.csv'))
# View(installations)
head(installations)
# Package User Installed
#1 abind 1 1
#2 AcceptanceSampling 1 0
#3 ACCLMA 1 0
#4 accuracy 1 1
#5 acepack 1 0
#6 aCGH.Spline 1 0

# Ninth code snippet
library('reshape')

user.package.matrix <- cast(installations, User ~ Package, value = 'Installed')
# reshape 패키지의 cast() 함수를 이용해서, User를 행으로, Package를 열로, Insatlled를 원소로 하는 매트릭스 생성.
View(user.package.matrix)
# dim(user.package.matrix) # 52명의 이용자와 2488개의 패키지.
user.package.matrix[, 1] # 매트릭스의 첫 번째 열은, 유저들의 ID를 의미.
# [1] 1 3 4 5 6 7 8 9 11 13 14 15 16 19 21 23 25 26 27 28 29 30 31 33 34
#[26] 35 36 37 40 41 42 43 44 45 46 47 48 49 50 51 54 55 56 57 58 59 60 61 62 63
#[51] 64 65

user.package.matrix[, 2] # abind 패키지에 대한 52명의 이용자들의 설치 여부.
# [1] 1 1 0 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 0 0 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1
#[39] 1 1 1 1 1 1 1 1 0 1 1 1 1 1

row.names(user.package.matrix) <- user.package.matrix[, 1] # 매트릭스의 user 열을 없애는 대신, 인덱스명을 User ID로 사용.

user.package.matrix <- user.package.matrix[, -1]

# Tenth code snippet
similarities <- cor(user.package.matrix) # 2488개의 패키지 간의 상관행렬 생성.
View(similarities)
nrow(similarities)
#[1] 2487
ncol(similarities)
#[1] 2487
similarities[1, 1]
#[1] 1
similarities[1, 2]
#[1] -0.04822428

# Eleventh code snippet
distances <- -log((similarities / 2) + 0.5) # 상관계수가 높을수록 거리가 작고, 낮을수록 커지도록 거리 척도 변환.

# Twelfth code snippet
k.nearest.neighbors <- function(i, distances, k = 25)
{
  return(order(distances[i, ])[2:(k + 1)])
}

# Thirteenth code snippet
installation.probability <- function(user, package, user.package.matrix, distances, k = 25)
{
  neighbors <- k.nearest.neighbors(package, distances, k = k)
  
  return(mean(sapply(neighbors, function (neighbor) {user.package.matrix[user, neighbor]})))
}
# test <- k.nearest.neighbors(1, distances, k = 25)
# View(test)
# View(user.package.matrix)
# sapply(test, function(neighbor) {print(neighbor)})
# View(distances)
installation.probability(1, 1, user.package.matrix, distances)
#[1] 0.76

# Fourteenth code snippet
most.probable.packages <- function(user, user.package.matrix, distances, k = 25) # 특정 유저의 패키지와 관련하여  
{
  return(order(sapply(1:ncol(user.package.matrix),
               function (package)
               {
                 installation.probability(user,
                                          package,
                                          user.package.matrix,
                                          distances,
                                          k = k)
               }),
         decreasing = TRUE))
}

user <- 1

listing <- most.probable.packages(user, user.package.matrix, distances)
# View(listing)
A<-colnames(user.package.matrix)[listing[1:10]]
user.package.matrix[user,A]
 