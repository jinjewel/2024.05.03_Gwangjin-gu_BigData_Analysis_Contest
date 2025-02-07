rm(list=ls())
### 원본데이터불러오기
# install.packages("readxl")
library(readxl)
data <- read_excel("C:/Users/User/Desktop/2024_05_2024년광진구빅데이터분석공모전_본선/예선 준비/code/광진구_내_지하철역별_월별_사용자수_23.3_24.4_데이터.xlsx")
data


### 각 지하철역별로 데이터 분할
# 7호선 건대입구역
Konkuk_University_Entrance_7Line_data <- data[1:14,]
Konkuk_University_Entrance_7Line_data
# 7호선 중곡역
Junggok_7Line_data <- data[15:28,]
Junggok_7Line_data
# 7호선 군자역
Gunja_7Line_data <- data[29:42,]
Gunja_7Line_data
# 7호선 어린이대공원역
Childrens_Grand_Park_7Line_data <- data[43:56,]
Childrens_Grand_Park_7Line_data
# 7호선 자양역
Jayang_7Line_data <- data[57:70,]
Jayang_7Line_data
# 5호선 광나루역
Gwangnaru_5Line_data <- data[71:84,]
Gwangnaru_5Line_data
# 5호선 군자역
Gunja_5Line_data <- data[85:98,]
Gunja_5Line_data
# 5호선 아차산역
Achasan_5Line_data <- data[99:112,]
Achasan_5Line_data
# 2호선 건대입구역
Konkuk_University_Entrance_2Line_data <- data[113:126,]
Konkuk_University_Entrance_2Line_data
# 2호선 구의역
Guui_2Line_data <- data[127:140,]
Guui_2Line_data
# 2호선 강변역
Gangbyeon_2Line_data <- data[141:154,]
Gangbyeon_2Line_data


######################################### 7호선 건대입구역  ###################################################

### ARIMA 모형으로 시계열 데이터 예측 진행
## 데이터 분할(승차, 하차, 통합)
Subway_7_1_ride_data <- ts(Konkuk_University_Entrance_7Line_data[,5], start=c(2023,3), frequency=12)
Subway_7_1_getoff_data <- ts(Konkuk_University_Entrance_7Line_data[,6], start=c(2023,3), frequency=12)
Subway_7_1_total_data <- ts(Konkuk_University_Entrance_7Line_data[,4], start=c(2023,3), frequency=12)
## 그래프 표현
plot(Subway_7_1_ride_data, type="l", col="black", ylim=c(90000,250000), main="Konkuk_University_Entrance_7Line")
points(Subway_7_1_getoff_data, type="l", col="red", ylim=c(90000,250000))
points(Subway_7_1_total_data, type="l", col="blue", ylim=c(90000,250000))


### 정상시계열 변환
par(mfrow=c(3,2))
## 승차 데이터
ts.plot(Subway_7_1_ride_data)
Subway_7_1_ride_diff_data <- diff(Subway_7_1_ride_data)
plot(Subway_7_1_ride_diff_data)
## 하차 데이터
ts.plot(Subway_7_1_getoff_data)
Subway_7_1_getoff_diff_data <- diff(Subway_7_1_getoff_data)
plot(Subway_7_1_getoff_diff_data)
## 통합 데이터
ts.plot(Subway_7_1_total_data)
Subway_7_1_total_diff_data <- diff(Subway_7_1_total_data)
plot(Subway_7_1_total_diff_data)
par(mfrow=c(1,1))


### 모형식별 및 ARIMA 차수 확인
# install.packages("forecast")
library(forecast)
## 승차 데이터
Subway_7_1_ride_arima_data <- auto.arima(Subway_7_1_ride_data)
Subway_7_1_ride_arima_data # arima(0,0,0)
## 하차 데이터
Subway_7_1_getoff_arima_data <- auto.arima(Subway_7_1_getoff_data)
Subway_7_1_getoff_arima_data # arima(0,0,0)
## 통합 데이터
Subway_7_1_total_arima_data <- auto.arima(Subway_7_1_total_data)
Subway_7_1_total_arima_data # arima(0,0,0)


### 예측을 위한 모형 생성
## 승차 데이터
Subway_7_1_ride_model <- arima(Subway_7_1_ride_data, order=c(0,0,0))
Subway_7_1_ride_model
## 하차 데이터
Subway_7_1_getoff_model <- arima(Subway_7_1_getoff_data, order=c(0,0,0))
Subway_7_1_getoff_model
## 통합 데이터
Subway_7_1_total_model <- arima(Subway_7_1_total_data, order=c(0,0,0))
Subway_7_1_total_model


### 자기상관함수에 의한 모형 진단
## 승차 데이터
tsdiag(Subway_7_1_ride_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_1_ride_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 하차 데이터
tsdiag(Subway_7_1_getoff_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_1_getoff_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 통합 데이터
tsdiag(Subway_7_1_total_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_1_total_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인


### 미래 예측
par(mfrow=c(1,3))
## 승차 데이터
Subway_7_1_ride_forecast <- forecast(Subway_7_1_ride_model, h=1) # 향후 1개월치 예측
Subway_7_1_ride_forecast
plot(Subway_7_1_ride_forecast)
## 하차 데이터
Subway_7_1_getoff_forecast <- forecast(Subway_7_1_getoff_model, h=1) # 향후 1개월치 예측
Subway_7_1_getoff_forecast
plot(Subway_7_1_getoff_forecast)
## 통합 데이터
Subway_7_1_total_forecast <- forecast(Subway_7_1_total_model, h=1) # 향후 1개월치 예측
Subway_7_1_total_forecast
plot(Subway_7_1_total_forecast)

######################################### 7호선 중곡역  ###################################################

### ARIMA 모형으로 시계열 데이터 예측 진행
## 데이터 분할(승차, 하차, 통합)
Subway_7_2_ride_data <- ts(Junggok_7Line_data[,5], start=c(2023,3), frequency=12)
Subway_7_2_getoff_data <- ts(Junggok_7Line_data[,6], start=c(2023,3), frequency=12)
Subway_7_2_total_data <- ts(Junggok_7Line_data[,4], start=c(2023,3), frequency=12)
## 그래프 표현
plot(Subway_7_2_ride_data, type="l", col="black", ylim=c(90000,250000), main="Konkuk_University_Entrance_7Line")
points(Subway_7_2_getoff_data, type="l", col="red", ylim=c(90000,250000))
points(Subway_7_2_total_data, type="l", col="blue", ylim=c(90000,250000))


### 정상시계열 변환
par(mfrow=c(3,2))
## 승차 데이터
ts.plot(Subway_7_2_ride_data)
Subway_7_2_ride_diff_data <- diff(Subway_7_2_ride_data)
plot(Subway_7_2_ride_diff_data)
## 하차 데이터
ts.plot(Subway_7_2_getoff_data)
Subway_7_2_getoff_diff_data <- diff(Subway_7_2_getoff_data)
plot(Subway_7_2_getoff_diff_data)
## 통합 데이터
ts.plot(Subway_7_2_total_data)
Subway_7_2_total_diff_data <- diff(Subway_7_2_total_data)
plot(Subway_7_2_total_diff_data)
par(mfrow=c(1,1))


### 모형식별 및 ARIMA 차수 확인
# install.packages("forecast")
library(forecast)
## 승차 데이터
Subway_7_2_ride_arima_data <- auto.arima(Subway_7_2_ride_data)
Subway_7_2_ride_arima_data # arima(0,0,0)
## 하차 데이터
Subway_7_2_getoff_arima_data <- auto.arima(Subway_7_2_getoff_data)
Subway_7_2_getoff_arima_data # arima(0,0,0)
## 통합 데이터
Subway_7_2_total_arima_data <- auto.arima(Subway_7_2_total_data)
Subway_7_2_total_arima_data # arima(0,0,0)


### 예측을 위한 모형 생성
## 승차 데이터
Subway_7_2_ride_model <- arima(Subway_7_2_ride_data, order=c(0,0,0))
Subway_7_2_ride_model
## 하차 데이터
Subway_7_2_getoff_model <- arima(Subway_7_2_getoff_data, order=c(0,0,0))
Subway_7_2_getoff_model
## 통합 데이터
Subway_7_2_total_model <- arima(Subway_7_2_total_data, order=c(0,0,0))
Subway_7_2_total_model


### 자기상관함수에 의한 모형 진단
## 승차 데이터
tsdiag(Subway_7_2_ride_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_2_ride_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 하차 데이터
tsdiag(Subway_7_2_getoff_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_2_getoff_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 통합 데이터
tsdiag(Subway_7_2_total_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_2_total_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인


### 미래 예측
par(mfrow=c(1,3))
## 승차 데이터
Subway_7_2_ride_forecast <- forecast(Subway_7_2_ride_model, h=1) # 향후 1개월치 예측
Subway_7_2_ride_forecast
plot(Subway_7_2_ride_forecast)
## 하차 데이터
Subway_7_2_getoff_forecast <- forecast(Subway_7_2_getoff_model, h=1) # 향후 1개월치 예측
Subway_7_2_getoff_forecast
plot(Subway_7_2_getoff_forecast)
## 통합 데이터
Subway_7_2_total_forecast <- forecast(Subway_7_2_total_model, h=1) # 향후 1개월치 예측
Subway_7_2_total_forecast
plot(Subway_7_2_total_forecast)


######################################### 7호선 군자(능동)역  ###################################################

### ARIMA 모형으로 시계열 데이터 예측 진행
## 데이터 분할(승차, 하차, 통합)
Subway_7_3_ride_data <- ts(Gunja_7Line_data[,5], start=c(2023,3), frequency=12)
Subway_7_3_getoff_data <- ts(Gunja_7Line_data[,6], start=c(2023,3), frequency=12)
Subway_7_3_total_data <- ts(Gunja_7Line_data[,4], start=c(2023,3), frequency=12)
## 그래프 표현
plot(Subway_7_3_ride_data, type="l", col="black", ylim=c(90000,250000), main="Konkuk_University_Entrance_7Line")
points(Subway_7_3_getoff_data, type="l", col="red", ylim=c(90000,250000))
points(Subway_7_3_total_data, type="l", col="blue", ylim=c(90000,250000))


### 정상시계열 변환
par(mfrow=c(3,2))
## 승차 데이터
ts.plot(Subway_7_3_ride_data)
Subway_7_3_ride_diff_data <- diff(Subway_7_3_ride_data)
plot(Subway_7_3_ride_diff_data)
## 하차 데이터
ts.plot(Subway_7_3_getoff_data)
Subway_7_3_getoff_diff_data <- diff(Subway_7_3_getoff_data)
plot(Subway_7_3_getoff_diff_data)
## 통합 데이터
ts.plot(Subway_7_3_total_data)
Subway_7_3_total_diff_data <- diff(Subway_7_3_total_data)
plot(Subway_7_3_total_diff_data)
par(mfrow=c(1,1))


### 모형식별 및 ARIMA 차수 확인
# install.packages("forecast")
library(forecast)
## 승차 데이터
Subway_7_3_ride_arima_data <- auto.arima(Subway_7_3_ride_data)
Subway_7_3_ride_arima_data # arima(0,0,0)
## 하차 데이터
Subway_7_3_getoff_arima_data <- auto.arima(Subway_7_3_getoff_data)
Subway_7_3_getoff_arima_data # arima(0,0,0)
## 통합 데이터
Subway_7_3_total_arima_data <- auto.arima(Subway_7_3_total_data)
Subway_7_3_total_arima_data # arima(0,0,0)


### 예측을 위한 모형 생성
## 승차 데이터
Subway_7_3_ride_model <- arima(Subway_7_3_ride_data, order=c(0,0,0))
Subway_7_3_ride_model
## 하차 데이터
Subway_7_3_getoff_model <- arima(Subway_7_3_getoff_data, order=c(0,0,0))
Subway_7_3_getoff_model
## 통합 데이터
Subway_7_3_total_model <- arima(Subway_7_3_total_data, order=c(0,0,0))
Subway_7_3_total_model


### 자기상관함수에 의한 모형 진단
## 승차 데이터
tsdiag(Subway_7_3_ride_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_3_ride_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 하차 데이터
tsdiag(Subway_7_3_getoff_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_3_getoff_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 통합 데이터
tsdiag(Subway_7_3_total_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_3_total_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인


### 미래 예측
par(mfrow=c(1,3))
## 승차 데이터
Subway_7_3_ride_forecast <- forecast(Subway_7_3_ride_model, h=1) # 향후 1개월치 예측
Subway_7_3_ride_forecast
plot(Subway_7_3_ride_forecast)
## 하차 데이터
Subway_7_3_getoff_forecast <- forecast(Subway_7_3_getoff_model, h=1) # 향후 1개월치 예측
Subway_7_3_getoff_forecast
plot(Subway_7_3_getoff_forecast)
## 통합 데이터
Subway_7_3_total_forecast <- forecast(Subway_7_3_total_model, h=1) # 향후 1개월치 예측
Subway_7_3_total_forecast
plot(Subway_7_3_total_forecast)


######################################### 7호선 어린이대공원(세종대)역  ###################################################

### ARIMA 모형으로 시계열 데이터 예측 진행
## 데이터 분할(승차, 하차, 통합)
Subway_7_4_ride_data <- ts(Childrens_Grand_Park_7Line_data[,5], start=c(2023,3), frequency=12)
Subway_7_4_getoff_data <- ts(Childrens_Grand_Park_7Line_data[,6], start=c(2023,3), frequency=12)
Subway_7_4_total_data <- ts(Childrens_Grand_Park_7Line_data[,4], start=c(2023,3), frequency=12)
## 그래프 표현
plot(Subway_7_4_ride_data, type="l", col="black", ylim=c(90000,250000), main="Konkuk_University_Entrance_7Line")
points(Subway_7_4_getoff_data, type="l", col="red", ylim=c(90000,250000))
points(Subway_7_4_total_data, type="l", col="blue", ylim=c(90000,250000))


### 정상시계열 변환
par(mfrow=c(3,2))
## 승차 데이터
ts.plot(Subway_7_4_ride_data)
Subway_7_4_ride_diff_data <- diff(Subway_7_4_ride_data)
plot(Subway_7_4_ride_diff_data)
## 하차 데이터
ts.plot(Subway_7_4_getoff_data)
Subway_7_4_getoff_diff_data <- diff(Subway_7_4_getoff_data)
plot(Subway_7_4_getoff_diff_data)
## 통합 데이터
ts.plot(Subway_7_4_total_data)
Subway_7_4_total_diff_data <- diff(Subway_7_4_total_data)
plot(Subway_7_4_total_diff_data)
par(mfrow=c(1,1))


### 모형식별 및 ARIMA 차수 확인
# install.packages("forecast")
library(forecast)
## 승차 데이터
Subway_7_4_ride_arima_data <- auto.arima(Subway_7_4_ride_data)
Subway_7_4_ride_arima_data # arima(0,0,0)
## 하차 데이터
Subway_7_4_getoff_arima_data <- auto.arima(Subway_7_4_getoff_data)
Subway_7_4_getoff_arima_data # arima(0,0,0)
## 통합 데이터
Subway_7_4_total_arima_data <- auto.arima(Subway_7_4_total_data)
Subway_7_4_total_arima_data # arima(0,0,0)


### 예측을 위한 모형 생성
## 승차 데이터
Subway_7_4_ride_model <- arima(Subway_7_4_ride_data, order=c(0,0,0))
Subway_7_4_ride_model
## 하차 데이터
Subway_7_4_getoff_model <- arima(Subway_7_4_getoff_data, order=c(0,0,0))
Subway_7_4_getoff_model
## 통합 데이터
Subway_7_4_total_model <- arima(Subway_7_4_total_data, order=c(0,0,0))
Subway_7_4_total_model


### 자기상관함수에 의한 모형 진단
## 승차 데이터
tsdiag(Subway_7_4_ride_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_4_ride_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 하차 데이터
tsdiag(Subway_7_4_getoff_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_4_getoff_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 통합 데이터
tsdiag(Subway_7_4_total_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_4_total_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인


### 미래 예측
par(mfrow=c(1,3))
## 승차 데이터
Subway_7_4_ride_forecast <- forecast(Subway_7_4_ride_model, h=1) # 향후 1개월치 예측
Subway_7_4_ride_forecast
plot(Subway_7_4_ride_forecast)
## 하차 데이터
Subway_7_4_getoff_forecast <- forecast(Subway_7_4_getoff_model, h=1) # 향후 1개월치 예측
Subway_7_4_getoff_forecast
plot(Subway_7_4_getoff_forecast)
## 통합 데이터
Subway_7_4_total_forecast <- forecast(Subway_7_4_total_model, h=1) # 향후 1개월치 예측
Subway_7_4_total_forecast
plot(Subway_7_4_total_forecast)


######################################### 7호선 자양(뚝섬한강공원)역  ###################################################

### ARIMA 모형으로 시계열 데이터 예측 진행
## 데이터 분할(승차, 하차, 통합)
Subway_7_5_ride_data <- ts(Jayang_7Line_data[,5], start=c(2023,3), frequency=12)
Subway_7_5_getoff_data <- ts(Jayang_7Line_data[,6], start=c(2023,3), frequency=12)
Subway_7_5_total_data <- ts(Jayang_7Line_data[,4], start=c(2023,3), frequency=12)
## 그래프 표현
plot(Subway_7_5_ride_data, type="l", col="black", ylim=c(90000,250000), main="Konkuk_University_Entrance_7Line")
points(Subway_7_5_getoff_data, type="l", col="red", ylim=c(90000,250000))
points(Subway_7_5_total_data, type="l", col="blue", ylim=c(90000,250000))


### 정상시계열 변환
par(mfrow=c(3,2))
## 승차 데이터
ts.plot(Subway_7_5_ride_data)
Subway_7_5_ride_diff_data <- diff(Subway_7_5_ride_data)
plot(Subway_7_5_ride_diff_data)
## 하차 데이터
ts.plot(Subway_7_5_getoff_data)
Subway_7_5_getoff_diff_data <- diff(Subway_7_5_getoff_data)
plot(Subway_7_5_getoff_diff_data)
## 통합 데이터
ts.plot(Subway_7_5_total_data)
Subway_7_5_total_diff_data <- diff(Subway_7_5_total_data)
plot(Subway_7_5_total_diff_data)
par(mfrow=c(1,1))


### 모형식별 및 ARIMA 차수 확인
# install.packages("forecast")
library(forecast)
## 승차 데이터
Subway_7_5_ride_arima_data <- auto.arima(Subway_7_5_ride_data)
Subway_7_5_ride_arima_data # arima(0,0,0)
## 하차 데이터
Subway_7_5_getoff_arima_data <- auto.arima(Subway_7_5_getoff_data)
Subway_7_5_getoff_arima_data # arima(0,0,0)
## 통합 데이터
Subway_7_5_total_arima_data <- auto.arima(Subway_7_5_total_data)
Subway_7_5_total_arima_data # arima(0,0,0)


### 예측을 위한 모형 생성
## 승차 데이터
Subway_7_5_ride_model <- arima(Subway_7_5_ride_data, order=c(1,0,0))
Subway_7_5_ride_model
## 하차 데이터
Subway_7_5_getoff_model <- arima(Subway_7_5_getoff_data, order=c(0,0,0))
Subway_7_5_getoff_model
## 통합 데이터
Subway_7_5_total_model <- arima(Subway_7_5_total_data, order=c(1,0,0))
Subway_7_5_total_model


### 자기상관함수에 의한 모형 진단
## 승차 데이터
tsdiag(Subway_7_5_ride_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_5_ride_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 하차 데이터
tsdiag(Subway_7_5_getoff_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_5_getoff_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 통합 데이터
tsdiag(Subway_7_5_total_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_7_5_total_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인


### 미래 예측
par(mfrow=c(1,3))
## 승차 데이터
Subway_7_5_ride_forecast <- forecast(Subway_7_5_ride_model, h=1) # 향후 1개월치 예측
Subway_7_5_ride_forecast
plot(Subway_7_5_ride_forecast)
## 하차 데이터
Subway_7_5_getoff_forecast <- forecast(Subway_7_5_getoff_model, h=1) # 향후 1개월치 예측
Subway_7_5_getoff_forecast
plot(Subway_7_5_getoff_forecast)
## 통합 데이터
Subway_7_5_total_forecast <- forecast(Subway_7_5_total_model, h=1) # 향후 1개월치 예측
Subway_7_5_total_forecast
plot(Subway_7_5_total_forecast)


######################################### 5호선 광나루(장신대)역  ###################################################

### ARIMA 모형으로 시계열 데이터 예측 진행
## 데이터 분할(승차, 하차, 통합)
Subway_5_1_ride_data <- ts(Gwangnaru_5Line_data[,5], start=c(2023,3), frequency=12)
Subway_5_1_getoff_data <- ts(Gwangnaru_5Line_data[,6], start=c(2023,3), frequency=12)
Subway_5_1_total_data <- ts(Gwangnaru_5Line_data[,4], start=c(2023,3), frequency=12)
## 그래프 표현
plot(Subway_5_1_ride_data, type="l", col="black", ylim=c(90000,250000), main="Konkuk_University_Entrance_7Line")
points(Subway_5_1_getoff_data, type="l", col="red", ylim=c(90000,250000))
points(Subway_5_1_total_data, type="l", col="blue", ylim=c(90000,250000))


### 정상시계열 변환
par(mfrow=c(3,2))
## 승차 데이터
ts.plot(Subway_5_1_ride_data)
Subway_5_1_ride_diff_data <- diff(Subway_5_1_ride_data)
plot(Subway_5_1_ride_diff_data)
## 하차 데이터
ts.plot(Subway_5_1_getoff_data)
Subway_5_1_getoff_diff_data <- diff(Subway_5_1_getoff_data)
plot(Subway_5_1_getoff_diff_data)
## 통합 데이터
ts.plot(Subway_5_1_total_data)
Subway_5_1_total_diff_data <- diff(Subway_5_1_total_data)
plot(Subway_5_1_total_diff_data)
par(mfrow=c(1,1))


### 모형식별 및 ARIMA 차수 확인
# install.packages("forecast")
library(forecast)
## 승차 데이터
Subway_5_1_ride_arima_data <- auto.arima(Subway_5_1_ride_data)
Subway_5_1_ride_arima_data # arima(0,0,0)
## 하차 데이터
Subway_5_1_getoff_arima_data <- auto.arima(Subway_5_1_getoff_data)
Subway_5_1_getoff_arima_data # arima(0,0,0)
## 통합 데이터
Subway_5_1_total_arima_data <- auto.arima(Subway_5_1_total_data)
Subway_5_1_total_arima_data # arima(0,0,0)


### 예측을 위한 모형 생성
## 승차 데이터
Subway_5_1_ride_model <- arima(Subway_5_1_ride_data, order=c(0,0,0))
Subway_5_1_ride_model
## 하차 데이터
Subway_5_1_getoff_model <- arima(Subway_5_1_getoff_data, order=c(0,0,0))
Subway_5_1_getoff_model
## 통합 데이터
Subway_5_1_total_model <- arima(Subway_5_1_total_data, order=c(0,0,0))
Subway_5_1_total_model


### 자기상관함수에 의한 모형 진단
## 승차 데이터
tsdiag(Subway_5_1_ride_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_5_1_ride_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 하차 데이터
tsdiag(Subway_5_1_getoff_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_5_1_getoff_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 통합 데이터
tsdiag(Subway_5_1_total_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_5_1_total_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인


### 미래 예측
par(mfrow=c(1,3))
## 승차 데이터
Subway_5_1_ride_forecast <- forecast(Subway_5_1_ride_model, h=1) # 향후 1개월치 예측
Subway_5_1_ride_forecast
plot(Subway_5_1_ride_forecast)
## 하차 데이터
Subway_5_1_getoff_forecast <- forecast(Subway_5_1_getoff_model, h=1) # 향후 1개월치 예측
Subway_5_1_getoff_forecast
plot(Subway_5_1_getoff_forecast)
## 통합 데이터
Subway_5_1_total_forecast <- forecast(Subway_5_1_total_model, h=1) # 향후 1개월치 예측
Subway_5_1_total_forecast
plot(Subway_5_1_total_forecast)


######################################### 5호선 군자(능동)역  ###################################################

### ARIMA 모형으로 시계열 데이터 예측 진행
## 데이터 분할(승차, 하차, 통합)
Subway_5_2_ride_data <- ts(Gunja_5Line_data[,5], start=c(2023,3), frequency=12)
Subway_5_2_getoff_data <- ts(Gunja_5Line_data[,6], start=c(2023,3), frequency=12)
Subway_5_2_total_data <- ts(Gunja_5Line_data[,4], start=c(2023,3), frequency=12)
## 그래프 표현
plot(Subway_5_2_ride_data, type="l", col="black", ylim=c(90000,250000), main="Konkuk_University_Entrance_7Line")
points(Subway_5_2_getoff_data, type="l", col="red", ylim=c(90000,250000))
points(Subway_5_2_total_data, type="l", col="blue", ylim=c(90000,250000))


### 정상시계열 변환
par(mfrow=c(3,2))
## 승차 데이터
ts.plot(Subway_5_2_ride_data)
Subway_5_2_ride_diff_data <- diff(Subway_5_2_ride_data)
plot(Subway_5_2_ride_diff_data)
## 하차 데이터
ts.plot(Subway_5_2_getoff_data)
Subway_5_2_getoff_diff_data <- diff(Subway_5_2_getoff_data)
plot(Subway_5_2_getoff_diff_data)
## 통합 데이터
ts.plot(Subway_5_2_total_data)
Subway_5_2_total_diff_data <- diff(Subway_5_2_total_data)
plot(Subway_5_2_total_diff_data)
par(mfrow=c(1,1))


### 모형식별 및 ARIMA 차수 확인
# install.packages("forecast")
library(forecast)
## 승차 데이터
Subway_5_2_ride_arima_data <- auto.arima(Subway_5_2_ride_data)
Subway_5_2_ride_arima_data # arima(0,0,0)
## 하차 데이터
Subway_5_2_getoff_arima_data <- auto.arima(Subway_5_2_getoff_data)
Subway_5_2_getoff_arima_data # arima(0,0,0)
## 통합 데이터
Subway_5_2_total_arima_data <- auto.arima(Subway_5_2_total_data)
Subway_5_2_total_arima_data # arima(0,0,0)


### 예측을 위한 모형 생성
## 승차 데이터
Subway_5_2_ride_model <- arima(Subway_5_2_ride_data, order=c(0,0,0))
Subway_5_2_ride_model
## 하차 데이터
Subway_5_2_getoff_model <- arima(Subway_5_2_getoff_data, order=c(0,0,0))
Subway_5_2_getoff_model
## 통합 데이터
Subway_5_2_total_model <- arima(Subway_5_2_total_data, order=c(0,0,0))
Subway_5_2_total_model


### 자기상관함수에 의한 모형 진단
## 승차 데이터
tsdiag(Subway_5_2_ride_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_5_2_ride_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 하차 데이터
tsdiag(Subway_5_2_getoff_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_5_2_getoff_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 통합 데이터
tsdiag(Subway_5_2_total_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_5_2_total_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인


### 미래 예측
par(mfrow=c(1,3))
## 승차 데이터
Subway_5_2_ride_forecast <- forecast(Subway_5_2_ride_model, h=1) # 향후 1개월치 예측
Subway_5_2_ride_forecast
plot(Subway_5_2_ride_forecast)
## 하차 데이터
Subway_5_2_getoff_forecast <- forecast(Subway_5_2_getoff_model, h=1) # 향후 1개월치 예측
Subway_5_2_getoff_forecast
plot(Subway_5_2_getoff_forecast)
## 통합 데이터
Subway_5_2_total_forecast <- forecast(Subway_5_2_total_model, h=1) # 향후 1개월치 예측
Subway_5_2_total_forecast
plot(Subway_5_2_total_forecast)


######################################### 5호선 아차산(어린이대공원후문)역  ###################################################

### ARIMA 모형으로 시계열 데이터 예측 진행
## 데이터 분할(승차, 하차, 통합)
Subway_5_3_ride_data <- ts(Achasan_5Line_data[,5], start=c(2023,3), frequency=12)
Subway_5_3_getoff_data <- ts(Achasan_5Line_data[,6], start=c(2023,3), frequency=12)
Subway_5_3_total_data <- ts(Achasan_5Line_data[,4], start=c(2023,3), frequency=12)
## 그래프 표현
plot(Subway_5_3_ride_data, type="l", col="black", ylim=c(90000,250000), main="Konkuk_University_Entrance_7Line")
points(Subway_5_3_getoff_data, type="l", col="red", ylim=c(90000,250000))
points(Subway_5_3_total_data, type="l", col="blue", ylim=c(90000,250000))


### 정상시계열 변환
par(mfrow=c(3,2))
## 승차 데이터
ts.plot(Subway_5_3_ride_data)
Subway_5_3_ride_diff_data <- diff(Subway_5_3_ride_data)
plot(Subway_5_3_ride_diff_data)
## 하차 데이터
ts.plot(Subway_5_3_getoff_data)
Subway_5_3_getoff_diff_data <- diff(Subway_5_3_getoff_data)
plot(Subway_5_3_getoff_diff_data)
## 통합 데이터
ts.plot(Subway_5_3_total_data)
Subway_5_3_total_diff_data <- diff(Subway_5_3_total_data)
plot(Subway_5_3_total_diff_data)
par(mfrow=c(1,1))


### 모형식별 및 ARIMA 차수 확인
# install.packages("forecast")
library(forecast)
## 승차 데이터
Subway_5_3_ride_arima_data <- auto.arima(Subway_5_3_ride_data)
Subway_5_3_ride_arima_data # arima(0,0,0)
## 하차 데이터
Subway_5_3_getoff_arima_data <- auto.arima(Subway_5_3_getoff_data)
Subway_5_3_getoff_arima_data # arima(0,0,0)
## 통합 데이터
Subway_5_3_total_arima_data <- auto.arima(Subway_5_3_total_data)
Subway_5_3_total_arima_data # arima(0,0,0)


### 예측을 위한 모형 생성
## 승차 데이터
Subway_5_3_ride_model <- arima(Subway_5_3_ride_data, order=c(0,0,0))
Subway_5_3_ride_model
## 하차 데이터
Subway_5_3_getoff_model <- arima(Subway_5_3_getoff_data, order=c(0,0,0))
Subway_5_3_getoff_model
## 통합 데이터
Subway_5_3_total_model <- arima(Subway_5_3_total_data, order=c(0,0,0))
Subway_5_3_total_model


### 자기상관함수에 의한 모형 진단
## 승차 데이터
tsdiag(Subway_5_3_ride_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_5_3_ride_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 하차 데이터
tsdiag(Subway_5_3_getoff_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_5_3_getoff_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 통합 데이터
tsdiag(Subway_5_3_total_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_5_3_total_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인


### 미래 예측
par(mfrow=c(1,3))
## 승차 데이터
Subway_5_3_ride_forecast <- forecast(Subway_5_3_ride_model, h=1) # 향후 1개월치 예측
Subway_5_3_ride_forecast
plot(Subway_5_3_ride_forecast)
## 하차 데이터
Subway_5_3_getoff_forecast <- forecast(Subway_5_3_getoff_model, h=1) # 향후 1개월치 예측
Subway_5_3_getoff_forecast
plot(Subway_5_3_getoff_forecast)
## 통합 데이터
Subway_5_3_total_forecast <- forecast(Subway_5_3_total_model, h=1) # 향후 1개월치 예측
Subway_5_3_total_forecast
plot(Subway_5_3_total_forecast)


######################################### 2호선 건대입구역  ###################################################

### ARIMA 모형으로 시계열 데이터 예측 진행
## 데이터 분할(승차, 하차, 통합)
Subway_2_1_ride_data <- ts(Konkuk_University_Entrance_2Line_data[,5], start=c(2023,3), frequency=12)
Subway_2_1_getoff_data <- ts(Konkuk_University_Entrance_2Line_data[,6], start=c(2023,3), frequency=12)
Subway_2_1_total_data <- ts(Konkuk_University_Entrance_2Line_data[,4], start=c(2023,3), frequency=12)
## 그래프 표현
plot(Subway_2_1_ride_data, type="l", col="black", ylim=c(90000,250000), main="Konkuk_University_Entrance_7Line")
points(Subway_2_1_getoff_data, type="l", col="red", ylim=c(90000,250000))
points(Subway_2_1_total_data, type="l", col="blue", ylim=c(90000,250000))


### 정상시계열 변환
par(mfrow=c(3,2))
## 승차 데이터
ts.plot(Subway_2_1_ride_data)
Subway_2_1_ride_diff_data <- diff(Subway_2_1_ride_data)
plot(Subway_2_1_ride_diff_data)
## 하차 데이터
ts.plot(Subway_2_1_getoff_data)
Subway_2_1_getoff_diff_data <- diff(Subway_2_1_getoff_data)
plot(Subway_2_1_getoff_diff_data)
## 통합 데이터
ts.plot(Subway_2_1_total_data)
Subway_2_1_total_diff_data <- diff(Subway_2_1_total_data)
plot(Subway_2_1_total_diff_data)
par(mfrow=c(1,1))


### 모형식별 및 ARIMA 차수 확인
# install.packages("forecast")
library(forecast)
## 승차 데이터
Subway_2_1_ride_arima_data <- auto.arima(Subway_2_1_ride_data)
Subway_2_1_ride_arima_data # arima(0,0,0)
## 하차 데이터
Subway_2_1_getoff_arima_data <- auto.arima(Subway_2_1_getoff_data)
Subway_2_1_getoff_arima_data # arima(0,0,0)
## 통합 데이터
Subway_2_1_total_arima_data <- auto.arima(Subway_2_1_total_data)
Subway_2_1_total_arima_data # arima(0,0,0)


### 예측을 위한 모형 생성
## 승차 데이터
Subway_2_1_ride_model <- arima(Subway_2_1_ride_data, order=c(0,0,0))
Subway_2_1_ride_model
## 하차 데이터
Subway_2_1_getoff_model <- arima(Subway_2_1_getoff_data, order=c(0,0,0))
Subway_2_1_getoff_model
## 통합 데이터
Subway_2_1_total_model <- arima(Subway_2_1_total_data, order=c(0,0,0))
Subway_2_1_total_model


### 자기상관함수에 의한 모형 진단
## 승차 데이터
tsdiag(Subway_2_1_ride_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_2_1_ride_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 하차 데이터
tsdiag(Subway_2_1_getoff_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_2_1_getoff_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 통합 데이터
tsdiag(Subway_2_1_total_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_2_1_total_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인


### 미래 예측
par(mfrow=c(1,3))
## 승차 데이터
Subway_2_1_ride_forecast <- forecast(Subway_2_1_ride_model, h=1) # 향후 1개월치 예측
Subway_2_1_ride_forecast
plot(Subway_2_1_ride_forecast)
## 하차 데이터
Subway_2_1_getoff_forecast <- forecast(Subway_2_1_getoff_model, h=1) # 향후 1개월치 예측
Subway_2_1_getoff_forecast
plot(Subway_2_1_getoff_forecast)
## 통합 데이터
Subway_2_1_total_forecast <- forecast(Subway_2_1_total_model, h=1) # 향후 1개월치 예측
Subway_2_1_total_forecast
plot(Subway_2_1_total_forecast)


######################################### 2호선 구의(광진구청)역  ###################################################

### ARIMA 모형으로 시계열 데이터 예측 진행
## 데이터 분할(승차, 하차, 통합)
Subway_2_2_ride_data <- ts(Guui_2Line_data[,5], start=c(2023,3), frequency=12)
Subway_2_2_getoff_data <- ts(Guui_2Line_data[,6], start=c(2023,3), frequency=12)
Subway_2_2_total_data <- ts(Guui_2Line_data[,4], start=c(2023,3), frequency=12)
## 그래프 표현
plot(Subway_2_2_ride_data, type="l", col="black", ylim=c(90000,250000), main="Konkuk_University_Entrance_7Line")
points(Subway_2_2_getoff_data, type="l", col="red", ylim=c(90000,250000))
points(Subway_2_2_total_data, type="l", col="blue", ylim=c(90000,250000))


### 정상시계열 변환
par(mfrow=c(3,2))
## 승차 데이터
ts.plot(Subway_2_2_ride_data)
Subway_2_2_ride_diff_data <- diff(Subway_2_2_ride_data)
plot(Subway_2_2_ride_diff_data)
## 하차 데이터
ts.plot(Subway_2_2_getoff_data)
Subway_2_2_getoff_diff_data <- diff(Subway_2_2_getoff_data)
plot(Subway_2_2_getoff_diff_data)
## 통합 데이터
ts.plot(Subway_2_2_total_data)
Subway_2_2_total_diff_data <- diff(Subway_2_2_total_data)
plot(Subway_2_2_total_diff_data)
par(mfrow=c(1,1))


### 모형식별 및 ARIMA 차수 확인
# install.packages("forecast")
library(forecast)
## 승차 데이터
Subway_2_2_ride_arima_data <- auto.arima(Subway_2_2_ride_data)
Subway_2_2_ride_arima_data # arima(0,0,0)
## 하차 데이터
Subway_2_2_getoff_arima_data <- auto.arima(Subway_2_2_getoff_data)
Subway_2_2_getoff_arima_data # arima(0,0,0)
## 통합 데이터
Subway_2_2_total_arima_data <- auto.arima(Subway_2_2_total_data)
Subway_2_2_total_arima_data # arima(0,0,0)


### 예측을 위한 모형 생성
## 승차 데이터
Subway_2_2_ride_model <- arima(Subway_2_2_ride_data, order=c(0,0,0))
Subway_2_2_ride_model
## 하차 데이터
Subway_2_2_getoff_model <- arima(Subway_2_2_getoff_data, order=c(0,0,0))
Subway_2_2_getoff_model
## 통합 데이터
Subway_2_2_total_model <- arima(Subway_2_2_total_data, order=c(0,0,0))
Subway_2_2_total_model


### 자기상관함수에 의한 모형 진단
## 승차 데이터
tsdiag(Subway_2_2_ride_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_2_2_ride_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 하차 데이터
tsdiag(Subway_2_2_getoff_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_2_2_getoff_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 통합 데이터
tsdiag(Subway_2_2_total_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_2_2_total_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인


### 미래 예측
par(mfrow=c(1,3))
## 승차 데이터
Subway_2_2_ride_forecast <- forecast(Subway_2_2_ride_model, h=1) # 향후 1개월치 예측
Subway_2_2_ride_forecast
plot(Subway_2_2_ride_forecast)
## 하차 데이터
Subway_2_2_getoff_forecast <- forecast(Subway_2_2_getoff_model, h=1) # 향후 1개월치 예측
Subway_2_2_getoff_forecast
plot(Subway_2_2_getoff_forecast)
## 통합 데이터
Subway_2_2_total_forecast <- forecast(Subway_2_2_total_model, h=1) # 향후 1개월치 예측
Subway_2_2_total_forecast
plot(Subway_2_2_total_forecast)


######################################### 2호선 강변(동서울터미널)역  ###################################################

### ARIMA 모형으로 시계열 데이터 예측 진행
## 데이터 분할(승차, 하차, 통합)
Subway_2_3_ride_data <- ts(Gangbyeon_2Line_data[,5], start=c(2023,3), frequency=12)
Subway_2_3_getoff_data <- ts(Gangbyeon_2Line_data[,6], start=c(2023,3), frequency=12)
Subway_2_3_total_data <- ts(Gangbyeon_2Line_data[,4], start=c(2023,3), frequency=12)
## 그래프 표현
plot(Subway_2_3_ride_data, type="l", col="black", ylim=c(90000,250000), main="Konkuk_University_Entrance_7Line")
points(Subway_2_3_getoff_data, type="l", col="red", ylim=c(90000,250000))
points(Subway_2_3_total_data, type="l", col="blue", ylim=c(90000,250000))


### 정상시계열 변환
par(mfrow=c(3,2))
## 승차 데이터
ts.plot(Subway_2_3_ride_data)
Subway_2_3_ride_diff_data <- diff(Subway_2_3_ride_data)
plot(Subway_2_3_ride_diff_data)
## 하차 데이터
ts.plot(Subway_2_3_getoff_data)
Subway_2_3_getoff_diff_data <- diff(Subway_2_3_getoff_data)
plot(Subway_2_3_getoff_diff_data)
## 통합 데이터
ts.plot(Subway_2_3_total_data)
Subway_2_3_total_diff_data <- diff(Subway_2_3_total_data)
plot(Subway_2_3_total_diff_data)
par(mfrow=c(1,1))


### 모형식별 및 ARIMA 차수 확인
# install.packages("forecast")
library(forecast)
## 승차 데이터
Subway_2_3_ride_arima_data <- auto.arima(Subway_2_3_ride_data)
Subway_2_3_ride_arima_data # arima(0,0,0)
## 하차 데이터
Subway_2_3_getoff_arima_data <- auto.arima(Subway_2_3_getoff_data)
Subway_2_3_getoff_arima_data # arima(0,0,0)
## 통합 데이터
Subway_2_3_total_arima_data <- auto.arima(Subway_2_3_total_data)
Subway_2_3_total_arima_data # arima(0,0,0)


### 예측을 위한 모형 생성
## 승차 데이터
Subway_2_3_ride_model <- arima(Subway_2_3_ride_data, order=c(1,1,0))
Subway_2_3_ride_model
## 하차 데이터
Subway_2_3_getoff_model <- arima(Subway_2_3_getoff_data, order=c(1,1,0))
Subway_2_3_getoff_model
## 통합 데이터
Subway_2_3_total_model <- arima(Subway_2_3_total_data, order=c(1,1,0))
Subway_2_3_total_model


### 자기상관함수에 의한 모형 진단
## 승차 데이터
tsdiag(Subway_2_3_ride_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_2_3_ride_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 하차 데이터
tsdiag(Subway_2_3_getoff_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_2_3_getoff_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인
## 통합 데이터
tsdiag(Subway_2_3_total_model) 
# 모두 임계치(파란선) 안에 들어있는 것으로 보아 자기 상관관계가 없음
# 또한 규칙성이 없고(불규칙성 성립) p값이 0이상으로 분포(양호한 ARIMA MODEL)
Box.test(Subway_2_3_total_model$residuals, lag=1, type="Ljung")
# P값이 0.05이상이므로 모형이 통계적으로 적절하다는 것을 확인


### 미래 예측
par(mfrow=c(1,3))
## 승차 데이터
Subway_2_3_ride_forecast <- forecast(Subway_2_3_ride_model, h=1) # 향후 1개월치 예측
Subway_2_3_ride_forecast
plot(Subway_2_3_ride_forecast)
## 하차 데이터
Subway_2_3_getoff_forecast <- forecast(Subway_2_3_getoff_model, h=1) # 향후 1개월치 예측
Subway_2_3_getoff_forecast
plot(Subway_2_3_getoff_forecast)
## 통합 데이터
Subway_2_3_total_forecast <- forecast(Subway_2_3_total_model, h=1) # 향후 1개월치 예측
Subway_2_3_total_forecast
plot(Subway_2_3_total_forecast)

####################################################################################

### 예측 그래프 출력

par(mfrow=c(2,4))
# 7호선 건대입구역
plot(Subway_7_1_ride_forecast, main="Konkuk_Uni_Entrance_7Line_ride")
plot(Subway_7_1_getoff_forecast, main="Konkuk_Uni_Entrance_7Line_getoff")
# 7호선 중곡역
plot(Subway_7_2_ride_forecast, main="Junggok_7Line_ride")
plot(Subway_7_2_getoff_forecast, main="Junggok_7Line_getoff")
# 7호선 군자역
plot(Subway_7_3_ride_forecast, main="Gunja_7Line_ride")
plot(Subway_7_3_getoff_forecast, main="Gunja_7Line_getoff")
# 7호선 어린이대공원역
plot(Subway_7_4_ride_forecast, main="Childrens_Park_7Line_ride")
plot(Subway_7_4_getoff_forecast, main="Childrens_Park_7Line_getoff")
# 7호선 자양역
plot(Subway_7_5_ride_forecast, main="Jayang_7Line_ride")
plot(Subway_7_5_getoff_forecast, main="Jayang_7Line_getoff")
# 5호선 광나루역
plot(Subway_5_1_ride_forecast, main="Gwangnaru_5Line_ride")
plot(Subway_5_1_getoff_forecast, main="Gwangnaru_5Line_getoff")
# 5호선 군자역
plot(Subway_5_2_ride_forecast, main="Gunja_5Line_ride")
plot(Subway_5_2_getoff_forecast, main="Gunja_5Line_getoff")
# 5호선 아차산역
plot(Subway_5_3_ride_forecast, main="Achasan_5Line_ride")
plot(Subway_5_3_getoff_forecast, main="Achasan_5Line_getoff")
# 2호선 건대입구역
plot(Subway_2_1_ride_forecast, main="Konkuk_Uni_Entrance_2Line_ride")
plot(Subway_2_1_getoff_forecast, main="Konkuk_Uni_Entrance_2Line_getoff")
# 2호선 구의역
plot(Subway_2_2_ride_forecast, main="Guui_2Line_ride")
plot(Subway_2_2_getoff_forecast, main="Guui_2Line_getoff")
# 2호선 강변역
plot(Subway_2_3_ride_forecast, main="Gangbyeon_2Lin_ride")
plot(Subway_2_3_getoff_forecast, main="Gangbyeon_2Lin_getoff")

  
  
  
  
  
  





