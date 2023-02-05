## 1. 준비 과정
### 1. 필요한 패키지 로드

library(dplyr)
library(readxl)
library(dygraphs)
library(xts)

### 2. smoking 데이터 준비
# 데이터 불러오기
raw_smoking <- read_excel("smoking(08~19).xlsx")

# 데이터 파악
head(raw_smoking)
summary(raw_smoking)
class(raw_smoking)

raw_smoking <- raw_smoking[-1, -1] # 불필요한 행/열 삭제
raw_smoking[8, c(1 : 4)] <- NA # 빈 부분 NA 지정
table(is.na(raw_smoking))

# tibble로 변경, 행/열 전치, 행이름 설정
smoking <- as_tibble(t(smok), rownames = "date") 
head(smoking)
class(smoking)
str(smoking)

# 변수 타입 변경
smoking$V1 <- as.integer(smoking$V1)
smoking$V2 <- as.integer(smoking$V2)
smoking$V3 <- as.integer(smoking$V3)
smoking$V4 <- as.integer(smoking$V4)
smoking$V5 <- as.integer(smoking$V5)
smoking$V6 <- as.integer(smoking$V6)
smoking$V7 <- as.integer(smoking$V7)
smoking$V8 <- as.integer(smoking$V8)
smoking$V9 <- as.integer(smoking$V9)
smoking$V10 <- as.integer(smoking$V10)
smoking$V11 <- as.integer(smoking$V11)
smoking$V12 <- as.integer(smoking$V12)
smoking$V13 <- as.integer(smoking$V13)
smoking$V14 <- as.integer(smoking$V14)
smoking$V15 <- as.integer(smoking$V15)
smoking$V16 <- as.integer(smoking$V16)
smoking$V17 <- as.integer(smoking$V17)
smoking$date <- as.Date(smoking$date, format = "%Y")
str(smoking)

# 데이터 정제
mean(smoking$V8, na.rm = T) # 결측값 제외 평균
smoking$V8 <- ifelse(is.na(smoking$V8), 922.5, smoking$V8) # 결측값 대체

# 총 흡연자 수 변수 생성
smoking <- smoking %>% 
  select(date) %>% 
  mutate(sum = smoking$V1 + smoking$V2 + smoking$V3 + smoking$V4 + smoking$V5 + smoking$V6 + smoking$V7 
         + smoking$V8 + smoking$V9 + smoking$V10 + smoking$V11 + smoking$V12 + smoking$V13 + smoking$V14
         + smoking$V15 + smoking$V16 +smoking$V17)
smoking

### 3. alcool 데이터 준비
# 데이터 불러오기
raw_alcool <- read_excel("alcool(07~18).xlsx")

# 데이터 파악
head(raw_alcool)
summary(raw_alcool)

# tibble로 변경, 행/열 전치, 행이름 설정
alc <- as_tibble(t(raw_alcool), row.names = "date")
alc <- alc[-c(1 : 3),]
alc$V1 <- c(2007 : 2018)
alc <- rename(alc, date = V1, sum = V2)

# 변수 타입 변경
alc$date <- as.character(alc$date)
alc$date <- as.Date(alc$date, format = "%Y")
alc$sum <- as.integer(alc$sum)
str(alc)

alc

### 4. fat 데이터 준비
# 데이터 불러오기
raw_fat <- read_excel("fat(08~18).xlsx")

# 데이터 파악
head(raw_fat)
summary(raw_fat)

raw_fat <- raw_fat[-1, -1] # 불필요한 행/열 삭제
raw_fat[8, c(1 : 4)] <- NA # 빈 부분 NA 지정
table(is.na(raw_fat))

# tibble로 변경, 행/열 전치, 행이름 설정
fat <- as_tibble(t(raw_fat), rownames = "date") 

# 변수 타입 변경
fat$V1 <- as.integer(fat$V1)
fat$V2 <- as.integer(fat$V2)
fat$V3 <- as.integer(fat$V3)
fat$V4 <- as.integer(fat$V4)
fat$V5 <- as.integer(fat$V5)
fat$V6 <- as.integer(fat$V6)
fat$V7 <- as.integer(fat$V7)
fat$V8 <- as.integer(fat$V8)
fat$V9 <- as.integer(fat$V9)
fat$V10 <- as.integer(fat$V10)
fat$V11 <- as.integer(fat$V11)
fat$V12 <- as.integer(fat$V12)
fat$V13 <- as.integer(fat$V13)
fat$V14 <- as.integer(fat$V14)
fat$V15 <- as.integer(fat$V15)
fat$V16 <- as.integer(fat$V16)
fat$V17 <- as.integer(fat$V17)
fat$date <- as.Date(fat$date, format = "%Y")
str(fat)

# 데이터 정제
mean(fat$V8, na.rm = T) # 결측값 제외 평균
fat$V8 <- ifelse(is.na(fat$V8), 909.8571, fat$V8 ) # 결측값 대체

# 총 비만자 변수 생성
fat <- fat %>% 
  select(date) %>% 
  mutate(sum = fat$V1 + fat$V2 + fat$V3 + fat$V4 + fat$V5 + fat$V6 + fat$V7 + fat$V8 + fat$V9
         + fat$V10 + fat$V11 + fat$V12 + fat$V13 + fat$V14 + fat$V15 + fat$V16 + fat$V17)
fat

### 5. stress 데이터 준비
# 데이터 불러오기
raw_stress <- read_excel("stress(08~19).xlsx")

# 데이터 파악
head(raw_stress)
summary(raw_stress)

raw_stress <- raw_stress[-1, -1] # 불필요한 행/열 삭제
raw_stress[8, c(1 : 4)] <- NA # 빈 부분 NA 지정
table(is.na(raw_stress))

# tibble로 변경, 행/열 전치, 행이름 설정
stress <- as_tibble(t(raw_stress), rownames = "date")

# 변수 타입 변경
stress$V1 <- as.integer(stress$V1)
stress$V2 <- as.integer(stress$V2)
stress$V3 <- as.integer(stress$V3)
stress$V4 <- as.integer(stress$V4)
stress$V5 <- as.integer(stress$V5)
stress$V6 <- as.integer(stress$V6)
stress$V7 <- as.integer(stress$V7)
stress$V8 <- as.integer(stress$V8)
stress$V9 <- as.integer(stress$V9)
stress$V10 <- as.integer(stress$V10)
stress$V11 <- as.integer(stress$V11)
stress$V12 <- as.integer(stress$V12)
stress$V13 <- as.integer(stress$V13)
stress$V14 <- as.integer(stress$V14)
stress$V15 <- as.integer(stress$V15)
stress$V16 <- as.integer(stress$V16)
stress$V17 <- as.integer(stress$V17)
stress$date <- as.Date(stress$date, format = "%Y")
str(stress)

# 데이터 정제
mean(stress$V8, na.rm = T) # 결측값 제외 평균
stress$V8 <- ifelse(is.na(stress$V8), 922.25, stress$V8) # 결측값 대체

# 총 스트레스 고증상자 변수 생성
stress <- stress %>% 
  select(date) %>% 
  mutate(sum = stress$V1 + stress$V2 + stress$V3 + stress$V4 + stress$V5 + stress$V6 +
           stress$V7 + stress$V8 + stress$V9 + stress$V10 + stress$V11 + stress$V12 + stress$V13 + 
           stress$V14 + stress$V15 + stress$V16 + stress$V17)
stress

### 6. walk 데이터 준비
# 데이터 불어오기
raw_walk <- read_excel("walk(08~19).xlsx")

# 데이터 파악
head(raw_walk)
summary(raw_walk)

raw_walk <- raw_walk[-1, -1] # 불필요한 행/열 삭제
raw_walk[8, c(1 : 4)] <- NA # 빈 부분 NA 지정
table(is.na(raw_walk))

# tibble로 변경, 행/열 전치, 행이름 설정
walk <- as_tibble(t(raw_walk), rownames = "date")

# 변수 타입 변경
walk$V1 <- as.integer(walk$V1)
walk$V2 <- as.integer(walk$V2)
walk$V3 <- as.integer(walk$V3)
walk$V4 <- as.integer(walk$V4)
walk$V5 <- as.integer(walk$V5)
walk$V6 <- as.integer(walk$V6)
walk$V7 <- as.integer(walk$V7)
walk$V8 <- as.integer(walk$V8)
walk$V9 <- as.integer(walk$V9)
walk$V10 <- as.integer(walk$V10)
walk$V11 <- as.integer(walk$V11)
walk$V12 <- as.integer(walk$V12)
walk$V13 <- as.integer(walk$V13)
walk$V14 <- as.integer(walk$V14)
walk$V15 <- as.integer(walk$V15)
walk$V16 <- as.integer(walk$V16)
walk$V17 <- as.integer(walk$V17)
walk$date <- as.Date(walk$date, format = "%Y")
str(walk)

# 데이터 정제
mean(walk$V8, na.rm = T) # 결측값 제외 평균
walk$V8 <- ifelse(is.na(walk$V8), 922, walk$V8) # 결측값 대체

# 총 걷기 인원 변수 생성
walk <- walk %>% 
  select(date) %>% 
  mutate(sum = walk$V1 + walk$V2 + walk$V3 + walk$V4 + walk$V5 + walk$V6 + walk$V7 + walk$V8 + walk$V9 + 
           walk$V10 + walk$V11 + walk$V12 + walk$V13 + walk$V14 + walk$V15 + walk$V16 + walk$V17)
walk

### 7. dis 데이터 준비
# 데이터 불러오기
raw_dis <- read_excel("dis(08~19).xlsx")

# 데이터 파악
head(raw_dis)
summary(raw_dis)

raw_dis <- raw_dis[-1, -1] # 불필요한 행/열 삭제
raw_dis[8, c(1 : 4)] <- NA # 빈 부분 NA 지정
table(is.na(raw_dis))

# tibble로 변경, 행/열 전치, 행이름 설정
dis <- as_tibble(t(raw_dis), rownames = "date")

# 변수 타입 변경
dis$V1 <- as.integer(dis$V1)
dis$V2 <- as.integer(dis$V2)
dis$V3 <- as.integer(dis$V3)
dis$V4 <- as.integer(dis$V4)
dis$V5 <- as.integer(dis$V5)
dis$V6 <- as.integer(dis$V6)
dis$V7 <- as.integer(dis$V7)
dis$V8 <- as.integer(dis$V8)
dis$V9 <- as.integer(dis$V9)
dis$V10 <- as.integer(dis$V10)
dis$V11 <- as.integer(dis$V11)
dis$V12 <- as.integer(dis$V12)
dis$V13 <- as.integer(dis$V13)
dis$V14 <- as.integer(dis$V14)
dis$V15 <- as.integer(dis$V15)
dis$V16 <- as.integer(dis$V16)
dis$V17 <- as.integer(dis$V17)
dis$date <- as.Date(dis$date, format = "%Y")
str(dis)

# 데이터 정제
mean(dis$V8, na.rm = T) # 결측값 제외 평균
dis$V8 <- ifelse(is.na(dis$V8), 922.25, dis$V8) # 결측값 대체

# 총 우울감 인원 변수 생성
dis <- dis %>% 
  select(date) %>% 
  mutate(sum = dis$V1 + dis$V2 + dis$V3 + dis$V4 + dis$V5 + dis$V6 + dis$V7 + dis$V8 + dis$V9 + 
           dis$V10 + dis$V11 + dis$V12 + dis$V13 + dis$V14 + dis$V15 + dis$V16 + dis$V17)
dis

## 2. 흡연자 수 변화(2008 ~ 2019)
change_smok <- xts(smoking$sum, order.by = smoking$date)
head(change_smok)
dygraph(change_smok) %>% dyRangeSelector() # 날짜 범위 선택 기능

## 3. 월간 음주자의 변화(2007 ~ 2018)
change_alc <- xts(alc$sum, order.by = alc$date)
head(change_alc)
dygraph(change_alc) %>% dyRangeSelector() # 날짜 범위 선택 기능

## 4. 걷기 실천자와 비만인 수(2008 ~ 2018)
walk_a <- walk[-12, ] # 19년도 데이터를 제거한 걷기 실천자 수

change_walk_a <- xts(walk_a$sum, order.by = walk_a$date)
change_fat <- xts(fat$sum, order.by = fat$date)

walk_fat <- cbind(change_walk_a, change_fat) # 데이터 결합
walk_fat

colnames(walk_fat) <- c("walk", "fat") # 변수명 변경

dygraph(walk_fat) %>% dyRangeSelector() # 날짜 범위 선택 기능

cor.test(walk_a$sum, fat$sum) # 상관분석

## 5. 우울감 경험자의 변화(2008 ~ 2019)
change_dis <- xts(dis$sum, order.by = dis$date)
change_dis
dygraph(change_dis) %>% dyRangeSelector() # 날짜 범위 선택 기능

## 6. 스트레스 인지하는 사람수와 비만인 수(2008 ~ 2018)
stress_a <- stress[-12, ]

change_stress_a <- xts(stress_a$sum, order.by = stress_a$date)

stress_fat <- cbind(change_stress_a, change_fat)
stress_fat

colnames(stress_fat) <- c("stress", "fat") # 변수명 변경

dygraph(stress_fat) %>% dyRangeSelector() # 날짜 범위 선택 기능

## 7. 스트레스 인지하는 사람수와 우울감 경험자(2008 ~ 2019)
change_stress <- xts(stress$sum, order.by = stress$date)

stress_dis <- cbind(change_stress, change_dis)
stress_dis

colnames(stress_dis) <- cbind("stress", "dis") # 변수명 변경

dygraph(stress_dis) %>% dyRangeSelector() # 날짜 범위 선택 기능

