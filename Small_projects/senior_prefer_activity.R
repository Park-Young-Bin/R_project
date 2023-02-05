# 1. 필요한 패키지 로드
library(dplyr)
library(ggplot2)
library(readxl)

# 2. 데이터 불러오기
raw_data <- read_excel("senior_pre.xlsx",
                       sheet = 1, # 1번 시트 불러오기
                       range = "A1:L18", # 범위 설정
                       col_names = T, # 열 이름 불러오기
                       na = "NA") # 비어있는 값에 사용할 문자열 지정
# 3. 데이터 속성 파악
head(raw_data)
summary(raw_data)
str(raw_data)
table(is.na(raw_data))
class(raw_data)

# 4. 성별에 따른 여가 선호도 분석(파이차트, 막대 그래프)
# 4-1. 남자 데이터 추출
raw_male <- raw_data[2, c(4 : 12)] # 필요한 데이터만 추출
male <- as_tibble(t(raw_male), rownames = "group") # 행, 열 전치
str(male)

male$V1 <- as.numeric(male$V1) # 변수 타입 변경
str(male)

male[c(1 : 8), 1] <- c("운동/건강", "노래/오락", "교육/교양", "직업(수입)",
                     "여행/레저", "사회봉사", "사교", "전통문화") # 데이터 변경

male <- rename(male, value = V1) # 변수명 변경

male
# 4-2. 막대 그래프 그리기
bar_male <- ggplot(data = male, aes(x = " ", y = value, fill = group)) + 
  geom_bar(width = 1, stat = "identity", color = "white")
bar_male

# 4-3. 파이 그래프 그리기
pie_male <- bar_male + 
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(value, "%")),
            position = position_stack(vjust = 0.5)) + 
  theme_void()
pie_male

# 4-4. 여자 데이터 추출
raw_female <- raw_data[3, c(4 : 12)] # 필요한 데이터만 추출
female <- as_tibble(t(raw_female), rownames = "group") # 행, 열 전치
str(female)

female$V1 <- as.numeric(female$V1) # 변수 타입 변경
str(female)

female[c(1 : 8), 1] <- c("운동/건강", "노래/오락", "교육/교양", "직업(수입)",
                         "여행/레저", "사회봉사", "사교", "전통문화") # 데이터 변경

female <- rename(female, value = V1) # 변수명 변경

# 4-4. 성별 그래프 화면분할
install.packages("gridExtra")
library(gridExtra)

g_male <- ggplot(data = male, aes(x = reorder(group, -value), y = value, fill = group)) + 
  geom_col() + 
  geom_text(aes(label = value), vjust = 1.5) + # 레이블 표시
  ggtitle("19년 65세 이상 성별 선호 여가활동") + 
  xlab("선호여가(남)") +
  ylab("선호도(%)") + 
  theme_bw() + # 그래프 테마 변경
  theme(legend.position = " ", #범례 제거
        plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 18)) 
  
g_male

g_female <- ggplot(data = female, aes(x = reorder(group, -value), y = value, fill = group)) + 
  geom_col() + 
  geom_text(aes(label = value), vjust = 1.5) + # 레이블 표시
  xlab("선호여가(여)") +
  ylab("선호도(%)") + 
  theme_bw() + # 그래프 테마 변경
  theme(legend.position='none') #범례 제거
g_female

grid.arrange(g_male, g_female) # 그래프 화면분할

# 5. 19년도 소득별 선호여가활동 top5 (100만원 미만, 500만원 이상)
## 5-1. 100만원 미만 그룹 데이터 전처리
raw_low <- raw_data[9, c(4 : 12)]  # 필요한 데이터만 추출
low <- as_tibble(t(raw_low), rownames = "group")
str(low)

low$V1 <- as.numeric(low$V1) # 변수 타입 변경
str(male)

low[c(1 : 8), 1] <- c("운동/건강", "노래/오락", "교육/교양", "직업(수입)",
                       "여행/레저", "사회봉사", "사교", "전통문화") # 데이터 변경

low <- rename(low, value = V1) # 변수명 변경
low

# 저소득 노인의 상위 5가지 선호여가 활동
top5_low <- low %>% 
  arrange(desc(value)) %>% 
  head(5)
top5_low

df_1 <- ggplot(data = top5_low, aes(x = reorder(group, -value), y = value, fill = group)) + 
  geom_col() + 
  geom_text(aes(label = value), vjust = 1.5) + # 레이블 표시
  xlab("선호여가(100만원 미만)") +
  ylab("선호도(%)") + 
  ggtitle("19년 소득별 선호 여가 활동(100만원 미만 vs 500만원 이상)") + 
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 18))
df_1

## 5-2. 500만원 이상 그룹 데이터 전처리
raw_high <- raw_data[14, c(4 : 12)]  # 필요한 데이터만 추출
high <- as_tibble(t(raw_high), rownames = "group")
str(high)

high$V1 <- as.numeric(high$V1) # 변수 타입 변경
str(high)

high[c(1 : 8), 1] <- c("운동/건강", "노래/오락", "교육/교양", "직업(수입)",
                      "여행/레저", "사회봉사", "사교", "전통문화") # 데이터 변경

high <- rename(high, value = V1) # 변수명 변경
high

# 고소득 노인의 상위 5가지 선호여가 활동
top5_high <- high %>% 
  arrange(desc(value)) %>% 
  head(5)
top5_high

df_2 <- ggplot(data = top5_high, aes(x = reorder(group, -value), y = value, fill = group)) + 
  geom_col() + 
  geom_text(aes(label = value), vjust = 1.5) + # 레이블 표시
  xlab("선호여가(500만원 미만)") +
  ylab("선호도(%)")

df_2

grid.arrange(df_1, df_2) # 그래프 화면분할

# 6. 15~19년 서울시 선호여가 활동 선 그래프
## 6-1. 데이터 불러오기
data_19 <- read_excel("senior_pre.xlsx",
                      sheet = 1, # 1번 시트 불러오기
                      range = "A1:L2", # 범위 설정
                      col_names = T, # 열 이름 불러오기
                      na = "NA") # 비어있는 값에 사용할 문자열 지정

data_18 <- read_excel("senior_pre.xlsx",
                      sheet = 2, # 1번 시트 불러오기
                      range = "A1:L2", # 범위 설정
                      col_names = T, # 열 이름 불러오기
                      na = "NA") # 비어있는 값에 사용할 문자열 지정

data_17 <- read_excel("senior_pre.xlsx",
                      sheet = 3, # 1번 시트 불러오기
                      range = "A1:M2", # 범위 설정
                      col_names = T, # 열 이름 불러오기
                      na = "NA") # 비어있는 값에 사용할 문자열 지정

data_16 <- read_excel("senior_pre.xlsx",
                      sheet = 4, # 1번 시트 불러오기
                      range = "A1:L2", # 범위 설정
                      col_names = T, # 열 이름 불러오기
                      na = "NA") # 비어있는 값에 사용할 문자열 지정

data_15 <- read_excel("senior_pre.xlsx",
                      sheet = 5, # 1번 시트 불러오기
                      range = "A1:L2", # 범위 설정
                      col_names = T, # 열 이름 불러오기
                      na = "NA") # 비어있는 값에 사용할 문자열 지정

## 6-2 데이터 전처리
raw_year <- bind_rows(data_19,data_18, data_17, data_16, data_15) # 데이터 세로로 합치기
raw_year <- raw_year[, c(-2, -3, -13)] # '기타' 열 제목
class(raw_data)
raw_year

colnames(raw_year) <- c("year", "운동/건강", "노래/오락", "교육/교양", "직업(수입)",
                        "여행/레저", "사회봉사", "사교", "전통문화", "없음")
str(raw_year)

library(reshape2) # reshape2 패키지 로드
year <- melt(raw_year, id = c("year")) # year변수 기준으로 데이터 변형형
head(year)

year <- rename(year, group = variable, per = value) # 변수명 변경

## 6-3 선 그래프 작성(+ 인터랙티브)
library(plotly)

p <- ggplot(data = year, aes(x = year, y = per, col = group)) + 
  geom_line(size = 0.5, aes(linetype = group)) + 
  ggtitle("15~19년 선호여가 활동 변화") + # 그래프 제목
  xlab("연도") + # x축 제목
  ylab("퍼센트(%)") + # y축 제목
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        legend.title = element_blank()) # 범례 제목 제거

ggplotly(p)
