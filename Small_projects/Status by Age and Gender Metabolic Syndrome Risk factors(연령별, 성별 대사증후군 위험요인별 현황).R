# 0. 패키지, 파일 불러오기
library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(plotly)

list.files()

# 1. 시간에 따른 대사증후군 위험요인별 변화 (시계열 그래프)
raw_time <- read_excel("medical_total.xlsx",
                        range = "C2:H8")
class(raw_time)
str(raw_time)
head(raw_time)
raw_time <- rename(raw_time, year = 시점) # 변수명 변경

time <- melt(raw_time, id = "year")
head(time)

time <- rename(time, risk_factor = variable, per = value) # 변수명 변경
str(time)
time$year <- as.numeric(time$year)
time$risk_factor <- as.character(time$risk_factor)

p <- ggplot(data = time, aes(x = year, y = per, col = risk_factor)) +
  geom_line(size = 0.6, aes(linetype = risk_factor)) + 
  ggtitle("대사증후군 위험요인별 변화(13년 ~ 18년)") +  # 그래프 제목 설정
  theme_bw() + 
  theme(plot.title = element_text(face = "bold", # 그래프 제목 서식
                                  size = 15),
        legend.title = element_blank()) # 범례 제목 제거
ggplotly(p)
  
# 2. 18년 성별 대사증후군 위험요인 순위(막대 그래프)
raw_data <- read_excel("medical_18(비율).xlsx",
                       range = "A2:G5")
raw_data <- raw_data[-1, -2]
raw_data

data <- melt(raw_data, id = "성별(1)")
data <- rename(data, sex = "성별(1)", risk_factor = variable, per = value)
data
str(data)

data$per <- as.numeric(data$per)

ggplot(data = data, aes(x = risk_factor, y = per, fill = sex)) +
  geom_col() + 
  facet_grid(~sex) + # 집단 간 비교를 위한 화면분할 +
  ggtitle("18년 성별 위험요인 순위") + 
  xlab("위험요인") +
  ylab("비율(%)") +
  theme(plot.title = element_text(face = "bold",
                                  size = 20,
                                  hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust = 1,
                                   angle = 20),
        legend.position = "none")
  
# 3. 18년 성별 '높은혈압'이 가장 높은 비율을 차지하는 연령대(top3, 막대 그래프)
raw_high_blood <- read_excel("medical_18_high.xlsx",
                             range = "A2:D35")
raw_high_blood <- raw_high_blood[-1, -3]
high_blood <- rename(raw_high_blood, sex = '성별(1)', ageg = '연령별(1)', high_bp = 높은혈압)
str(high_blood)
high_blood$high_bp <- as.numeric(high_blood$high_bp)

## (1) 남자 데이터 추출
high_bp <- c(high_blood[1, 3],
                  high_blood[2, 3],
                  high_blood[3, 3] + high_blood[4, 3],
                  high_blood[5, 3] + high_blood[6, 3],
                  high_blood[7, 3] + high_blood[8, 3],
                  high_blood[9, 3] + high_blood[10, 3],
                  high_blood[11, 3] + high_blood[12, 3],
                  high_blood[13, 3] + high_blood[14, 3],
                  high_blood[15, 3] + high_blood[16, 3])

high_bp
class(high_bp)
high_bp <- as.numeric(high_bp)

sex <- rep("남자", 9)
sex

ageg <- rep(c("총", "19세 이하", "20대", "30대", "40대", "50대", "60대", "70대", "80대 이상"))
ageg

male <- data.frame(sex, ageg, high_bp)
male <- male %>% 
  filter(ageg != "총") %>% 
  arrange(desc(high_bp)) %>% 
  mutate(per = round(high_bp / 1438467 * 100, 1)) %>%
  head(3)
male

## (2) 여자 데이터 추출
high_bp <- c(high_blood[17, 3],
                    high_blood[18, 3],
                    high_blood[19, 3] + high_blood[20, 3],
                    high_blood[21, 3] + high_blood[22, 3],
                    high_blood[23, 3] + high_blood[24, 3],
                    high_blood[25, 3] + high_blood[26, 3],
                    high_blood[27, 3] + high_blood[28, 3],
                    high_blood[29, 3] + high_blood[30, 3],
                    high_blood[31, 3] + high_blood[32, 3])
high_bp <- as.numeric(high_bp)

sex <- rep("여자", 9)
sex

female <- data.frame(sex, ageg, high_bp)
female <- female %>% 
  filter(ageg != "총") %>% 
  arrange(desc(high_bp)) %>% 
  mutate(per = round(high_bp / 1021417 * 100, 1)) %>% 
  head(3)
female

sex_18 <- bind_rows(male, female)
sex_18

## (3) 그래프 그리기
ggplot(data = sex_18, aes(x = ageg, y = per, fill = sex)) +
  geom_col()+
  facet_grid(~sex) + 
  ggtitle("18년 성별 높은혈압이 많은 연령대 Top3") +
  theme_bw() +
  ylab("비율(%)") + 
  theme(plot.title = element_text(face = "bold",
                                  size = 16,
                                  hjust = 0.5),
        legend.position = "none",
        axis.title.x = element_blank())

# 4. 13~18년 20~30대의 대사증후군 위험요인 순위(막대 그래프)
raw_medical <- read_excel("medical_ageg.xlsx")

## 20대 데이터 분석
raw_20 <- raw_medical %>% 
  filter(연령별 %in% c('20 ~ 24세', '25 ~ 29세')) %>% 
  arrange(desc(시점), 대사증후군위험요인별)
raw_20 <- rename(raw_20, year = 시점, ageg = 연령별, risk_factor = 대사증후군위험요인별, num = '인원 (명)')

# 패키지 로드
library(stringr)

# 연령대 재분할
raw_20$ageg <- raw_20$ageg %>%
  strsplit("~") %>% # '~' 기준으로 문자 분리 (리스트 형태로 반환됨)
  sapply("[[", 1) %>% # 각 리스트에서 첫 번째 값만 가져옴
  str_trim %>% # 공백 제거
  as.numeric %>%  # 변수 타입 변경
  cut(breaks = c(-Inf, seq(10, 90, by = 10), Inf), # breaks: 구간수
      labels = paste0(seq(0, 90, by = 10), "대"), right = F) # labels: 구간 이름

# 시점/연령/위험요인별 인원 합계
raw_20 %>% group_by(year, ageg, risk_factor) %>%
  summarise(sum = sum(num), .groups = "drop")

