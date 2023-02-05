### 1. 준비 작업
### (1) 패키지 로드 및 데이터 불러오기, 데이터 파악
library(dplyr)
library(ggplot2)
library(plotly)

data1 <- read.csv("gender_submission.csv")
data2 <- read.csv("test.csv")
data3 <- read.csv("train.csv")

# 데이터 파악
head(data1)
str(data1)
summary(data1)
table(is.na(data1)) # 결측값 없음


head(data2)
str(data2)
summary(data2)
table(is.na(data2)) # 결측값 존재(Age: 86개, Fare: 1개)

head(data3)
str(data3)
summary(data3)
table(is.na(data3)) # 결측값 존재(Age: 177개)

### (2) 데이터 전처리

# 데이터 통합
raw_full <- left_join(data1, data2, by = "PassengerId") # 변수(열) 추가/ PassengerId 기준으로 통합
head(raw_full)
tail(raw_full)

full <- bind_rows(data3, raw_full) # 행 추가
head(full)
tail(full)
str(full)
summary(full)
table(is.na(full)) # 결측값 존재(Age: 263개, Fare: 1개)

# Age 결측값, 이상치 처리
boxplot(full$Age)$stats # 상자 그림 통계치 출력
median(full$Age, na.rm = T) # 결측치 제외한 Age 변수 중앙값 출력

full$Age <- ifelse(is.na(full$Age), 28, full$Age) # 중앙값으로 결측값 대체
table(is.na(full$Age)) # 결측값 대체 확인(결측값 0)

### 2. 연령대별 생존여부
#Child < Youth < YouthAdult < MiddleAged < Senior 순서로 연령대를 선정하였다.

#그래프 결과, 26~35세 사이(YouthAdult)의 생존율이 가장 높았으며, Senior의 생존율은 대략 1%로 가장 낮았다.

# 연령대 파생변수 추가
full <- full %>% 
  mutate(Ageg = ifelse(full$Age > 0 & full$Age < 20, "Child",
                       ifelse(full$Age >= 20 & full$Age < 26, "Youth", 
                              ifelse(full$Age >= 26 & full$Age < 36, "YouthAdult",
                                     ifelse(full$Age >= 36 & full$Age < 65, "MiddleAged", 
                                            ifelse(full$Age >= 65, "Senior", NA))))))

# 연령대별 생존율 분석
sur_ageg <- full %>% 
  group_by(Ageg) %>% 
  summarise(n = n()) %>% 
  mutate(per = round(n/sum(n) * 100, 1)) %>% 
  arrange(desc(per))
sur_ageg

ggplot(data = sur_ageg, aes(x = reorder(Ageg, -per), y = per, fill = Ageg)) +
  geom_col() + 
  ggtitle("연령대별 생존율") + 
  theme_bw() + 
  xlab("연령대") + 
  ylab("퍼센트(%)") + 
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        legend.position = "top")

### 2-1. 성별에 따른 연령과 생존자수 그래프
#(1)의 결과에 따르면 10세 이하는 많이 살아남았지만 청장년층과 노인은 사망자가 더 많은 경향을 보인다.

#또한 성별에 따른 생존자수를 통해서 남성 생존자가 여성보다 압도적으로 많이 탑승에도 불구하고 여성 생존자가 휠씬 더 많이있다.

# (1) 연령_생존자수(인터랙티브)
gg <-ggplot(data = full, aes(x = Age, fill = factor(Survived))) + 
  geom_density(position = "identity", alpha = 0.5) + # "identity": 위치 조정 하지않고, 그래프를 겹쳐 그림
  geom_line(stat = "density") + # stat = "density" : smooth한 곡선
  geom_vline(aes(xintercept = median(Age)), linetype = "dashed", color = "black", size = .7) # 세로선 추가
gg
ggplotly(gg)

# (2) 성별_생존자수
sex_sur <- full %>% 
  select(Sex, Survived) %>% 
  mutate()

# 성별_생존자수 그래프
ggplot(data = sex_sur, aes(x = Sex, fill = factor(Survived))) + # Survived 변수 범주화
  geom_bar(position = "dodge") + # 그래프 분리
  ggtitle("Sex vs Survived") + 
  scale_fill_manual(values = c("Sky Blue 2","Dodger Blue2"), # 범례 서식 설정
                    name = ("Survived"),
                    breaks = c(0,1),
                    labels = c("사망","생존")) + 
  theme(legend.position = c(0.87,0.85)) # 범례 위치


### 3. 티켓 등급별 생존율
#티켓 등급별 생존율은 가장 저렴한 3등급 티켓을 구매한 승객들이 많이 생존 했음을 알 수 있다.

table(is.na(full$Pclass))
class(full$Pclass)

pclass_life <- full %>% 
  group_by(Pclass) %>% 
  summarise(n = n()) %>% 
  mutate(per = round(n/sum(n) * 100, 1))
pclass_life

ggplot(data = pclass_life, aes(x = Pclass, y = per, fill = factor(Pclass))) + # Pclass 변수 범주화
  geom_col() + 
  scale_fill_manual(values = c("Yellow 1", "Tomato 1", "Sky Blue 2"), # 범례 서식 설정
                    name = ("Pclass"),
                    breaks = c(1, 2, 3),
                    labels = c("1st", "2nd", "3rd")) + 
  theme(legend.position = c(0.15, 0.8)) + # 범례 위치
  scale_x_continuous(breaks = c(1, 2, 3), # x축 레이블 서식 설정
                     labels = c("1st", "2nd", "3rd"))


### 4. 티켓 등급별 성별에 따른 생존여부
#그리고 티켓 등급별 성별에 따른 생존자를 확인해 보면 남성보다 여성의 생존율이 높았다. 그리고 3등급 티켓을 구매한 남성은 사망가수가 압적으로 높다.

ggplot(data = full, aes(x = Pclass, fill = factor(Survived))) + # Survived 변수 범주화
  geom_bar() + 
  ggtitle("Sex vs Pclass vs Survived") + 
  facet_grid(~Sex) +  # 집단 간 비교를 위한 화면 분할
  scale_fill_discrete(name = element_text("Survived")) # 범례 제목 설정


### 5.탑승한 부모/자녀 수에 따라 생존여부
#우선 부모/자녀의 수는 거의 0명이거나 1, 2명이 대부분이다.
#탑승한 부모/자녀 수에 따른 생존자 수를 확인해 보면 0명인 경우가 사망자수가 많고, 1, 2명인 경우에는 생존자 수가 조금 더 많다고 볼 수 있다.

max(full$Parch)
min(full$Parch)

ggplot(data = full, aes(x = Parch, fill = factor(Survived))) + 
  geom_bar(position = "dodge") +
  ggtitle("Parch vs Survived") +
  scale_x_continuous(breaks=seq(0, 9, 1)) + # x축 간격 설정(최소, 최대, 간격)
  theme(legend.position = c(0.9, 0.8)) + 
  scale_fill_discrete(name = element_text("Survived")) # 범례 제목 설정


