#### 1. 필요한 패키지와 데이터 불러오기 ----

library(ggplot2) # 시각화 패키지
dt <- read.csv('insurance.csv') # csv 파일 불러오기

#### 2. 데이터 탐색 ----

dim(dt) # 데이터 차원 추출(행, 열 정보 확인)
head(dt) # 상위 5개 데이터 추출
tail(dt) # 하위 5개 데이터 추출
str(dt) # 데이터 속성 출력
summary(dt) # 요약통계량 추출

# 2.1 수치 변수들의 표준편차 확인

sd(dt$age) # 14.04996
sd(dt$bmi) # 6.098382
sd(dt$children) # 1.205493
sd(dt$expenses) # 12110.01

#### 3. 데이터 시각화 ----
#### 3.1 히스토그램 ----
# 나이
ggplot(data = dt, aes(x = age)) + 
  geom_bar() + 
  theme_classic() + # 그래프 테마 설정
  xlab('나이') + # x축 이름
  ylab('빈도수') + # y축 이름
  ggtitle('나이 히스토그램') + # 제목
  theme(plot.title = element_text(face = 'bold', # 제목 굵게
                                  hjust = 0.5, # 제목 위치
                                  size = 20)) # 글꼴 크기
# 성별
ggplot(data = dt, aes(x = sex)) + 
  geom_bar() + 
  theme_classic() + 
  xlab('성별') + 
  ylab('빈도수') + 
  ggtitle('성별 히스토그램') + 
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 20))
# bmi
ggplot(data = dt, aes(x = bmi)) + 
  geom_bar() + 
  theme_classic() + 
  xlab('bmi') + 
  ylab('빈도수') + 
  ggtitle('bmi 히스토그램') + 
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 20))

# children
ggplot(data = dt, aes(x = children)) + 
  geom_bar() + 
  theme_classic() + 
  xlab('children') + 
  ylab('빈도수') + 
  ggtitle('children 히스토그램') + 
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 20))

# smoker
ggplot(data = dt, aes(x = smoker)) + 
  geom_bar() + 
  theme_classic() + 
  xlab('smoker') + 
  ylab('빈도수') + 
  ggtitle('smoker 히스토그램') + 
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 20))
# region
ggplot(data = dt, aes(x = region)) + 
  geom_bar() + 
  theme_classic() + 
  xlab('region') + 
  ylab('빈도수') + 
  ggtitle('region 히스토그램') + 
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 20))

####
#### 3.2 상자그림 ----
# 성별
ggplot(data = dt, aes(x = sex, y = expenses)) + 
  geom_boxplot() + 
  theme_classic() + 
  ggtitle('성별 상자그림') + 
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 20))

# smoker
ggplot(data = dt, aes(x = smoker, y = expenses)) + 
  geom_boxplot() + 
  theme_classic() + 
  ggtitle('smoker 상자그림') + 
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 20))
# region
ggplot(data = dt, aes(x = region, y = expenses)) + 
  geom_boxplot() + 
  theme_classic() + 
  ggtitle('region 상자그림') + 
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 15))
# expenses(종속변수)
ggplot(data = dt, aes(x = expenses)) + # 내림차순
  geom_boxplot() + 
  coord_flip()+ # 그래프 90도 회전
  theme_classic() + 
  ggtitle('expenses 상자그림') + 
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 15))

#### 3.3 줄기잎그림 ----
# 나이
stem(dt$age)
stem(dt$bmi)
stem(dt$children)
stem(dt$expenses)


#### 4. 산점도 ----
# 나이와 보험료의 산점도
ggplot(data = dt, aes(x = age, y = expenses)) + 
  geom_point() + 
  theme_classic() + 
  xlab('보험료') + 
  ylab('나이') + 
  ggtitle('나이_보험료 산점도') + 
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 20))

# bmi와 보험료의 산점도
ggplot(data = dt, aes(x = bmi, y = expenses)) + 
  geom_point() + 
  theme_classic() + 
  xlab('bmi') + 
  ylab('보험료') + 
  ggtitle('bmi_보험료 산점도') + 
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 20))
#### 5. 상관분석(수치형 자료만 가능) ----
cor.test(dt$age, dt$expenses)
cor.test(dt$bmi, dt$expenses)
cor.test(dt$children, dt$expenses)

#### 6. 회귀분석 ----

model <- lm(expenses ~., data = dt)
summary(model)

# 모형진단
par(mfrow = c(2, 2))
plot(model)

library(car) # 데이터 모델링 패키지
durbinWatsonTest(model)

car

# 예측
predict(model, newdata = data.frame(age = 25,
                                    sex = 'male',
                                    bmi = 20.2,
                                    children = 0,
                                    smoker = 'no',
                                    region = 'southwest'))
        
        