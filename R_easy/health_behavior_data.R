# 패키지 로드 및 데이터 불러오기

library(ggplot2) # 시각화 패키지
library(dplyr) # 전처리 패키지
library(gridExtra) # 그래프 화면 분할 패키지
data <- read.csv('data.csv') # 데이터 불러오기

colnames(data) # 열 이름 추출
str(data) # 데이터 구성 파악
summary(data) # 요약통계량 산출

# (1) 현재 흡연율 top5
smoke_top5 <- data %>% 
  select(지역, 현재흡연율) %>% 
  filter(지역 != '서울시') %>% 
  arrange(desc(현재흡연율)) %>% 
  head(5)

# 시각화  
ggplot(data = smoke_top5, aes(x = reorder(지역, -현재흡연율), y = 현재흡연율, fill = 지역)) +
  geom_col() +
  theme_classic() +
  xlab('지역') +
  ggtitle('흡연율 상위 5지역') +
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 20),
        legend.position = "bottom")
  
# (2) 스트레스 인지율 bottom5
stress_bottom5 <- data %>% 
  select(지역, 스트레스_인지율) %>% 
  filter(지역 != '서울시') %>% 
  arrange(스트레스_인지율) %>% 
  head(5)

# 시각화
ggplot(data = stress_bottom5, aes(x = reorder(지역, 스트레스_인지율), y=스트레스_인지율, fill = 지역)) +
  geom_col() +
  theme_classic() +
  xlab('지역') +
  ggtitle('스트레스 인지율 하위 5지역') +
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 20),
        legend.position = "bottom")

# (3) 비만율 top7
fat_top7 <- data %>% 
  select(지역, 비만율) %>% 
  filter(지역 != '서울시') %>% 
  arrange(desc(비만율)) %>% 
  head(7)

# 시각화
g1 <- ggplot(data = fat_top7, aes(x = reorder(지역, -비만율), y = 비만율, fill = 지역)) +
  geom_col() +
  theme_classic() +
  xlab('지역') +
  ggtitle('비만율 상위 7지역') +
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 15),
        legend.position = '')

# (4) 중증도 이상 신체활동율 bottom7
exercise_bottom7 <- data %>% 
  select(지역, 중증도_이상_신체활동_실천율) %>% 
  filter(지역 != '서울시') %>% 
  arrange(중증도_이상_신체활동_실천율) %>% 
  head(7)

# 시각화
g2 <- ggplot(data = exercise_bottom7, aes(x = reorder(지역, -중증도_이상_신체활동_실천율), 
                                 y = 중증도_이상_신체활동_실천율, fill = 지역)) +
  geom_col() +
  theme_classic() +
  xlab('지역') +
  ylab('중증도 이상 신체활동율') +
  ggtitle('중증도 이상 신체활동율 하위 7지역') +
  theme(plot.title = element_text(face = 'bold',
                                  hjust = 0.5,
                                  size = 15),
        legend.position = " ")

grid.arrange(g1, g2)
