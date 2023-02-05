
library(ggplot2) # ggplot2 패키지 로드
library(dplyr) # dplyr 패키지 로드
library(readxl) # readxl 패키지 로드

raw_data <- read_excel("seoul_pop(gu).xlsx") # 엑셀 파일 불러오기
raw_data <- as.data.frame(raw_data) # 데이터 프레임 형태로 불러오기

head(raw_data) # 데이터 앞 부분 출력

seoul <- raw_data %>% 
  filter(기간 != "기간" & 자치구 != "합계") %>% # 필요 없는 행 삭제
  select(-세대, -세대당인구) # 필요 없는 열 삭제

seoul$기간 <- c(1:25) # 변수 속성 변경
seoul <- rename(seoul,
                id = 기간,
                region = 자치구, # 자치구
                tot = 인구...4, # 전체 인구 수
                tot_m = 인구...5, # 전체 남성 수
                tot_f = 인구...6, # 전체 여성 수
                kor_tot = 인구...7, # 전체 한국인 수
                kor_m = 인구...8, # 한국인 남성 수
                kor_f = 인구...9, # 한국인 여성 수
                for_tot = 인구...10, # 전체 등록외국인 수
                for_m = 인구...11, # 등록 외국 남성 수
                for_f = 인구...12, # 등록 외국 여성 수
                senior = "65세이상고령자") # 65세 이상 고령자

head(seoul) # 데이터 앞 부분 출력
str(seoul) # 데이터 속성 파악
is.na(seoul) # 결측치 확인
table(is.na(seoul)) # 결측치 확인

# int 타입으로 변환
seoul$tot <- as.integer(seoul$tot) 
seoul$tot_m <- as.integer(seoul$tot_m)
seoul$tot_f <- as.integer(seoul$tot_f)
seoul$kor_tot <- as.integer(seoul$kor_tot)
seoul$kor_m <- as.integer(seoul$kor_m)
seoul$kor_f <- as.integer(seoul$kor_f)
seoul$for_tot <- as.integer(seoul$for_tot)
seoul$for_m <- as.integer(seoul$for_m)
seoul$for_f <- as.integer(seoul$for_f)
seoul$senior <- as.integer(seoul$senior)
str(seoul)

# 총인구 상위 5지역
top5 <- seoul %>% 
  select(region, tot) %>% 
  arrange(desc(tot)) %>% 
  head(5)

ggplot(data = top5, aes(x = reorder(region, tot), y = tot, fill = region)) + 
  geom_col() + # 막대 그래프
  ggtitle("Top5_tot") + # 그래프 제목
  theme(plot.title = element_text(face = "bold", # 그래프 제목 굵게
                                  hjust = 0.5,# 0 = 왼쪽, 0.5 = 가운데, 1 = 오른쪽
                                  size = 20), # 글자 크기
        axis.title.x = element_text(face = "bold", # x축 제목 굵게
                                    hjust = 0.5, # 0 = 왼쪽정렬, 0.5 = 중앙정렬, 1 = 우측정렬
                                    size = 15, # x축 제목 크기
                                    angle = 0), # x축 제목 회전
        axis.title.y = element_text(face = "bold",
                                    size = 15, 
                                    vjust = 0.5, # 0 = 하단정렬, 0.5 = 중앙정렬, 1 = 상단정렬
                                    angle = 0),
        legend.position = "bottom") + # 범례 위치 설정
  xlab("지역구") + # x축 제목
  ylab("총 인구") + # y축 제목
  scale_y_continuous(limits=c(0, 690000), labels=scales::comma) # 정수 형태로 축 범위 설정

# 총인구 하위 5지역
bottom5 <- seoul %>% 
  select(region, tot) %>% 
  arrange(tot) %>% 
  head(5)

ggplot(data = bottom5, aes(x = reorder(region, -tot), y = tot, fill = region)) +
  geom_col() +
  ggtitle("Bottom5_tot") +
  theme(plot.title = element_text(face = "bold", # 그래프 제목 굵게
                                  hjust = 0.5,# 0 = 왼쪽, 0.5 = 가운데, 1 = 오른쪽
                                  size = 20), # 글자 크기
        axis.title.x = element_text(face = "bold", # x축 제목 굵게
                                    hjust = 0.5, # 0 = 왼쪽정렬, 0.5 = 중앙정렬, 1 = 우측정렬
                                    size = 15, # x축 제목 크기
                                    angle = 0), # x축 제목 회전
        axis.title.y = element_text(face = "bold",
                                    size = 15, 
                                    vjust = 0.5, # 0 = 하단정렬, 0.5 = 중앙정렬, 1 = 상단정렬
                                    angle = 0),
        legend.position = "bottom") + # 범례 위치 설정
  xlab("지역구") + # x축 제목
  ylab("총 인구") + # y축 제목
  scale_y_continuous(limits = c(0, 690000), labels = scales::comma) # 정수 형태로 축 범위 설정

# 고령자가 많은 지역구
Top_senior <- seoul %>% 
  select(region, senior) %>% 
  arrange(desc(senior)) %>% 
  head(5)
Top_senior

ggplot(data = Top_senior, aes(x = reorder(region, senior), y = senior, fill = region)) +
  geom_col() +
  theme(legend.position = "bottom") +
  ggtitle("Top_senior") +
  theme(plot.title = element_text(face = "bold", # 그래프 제목 굵게
                                  hjust = 0.5,# 0 = 왼쪽, 0.5 = 가운데, 1 = 오른쪽
                                  size = 20), # 글자 크기
        axis.title.x = element_text(face = "bold", # x축 제목 굵게
                                    hjust = 0.5, # 0 = 왼쪽정렬, 0.5 = 중앙정렬, 1 = 우측정렬
                                    size = 15, # x축 제목 크기
                                    angle = 0), # x축 제목 회전
        axis.title.y = element_text(face = "bold",
                                    size = 15, 
                                    vjust = 0.5, # 0 = 하단정렬, 0.5 = 중앙정렬, 1 = 상단정렬
                                    angle = 0),
        legend.position = "bottom") + # 범례 위치 설정
  xlab("지역구") +
  ylab("고령자 인구") +
  scale_y_continuous(limits = c(0, 690000), labels = scales::comma)

# 한국 남성이 많은 지역
Top_kor_m <- seoul %>% 
  select(region, kor_m) %>% 
  arrange(desc(kor_m)) %>% 
  head(5)

ggplot(data = Top_kor_m, aes(x = reorder(region, kor_m), y = kor_m, fill = region)) +
  geom_col() +
  ggtitle("Top_kor_m") +
  theme(plot.title = element_text(face = "bold", # 그래프 제목 굵게
                                  hjust = 0.5,# 0 = 왼쪽, 0.5 = 가운데, 1 = 오른쪽
                                  size = 20), # 글자 크기
        axis.title.x = element_text(face = "bold", # x축 제목 굵게
                                    hjust = 0.5, # 0 = 왼쪽정렬, 0.5 = 중앙정렬, 1 = 우측정렬
                                    size = 15, # x축 제목 크기
                                    angle = 0), # x축 제목 회전
        axis.title.y = element_text(face = "bold",
                                    size = 15, 
                                    vjust = 0.5), # 0 = 하단정렬, 0.5 = 중앙정렬, 1 = 상단정렬
        legend.position = "bottom") +
  xlab("지역구") +
  ylab("한국 남성 인구") + 
  scale_y_continuous(limits = c(0, 690000), labels = scales::comma)

# 한국 여성이 많은 지역
Top_kor_f <- seoul %>% 
  select(region, kor_f) %>% 
  arrange(desc(kor_f)) %>% 
  head(5)

ggplot(data = Top_kor_f, aes(x = reorder(region, kor_f), y = kor_f, fill = region)) +
  geom_col() +
  ggtitle("Top_kor_f") + 
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        axis.title.x = element_text(face = "bold",
                                    size = 15),
        axis.title.y = element_text(face = "bold",
                                   size = 15),
        legend.position = "bottom") +
  xlab("지역구") +
  ylab("한국 여성 인구") + 
  scale_y_continuous(limits = c(0, 690000), labels = scales::comma)

# 등록 외국인 수 남/여 평균
mean_for <- data.frame(sex = c("male", "female"),
                       mean = c(mean(seoul$for_m), mean(seoul$for_f)))
head(mean_for)

ggplot(data = mean_for, aes(x = reorder(sex, -mean), y = mean, fill = sex)) +
         geom_col() +
  ggtitle("For_mean") +
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        axis.title.x = element_text(face = "bold",
                                    size = 15),
        axis.title.y = element_text(face = "bold",
                                    size = 15),
        legend.position = "bottom") +
  xlab("성별") +
  ylab("등록 외국인 평균")
