# 필요한 패키지 설치 및 로드
install.packages("reshape2")
library(reshape2)
library(dplyr)
library(ggplot2)

# 데이터 불러오기
raw_bus <- read.csv("bus_승하차.csv")
head(raw_bus)
str(raw_bus)
table(is.na(raw_bus))

# 승/하차가 가장 많은 노선번호(21년 1월 1일)
df <- raw_bus %>% 
  select("사용일자", "노선번호", "노선명", "승차총승객수", "하차총승객수") # 필요한 데이터만 추출

raw_Top5_bus <- df %>% 
  filter(사용일자 == "20210101") %>% 
  group_by(노선번호) %>% 
  summarise(sum_승차 = sum(승차총승객수),
            sum_하차 = sum(하차총승객수)) %>% 
  arrange(desc(sum_승차)) %>% 
  head(5)
raw_Top5_bus

Top5_bus <- melt(raw_Top5_bus, id = "노선번호")
Top5_bus

colnames(Top5_bus) <- c("노선번호", "승하차", "사람")

ggplot(data = Top5_bus, aes(x = reorder(노선번호, 사람/1000), y = 사람/1000, fill = 승하차)) +
  geom_col(position = "dodge") +
  ggtitle("Top_bus(21년 1월 1일)") +
  theme_bw() + # 배경설정
  xlab("노선번호") + 
  ylab("사람수(단위: 천 명)") +
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        axis.title.x = element_text(face = "bold",
                                    size = 15),
        axis.title.y = element_text(face = "bold",
                                    size = 15),
        legend.position = "top")

# 종로03번 정류장별 승/하차 승객수(21년 1월 4일)
jongro_03 <- raw_bus %>% 
  filter(사용일자 == "20210104" & 노선번호 == "종로03") %>% 
  select("역명", "승차총승객수", "하차총승객수")

jongro_03 <- rename(jongro_03,
                    승차인원 = 승차총승객수,
                    하차인원 = 하차총승객수) # 변수명 변경

jongro_03 <- melt(jongro_03, id = "역명")
head(jongro_03)

colnames(jongro_03) <- c("역명", "승하차", "승객수") # 열 변수명 변경

ggplot(data = jongro_03, aes(x = 역명, y = 승객수, col = 승하차, group = 승하차)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  ggtitle("종로03번 정류장별 승/하차 승객수") + 
  theme_bw() + # 배경설정
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        axis.text.x = element_text(hjust = 1,
                                   angle = 30),
        legend.title = element_blank(), # 범례 제목 삭제
        legend.position = "top",
        axis.title = element_blank()) # 축 제목 삭제

# 종로08번 정류장별 승/하차 승객수(21년 1월 4일)
jongro_08 <- raw_bus %>% 
  filter(사용일자 == "20210104" & 노선번호 == "종로08") %>% 
  select("역명", "승차총승객수", "하차총승객수")

jongro_08 <- melt(jongro_08, id = "역명")
head(jongro_08)

colnames(jongro_08) <- c("역명", "승하차", "승객수")

ggplot(data = jongro_08, aes(x = 역명, y = 승객수, col = 승하차, group = 승하차)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  ggtitle("종로08번 정류장별 승/하차 승객수") + 
  theme_bw() + # 배경설정
  theme(plot.title = element_text(face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        axis.text.x = element_text(hjust = 1,
                                   angle = 30),
        legend.title = element_blank(), # 범례 제목 삭제
        legend.position = "top",
        axis.title = element_blank()) # 축 제목 삭제
