##### 2007년부터 2018년까지 서울시 인터넷 쇼핑몰 100평가 정보----
# 서울시에 소재한 인터넷쇼핑몰에 대한 이용평가결과로 
# 종합쇼핑몰 50개, 오픈마켓6개, 해외구매대행사이트 5개, 컴퓨터전문몰 10개, 의류전문몰 10개, 가전전문몰 10개, 
# 화장품전문몰 5개, 도서전문몰 4개 총 100개 쇼핑몰에 대해 소비자보호평가, 소비자 이용만족평가, 피해발생평가 세 분야에 대해 심층분석 실시 결과입니다.

# 소비자보호평가: 50점
# 이용자만족평가: 40점
# 피해발생평가: 50점
# 전체평가: 100점

#### 1. 필요한 패키지 및 데이터 불러오기----

library(dplyr)
library(ggplot2)
library(readxl)

getwd()
df <- read.csv('seoul_shopping_mall(2007_2018).csv', header = T)

#### 2. 데이터 구조 확인 및 시각화(히스토그램, 상자 그림)----
head(df)
tail(df)
dim(df)
str(df)
summary(df)

# 각각 작성
par(mfrow = c(2,2)) # 2행 2열로 화면분할
hist(df$소비자보호평가, xlab='평가점수', ylab = '빈도수', main='소비자보호평가 히스토그램')
hist(df$이용자만족평가, xlab='평가점수', ylab = '빈도수', main='이용자만족평가 히스토그램')
hist(df$피해발생평가, xlab='평가점수', ylab = '빈도수', main='피해자발생평가 히스토그램')
hist(df$전체평가, xlab='평가점수', ylab = '빈도수', breaks=5, main='전체평가 히스토그램')
par(mfrow=c(1,1)) # 복구

# 한 프레임에 모두 작성
par(mfrow = c(2,2)) # 2행 2열로 화면분할
boxplot(df$소비자보호평가, main='소비자보호평가')
boxplot(df$이용자만족평가, main ='이용자만족평가')
boxplot(df$피해발생평가, main='피해발생평가')
boxplot(df$전체평가, main='전체평가')
par(mfrow=c(1,1)) # 복구

boxplot(df$소비자보호평가, df$이용자만족평가, df$피해발생평가, df$전체평가, 
        names=c('소비자보호평가', '이용자만족평가', '피해발생평가', '전체평가'))

#### 3. 데이터 전처리----
## 1) 분류명 통일
## 데이터를 보면 비슷한 분류명을 가진 쇼핑몰들이 있다. 이것을 하나의 분류명으로 통일시킨다.
table(df$분류명)

## (1) '종합' 문자열을 포함하는 분류명 추출
df %>% select(분류명) %>% filter(grepl('종합', 분류명))

## (2) '가전' 문자열을 포함하는 분류명 추출 
df %>% select('분류명') %>% filter(grepl('가전', 분류명))

## (3) 데이터 값 변경
df$분류명 <- ifelse(df$분류명 == '종합몰', '종합쇼핑몰', # 종합 -> 종합쇼핑몰
                 ifelse(df$분류명 == '가전', '가전제품', df$분류명)) # 가전 -> 가전제품

table(df$분류명) # 변경된 값 확인

## (4) 필요 없는 칼럼 삭제
df1 <- df %>% select(-일련번호, - 도메인명)
head(df1)

#### 4. 쇼핑몰 분류명별 현황----

## 중복된 쇼핑몰 유무 확인(시간에 따른 데이터이므로 같은 쇼핑몰에 대한 데이터가 있을 수 있다.)
## unique 함수: 중복된 항을 하나만 남기고 제거해주는 함수
## nrow 함수: dataframe의 행의 수를 출력하는 함수
## distinct 함수: 중복 없는 유일한 값 추출

nrow(unique(df1 %>% select(분류명, 쇼핑몰명))) # 중복을 제외한 행 개수 = 435
which(duplicated(df1 %>% select(분류명, 쇼핑몰명))) # 중복된 행 번호 추출, 중복 개수 = 764
df1_dist <- df1 %>% select(분류명, 쇼핑몰명) %>% distinct()

## 분류명별 쇼핑몰의 개수에 대한 정보를 담는 데이터 생성
df1_dist <- df1_dist %>% 
  group_by(분류명) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

head(df1_dist)
colnames(df1_dist)

## 변수명 변경
df1_dist <- rename(df1_dist, category = 분류명) # 새로운 변수명 = 기존 변수명

## 막대 그래프 시각화
ggplot(data = df1_dist, aes(x = reorder(category, count), y = count, fill = category)) +
  geom_col() +
  geom_text(aes(label = count), hjust = 1.25, size = 4, color ='white')+
  coord_flip() + 
  ggtitle('쇼핑몰 분류 개수') +
  theme_minimal() +
  xlab('category') + 
  theme(plot.title = element_text(face = 'bold',
                                  size=15,
                                  hjust = 0.5),
        legend.position = 'none',
        axis.text.y = element_text(angle = 20))
## 결과
## 종합쇼핑몰이 압도적으로 많고 그 다음에는 의류쇼핑몰이 가장 많이 존재한다. 가장 적은 쇼핑몰은 도서, 여행, 티켓과 관련된 쇼핑몰이다.

#### 5. 2018년 분류명별 전체평가가 가장 높은 쇼핑몰 이름----

## 1) 데이터 프레임과 dplyr 패키지를 이용하는 방법
df_2018 <- df1 %>% 
  filter(년도 == 2018) %>% 
  group_by(분류명) %>% 
  summarise(전체평가 = max(전체평가))

## df_2018은  분류명과 전체평가 칼럼만 존재하므로 원본 데이터인 df1과 병합(merge)해서 쇼핑몰명까지 출력한다.
## all.x = T 옵션은 왼쪽 데이터를 기준으로 병합한다는 의미이다.(SQL의 왼쪽조인과 유사)
df_merge_18 <- merge(df_2018, df1 %>% select(쇼핑몰명, 전체평가), all.x = T)
df_merge_18

## 열 순서 변경
## 열 순서를 변경하기 전,  1은 전체평가, 2는 분류명, 3은 쇼핑몰명이다. 순서를 '분류명', '쇼핑몰명', '전체평가'로 하고 싶으므로 순서를 (2, 3, 1)로 지정한다.
df_merge_18 <- df_merge_18[c(2, 3, 1)]

# df1[, .SD[which.max(전체평가)], by = 분류명] 
# Error in `[.data.frame`(df1, , .SD[which.max(전체평가)], by = 분류명) : 사용되지 않은 인자 (by = 분류명)
# -> data.frame 형식에서 data.table 패키지 함수 사용 불가

## 2) 데이터 테이블과 data.table 형식을 이용하는 방법
## data.table은 행과 열로 구성된 2차원의 테이블 형태를 갖는 'data.frame'의 확장 데이터 구조이다. 구조는 data.frame과 비슷하다.

# install.packages('data.table')
library(data.table)
str(df1) # 원본 데이터: data.frame
df1_copy <- df1 # df1_copy 복사본 생성
df1_table <- data.table(df1_copy) # data.table 생성
df1_table <- df1_table %>% filter(년도 == 2018) # 2018년만 추출 (data.table에는 dplyr 패키지 함수 적용 가능)
df1_table <- df1_table[, .SD[which.max(전체평가)], by = '분류명'] 
# 그룹별 최대값 행 가져오기(최소행을 알고 싶으면 which.min 옵션 사용)
# .SD는 data.table 그 자체를 참조한다는 의미이다.

## 결과
## 데이터 프레임과 dplyr 패키지를 이용한 결과는 전체평가의 점수가 같은 행까지 출력되었지만, 데이터 테이블과 data.table 패키지를 이용한 결과는 전체평가 점수가 같아도 한 개의 행만 출력되었다. 본 분석에서는 데이터프레임을 이용한 첫 번째 방법을 두고 해석할 것이다.
## 티켓 관련 쇼핑몰에는 온라인 투어와 맥스무비가 82.43점으로 가장 높다. 소셜커머스는 위메트가 82.73점이고, 오픈마켓에서는 네이버 스마트스토어가 84.95점이다. 가저제품에서는 하이마트와 오뚜기몰이 85.16점으로 동점이고, 해외구매대행에서는 동원몰, 정원e샵, 위즈위드가 85.56점으로 가장 높다. 화장품에서는 메가마트 쇼핑몰, 쏘내추럴이 85.67점으로 동점이고 의류에서는 하프클럽이 85.92점으로 의류 쇼핑몰 중에서 가장 높다. 식품에서는 홈플러스쇼핑몰과 CJ ONmart가 85.94점으로 가장 높다. 여행 관련 쇼핑몰에서는 노랑풍선이 86.43점으로 가장 높다. 컴퓨터에서는 아이스타일24와 컴퓨존이 86.46점으로 동점이다. 종합쇼핑몰은 이랜드몰이 86.69점으로 가장 높으며, 도서 쇼핑몰에서는 영풍문고가 86.92%로 가장 높다.
## 분류명 가장 높은 점수를 차지하는 쇼핑몰은 대부분 82점 이상을 기록한다는 것을 알 수 있다.

#### 6. 2007년 ~ 2018년 교보문고 각 평가점수 변화----
df1_yp <- df1 %>% filter(쇼핑몰명 == '영풍문고') %>% select(-쇼핑몰명, -분류명)
df1_yp

# df1_yp$년도 <-  as.Date(df1_yp$년도, format = '%Y') 오류 발생
# Error in as.Date.numeric(df1_yp$년도, format = "%Y") : 'origin'이 반드시 주어져야 합니다
# 이것에 대해서 구글링 해봤는데 큰 도움은 없었고 아래와 같은 방법으로 구할 수 있었다.

df1_yp$년도 <-  as.Date(as.character(df1_yp$년도), format = '%Y')
str(df1_yp) 
# '년도' 변수타입이 Date로 변했음을 알 수 있다. 결과는 '년도-월-일'로 나오는데, 월과 일자는 현재를 기준으로 반환되는 것 같다.

# 하지만 '월-일'은 필요없는 정보임으로 이를 제거했다.(새로운 변수 생성_year)
df1_yp$year <- format(as.Date(df1_yp$년도), '%Y')
str(df1_yp) 
# 혹은 format(as.Date(as.character(df1_yp$년도)), '%Y') 코드로 한 번에 실행할 수 있다.
# 위의 코드를 실행하면 year 라는 변수가 chr 타입으로 생성된다.

# 년도 변수는 필요없으니 삭제
df1_yp <- df1_yp %>% select(-년도)

# 데이터 구조 변환 실시
library(reshape2)
df1_yp_reshape <- melt(df1_yp, id.vars = 'year')

df1_yp_reshape$year <- as.integer(df1_yp_reshape$year)# x축이 문자형이거나 factor 형이면 오류가 발생하므로 숫자형태로 바꾼다.

# 선그래프 시각화
ggplot(data=df1_yp_reshape, aes(x = year, y = value, col = variable)) +
  geom_line(size=1.2) + # 선 굵기 지정
  ggtitle('영풍문고 시간별 평가요소 점수 변화') + 
  theme_classic() + 
  xlab('점수')+
  ylab('연도') + 
  ylim(0, 100) + # y축 범위 지정
  scale_x_continuous(breaks = seq(2007, 2018, 1)) + # 일정한 간격으로 x축 설정
  scale_y_continuous(breaks = seq(0,100, 10))+ # 일정한 간격으로 y축 설정
  theme(plot.title = element_text(face = 'bold',
                                  size = 18,
                                  hjust = 0.5),
        legend.position = 'top',
        legend.title = element_blank())

## 결과
## 영풍문고의 2007년~2018년(2010, 2011, 2012년 제외)의 평가점수 변화 그래프를 그렸다. 소비자보호평가, 이용자만족평가, 피해발생평가 모두 비슷한 경향을 보이다가 2017년에 반대로 낮은 점수를 받거나 높은 점수를 받았다. 이 부분에 대해서는 추가적인 조사가 필요할 것이다. 그리고 전체평가 점수는 2014년부터 점차 높은 점수를 받고 있다.
## 이용자만족평가와 소비자보호평가가 동시에 떨어졌고, 피해발생평가가 그만큼 상승했으므로 연관성이 있어보인다. 그 이후에는 다시 이전과 같은 경향을 보인다.
## 또한 2010, 2011, 2012년에 받은 점수는 없으므로 해당 년도 데이터는 제외해서 봐야할 것이다.
