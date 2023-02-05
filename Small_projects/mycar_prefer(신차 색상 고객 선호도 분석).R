# 빈도와 비율의 기술통계량 분석 패키지
install.packages("Hmisc")
library(Hmisc)

# 데이터 테이블의 빈도분석 중심의 기술통계량 분석 패키지
install.packages("prettyR")
library(prettyR)

mycar <- read.csv("C:/Users/user/Desktop/R/Practice/ubion/mycar.csv", header = T)

# 실습 데이터 읽기
table(mycar$color) # 색상 빈도 확인
table(mycar[2]) # 2열 빈도 확인

prop.table(table(mycar$color))
prop.table(table(mycar[2]))
prop.table(table(mycar$color)) * 100
round(prop.table(table(mycar[2])) * 100, 1)

# 데이터 프레임 작성
surveyFreq <- c(table(mycar$color)) # 항목별(color) 빈도수
surveyProp <- c(round(prop.table(table(mycar$color)) * 100, 1)) # 항목별(color) 백분율
surveytable <- data.frame(Freq = surveyFreq, Prop = surveyProp)
surveytable

# 기술통계량 추출
describe(mycar)
describe(mycar$color)

# 빈도와 백분율을 테이블 형식으로 추출
freq(mycar) 
freq(mycar$color)
