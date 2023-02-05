install.packages("arules")
library(arules)

list.files()
basket <- readLines("basket.csv") # 문자열로 읽어오기
# 연관성 분석 시, 하나의 거래 내역을 Transaction이라고 한다.
# Transaction을 만들기 위해서 하나의 거래 내역이 문자열로 이루어져 있어야 사용하기 쉽다.
basket # 5개의 거래 확인

# Transaction으로 변환하기 위해 ',' 기준으로 잘라냄
basket_trans <- strsplit(basket, ",")
basket_trans

# Transaction 형태로 변환
basket_trans <- as(basket_trans, "transactions")
basket_trans

# 모든 데이터는 Transaction 형태로 되어있어야 함
inspect(basket_trans)

# 연관성규칙 도출(지지도 0.1이상, 신뢰도 0.8 이상인 연관성 규칙만 도출)
basket_apriori <- apriori(basket_trans, 
        parameter = list(support = 0.1, 
                         confidence = 0.8))
basket_apriori
summary(basket_apriori)

inspect(basket_apriori)
# (lhs 조건, rhc 결과, support 지지도, confidence 신뢰도, lift 향상도 count 연관성규칙이 발생한 건수)

# 조건을 통해 원하는 연관성 분석 도출
# 향상도가 1.2 이상인 데이터 확인
inspect(subset(basket_apriori, subset= lift > 1.2))

# 조건에 삼겹살이 포함된 연관성 규칙
inspect(subset(basket_apriori, subset = lhs %in% c("삼겹살")))

# 조건에 삼겹살이나 생수가 포함된 규칙
inspect(subset(basket_apriori, subset = lhs %in% c("삼겹살", "생수")))
        
# 조건에 삼겹살과 생수가 포함된 규칙
inspect(subset(basket_apriori, subset = lhs %ain% c("삼겹살", "생수"))) # %ain%: and 연산

# 시각화
install.packages("arulesViz")
library(arulesViz)

plot(basket_apriori)
plot(basket_apriori, method = "grouped")
plot(basket_apriori, method ="graph", interactive = T)
