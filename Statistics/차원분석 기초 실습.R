# 차원분석

# 주성분분석----
# 서로 상관관계를 갖는 많은 변수를 상관관계가 없는 소수의 변수로 변환하는 차원 축소 기법
# 변환에 사용하는 소수의 변수를 주성분 or 성분이라고 함
# 상관관계를 갖는 다수의 변수에 포함된 정보를 가능한 많이 포착하면서 이 다수의 변수를 상관관계를 갖지 않는 그보다 더 적은 개수의 새로운 변수로 대체
# 변수들이 가지고 있는 총표본분산을 많이 설명해주는 순서대로 순차적으로 변수 개수만큼의 성분을 추출
# 추출된 성분 중 가장 많은 설명력을 제공하는 처음 몇 개의 소수의 성분만을 이용해 데이터를 분석함으로써 데이터의 복잡성 감소

# Population : 인구
# Income : 1인당 소득
# Illiteracy : 문맹률
# Life Exp : 기대 수명
# Murder : 인구 10만명당 살인율
# HS Grad : 고등학교 졸업자 비율 
# Frost : 수도 또는 대도시의 최저 기온이 영하인 평균 일수
# Area : 마일 단위의 토지 면적
str(state.x77)
head(state.x77) # 미국 50개 주에 대한 8가지 정보

pca <- prcomp(state.x77, scale=T) # 측정 단위 통일(평균=0, 표준편차=1)
summary(pca)

# Screeplot
# 변수의 개수가 증가함에 따라 설명되는 분산의 양이 변화하는 정도
# 해석1: 그래프의 꺾이는 부분이 가장 많은 설명력을 나타내는 성분의 개수를 의미함
# 해석2: 처음 2~3개 성분이 원래 데이터셋에 포함된 8개 변수 분산의 상당 부분을 차지함
plot(pca, type='l', pch=19, lwd=2, col='red', main='Screeplot')

# 성분적재값(성분과 변수 간 상관정도)
# 값이 클수록 관련성이 높음, rotation으로 성분적재값 확인
# 이를 가중치로 이용해 기존 변수들의 선형결합으로 표현 가능 
round(pca$rotation, 3) # 성분적재값 = 선형결합에 사용되는 가중치

# 성분점수(변수들의 선형결합으로 변환된 값)
# 표준화된 전체 데이터 행렬과 성분적재값 행렬의 곱
round(scale(state.x77) %*% pca$rotation, 3)
round(pca$x, 3) # 위와 같은 결과

round(pca$x[, c(1,2)], 3) # 설명력이 큰 상위 2개 성분 점수 추출 → 원래 변수값 대신 사용 가능

round(cor(pca$x)) # 주성분간 상관계수행렬(주성분분석은 성분 간에 상관관계가 없도록 함)

# 주성분분석 결과 시각화
# 해석1: 화살표 방향, 각도는 변수 간 관계를 나타냄
# 해석2: 좁은 각도와 가까운 방향 → 양의 상관관계 / Life Exp ~ Frost
# 해석3: 직각 → 상관관계X / Life Exp ~ Area
# 해석4: 넓은 각도와 반대 방향 → 음의 상관관계 / Life Exp ~ Illiteracy, Murder
# 해석5: 화살표가 축과 평행일수록 대응되는 변수와 성분은 서로 밀접한 상관관계 존재
## Life Exp, Frost는 음의 방향으로 첫번째 성분과 평행선
## Illiteracy는 양의 방향으로 첫번째 성분과 평행선
## 위 3개 변수는 첫번째 성분과 상대적으로 높은 상관관계가 존재
## Murder, HS Grad는 앞 3개 변수만큼은 아니지만 첫번째 성분과 상관관계 존재
## 첫번째 성분: 교육률이 높고 안전하며 기대 수명이 긴, 기온이 높은 지역과 교육수준이 낮고 기대수명이 짧으며 치안이 위험한 더운 지역으로 구분됨
## 두번째 성분: 면적이 넓고 인구가 많으며 소득이 높은 지역과 그렇지 않은 지역으로 구분됨
biplot(pca, cex=c(.5, .75), main='Bipot') # c(케이스 글씨 크기, 변수 레이블 글씨 크기)

# 요인 분석----
# 관측 가능한 여러 변수로부터 소수의 요인을 추출하여 이 요인들을 통해 변수 간의 관련성을 설명하려는 통계 데이터 분석 기법
# 
# 목적: 측정 가능한 변수들로부터 그 안에 잠재되어 있는 해석 가능한 소수의 요인을 찾는 것

# install.packages('ade4')
library(ade4)
data(olympic) # 올림픽 육상 10종 경기에 참가한 33명 선수 기록
str(olympic)
?olympic

# 요인 개수 결정
## 고유값
## 스크리도표

library(psych)
?fa.parallel
fa.parallel(olympic$tab, fm='ml', # fm: 요인 추출 방법 결정(ml: 최대우도법)
            fa='fa', 
            n.iter=100) # 시뮬레이션 횟수

# install.packages('nFactors')
library(nFactors)
nScree(olympic$tab) # 복수의 방법을 적용하여 screetest로부터 적정 요인 개수 추출

eigen(cor(olympic$tab)) # value의 처음 두개 고유값이 1보다 크므로 2개의 요인이 적정 요인으로 선택 가능

?factanal
fa <- factanal(olympic$tab, factors=2, scores='regression')
fa 

fa$loadings # 요인적재값 출력(0.1보다 작은 값은 출력X)
print(fa$loadings, cutoff=0.001) # 모든 요인적재값 출력

round(fa$uniquenesses, 3)
1-round(fa$uniquenesses, 3) # 공통성 확인

factor.plot(fa, labels=colnames(olympic$tab),
            pos = 4, title = 'Factor Plot')

library(gplots)
library(RColorBrewer)
# 결과: 요인과 변수 간 관성성이 높으면 짙은 색
heatmap.2(abs(fa$loadings), col=brewer.pal(9, 'Blues'),
          trace='none', key=F, dendrogram='none',
          cexCol=1.2, main='Factor Loadingㄴ')

# install.packages('semPlot')
library(semPlot)
semPaths(fa, what='est', residuals=F, # 잔차 생략
         cut=0.3, posCol=c('White', 'darkgreen'),
         negCol=c('White', 'blue'),
         edge.label.cex=.75)

fa.scores <- fa$scores
fa.scores

colnames(fa.scores) <- c('Run', 'Throw')
heatmap.2(fa.scores, col=brewer.pal(9, 'GnBu'), # 녹색은 작은 값, 파란색은 큰 값
          trace='none', key=F, dendrogram='none',
          cexCol=1.2, main='Factor Scores by Athletes')
