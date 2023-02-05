# 1. 필요한 패키지 로드
library(KoNLP) # 자연어 처리 패키지
library(wordcloud2) # 워드 클라우드2 패키지
library(ggplot2) # 시각화 패키지
library(dplyr) # 전처리 패키지
library(RColorBrewer) # 글자색 표현 패키지
useNIADic() # 사전 로드

# 2. useNIADic 사전에 단어 추가
mergeUserDic(data.frame(c("G-CA"), c("ncn")))
mergeUserDic(data.frame(c("코로나19"), c("ncn")))
mergeUserDic(data.frame(c("유증상자"), c("ncn")))
mergeUserDic(data.frame(c("자택격리자"), c("ncn")))
mergeUserDic(data.frame(c("FAM"), c("ncn")))
mergeUserDic(data.frame(c("비모수"), c("ncn")))
mergeUserDic(data.frame(c("팸인원"), c("ncn")))
mergeUserDic(data.frame(c("업로드"), c("ncn")))
mergeUserDic(data.frame(c("slc"), c("ncn")))
mergeUserDic(data.frame(c("조교쌤"), c("ncn")))
mergeUserDic(data.frame(c("e참뜰"), c("ncn")))
mergeUserDic(data.frame(c("해인자"), c("ncn")))
mergeUserDic(data.frame(c("토익"), c("ncn")))
mergeUserDic(data.frame(c("컴활"), c("ncn")))
mergeUserDic(data.frame(c("e스포츠"), c("ncn")))
mergeUserDic(data.frame(c("라온"), c("ncn")))
mergeUserDic(data.frame(c("늘봄"), c("ncn")))
mergeUserDic(data.frame(c("카톡"), c("ncn")))
mergeUserDic(data.frame(c("생협쿠폰"), c("ncn")))
mergeUserDic(data.frame(c("확산"), c("ncn")))
mergeUserDic(data.frame(c("코로나"), c("ncn")))
mergeUserDic(data.frame(c("통개학개론"), c("ncn")))
mergeUserDic(data.frame(c("분포론"), c("ncn")))
mergeUserDic(data.frame(c("학업코칭"), c("ncn")))
mergeUserDic(data.frame(c("갠톡"), c("ncn")))
mergeUserDic(data.frame(c("동행"), c("ncn")))
mergeUserDic(data.frame(c("멘토"), c("ncn")))
mergeUserDic(data.frame(c("계절학기"), c("ncn")))
mergeUserDic(data.frame(c("과대"), c("ncn")))
mergeUserDic(data.frame(c("부과대"), c("ncn")))
mergeUserDic(data.frame(c("특별학점"), c("ncn")))
mergeUserDic(data.frame(c("실험계획법"), c("ncn")))
mergeUserDic(data.frame(c("페이스북"), c("ncn")))
mergeUserDic(data.frame(c("학우"), c("ncn")))
mergeUserDic(data.frame(c("수강신청"), c("ncn")))
mergeUserDic(data.frame(c("장바구니제"), c("ncn")))
mergeUserDic(data.frame(c("총학생회"), c("ncn")))
mergeUserDic(data.frame(c("동행"), c("ncn")))
mergeUserDic(data.frame(c("라온"), c("ncn")))
mergeUserDic(data.frame(c("단기어학연수"), c("ncn")))
mergeUserDic(data.frame(c("동계계절학기"), c("ncn")))
mergeUserDic(data.frame(c("동계"), c("ncn")))
mergeUserDic(data.frame(c("하계"), c("ncn")))
mergeUserDic(data.frame(c("하계계절학기"), c("ncn")))
mergeUserDic(data.frame(c("계절학기"), c("ncn")))
mergeUserDic(data.frame(c("학과 특성화 장학금"), c("ncn")))
mergeUserDic(data.frame(c("선형대수학"), c("ncn")))
mergeUserDic(data.frame(c("강릉원주대학교"), c("ncn")))
mergeUserDic(data.frame(c("국립강릉원주대학교"), c("ncn")))
mergeUserDic(data.frame(c("복수전공"), c("ncn")))
mergeUserDic(data.frame(c("복학"), c("ncn")))
mergeUserDic(data.frame(c("휴학"), c("ncn")))
mergeUserDic(data.frame(c("학습법"), c("ncn")))
mergeUserDic(data.frame(c("3학년"), c("ncn")))
mergeUserDic(data.frame(c("4학년"), c("ncn")))
mergeUserDic(data.frame(c("여름계절학기"), c("ncn")))
mergeUserDic(data.frame(c("품질관리"), c("ncn")))
mergeUserDic(data.frame(c("등록금"), c("ncn")))
mergeUserDic(data.frame(c("추가"), c("ncn")))
mergeUserDic(data.frame(c("납부"), c("ncn")))
mergeUserDic(data.frame(c("멘토링"), c("ncn")))
mergeUserDic(data.frame(c("정보통계학과"), c("ncn")))
mergeUserDic(data.frame(c("학회장"), c("ncn")))
mergeUserDic(data.frame(c("톡게시판"), c("ncn")))
mergeUserDic(data.frame(c("롤"), c("ncn")))
mergeUserDic(data.frame(c("201호"), c("ncn")))
mergeUserDic(data.frame(c("연습문제"), c("ncn")))
mergeUserDic(data.frame(c("기말고사"), c("ncn")))
mergeUserDic(data.frame(c("중간고사"), c("ncn")))
mergeUserDic(data.frame(c("단일후보"), c("ncn")))
mergeUserDic(data.frame(c("차기학회장"), c("ncn")))
mergeUserDic(data.frame(c("파일"), c("ncn")))
mergeUserDic(data.frame(c("근로 장학금"), c("ncn")))
mergeUserDic(data.frame(c("국가장학금"), c("ncn")))
mergeUserDic(data.frame(c("자연대"), c("ncn")))
mergeUserDic(data.frame(c("실험계획"), c("ncn")))
mergeUserDic(data.frame(c("특별학점"), c("ncn")))
mergeUserDic(data.frame(c("2학기"), c("ncn")))
mergeUserDic(data.frame(c("배움모둠"), c("ncn")))
mergeUserDic(data.frame(c("차기학회장"), c("ncn")))
mergeUserDic(data.frame(c("강릉"), c("ncn")))
mergeUserDic(data.frame(c("체크"), c("ncn")))
mergeUserDic(data.frame(c("미리미리"), c("ncn")))
mergeUserDic(data.frame(c("승인절차"), c("ncn")))
mergeUserDic(data.frame(c("이수증"), c("ncn")))
mergeUserDic(data.frame(c("안전교육"), c("ncn")))
mergeUserDic(data.frame(c("입학본부"), c("ncn")))
mergeUserDic(data.frame(c("공약설명회"), c("ncn")))
mergeUserDic(data.frame(c("질의응답"), c("ncn")))
mergeUserDic(data.frame(c("영상재생"), c("ncn")))
mergeUserDic(data.frame(c("오류"), c("ncn")))

# 3. txt 파일 불러오기
text1 <- readLines("text2.txt")
head(text1, 20) # 상위 20개 추출

# 4. 명사 추출
text2 <- extractNoun(text1)
head(text2,20) # 상위 20개 추출

# 5.리스트 형태 데이터를 순수 벡터 형태로 변환
data_list <- unlist(text2)
head(data_list, 110) # 상위 20개 추출

# 6. 불용어 처리
data_list <- gsub("\\d+", "", data_list) # 숫자 제거
data_list <- gsub("\\(", "", data_list) # ( 괄호 제거
data_list <- gsub("\\)", "", data_list) # ) 괄호 제거
data_list <- gsub("[A-Za-z]", "", data_list) # 영문자 제거
data_list <- gsub("[\\(]~!@#$%&*()_+=?", '', data_list) # 특수문자 제거
data_list <- gsub('[ㄱ-ㅎ]', '', data_list) # 자음제거
data_list <- gsub('(ㅜ|ㅠ)', '', data_list) # ㅜㅜ or ㅠㅠ 제거
data_list <- gsub("[0-9]", '', data_list)
data_list <- gsub("주기", "", data_list)
data_list <- gsub("차기", "", data_list)
data_list <- gsub("관련", "", data_list)
data_list <- gsub("참뜰", "e참뜰", data_list)
data_list <- gsub("과제\\s*", "과제", data_list) # '과제'로 시작하는 단어는 무엇이든지 '과제'로 변환
data_list <- gsub("갠톡\\s*", "갠톡", data_list)
data_list <- gsub("자연\\s*", "", data_list)
data_list <- gsub("자대\\s*", "", data_list)
data_list <- gsub("자연\\s*", "", data_list)
data_list <- gsub("조교쌤", "조교", data_list)
data_list <- gsub("고싶", "", data_list)
data_list <- gsub("거점", "", data_list)
data_list <- gsub("계절학", "", data_list)
data_list <- gsub("그전", "", data_list)
data_list <- gsub("감시", "", data_list)
data_list <- gsub('(기말|기말고사)', "기말고사", data_list)
data_list <- gsub("국가장", "", data_list)
data_list <- gsub("꼭해주세", "", data_list)
data_list <- gsub("내년", "", data_list)
data_list <- gsub("다다", "", data_list)
data_list <- gsub("들입", "", data_list)
data_list <- gsub("만나뵙", "", data_list)
data_list <- gsub("다급", "", data_list)
data_list <- gsub("기본적", "", data_list)
data_list <- gsub("동연", "", data_list)
data_list <- gsub("다시", "", data_list)
data_list <- gsub("동영상올릴", "", data_list)
data_list <- gsub("단일후보", "", data_list)
data_list <- gsub("목요", "", data_list)
data_list <- gsub("부조", "", data_list)
data_list <- gsub("부과", "", data_list)
data_list <- gsub("본부", "", data_list)
data_list <- gsub("보행로", "", data_list)
data_list <- gsub("아무", "", data_list)
data_list <- gsub("안녕", "", data_list)
data_list <- gsub("안녕하십니까", "", data_list)
data_list <- gsub("사다리타기", "", data_list)
data_list <- gsub("오후9시까지는", "", data_list)
data_list <- gsub("오전8시30분부터", "", data_list)
data_list <- gsub("이거", "", data_list)
data_list <- gsub("있습니", "", data_list)
data_list <- gsub("이번", "", data_list)
data_list <- gsub("유증", "", data_list)
data_list <- gsub("약간", "", data_list)
data_list <- gsub("오후", "", data_list)
data_list <- gsub("^특별학점\\W토익", "", data_list)
data_list <- gsub("^수정\\W중간", "", data_list)
data_list <- gsub("저번", "", data_list)
data_list <- gsub("준성", "", data_list)
data_list <- gsub("중이", "", data_list)
data_list <- gsub("제35대", "", data_list)
data_list <- gsub("하게", "", data_list)
data_list <- gsub("학년", "", data_list)
data_list <- gsub("^하", "", data_list) # '하'로 시작하는 제거
data_list <- gsub("^해", "", data_list) # '해'로 시작하는 제거
data_list <- gsub("학우여러분", "", data_list)
data_list <- gsub("한거", "", data_list)
data_list <- gsub("한점", "", data_list)
data_list <- gsub("주시", "", data_list)
data_list <- gsub("십니까", "", data_list)

data_list <- Filter(function(x){nchar(x)>=2}, data_list) # 글자수 2 이상 추출
table(data_list) # 빈도수 확인

# 7. 글자수 범위 설정
data_list <- data_list[nchar(data_list) < 5] # 글자수 5 미만

# 8. 내림차순 정렬
data_sort <- sort(table(data_list), decreasing = T)
data_sort

# 9. 상위 50개 데이터 추출
data_final <- head(data_sort, 50)
data_final

# 10. 빈도수 막대 그래프로 확인하기
df <- as.data.frame(data_final) 
df <- rename(df, word = data_list)

ggplot(data = df, aes(x = reorder(word, Freq), y = Freq)) + 
  geom_col() + 
  coord_flip() + 
  xlab('단어') + 
  ylab('빈도수')

# 11. 워드 클라우드 생성
# wordcloud2 함수를 이용해 최종적으로 워드 클라우드를 생성한다.
# size = 숫자: 크기 조정(숫자가 클수록 크기가 커짐)
# fontFamliy: 폰트 지정 함수

set.seed(1234) # 난수 설정
pal <- brewer.pal(9, "Blues")[5:9]
wordcloud2(data_final, size = 0.7, fontFamily = "Tmon몬소리", color = pal)
