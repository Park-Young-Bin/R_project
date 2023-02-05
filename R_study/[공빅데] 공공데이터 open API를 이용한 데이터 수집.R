# [공빅데] 공공데이터 open API를 이용한 데이터 수집1----

# install.packages('rvest')
library(rvest)

# 서비스 url 주소
cctv_api <- "http://api.data.go.kr/openapi/tn_pubr_public_unmanned_traffic_camera_api"

# 인증키
my_key <- "9dNbZ%2FxAAEdZTV3B%2BxJmwyqgjb4W5TrpKYIYzkFemFeFFseRZdnGbJSSHDf5cJpGZNdMLtIfjnAR%2B%2BBJKGvebg%3D%3D"

# 페이지 번호
cctv_page <- as.numeric("")

# 한 페이지 결과 수
cctv_rows <- ""

# xml / json 여부
cctv_type <- "xml"

# 시도명
cctv_city <- "서울특별시"

# 시군구명
cctv_gu <- "종로구"

# 하나의 cctv_url로 합치기
cctv_url <- paste(cctv_api,
                  "?serviceKey=", my_key,
                  "&pageNo=", cctv_page,
                  '&numOfRows=', cctv_rows,
                  "&type=", cctv_type,
                  "&ctprvnNm=", cctv_city,
                  "&signguNm=", cctv_gu,
                  sep = "")

cctv_url

# item노드 안에 있는 데이터의 텍스트만 수집한 후 데이터프레임으로 저장
read_html(cctv_url) %>% html_nodes("item") %>% html_text() %>% data.frame() -> cctv_data100

# 지저분하게 출력된 이유를 찾기 위해 공공데이터포털에서 제공하는 open API로 본 페이지 결과는 아래와 같다.
# respone > body > items > item > ... 순으로 되어 있는데, 자세히 보면 item 아래에 위치한 데이터의 컬럼 이름은 각각의 # node로 구성된 것이 아니라 line아래에 html-tag의 text로 구성된 것을 알 수 있다. 때문에 rvest라이브러리에 있는 
# html_nodes()를 이용해 item 태그의 text를 출력하면 그 안에 있는 데이터들이 한 줄로 인식되어 출력된 것이다.  

# [공빅데] 공공데이터 open API를 이용한 데이터 수집2(데이터 프레임 형태로 수집)----
#사용 라이브러리
library(XML)
library(data.table)

# 서비스 url 주소(﻿전국무인교통단속카메라표준데이터﻿ open API)
cctv_api <- "http://api.data.go.kr/openapi/tn_pubr_public_unmanned_traffic_camera_api"

#인증키
my_key <- "9dNbZ%2FxAAEdZTV3B%2BxJmwyqgjb4W5TrpKYIYzkFemFeFFseRZdnGbJSSHDf5cJpGZNdMLtIfjnAR%2B%2BBJKGvebg%3D%3D"

# 마지막 페이지까지
cctv_page <- 18

# 한 페이지에 1000건의 출력으로 설정
cctv_rows <- "1000"

# xml / json 여부
cctv_type <- "xml"

cctv_c <- list()

# 1~18번째 페이지까지 반복
for (j in 1:cctv_page) {
  cctv_url <- paste(cctv_api,
                    "?serviceKey=", my_key,
                    "&pageNo=", j,
                    "&numOfRows=", cctv_rows,
                    "&type=", cctv_type,
                    sep = "")
  
  #j번째 cctv_url를 xmlTreeParse를 이용해 parsing
  all_data <- xmlTreeParse(cctv_url,useInternalNodes = TRUE, encoding = "UTF-8")
  rootNode <- xmlRoot(all_data)
  
  #rootNode의 2번째 element의 items라는 서브 element를 items라는 변수에 넣음.
  items <- rootNode[[2]][["items"]]
  size <- xmlSize(items)
  cctv_a <- data.frame()
  cctv_b <- list()
  
  #xmlSApply : items이라는 element에 있는 모든 변수들을 추출
  for(i in 1:size) {
    test <- xmlSApply(items[[i]],xmlValue)  
    
    cctv_a <- data.table(mnlssRegltCameraManageNo=test[1], #무인교통단속카메라관리번호
                         ctprvnNm=test[2], #시도명
                         signguNm=test[3], #시군구명
                         roadKnd=test[4], #도로종류
                         roadRouteNo=test[5], #도로노선번호
                         roadRouteNm=test[6], #도로노선명
                         roadRouteDrc=test[7], #도로노선방향
                         rdnmadr=test[8], #소재지도로명주소
                         lnmadr=test[9], #소재지지번주소
                         latitude=test[10], #위도
                         longitude=test[11], #경도
                         itlpc=test[12], #설치장소
                         regltSe=test[13], #단속구분
                         lmttVe=test[14], #제한속도
                         regltSctnLcSe=test[15], #단속구간위치구분
                         ovrspdRegltSctnLt=test[16], #과속단속구간길이
                         prtcareaType=test[17], #보호구역구분
                         installationYear=test[18], #설치년도
                         institutionNm=test[19], #관리기관명
                         phoneNumber=test[20], #관리기관전화번호
                         referenceDate = test[21], #데이터기준일자
                         instt_code = test[22]) #제공기관코드
    
    cctv_b[[i]] <- cctv_a
  }
  
  cctv_c[[j]] <- cctv_b
}

cctv_final <- list()

#rbindlist : 여러개의 데이터프레임을 하나의 데이터프레임으로 만들기
for (i in 1:cctv_page){
  cctv_final[[i]] <- rbindlist(cctv_c[[i]])
}

cctv_final <- rbindlist(cctv_final)

#최종 결과 확인
View(cctv_final)
