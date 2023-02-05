# 동시 출현 단어 분석----

# - 단어 간의 관계를 살펴보는 분석 방법
# - ex. 손-장갑, 머리-모자
# - 단어의 관계를 표현한 의미망 만드는데 활용
# https://youngwoos.github.io/rmeetup_tidy_textmining/tidy_textmining.html?fbclid=IwAR3EpcDlhfwhVo6NkPYxpilRUg_nKYJb3ZVSEMthK0UYfRCHpYn2d6MKmro#86

# 1. 기본적인 전처리----

# 기생충 기사 댓글 불러오기
library(readr) #  to provide a fast and friendly way to read rectangular data (like 'csv', 'tsv', and 'fwf')
raw_news_comment <- read_csv('news_comment_parasite.csv')

# 전처리
library(dplyr)
library(stringr)
# install.packages('textclean')
library(textclean)

news_comment <- raw_news_comment %>% 
  select(reply) %>% 
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "), # 한글만 남기기
         reply = str_squish(reply), # 중복 공백 제거
         id = row_number()) # 행 순서 반환

# 2. 토큰화하기----

# 1) 형태소 분석기를 이용해 품사 기준으로 토큰화하기
# install.packages('tidytext')
library(tidytext)
library(KoNLP)

comment_pos <- news_comment %>% 
  unnest_tokens(input = reply, # 토큰화할 테스트
                output = word, # 토큰을 담을 변수명
                token = SimplePos22, # 문장의 단어를 22개의 품사로 구분
                drop = F) 

comment_pos %>% 
  select(reply, word)

# 2) 품사 분리하여 행 구성하기
# - 원하는 품사를 추출하기 쉽도록 한 행을 한 품사로 구성하기
# - tidyr::separate_rows(): 
#   - 정규 표현식에 따라 텍스트를 여러 행으로 나누기
#   - sep = "[+]": "+"가 등장할 때마다 행을 나눔

# 품사별로 행 분리
library(tidyr)
comment_pos <- comment_pos %>% 
  separate_rows(word, sep = '[+]')

comment_pos %>% 
  select(word, reply)

# 3. 품사 추출하기----
# (1) 명사 추출하기
noun <- comment_pos %>%
  filter(str_detect(word, "/n")) %>% # "/n"이 붙어있는 단어 추출
  mutate(word = str_remove(word, "/.*$")) # 태그 제거: '/로 시작하는 모든 문자' 제거

noun %>%
  select(word, reply)

# (2) 동사, 형용사 추출하기
# - 동사 "/pv", 형용사:"/pa" 붙어있는 단어 추출
# - 단어 뒤에 태그 대신 '다'를 붙여 이해하기 편하게 수정하기
#   - ex) "받" → "받다", "멋지" → "멋지다"

# 동사, 형용사 추출하기
pvpa <- comment_pos %>%
  filter(str_detect(word, "/pv|/pa")) %>%         # "/pv", "/pa" 추출
  mutate(word = str_replace(word, "/.*$", "다"))  # "/"로 시작 문자를 "다"로 바꾸기

pvpa %>%
  select(word, reply)

# (3) 추출한 데이터 결합하기
# 품사 결합
comment <- bind_rows(noun, pvpa) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

comment %>%
  select(word, reply)

# 4. 단어 동시 출현 빈도 구하기----
# install.packages("widyr")
library(widyr)
pair <- comment %>%
  pairwise_count(item = word,   # 단어
                 feature = id,   # 텍스트 구분 기준
                 sort = T)       # 빈도 높은 순 정렬

pair

# 특정 단어와 자주 함께 사용된 단어 살펴보기
pair %>% filter(item1 == "영화")
pair %>% filter(item1 == "봉준호")

# 동시 출현 네트워크----
# - 동시 출현 빈도를 이용해 단어의 관계를 네트워크 형태로 표현
# - 단어들이 어떤 맥락에서 함께 사용되었는지 이해할 수 있다

# 네트워크 그래프 데이터 만들기
# - tidygraph::as_tbl_graph()
# - 동시 출현 빈도 데이터를 '네트워크 그래프 데이터'로 변환하기
#   - 단어를 나타내는 노드(node, 꼭짓점)
#   - 단어를 연결하는 엣지(edge, 선)

# install.packages("tidygraph")
library(tidygraph)
graph_comment <- pair %>%
  filter(n >= 10) %>%
  as_tbl_graph()

graph_comment

# 네트워크 그래프 만들기
# install.packages("ggraph")
library(ggraph)
ggraph(graph_comment) +
  geom_edge_link() +                 # 엣지
  geom_node_point() +                # 노드
  geom_node_text(aes(label = name))  # 텍스트
