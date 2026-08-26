# 2차 팀 프로젝트 - 전국 공연행사 통계

# 데이터 불러오기
# install.packages("leaflet")
# install.packages("htmltools")
library(leaflet)
library(htmltools)
setwd("C:/R/teamproject")
getwd()

data <-read.csv("전국공연행사정보표준데이터.csv")

head(data)

# 컬럼명 변경 필요
names(data)[names(data)=="행사명"] <-"f.name"
names(data)[names(data)=="장소"] <-"f.place"
names(data)[names(data)=="행사내용"] <-"f.content"
names(data)[names(data)=="행사시작일자"] <-"f.startday"
names(data)[names(data)=="요금정보"] <-"f.feeyn"
names(data)[names(data)=="주최기관명"] <-"f.open"
names(data)[names(data)=="객석수"] <-"f.sit"
names(data)[names(data)=="관람요금"] <-"f.fee"
names(data)[names(data)=="입장연령"] <-"f.age"
names(data)[names(data)=="주차장보유여부"] <-"f.park"
names(data)[names(data)=="소재지도로명주소"] <-"f.add1"
names(data)[names(data)=="소재지지번주소"] <-"f.add2"
names(data)[names(data)=="위도"] <-"f.lat"
names(data)[names(data)=="경도"] <-"f.lng"
names(data)[names(data)=="제공기관명"] <-"f.provide"


# --------------------------------------------------------
# --------------------------------------------------------
# 워드클라우드
# install.packages("udpipe")
library(udpipe)
# install.packages('wordcloud')
# install.packages('tm')
# install.packages('RColorBrewer')
library(wordcloud)
library(tm)
library(RColorBrewer)

# 텍스트 데이터 생성
festivaltext<-data[,3]

# 텍스트 벡터를 하나의 문서로 결합
# VectorSourse() : 벡터 형태의 텍스트 데이터를 소스로 사용
# Corpus() : 텍스트 코퍼스(문서집합) 생성
text_corpus <-Corpus(VectorSource(festivaltext))

# 텍스트 전처리
# 1. 모든 언어를 소문자로 변환(A->a)
text_corpus <- tm_map(text_corpus,content_transformer(tolower))
# 2. 숫자 제거(2024, 123 등)
text_corpus <- tm_map(text_corpus,removeNumbers)
# 3. 문장부호 제거 (. ? ! , 등)
text_corpus <- tm_map(text_corpus,removePunctuation)
# 4. 불용어(stopwords) 제거 (the, is, a, an 등)
text_corpus <- tm_map(text_corpus,removeWords,stopwords('english'))

# 조사 제거
text_corpus <- tm_map(
  text_corpus,
  content_transformer(function(x){
    
    x <- gsub("을", "", x)
    x <- gsub("를", "", x)
    x <- gsub("은", "", x)
    x <- gsub("는", "", x)
    x <- gsub("가", "", x)
    x <- gsub("와", "", x)
    x <- gsub("과", "", x)
    x <- gsub("한", "", x)
    x <- gsub("의", "", x)
    x <- gsub("로", "", x)
    x
  })
)

# 5. 공백 제거 (Whitespace)
text_corpus <- tm_map(text_corpus,stripWhitespace)

# 6. 한글 불용어 제거 (조사, 접속사 등)
custom_stopwords_extended <-c("수", "할", "위한", "있는",
                              "입니다", "됩니다", "가장", "것이", "하나", 
                              "같은", "등", "매우", "정말", "아주", "하는",
                              "해야", "속에서", "같은","의","그리고","r을",
                              "있다","와","과","재미있다","함께하는","통해","있",
                              "개최","행사","함께","펼쳐지","년","선보이","위",
                              "함께하","다양","이야기","초청","즐길","및",
                              "주제로","족","찾아","대","배경으","프그램",
                              "작품","무대")
text_corpus <- tm_map(text_corpus,removeWords,custom_stopwords_extended)

dtm <-TermDocumentMatrix(text_corpus)

matrix <- as.matrix(dtm)
# rowSums()로 각 단어의 빈도 계산 후 내림차순 정렬
word_freqs <- sort(rowSums(matrix),decreasing = T)

# names(word_freqs)-> 단어 목록, word_freqs -> 해당 단어의 빈도 수
df <-data.frame(word=names(word_freqs),freqs=word_freqs)

# 컬러팔레트 생성
purple <- c(
  "#FFBBFF",
  "#CC99FF",
  "#C084FC",
  "#A855F7",
  "#FF33CC",
  "#7E22CE",
  "#6B21A8"
)

# 워드클라우드 생성
set.seed(1234)

wordcloud(word= df$word,freq=df$freq,    # 데이터프레임의 단어,  빈도 수
          min.freq = 1,                  # 최소 빈도수 
          max.words = 50,                # 최대 표시 단어 수
          colors = purple,               # 색상 팔레트
          scale = c(6, 1),
          random.order = F,               # FALSE면 빈도가 높은 단어가 중앙에 배치
          rot.per = 0.3                  # 글자 회전 비율 (0.3 -> 30% 회전)
)


# --------------------------------------------------------
# --------------------------------------------------------
# 행사 유,무료 비중 원그래프
library(ggplot2)

feedata<-data[,5]

# 결측값 확인
colSums(is.na(data))

# 혼합 값 정리
data$f.feeyn[grep("유료+무료", data$f.feeyn, fixed = TRUE)]<-"혼합"
data$f.feeyn[grep("무료+유료", data$f.feeyn, fixed = TRUE)]<-"혼합"

feeyn.dt<-data$f.feeyn

feeyn.dt<-table(feeyn.dt)
feeyn.dt

# 비율 생성
fee.percent <- prop.table(feeyn.dt) * 100

fee.df <- as.data.frame(feeyn.dt)
names(fee.df) <- c("구분", "개수")


fee.df$fee.percent <- fee.df$개수 / sum(fee.df$개수) * 100

# 내림차순
fee.df<-fee.df[order(fee.df$개수, decreasing = TRUE), ]

# 데이터 확인
fee.df

# ggplot의 파이그래프
library(ggplot2)

ggplot(fee.df, aes(x = 2, y = 개수, fill = 구분)) +
  geom_col(width = 1,colour = "white") +
  ggtitle('유·무료 행사 비율')+
  geom_label(
    aes(
      label = paste0(
        구분, "\n",
        round(fee.percent, 1), "%")
    ),
    position = position_stack(
      vjust = 0.5,
      reverse = TRUE),
    fill = "white",
    colour = "black",
    label.size = 0) +
  coord_polar("y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(
    values = c(
      "유료" = "purple",
      "무료" = "#D8B4FE",
      "혼합" = "gray")) +
  theme_void()+
  theme(
    plot.title = element_text(
      size = 18,
      hjust = 0.5,
      face = "bold"))

# ---------------------------------------------------------------
# ---------------------------------------------------------------
# 지역별 행사 막대그래프

# install.packages("stringr")
library(stringr)
# install.packages("dplyr")
library(dplyr)

library(ggplot2)

# 도로명 주소와 지번 주소를 '주소'컬럼에 통합
addrdata <- data %>%
  mutate(
    주소 = ifelse(
      is.na(f.add1) | trimws(f.add1) == "",
      f.add2,
      f.add1
    )
  )

# 지역명 생성
addrdata <- addrdata %>%
  mutate(
    주소 = case_when(
      str_detect(주소, "전라남도|광주광역시") ~ "전남광주",
      str_detect(주소, "서울") ~ "서울",
      str_detect(주소, "부산") ~ "부산",
      str_detect(주소, "대구") ~ "대구",
      str_detect(주소, "인천") ~ "인천",
      str_detect(주소, "대전") ~ "대전",
      str_detect(주소, "울산") ~ "울산",
      str_detect(주소, "세종특별자치시") ~ "세종",
      str_detect(주소, "경기") ~ "경기",
      str_detect(주소, "강원") ~ "강원",
      str_detect(주소, "충청북") ~ "충북",
      str_detect(주소, "충청남") ~ "충남",
      str_detect(주소, "전북") ~ "전북",
      str_detect(주소, "경상북") ~ "경북",
      str_detect(주소, "경상남") ~ "경남",
      str_detect(주소, "제주") ~ "제주",
      TRUE ~ NA_character_
    )
  )

head(addrdata)

# 지역별 행사 횟수
loc.cnt <-aggregate(addrdata$f.name,by=list(loc=addrdata$주소),FUN = 'length')
# 내림차순
loc.cnt <-loc.cnt[order(loc.cnt$x, decreasing = TRUE),]
# 데이터 확인
loc.cnt

rownames(loc.cnt) <- 1:nrow(loc.cnt)

# 상위 10개 지역 추출
loctop10 <-head(loc.cnt,10)

loctop10

# 막대그래프
ggplot(loctop10, aes(x=reorder(loc, -x), y=x, fill=x))+
  ggtitle('공연행사 개최 수 상위 10개 지역')+
  geom_col()+
  scale_fill_gradient(
    high = "violet",
    low = "gray"
  )+
  ylim(0,2000)+
  geom_text(
    aes(label = x),
    vjust = -0.5)+
  labs(x ="지역", y="횟수", fill="")+
  theme_classic()+
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.4)
  )

# ---------------------------------------------------------------
# ---------------------------------------------------------------
# 트리맵 - 지역 + 행사 건수(지역별 행사 비중)
# 트리맵 - 장소별 객석 수(장소 중에 객석 수 TOP10정도 뽑아서)
# install.packages('treemap')
# install.packages('treemapify')
library(treemap)
library(treemapify)
library(ggplot2)
class(data)

# 데이터 초기화
data <-read.csv("전국공연행사정보표준데이터.csv")

# 주소 처리(소재지지번주소로 대체)
data$소재지도로명주소[data$소재지도로명주소 == ""] <- NA
data$소재지지번주소[data$소재지지번주소 == ""] <- NA

data$소재지도로명주소[is.na(data$소재지도로명주소)] <-
data$소재지지번주소[is.na(data$소재지도로명주소)]

# 주소에서 지역 추출
data$지역 <- sub(" .*", "", data$소재지도로명주소)

# 강원도 명칭 통일
data$지역[data$지역 == "강원특별자치도"] <- "강원도"

#########  장소 + 객석 수
place_count <- aggregate(객석수 ~ 장소,data = data,FUN = max)

# 제공기관명 추가
place_count$제공기관명 <- data$제공기관명[
  match(place_count$장소, data$장소)
]

colnames(place_count) <- c("장소", "객석 수", "제공기관명")

# 객석 수가 많은 순서대로 정렬
place_count <- place_count[
  order(place_count$`객석 수`, decreasing = TRUE),
]

# 상위 15개
top15_place <- head(place_count, 15)

# 장소명 변경
top15_place$장소[top15_place$장소 == "주경기장"] <- "서울올림픽경기장"
top15_place$장소[top15_place$장소 == "운동장"] <- "성주문화예술회관(운동장)"

# 글자색 자동 지정
top15_place$글자색 <- ifelse(
  top15_place$`객석 수` > max(top15_place$`객석 수`) * 0.5,
  "white",
  "black"
)

# 그래프 생성
ggplot(top15_place,aes(area = `객석 수`,fill = `객석 수`)) +
  geom_treemap(colour = "black",size = 1) +
  geom_treemap_text(
    aes(label = 제공기관명,colour = 글자색),
    place = "topleft",size = 13,grow = FALSE) +
  geom_treemap_text(
    aes(label = 장소,colour = 글자색),
    place = "center",
    size = 18,
    grow = FALSE) +
  scale_colour_identity() +
  scale_fill_gradientn(
    colours = c("white","#ECBBF4","#9C27B0","#692498","#541D7A")) +
  labs(title = "객석 수 TOP15 장소") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18,hjust = 0.5)
  )

# ---------------------------------------------------------------
# ---------------------------------------------------------------
# 방사형 차트
# 입장연령 전처리(전체관람,유아·아동(0~6),어린이(7~12),청소년(13~18),성인(19~),"기타")

# 패키지 불러오기
library(fmsb)

# 데이터 초기화
data <-read.csv("전국공연행사정보표준데이터.csv")

unique(data$입장연령)

# 기타 제외
age_data <- data[data$입장연령 != "기타", ]


# 입장연령별 공연행사 수 계산
age_count <- table(age_data$입장연령)

# 원하는 연령대 순서 지정
age_order <- c("전체관람","유아·아동","어린이","청소년","성인")

age_count <- age_count[age_order]

# 최대값 / 최소값
max.score <- rep(max(age_count), 5)
min.score <- rep(0, 5)

score <- as.numeric(age_count)

# radarchart용 데이터 만들기
ds <- rbind(max.score,min.score,score)
ds <- data.frame(ds)

colnames(ds) <- c("전체관람","유아·아동","어린이","청소년","성인")

# 그래프 생성
radarchart(
  ds,
  title = "입장연령별 행사 비율",
  pcol = "#9a30ae",
  pfcol = rgb(0.5, 0.2, 0.8, 0.3),
  plwd = 4,
  pty = 16,
  cex = 2,
  cglcol = "#e0bfe6",
  cglwd = 2,
  cglty = 1,
  axistype = 0,
  seg = 5,
  vlcex = 1.5,
  caxislabels = 1.2,
  cex.main = 1.8
)

# ---------------------------------------------------------------
# ---------------------------------------------------------------
# 다중 선 그래프 - 연도, 월별 기준 공연 수 추이

# 행사시작일자(연도와 월 따로)
library(ggplot2)
library(ggrepel)

unique(data$행사시작일자)

# 행사시작일자를 연도·월 단위로 변환
data$연도 <- as.integer(format(as.Date(data$행사시작일자), "%Y"))
table(data$연도)

data$월 <- as.integer(format(as.Date(data$행사시작일자), "%m"))
table(data$월)

# 분석 대상 기간(2021년 이후)
data_recent <- data[data$연도 >= 2021, ]

month_count <- as.data.frame(table(data_recent$연도, data_recent$월))

names(month_count) <- c("연도","월","공연행사 수")

ggplot(
  month_count,
  aes(x = 월,y = `공연행사 수`,group = 연도,color = factor(연도))) +
  geom_line(linewidth = 1) +
  geom_point(shape=21, size = 2,fill = "white") +
  geom_text_repel(aes(label = `공연행사 수`),vjust = -0.8,size = 3,color = "black") +
  labs(title = "2021~2026년 월별 행사 수",x = "월",y = "횟수",color = "연도") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70", linewidth = 0.4),
        panel.grid.major.y = element_line(color = "grey85"))


# ---------------------------------------------------------------
# ---------------------------------------------------------------
# 연도별 선그래프
library(dplyr)
library(ggplot2)

# 1. 데이터 불러오기 및 전처리 (연도 파생변수 생성)
s <- read.csv('전국공연행사정보표준데이터.csv', header = TRUE, fileEncoding = "UTF-8")
s$시작일자 <- as.Date(s$행사시작일자)
s$연도 <- format(s$시작일자, "%Y")

# 2. 연도별(2019년~2026년) 행사 수 집계
yearly_summary <- s %>%
  filter(!is.na(연도) & 연도 >= "2019" & 연도 <= "2026") %>%
  group_by(연도) %>%
  summarise(행사수 = n())

# 3. 연도별 공연 수 추이 선그래프 시각화
ggplot(yearly_summary, aes(x = 연도, y = 행사수, group = 1)) +
  geom_line(color = "#8b5cf6", linewidth = 1.2) +
  geom_point(color = "#8b5cf6", size = 3) +
  geom_text(aes(label = format(행사수, big.mark = ",")), vjust = -1.2, size = 3.5, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "연도별 (2019-2026년) 공연·행사 개최 수 추이",
    subtitle = "엔데믹 전환 이후 행사 건수 변화 양상",
    x = "연도",
    y = "행사 수 (건)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(size = 10, face = "bold")
  )  

# ---------------------------------------------------------------
# ---------------------------------------------------------------
# 상자그림
library(dplyr)
library(ggplot2)
library(scales)

# ==============================================================================
# 1. 데이터 로드 및 정제 함수 (콤마 제거 로직 추가)
# ==============================================================================
s <- read.csv('전국공연행사정보표준데이터.csv', header = TRUE, fileEncoding = "UTF-8")

extract_price <- function(val) {
  if (is.na(val) || val == "") return(0)
  val_str <- as.character(val)
  
  if (grepl("무료", val_str)) return(0)
  
  val_str <- gsub(",", "", val_str)
  
  nums <- as.numeric(unlist(regmatches(val_str, gregexpr("[0-9]+", val_str))))
  nums <- nums[!is.na(nums)]
  
  ticket_nums <- nums[nums >= 1000]
  if (length(ticket_nums) > 0) {
    return(min(ticket_nums))
  }
  return(0)
}

s$정제가격 <- sapply(s$관람요금, extract_price)

# ==============================================================================
# 2. 유료 데이터 필터링 및 규모 구분
# ==============================================================================
df_paid_seat <- s %>%
  filter(정제가격 > 0 & !is.na(객석수) & 객석수 > 0) %>%
  mutate(
    규모구분 = case_when(
      객석수 < 300  ~ "소극장 (300석 미만)",
      객석수 <= 999 ~ "중극장 (300-999석)",
      TRUE          ~ "대극장 (1000석 이상)"
    )
  )

df_paid_seat$규모구분 <- factor(
  df_paid_seat$규모구분, 
  levels = c("소극장 (300석 미만)", "중극장 (300-999석)", "대극장 (1000석 이상)")
)

# ==============================================================================
# 3. 요약 통계 계산 및 라벨 텍스트 준비
# ==============================================================================
total_stats <- df_paid_seat %>%
  summarise(
    총공연수 = n(),
    전체평균 = mean(정제가격),
    전체중앙값 = median(정제가격)
  )

price_summary_table <- df_paid_seat %>%
  group_by(규모구분) %>%
  summarise(
    최솟값 = min(정제가격),
    중앙값_Q2 = median(정제가격),
    최댓값 = max(정제가격),
    평균값 = mean(정제가격),
    .groups = 'drop'
  )

avg_by_scale <- price_summary_table %>% select(규모구분, 평균값)

total_legend_text <- paste0(
  "📊 [ 전체 요약 통계 ]\n",
  "• 총 유료 공연수: ", format(total_stats$총공연수, big.mark = ","), "건\n",
  "• 전체 평균 요금: ", format(round(total_stats$전체평균, 0), big.mark = ","), "원\n",
  "────────────────────\n",
  "• 소극장 평균: ", format(round(avg_by_scale$평균값[avg_by_scale$규모구분 == "소극장 (300석 미만)"], 0), big.mark = ","), "원\n",
  "• 중극장 평균: ", format(round(avg_by_scale$평균값[avg_by_scale$규모구분 == "중극장 (300-999석)"], 0), big.mark = ","), "원\n",
  "• 대극장 평균: ", format(round(avg_by_scale$평균값[avg_by_scale$규모구분 == "대극장 (1000석 이상)"], 0), big.mark = ","), "원"
)

label_data <- price_summary_table %>%
  mutate(
    med_text = paste0("중앙값: ", format(중앙값_Q2, big.mark = ","), "원"),
    min_text = paste0("최소: ", format(최솟값, big.mark = ","), "원"),
    max_text = paste0("최대: ", format(최댓값, big.mark = ","), "원")
  )

outlier_data <- df_paid_seat %>%
  group_by(규모구분) %>%
  mutate(
    Q1 = quantile(정제가격, 0.25),
    Q3 = quantile(정제가격, 0.75),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(정제가격 < lower_bound | 정제가격 > upper_bound) %>%
  ungroup()

# ==============================================================================
# 4. 최종 시각화 실행 (오류 해결 버전)
# ==============================================================================
ggplot(df_paid_seat, aes(x = 규모구분, y = 정제가격, fill = 규모구분)) +
  
  geom_boxplot(alpha = 0.8, show.legend = FALSE, outlier.shape = NA) +
  
  scale_fill_manual(values = c(
    "소극장 (300석 미만)" = "#fca5a5",
    "중극장 (300-999석)" = "#4ade80",
    "대극장 (1000석 이상)" = "#93c5fd"
  )) +
  
  # ① 이상값 빨간 점 표시
  geom_point(
    data = outlier_data, aes(x = 규모구분, y = 정제가격),
    color = "red", size = 1.5, alpha = 0.8, show.legend = FALSE
  ) +
  
  # ② 중앙값 텍스트
  geom_text(
    data = label_data, aes(x = 규모구분, y = 중앙값_Q2, label = med_text),
    vjust = -0.8, fontface = "bold", size = 4.5, color = "black"
  ) +
  
  # ③ 최솟값 텍스트
  geom_text(
    data = label_data, aes(x = 규모구분, y = 최솟값, label = min_text),
    vjust = 1.8, size = 3.5, color = "gray30"
  ) +
  
  # ④ 최댓값 텍스트
  geom_text(
    data = label_data, aes(x = 규모구분, y = 최댓값, label = max_text),
    vjust = -1.2, size = 3.5, color = "gray30"
  ) +
  
  # ⑤ 우측 전체 요약 통계 박스 (x = "대극장 (1000석 이상)"으로 수정하여 에러 방지)
  ggplot2::annotate(
    "text", x = "대극장 (1000석 이상)", y = total_stats$전체중앙값, label = total_legend_text,
    hjust = -1, vjust = 0.5, size = 3.5, fontface = "plain", color = "gray20"
  ) +
  
  scale_y_log10(labels = scales::comma) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  labs(
    title = "공연장 규모별 유료 관람 요금 분포 및 요약 통계",
    subtitle = "소극장, 중극장, 대극장 규모별 요금 수준(최소·중앙·최대) 및 이상치 분포",
    x = "공연장 규모",
    y = "관람 요금 (원)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
    axis.text.x = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 220, b = 20, l = 20)
  )

# ---------------------------------------------------------------
# ---------------------------------------------------------------
# 지도
install.packages('leaflet')
install.packages('htmltools')
library(leaflet)
library(htmltools)
# 데이터 초기화
raw <- read.csv('전국공연행사정보표준데이터.csv', header = TRUE, fileEncoding = "UTF-8")

popup_text <- paste0("<b>",raw$장소,"</b><br>",raw$입장연령,"<br>",raw$행사내용)
popup_html <- lapply(popup_text, HTML)

# 지도 생성
m_ffire<- leaflet(raw) %>%
  #지도 배경
  addProviderTiles(providers$CartoDB.Positron) %>%
  #부산 중심 좌표표
  setView(
    lng=129.0756,
    lat=35.1796,
    
    zoom=11
  )%>%
  #마커추가
  addMarkers(
    lat = ~위도, #lat =ffire$위도
    lng = ~경도, #lng =ffire$경도
    popup = ~popup_html,
    clusterOptions = markerClusterOptions()
  )
# 지도 출력
print(m_ffire)

