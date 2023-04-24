##연관어 분석 코드
##패키지 설치
install.packages("stringr") #패키지 설치를 안하신경우만 설치해주세요 
library(stringr) 
getwd()

####데이터 불러오기
list.files()
기사21=read.csv("21년도외신기사.csv",header=T)
head(기사21,1)

####기사&대상어 전처리
####BTS를 대상어로 정하고 분석을 진행하실경우 예시코드입니다
####기사마다 BTS를 '방탄소년단', '방탄 소년단', 'bts'등 부르는 이름이 다양하기때문에
####대상어를 하나로 통합할 필요가 있습니다.

#####기사전처리
기사21제목=기사21[,3] ##일단 분석하시고 싶은 기사의 제목만 추출합니다. 연관어 분석에서는 제목컬럼만을 사용합니다(국가별로보신다면 국가컬럼도추출)
기사21제목=as.data.frame(기사21제목)
names(기사21제목)[1]="제목" ##컬럼명 바꿔주기
colnames(기사21제목)

#####대상어전처리 방탄소년단을 나타내는 단어들을 BTS로 통일시켜줍니다
기사21제목$제목=gsub("방탄소년단","BTS",기사21제목$제목) 
기사21제목$제목=gsub("방탄 소년단","BTS",기사21제목$제목)
기사21제목$제목=gsub("bts","BTS",기사21제목$제목)
head(기사21제목)

###대상어 포함 제목 추출
기사21제목$방탄소년단유무=str_detect(기사21제목$제목,"BTS") #해당 코드에서 제가 BTS라고 쓴 부분에 원하시는 대상어 넣으시면됩니다
#TRUE라고 나오는 기사들이 대상어(BTS)가 포함된 기사제목입니다.

####대상어포함 기사 빈도수확인 및 추출
head(기사21제목) 
table(기사21제목$방탄소년단유무) #TRUE값을 보시면 됩니다
방탄소년단21=subset(기사21제목,기사21제목$방탄소년단유무=="TRUE") #대상어 포함한 기사만 추출
nrow(방탄소년단21) #table의 TRUE값과 현재 코드 결과 같은지 확인.

#####방탄관련 기사제목만 추출
방탄소년단21=방탄소년단21[,1] #제목만 추출
방탄소년단21=as.data.frame(방탄소년단21) #데이터프레임으로 변환
head(방탄소년단21)

####제목 텍스트파일로 저장
write.table(방탄소년단21,"방탄소년단21.txt",sep=",",row.names=FALSE,fileEncoding="UTF-8")
방탄21=readLines("방탄소년단21.txt", encoding = "UTF-8" ) ##텍스트파일로 저장한 파일 한줄씩 불러오기
head(방탄21,20) #글자 안깨지는지 확인

##관련패키지 설치(설치안되면 install.packages("패키지명")하고 library(패키지명)해주세요)
library(rvest)
library(httr)
library(KoNLP)
library(stringr)
library(tm)
library(qgraph)
library('xml2')

###########명사+형용사 추출 함수
ko.words <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos09(d))
  extracted <- str_match(pos, '([가-힣]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}


#이제 분리된 단어를 기준으로 Corpus 라는 일종의 말뭉치로 끊어낸 뒤 각 단어별 노출 빈도수 계산.

options(mc.cores=1)    # 단일 Core 만 활용하도록 변경 (옵션)
cps <- Corpus(VectorSource(방탄21))  
tdm <- TermDocumentMatrix(cps,   
                          control=list(tokenize=ko.words,   ## token 분류시 활용할 함수명 지정
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 6), #wordLengths 는 최대 최소 단어의 길이를 지정하는 것입니다(문자길이에따라 조정하시면됩니다.). 너무 길 경우 문장이 섞여들어올 수 있습니다. 
                                       weighting=weightBin)) #weightBin은 한 문장에서 동일하게 반복되는 말의 경우를 여러차례 카운트 하는 것이 하니라 한번으로 만 카운트. 
                                                              #예를 들어 너무 너무 좋았다 라는 표현에서 너무 단어를 한번만 카운트


dim(tdm)
tdm.matrix <- as.matrix(tdm)
Encoding(rownames(tdm.matrix)) <- "UTF-8"
rownames(tdm.matrix)[1:100]

tdm.matrix
word.count <- rowSums(tdm.matrix)  
word.order <- order(word.count, decreasing=T)
freq.words <- tdm.matrix[word.order[c(1:100)], ] #BTS가 들어간 문맥에서 자주쓰인 상위 단어 30개 추출
freq.words 
연관어=as.data.frame(freq.words)
write.csv(연관어,"방탄연관어21.csv") #상위 30개 연관어 데이터프레임 저장 
#저장해서 보시면 대상어포함 자주쓰인 연관어들이 데이터프레임형태로 저장됩니다. 데이터마다 다르겠지만 '년', '월', '중' 등과 같이 쓸데없는 상위권에 들어간 경우가 있을겁니다.
#이러한 경우는 직접 데이터프레임 보시고 쓸만한 키워드들만 추출하셔서 시각화 쓰는 것을 추천드립니다.

co.matrix <- freq.words %*% t(freq.words)


###연관어 시각화

qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label 추가
       diag=F,                       ## 자신의 관계는 제거함
       layout='spring',              ##노드들의 위치를 spring으로 연결된 것 처럼 관련이 강하면 같이 붙어 있고 없으면 멀리 떨어지도록 표시됨
       edge.color='blue',
       vsize=9) ###시각화로 표현되는 원의 크기
