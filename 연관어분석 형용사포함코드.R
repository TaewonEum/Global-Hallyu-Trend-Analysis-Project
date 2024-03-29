###인도+인도네시아 21년도 외신기사
list.files()
기사21=read.csv("기사최종21.csv",header=T)
head(기사21)
인도네시아=subset(기사21,국가=="인도네시아")
인도=subset(기사21,국가=="인도")
head(인도네시아)
head(인도)
성장국가21=rbind(인도네시아,인도)
성장국가21=성장국가21[,2]
list.files()
성장국가21
write.table(성장국가21,"성장국가21.txt",sep=",",row.names=FALSE)

###
library(rvest)

library(httr)

library(KoNLP)

library(stringr)

library(tm)
install.packages("qgraph")
library(qgraph)

library('xml2')

성장국가21=readLines("성장국가21.txt", encoding = "UTF-8" )
####
성장국가21
ko.words <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos09(d))
  extracted <- str_match(pos, '([가-힣]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}
ko.words(성장국가21)
library(KoNLP)
######
options(mc.cores=1)    # 단일 Core 만 활용하도록 변경 (옵션)
성장국가21
cps <- Corpus(VectorSource(성장국가21))  

tdm <- TermDocumentMatrix(cps,   
                          control=list(tokenize=ko.words,   ## token 분류시 활용할 함수명 지정
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 6),  
                                       weighting=weightBin))  

#최종결과 확인
dim(tdm)
tdm.matrix <- as.matrix(tdm)
Encoding(rownames(tdm.matrix))= "UTF-8"
rownames(tdm.matrix)[1:100]

##
word.count <- rowSums(tdm.matrix)  ##각 단어별 합계를 구함
word.order <- order(word.count, decreasing=T)  #다음으로 단어들을 쓰인 횟수에 따라 내림차순으로 정렬
freq.words <- tdm.matrix[word.order[1:30], ] #Term Document Matrix에서 자주 쓰인 단어 상위 20개에 해당하는 것만 추출


co.matrix <- freq.words %*% t(freq.words)  #행렬의 곱셈을 이용해 Term Document Matrix를 Co-occurence Matrix로 변경

qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label 추가
       diag=F,                       ## 자신의 관계는 제거함
       layout='spring',              ##노드들의 위치를 spring으로 연결된 것 처럼 관련이 강하면 같이 붙어 있고 없으면 멀리 떨어지도록 표시됨
       edge.color='blue',
       vsize=log(diag(co.matrix))*2)

#####
head(기사21)
일본=subset(기사21,국가=="일본")
영국=subset(기사21,국가=="영국")
쇠퇴국가21=rbind(일본,영국)
쇠퇴국가21=쇠퇴국가21[,2]
쇠퇴국가21
write.table(쇠퇴국가21,"쇠퇴국가21.txt",sep=",",row.names=FALSE)


쇠퇴국가21=readLines("쇠퇴국가21.txt", encoding = "UTF-8" )


##
options(mc.cores=1)    # 단일 Core 만 활용하도록 변경 (옵션)
성장국가21
cps <- Corpus(VectorSource(쇠퇴국가21))  

tdm <- TermDocumentMatrix(cps,   
                          control=list(tokenize=ko.words,   ## token 분류시 활용할 함수명 지정
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 6),  
                                       weighting=weightBin))  

dim(tdm)
tdm.matrix <- as.matrix(tdm)
Encoding(rownames(tdm.matrix))= "UTF-8"
rownames(tdm.matrix)[1:100]


word.count <- rowSums(tdm.matrix)  ##각 단어별 합계를 구함
word.order <- order(word.count, decreasing=T)  #다음으로 단어들을 쓰인 횟수에 따라 내림차순으로 정렬
freq.words <- tdm.matrix[word.order[1:30], ]

co.matrix <- freq.words %*% t(freq.words)  #행렬의 곱셈을 이용해 Term Document Matrix를 Co-occurence Matrix로 변경

qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label 추가
       diag=F,                       ## 자신의 관계는 제거함
       layout='spring',              ##노드들의 위치를 spring으로 연결된 것 처럼 관련이 강하면 같이 붙어 있고 없으면 멀리 떨어지도록 표시됨
       edge.color='blue',
       vsize=log(diag(co.matrix))*2)






















