###¿Œµµ+¿Œµµ≥◊Ω√æ∆ 21≥‚µµ ø‹Ω≈±‚ªÁ
list.files()
±‚ªÁ21=read.csv("±‚ªÁ√÷¡æ21.csv",header=T)
head(±‚ªÁ21)
¿Œµµ≥◊Ω√æ∆=subset(±‚ªÁ21,±π∞°=="¿Œµµ≥◊Ω√æ∆")
¿Œµµ=subset(±‚ªÁ21,±π∞°=="¿Œµµ")
head(¿Œµµ≥◊Ω√æ∆)
head(¿Œµµ)
º∫¿Â±π∞°21=rbind(¿Œµµ≥◊Ω√æ∆,¿Œµµ)
º∫¿Â±π∞°21=º∫¿Â±π∞°21[,2]
list.files()
º∫¿Â±π∞°21
write.table(º∫¿Â±π∞°21,"º∫¿Â±π∞°21.txt",sep=",",row.names=FALSE)

###
library(rvest)

library(httr)

library(KoNLP)

library(stringr)

library(tm)
install.packages("qgraph")
library(qgraph)

library('xml2')

º∫¿Â±π∞°21=readLines("º∫¿Â±π∞°21.txt", encoding = "UTF-8" )
####
º∫¿Â±π∞°21
ko.words <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos09(d))
  extracted <- str_match(pos, '([∞°-∆R]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}
ko.words(º∫¿Â±π∞°21)
library(KoNLP)
######
options(mc.cores=1)    # ¥‹¿œ Core ∏∏ »∞øÎ«œµµ∑œ ∫Ø∞Ê (ø…º«)
º∫¿Â±π∞°21
cps <- Corpus(VectorSource(º∫¿Â±π∞°21))  

tdm <- TermDocumentMatrix(cps,   
                          control=list(tokenize=ko.words,   ## token ∫–∑˘Ω√ »∞øÎ«“ «‘ºˆ∏Ì ¡ˆ¡§
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 6),  
                                       weighting=weightBin))  

#√÷¡æ∞·∞˙ »Æ¿Œ
dim(tdm)
tdm.matrix <- as.matrix(tdm)
Encoding(rownames(tdm.matrix))= "UTF-8"
rownames(tdm.matrix)[1:100]

##
word.count <- rowSums(tdm.matrix)  ##∞¢ ¥‹æÓ∫∞ «’∞Ë∏¶ ±∏«‘
word.order <- order(word.count, decreasing=T)  #¥Ÿ¿Ω¿∏∑Œ ¥‹æÓµÈ¿ª æ≤¿Œ »Ωºˆø° µ˚∂Û ≥ª∏≤¬˜º¯¿∏∑Œ ¡§∑ƒ
freq.words <- tdm.matrix[word.order[1:30], ] #Term Document Matrixø°º≠ ¿⁄¡÷ æ≤¿Œ ¥‹æÓ ªÛ¿ß 20∞≥ø° «ÿ¥Á«œ¥¬ ∞Õ∏∏ √ﬂ√‚


co.matrix <- freq.words %*% t(freq.words)  #«‡∑ƒ¿« ∞ˆº¿¿ª ¿ÃøÎ«ÿ Term Document Matrix∏¶ Co-occurence Matrix∑Œ ∫Ø∞Ê

qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label √ﬂ∞°
       diag=F,                       ## ¿⁄Ω≈¿« ∞¸∞Ë¥¬ ¡¶∞≈«‘
       layout='spring',              ##≥ÎµÂµÈ¿« ¿ßƒ°∏¶ spring¿∏∑Œ ø¨∞·µ» ∞Õ √≥∑≥ ∞¸∑√¿Ã ∞≠«œ∏È ∞∞¿Ã ∫ŸæÓ ¿÷∞Ì æ¯¿∏∏È ∏÷∏Æ ∂≥æÓ¡ˆµµ∑œ «•Ω√µ 
       edge.color='blue',
       vsize=log(diag(co.matrix))*2)

#####
head(±‚ªÁ21)
¿œ∫ª=subset(±‚ªÁ21,±π∞°=="¿œ∫ª")
øµ±π=subset(±‚ªÁ21,±π∞°=="øµ±π")
ºË≈±π∞°21=rbind(¿œ∫ª,øµ±π)
ºË≈±π∞°21=ºË≈±π∞°21[,2]
ºË≈±π∞°21
write.table(ºË≈±π∞°21,"ºË≈±π∞°21.txt",sep=",",row.names=FALSE)


ºË≈±π∞°21=readLines("ºË≈±π∞°21.txt", encoding = "UTF-8" )


##
options(mc.cores=1)    # ¥‹¿œ Core ∏∏ »∞øÎ«œµµ∑œ ∫Ø∞Ê (ø…º«)
º∫¿Â±π∞°21
cps <- Corpus(VectorSource(ºË≈±π∞°21))  

tdm <- TermDocumentMatrix(cps,   
                          control=list(tokenize=ko.words,   ## token ∫–∑˘Ω√ »∞øÎ«“ «‘ºˆ∏Ì ¡ˆ¡§
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 6),  
                                       weighting=weightBin))  

dim(tdm)
tdm.matrix <- as.matrix(tdm)
Encoding(rownames(tdm.matrix))= "UTF-8"
rownames(tdm.matrix)[1:100]


word.count <- rowSums(tdm.matrix)  ##∞¢ ¥‹æÓ∫∞ «’∞Ë∏¶ ±∏«‘
word.order <- order(word.count, decreasing=T)  #¥Ÿ¿Ω¿∏∑Œ ¥‹æÓµÈ¿ª æ≤¿Œ »Ωºˆø° µ˚∂Û ≥ª∏≤¬˜º¯¿∏∑Œ ¡§∑ƒ
freq.words <- tdm.matrix[word.order[1:30], ]

co.matrix <- freq.words %*% t(freq.words)  #«‡∑ƒ¿« ∞ˆº¿¿ª ¿ÃøÎ«ÿ Term Document Matrix∏¶ Co-occurence Matrix∑Œ ∫Ø∞Ê

qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label √ﬂ∞°
       diag=F,                       ## ¿⁄Ω≈¿« ∞¸∞Ë¥¬ ¡¶∞≈«‘
       layout='spring',              ##≥ÎµÂµÈ¿« ¿ßƒ°∏¶ spring¿∏∑Œ ø¨∞·µ» ∞Õ √≥∑≥ ∞¸∑√¿Ã ∞≠«œ∏È ∞∞¿Ã ∫ŸæÓ ¿÷∞Ì æ¯¿∏∏È ∏÷∏Æ ∂≥æÓ¡ˆµµ∑œ «•Ω√µ 
       edge.color='blue',
       vsize=log(diag(co.matrix))*2)






















