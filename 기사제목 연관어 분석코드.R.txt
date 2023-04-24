##楷包绢 盒籍 内靛
##菩虐瘤 汲摹
install.packages("stringr") #菩虐瘤 汲摹甫 救窍脚版快父 汲摹秦林技夸 
library(stringr) 
getwd()

####单捞磐 阂矾坷扁
list.files()
扁荤21=read.csv("21斥档寇脚扁荤.csv",header=T)
head(扁荤21,1)

####扁荤&措惑绢 傈贸府
####BTS甫 措惑绢肺 沥窍绊 盒籍阑 柳青窍角版快 抗矫内靛涝聪促
####扁荤付促 BTS甫 '规藕家斥窜', '规藕 家斥窜', 'bts'殿 何福绰 捞抚捞 促剧窍扁锭巩俊
####措惑绢甫 窍唱肺 烹钦且 鞘夸啊 乐嚼聪促.

#####扁荤傈贸府
扁荤21力格=扁荤21[,3] ##老窜 盒籍窍矫绊 酵篮 扁荤狼 力格父 眠免钦聪促. 楷包绢 盒籍俊辑绰 力格拿烦父阑 荤侩钦聪促(惫啊喊肺焊脚促搁 惫啊拿烦档眠免)
扁荤21力格=as.data.frame(扁荤21力格)
names(扁荤21力格)[1]="力格" ##拿烦疙 官层林扁
colnames(扁荤21力格)

#####措惑绢傈贸府 规藕家斥窜阑 唱鸥郴绰 窜绢甸阑 BTS肺 烹老矫难凛聪促
扁荤21力格$力格=gsub("规藕家斥窜","BTS",扁荤21力格$力格) 
扁荤21力格$力格=gsub("规藕 家斥窜","BTS",扁荤21力格$力格)
扁荤21力格$力格=gsub("bts","BTS",扁荤21力格$力格)
head(扁荤21力格)

###措惑绢 器窃 力格 眠免
扁荤21力格$规藕家斥窜蜡公=str_detect(扁荤21力格$力格,"BTS") #秦寸 内靛俊辑 力啊 BTS扼绊 敬 何盒俊 盔窍矫绰 措惑绢 持栏矫搁邓聪促
#TRUE扼绊 唱坷绰 扁荤甸捞 措惑绢(BTS)啊 器窃等 扁荤力格涝聪促.

####措惑绢器窃 扁荤 后档荐犬牢 棺 眠免
head(扁荤21力格) 
table(扁荤21力格$规藕家斥窜蜡公) #TRUE蔼阑 焊矫搁 邓聪促
规藕家斥窜21=subset(扁荤21力格,扁荤21力格$规藕家斥窜蜡公=="TRUE")
nrow(规藕家斥窜21) #table狼 TRUE蔼苞 泅犁 内靛 搬苞 鞍篮瘤 犬牢.

#####规藕包访 扁荤力格父 眠免
规藕家斥窜21=规藕家斥窜21[,1]
规藕家斥窜21=as.data.frame(规藕家斥窜21)
head(规藕家斥窜21)

####力格 咆胶飘颇老肺 历厘
write.table(规藕家斥窜21,"规藕家斥窜21.txt",sep=",",row.names=FALSE,fileEncoding="UTF-8")
规藕21=readLines("规藕家斥窜21.txt", encoding = "UTF-8" ) ##咆胶飘颇老肺 历厘茄 颇老 茄临究 阂矾坷扁
head(规藕21,20) #臂磊 救柄瘤绰瘤 犬牢

##包访菩虐瘤 汲摹(汲摹救登搁 install.packages("菩虐瘤疙")窍绊 library(菩虐瘤疙)秦林技夸)
library(rvest)
library(httr)
library(KoNLP)
library(stringr)
library(tm)
library(qgraph)
library('xml2')

###########楷包绢 眠免
ko.words <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos09(d))
  extracted <- str_match(pos, '([啊-芌]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}


#捞力 盒府等 窜绢甫 扁霖栏肺 Corpus 扼绰 老辆狼 富苟摹肺 谗绢辰 第 阿 窜绢喊 畴免 后档荐 拌魂.

options(mc.cores=1)    # 窜老 Core 父 劝侩窍档废 函版 (可记)
cps <- Corpus(VectorSource(规藕21))  
tdm <- TermDocumentMatrix(cps,   
                          control=list(tokenize=ko.words,   ## token 盒幅矫 劝侩且 窃荐疙 瘤沥
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 6), #wordLengths 绰 弥措 弥家 窜绢狼 辨捞甫 瘤沥窍绰 巴涝聪促(巩磊辨捞俊蝶扼 炼沥窍矫搁邓聪促.). 呈公 辨 版快 巩厘捞 集咯甸绢棵 荐 乐嚼聪促. 
                                       weighting=weightBin)) #weightBin篮 茄 巩厘俊辑 悼老窍霸 馆汗登绰 富狼 版快甫 咯矾瞒肥 墨款飘 窍绰 巴捞 窍聪扼 茄锅栏肺 父 墨款飘. 
                                                              #抗甫 甸绢 呈公 呈公 亮疽促 扼绰 钎泅俊辑 呈公 窜绢甫 茄锅父 墨款飘


dim(tdm)
tdm.matrix <- as.matrix(tdm)
Encoding(rownames(tdm.matrix)) <- "UTF-8"
rownames(tdm.matrix)[1:100]

tdm.matrix
word.count <- rowSums(tdm.matrix)  
word.order <- order(word.count, decreasing=T)
freq.words <- tdm.matrix[word.order[c(1:100)], ] #BTS啊 甸绢埃 巩钙俊辑 磊林静牢 惑困 窜绢 30俺 眠免
freq.words 
楷包绢=as.data.frame(freq.words)
write.csv(楷包绢,"规藕楷包绢21.csv") #惑困 30俺 楷包绢 单捞磐橇饭烙 历厘 
#历厘秦辑 焊矫搁 措惑绢器窃 磊林静牢 楷包绢甸捞 单捞磐橇饭烙屈怕肺 历厘邓聪促. 单捞磐付促 促福摆瘤父 '斥', '岿', '吝' 殿苞 鞍捞 镜单绝绰 惑困鼻俊 甸绢埃 版快啊 乐阑疤聪促.
#捞矾茄 版快绰 流立 单捞磐橇饭烙 焊矫绊 镜父茄 虐况靛甸父 眠免窍寂辑 矫阿拳 静绰 巴阑 眠玫靛赋聪促.

co.matrix <- freq.words %*% t(freq.words)


qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label 眠啊
       diag=F,                       ## 磊脚狼 包拌绰 力芭窃
       layout='spring',              ##畴靛甸狼 困摹甫 spring栏肺 楷搬等 巴 贸烦 包访捞 碍窍搁 鞍捞 嘿绢 乐绊 绝栏搁 钢府 冻绢瘤档废 钎矫凳
       edge.color='blue',
       vsize=9) ###矫阿拳肺 钎泅登绰 盔狼 农扁



















