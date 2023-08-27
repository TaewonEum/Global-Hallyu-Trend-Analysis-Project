library(tidytext)
library(stringr)
library(plyr)
library(dplyr)
list.files()

dic=read.csv("카테고리분류사전.csv",header=T) #사전불러오기

#################외신데이터 불러오기
list.files()
기사17=read.csv("17년도외신기사_감성분석포함.csv",header=T) 


head(기사17) #추가적으로 데이터를 불러올때 인코딩이 깨지거나 한다면 외신데이터 인코딩을 UTF-8->그냥 csv파일로 바꿔주세요
기사17=기사17[,c(1,3,4)] #해당 기사데이터에서 ID, 제목, 국가컬럼만 추출.
head(기사17)
###제목 단어 분리
텍스트17<-기사17%>%unnest_tokens(input=제목, ###제목에는 기사제목에 해당하는 컬럼명을 적어주시면 됩니다.
                            output=word,
                            token="words",
                            drop=F)

head(텍스트17) #####단어 잘 나누어졌는지 확인(word컬럼확인해보시면됩니다)


#####카테고리분류사전단어들과 외신기사제목 매칭
카테고리분류=텍스트17%>%left_join(dic,by="word")%>%mutate(polarity=ifelse(is.na(polarity),0,polarity))
head(카테고리분류) ####polarity변수가 추가되었는지 확인.
str(카테고리분류)

전체보도량=nrow(기사17) ###조사하는 년도 총 보도량
전체보도량

카테고리점수=tapply(카테고리분류$polarity,카테고리분류$ID,sum,na.rm=T)
카테고리점수=as.data.frame(카테고리점수)
head(기사17_카테고리분류)
기사17_카테고리분류=cbind(기사17,카테고리점수)

###카테고리 나누기
기사17_카테고리분류$style=ifelse(기사17_카테고리분류$카테고리점수==0,'기타',ifelse(기사17_카테고리분류$카테고리점수<0,'음식/문화',ifelse(기사17_카테고리분류$카테고리점수>99,'KPOP','영화/드라마')))
write.csv(기사17_카테고리분류,"기사17_카테고리분류.csv") #기사 카테고리 분류된 데이터 저장

