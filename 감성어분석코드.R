###감성어 분석 코드
###워킹디렉토리 설정
#RGui 사용시:R console창 클릭하시고 파일->작업디렉토리변경->데이터있는파일 선택후 확인
#Rstudio 사용시: Tools->Global Options->Default Working directory->Browse->원하는작업폴더설정->OK

#####패키지 설치
#####패키지 하나하나 설치해주시면 됩니다.
install.packages("rJava")
library(rJava)
install.packages("multilinguer")
library(multilinguer)
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP',upgrade="never",INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)
install.packages("textclean")
library(textclean)
install.packages("tidytext")
library(tidytext)
install.packages("rvest")
library(rvest)
install.packages("stringr")
library(stringr)
install.packages("tm")
library(tm)
install.packages("tidyverse")
library(tidyverse)
install.packages("SentimentAnalysis")
library(SentimentAnalysis)
install.packages("plyr")
library(plyr)
useSejongDic()

#################외신데이터 불러오기
기사20=read.csv("기사20.csv",header=T) ####기사20.csv 자리에 분석에 사용하실 외신데이터 써주시면됩니다.
head(기사20) #추가적으로 데이터를 불러올때 인코딩이 깨지거나 한다면 외신데이터 인코딩을 UTF-8->그냥 csv파일로 바꿔주세요

###기사 제목과 추후에 감성사전에 있는 단어 매칭
감성어텍스트20<-기사20%>%unnest_tokens(input=제목, ###제목에는 기사제목에 해당하는 컬럼명을 적어주시면 됩니다.
					   output=word,
					   token="words",
					   drop=F)
head(감성어텍스트20) #####단어 잘 나누어졌는지 확인.

#####감성어사전 불러오기
list.files()
dic=read.csv("감성어사전.csv",header=T)
head(dic) #감성어사진 확인

#####감성어사전, 기사제목 결합
감성점수부여=감성어텍스트20%>%left_join(dic,by="word")%>%mutate(polarity=ifelse(is.na(polarity),0,polarity))
head(감성점수부여) ####polarity변수가 추가되었는지 확인.
감성점수부여$점수=as.numeric(감성점수부여$polarity) ####점수생성
제목긍부정=tapply(감성점수부여$점수,감성점수부여$ID,sum) 
#제가 가장 최근에 드린 외신데이터_감성분석+워드클라우드에 감성분석 5개년 데이터를 보면 데이터 셋마다 ID라는 컬럼이 있을겁니다.
#추가적으로 감성어분석 진행하실때 데이터셋에 ID컬럼이 없다면 그냥 기사 순서대로 1,2,3,4로 ID 고유번호를 생성해서 분석진행하시면 될 것 같습니다.

제목긍부정
제목긍부정=as.data.frame(제목긍부정) ##기사제목 감성점수 데이터프레임 변환
nrow(제목긍부정)

####외신데이터+감성점수 결합
head(기사20)
외신데이터20_감성어점수포함=cbind(기사20,제목긍부정)
head(외신데이터20_감성어점수포함)

##########긍정 부정 중립 관측치 추가
외신데이터20_감성어점수포함$style=ifelse(외신데이터20_감성어점수포함$제목긍부정>0,"긍정기사",ifelse(외신데이터20_감성어점수포함$제목긍부정<0,"부정기사","중립기사"))
head(외신데이터20_감성어점수포함)

##########데이터셋 저장
write.csv(외신데이터20_감성어점수포함,"외신데이터20_감성어점수포함.csv")

##########저장된 데이터셋에 긍정기사, 부정기사, 중립기사 컬럼이 잘 생성되었는지 확인해보시면 될 것 같습니다.