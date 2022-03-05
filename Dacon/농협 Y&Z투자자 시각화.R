#라이브러리 
library(forcats)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(scales)
library(car)
library(dplyr)
library(data.table)
library(lubridate)
library(forcats)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(scales)
library(car)
library(dplyr)
library(data.table)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)

#진짜 찐막 스크립트 

setwd("G:/")

#데이터 불러오기 

iem_info=fread("2_iem_info.csv",encoding="UTF-8")
cus=read.csv("realcus.csv")
act=read.csv("2_act_info.csv")
cus=cus[-1]

names(act)[1] <- c("act_id")
ko_iem=read.csv("ko_iem.csv")
ko_iem=ko_iem[-1]
names(trd_kr)[1]=c("act_id")

trd_kr=read.csv("trd_main.csv")

#cus 데이터에 gen 파생변수 생성

cus$gen[cus$cus_age>=40]="x"
cus$gen[cus$cus_age<=35 &cus$cus_age>=30]="y"
cus$gen[cus$cus_age<=25 &cus$cus_age>00]="z"
cus$gen[cus$cus_age==00]="others"

#cus 데이터에서 gen y&z 만뽑아내기 

cus_yzgen=subset(cus,gen!="x")
cus_yzgen=subset(cus_yzgen,gen!="others") # 총 3418 개의 데이터만 남음 

#cus_yzgen 5단위 범주를 10 단위로 재 범주화

cus_yzgen$cus_age<- fct_recode(as.factor(cus_yzgen$cus_age), "20" = "25", "30" = "35")

#해당사항 없음 ,등급없음 그리고 정보제공 미동의 제거 

cus_yzgen=subset(cus_yzgen,tco_cus_grd_cd !="99"&ivs_icn_cd !="99")
cus_yzgen=subset(cus_yzgen,tco_cus_grd_cd !="9"&ivs_icn_cd !="9")


#cus_yzgen 성별 비율 시각화

cus_clust<- cus_yzgen
cus_clust$sex_dit_cd<- as.factor(cus_clust$sex_dit_cd)

s1 <- cus_clust %>% 
  ggplot(aes("", fill = sex_dit_cd)) + 
  geom_bar(position = "fill") + 
  coord_polar("y") + 
  geom_text(data = . %>%
              group_by(sex_dit_cd) %>%
              tally() %>%
              mutate(p = n / sum(n)) %>%
              ungroup(), 
            aes(y = p, label = scales::percent(p)), 
            position = position_stack(vjust = 0.5), show.legend = FALSE, size = 6) +
  theme_void() + 
  scale_fill_manual(
    values = c('#4393C3', '#D6604D'), 
    name = "성별", 
    labels = c('남자', '여자')) +
  theme(
    legend.title = element_text(colour = "black", size = 10, face = 'bold'), 
    legend.text = element_text(colour = "black", size = 10), 
    legend.key.width = unit(0.5, 'cm'), 
    legend.key.height = unit(0.5, 'cm'))

s1


#나이 원 그래프

cus_clust$cus_age<- as.factor(cus_clust$cus_age)

s2 <- cus_clust %>%
  ggplot(aes("", fill = cus_age)) + 
  geom_bar(position = "fill") + 
  coord_polar("y") + 
  geom_text(data = . %>%
              group_by(cus_age) %>%
              tally() %>%
              mutate(p = n / sum(n)) %>%
              ungroup(), 
            aes(y = p, label = scales::percent(p)), 
            position = position_stack(vjust = 0.5), show.legend = FALSE, size = 6) +
  theme_void() + 
  scale_fill_manual(
    values = c( "#FFCC00", "#FF9900", 
                "#FF6600", "#FF3300"), 
    name = '나이', 
    labels = c('20','25', '30','35')) + 
  theme(
    legend.title = element_text(colour = "black", size = 10, face = 'bold'), 
    legend.text = element_text(colour = "black", size = 10), 
    legend.key.width = unit(0.5, 'cm'), 
    legend.key.height = unit(0.5, 'cm'))
s2

#세대별 원 그래프 

cus_clust$gen<- as.factor(cus_clust$gen)

s3 <- cus_clust %>%
  ggplot(aes("", fill = gen)) + 
  geom_bar(position = "fill") + 
  coord_polar("y") + 
  geom_text(data = . %>%
              group_by(gen) %>%
              tally() %>%
              mutate(p = n / sum(n)) %>%
              ungroup(), 
            aes(y = p, label = scales::percent(p)), 
            position = position_stack(vjust = 0.5), show.legend = FALSE, size = 6) +
  theme_void() + 
  scale_fill_manual(
    values = c('#FAEBD7', '#458B74'), 
    name = '세대', 
    labels = c('Y세대', 'Z세대')) + 
  theme(
    legend.title = element_text(colour = "black", size = 10, face = 'bold'), 
    legend.text = element_text(colour = "black", size = 10), 
    legend.key.width = unit(0.5, 'cm'), 
    legend.key.height = unit(0.5, 'cm'))
s3

options(repr.plot.width = 10, repr.plot.height = 5, repr.plot.res = 500)
grid.arrange(s1, s2,s3, nrow = 1, ncol = 3)


#세대별 고객 등급 히스토 그램 

cus_clust %>% 
  group_by(gen, tco_cus_grd_cd) %>% 
  summarize(n=n()) %>% 
  mutate(freq=round(n/sum(n), 4)*100) %>% 
  ggplot(aes(tco_cus_grd_cd, freq, fill=gen)) + geom_bar(stat='identity') +
  facet_wrap(~gen) + xlab('고객 등급') + ylab('비율(%)') + theme(legend.position='none')

#세대별 고객 투자성향 히스토 그램 

cus_clust %>% 
  group_by(gen, ivs_icn_cd) %>% 
  summarize(n=n()) %>% 
  mutate(freq=round(n/sum(n), 4)*100) %>% 
  ggplot(aes(ivs_icn_cd, freq, fill=gen)) + geom_bar(stat='identity') +
  facet_wrap(~gen) + xlab('투자 성향') + ylab('비율(%)') + theme(legend.position='none')





