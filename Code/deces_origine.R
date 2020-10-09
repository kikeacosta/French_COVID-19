### Code for COVID-19 French mortality 
## 
# PREPARATION OBLIGATOIRE #####
#install.packages("expp")
library(expp)
library(stringr)
library(readxl)
library(readr)
library(MortalitySmooth)
library(svcm)
library(gridExtra)
library(RColorBrewer)
library(lattice)
library(graphics)
library(scales)
library(haven)
library(foreign)
library(questionr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(cartography)
library(maptools)
library(spdep)
#install.packages("sp")
library(sp)
#install.packages("sf")
library(sf)

options(OutDec=",") #On choisit la virgule comme s?parateur des d?cimales
options(digits = 4) #On choisit d'arrondir les r?sultats ? un chiffre apr?s la virgule
options(scipen = 999) #On choisit d'emp?cher l'affichage des r?sultats en ?criture scientifique

#install.packages("extrafont")
library(extrafont)
loadfonts(device = "win")
font_import()
fonts()
fonttable()

deces2020M1 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/deces_2020_M01.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M2 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/deces_2020_M02.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M3 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/deces_2020_M03.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M4 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/deces_2020_M04.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M5 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/deces_2020_M05.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces_2015 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/deces-2015.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
deces_2016 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/deces-2016.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
deces_2017 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/deces-2017.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
deces_2018 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/deces-2018.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
deces_2019 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/deces-2019.csv",  ";", escape_double = FALSE, trim_ws = TRUE)

table_passage <- read_excel("D:/Donnees/Covid-19/passage communes.xlsx")

table_epci <- read_excel("D:/Donnees/table epci.xlsx") 
table_epci <- table_epci %>% 
  filter(!(siren %in% c("245600465","241700624","241700459","248500191"))) %>% 

  select(lieudeces=insee,epcideces=siren)

deces <- rbind(deces2020M1,
               deces2020M2,
               deces2020M3,
               deces2020M4,
               deces2020M5,
               deces_2015,
               deces_2016,
               deces_2017,
               deces_2018,
               deces_2019) 
rm(deces2020M1,
   deces2020M2,
   deces2020M3,
   deces2020M4,
   deces2020M5,
   deces_2015,
   deces_2016,
   deces_2017,
   deces_2018,
   deces_2019)


deces <- merge(deces,table_passage,by.x="lieudeces",by.y="COM_INI",all.x=T)
deces <- deces %>% 
  mutate(lieudeces=ifelse(is.na(COM_FIN),lieudeces,COM_FIN))
deces <- merge(deces,table_epci,all.x=T) 

EPCI <- st_read("D:/Donnees//Covid-19/EPCI2020_region.shp")
EPCI <- st_transform(st_as_sf(EPCI, crs = st_crs(4326)))
DEP <-  st_read("C:/Users/irwin/OneDrive/M2 D?mographie/Structure de la sant?/DEPARTEMENT.shp")
DEP <- st_transform(st_as_sf(DEP, crs = st_crs(4326)))
deces_sauv <- deces
#Importation et construction table####


write.table(deces,"deces1519.csv",sep = ";")
offsets <- read_excel("C:/Users/irwin/OneDrive/Donn?es stats/deces/offsets.xlsx",sheet = 2)

setwd("C:/Users/irwin/OneDrive/Donn?es stats/deces")
# gc()
# rm()
# rm(list=ls())
# FD_INDCVI_2016 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/FD_INDCVI_2016.csv", 
#                              ";", escape_double = T, trim_ws = TRUE)
# 
# FD_INDCVI_2016 <- FD_INDCVI_2016 %>% 
#   select(AGED,IMMI,INAI,INATC,IPONDI)
# 
# FD_INDCVI_2016 <- FD_INDCVI_2016 %>% 
#   mutate(INAI=case_when(INAI=="1"~"N?s en France",
#                         INAI=="2"~"N?s en France",
#                         INAI=="3"~"N?s en France",
#                         INAI=="4"~"N?s en France",
#                         INAI=="5"~"N?s en France",
#                         INAI=="6"~"N?s ? l'?tranger"),
#          IMMI=case_when(IMMI=="1"~"Immigr?s",
#                         IMMI=="2"~"Non immigr?s"),
#          INATC=case_when(INATC=="1"~"Fran?ais",
#                          INATC=="2"~"?trangers"))
# 
# ENSEMBLE <- wtd.table(FD_INDCVI_2016$IMMI,FD_INDCVI_2016$INAI,FD_INDCVI_2016$INATC,weights=FD_INDCVI_2016$IPONDI)
# table(FD_INDCVI_2016$INATC,FD_INDCVI_2016$IMMI,FD_INDCVI_2016$INAI)
# write.table(ENSEMBLE,"ENSEMBLE.csv",sep=";")
# dev.off()
# POPIMM <- wtd.table(FD_INDCVI_2016$AGED,FD_INDCVI_2016$INAI,weights = FD_INDCVI_2016$IPONDI)
# write.table(POPIMM,"POPIMM.csv",sep=";")

#Grand Est#
# gc()
# rm()
# rm(list=ls())
# FD_INDCVI_2016 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/FD_INDCVI_2016.csv",
#                              ";", escape_double = T, trim_ws = TRUE)
# 
# FD_INDCVI_2016 <- FD_INDCVI_2016 %>%
#   filter(REGION=="44") %>% 
#   select(AGED,IMMI,INAI,INATC,IPONDI)
# 
# FD_INDCVI_2016 <- FD_INDCVI_2016 %>%
#   mutate(INAI=case_when(INAI=="1"~"N?s en France",
#                         INAI=="2"~"N?s en France",
#                         INAI=="3"~"N?s en France",
#                         INAI=="4"~"N?s en France",
#                         INAI=="5"~"N?s en France",
#                         INAI=="6"~"N?s ? l'?tranger"),
#          IMMI=case_when(IMMI=="1"~"Immigr?s",
#                         IMMI=="2"~"Non immigr?s"),
#          INATC=case_when(INATC=="1"~"Fran?ais",
#                          INATC=="2"~"?trangers"))
# 
# 
# 
# 
# POPIMM <- wtd.table(FD_INDCVI_2016$AGED,FD_INDCVI_2016$INAI,weights = FD_INDCVI_2016$IPONDI)
# write.table(POPIMM,"POPIMMGE.csv",sep=";")

#Ile-de-France#
# gc()
# rm()
# rm(list=ls())
# FD_INDCVI_2016 <- read_delim("C:/Users/irwin/OneDrive/Donn?es stats/deces/FD_INDCVI_2016.csv",
#                              ";", escape_double = T, trim_ws = TRUE)
# 
# FD_INDCVI_2016 <- FD_INDCVI_2016 %>%
#   filter(REGION=="11") %>% 
#   select(AGED,IMMI,INAI,INATC,IPONDI)
# 
# FD_INDCVI_2016 <- FD_INDCVI_2016 %>%
#   mutate(INAI=case_when(INAI=="1"~"N?s en France",
#                         INAI=="2"~"N?s en France",
#                         INAI=="3"~"N?s en France",
#                         INAI=="4"~"N?s en France",
#                         INAI=="5"~"N?s en France",
#                         INAI=="6"~"N?s ? l'?tranger"),
#          IMMI=case_when(IMMI=="1"~"Immigr?s",
#                         IMMI=="2"~"Non immigr?s"),
#          INATC=case_when(INATC=="1"~"Fran?ais",
#                          INATC=="2"~"?trangers"))
# 
# 
# 
# 
# POPIMM <- wtd.table(FD_INDCVI_2016$AGED,FD_INDCVI_2016$INAI,weights = FD_INDCVI_2016$IPONDI)
# write.table(POPIMM,"POPIMMIDF.csv",sep=";")


deces <- deces %>% 
  mutate(anaiss=substr(datenaiss,1,4),
         adec=substr(datedeces,1,4),
         agedec=(as.numeric(adec)-as.numeric(anaiss)),
         agedec=ifelse(as.numeric(agedec)>=100,100,agedec),
         depdec=substr(lieudeces,1,2),
         depnaiss=substr(lieunaiss,1,2),
         moisnaiss=substr(datenaiss,5,6),
         moisdec=substr(datedeces,5,6),
         jdec=substr(datedeces,7,8),
         filtre=paste0(substr(moisdec,2,3),substr(jdec,1,3)),
         jnaiss=substr(datenaiss,7,8),
         datedeces=as.Date.character(datedeces,"%Y%m%d"),
         semdec=isoweek(datedeces),
         sexe=case_when(as.character(sexe)=="1"~"homme",
                        as.character(sexe)=="2"~"femme"),
         agedec=case_when(agedec>=100~100,
                          agedec<100~agedec),
         nomprenom=gsub("/","",nomprenom)) %>% 
  filter(semdec %in% c("10","11","12","13","14","15","16","17","18","19")) %>% 
  select(nomprenom,sexe,agedec,datenaiss,anaiss,moisnaiss,jnaiss,lieunaiss,depnaiss,commnaiss,paysnaiss,datedeces,adec,moisdec,jdec,semdec,lieudeces,epcideces,depdec,actedeces) %>% 
  ungroup()
deces$paysnaiss[is.na(deces$paysnaiss)] <- "FRANCE"
deces <- deces %>% 
  mutate(naiss=ifelse(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE"),"N?s en France","N?s ? l'?tranger"),
         semdec=case_when(semdec=="10"~"Semaine du 2 mars (10)", 
                          semdec=="11"~"Semaine du 9 mars (11)",
                          semdec=="12"~"Semaine du 16 mars (12)",
                          semdec=="13"~"Semaine du 23 mars (13)",
                          semdec=="14"~"Semaine du 30 mars (14)",
                          semdec=="15"~"Semaine du 6 avril (15)",
                          semdec=="16"~"Semaine du 13 avril (16)",
                          semdec=="17"~"Semaine du 20 avril (17)",
                          semdec=="18"~"Semaine du 27 avril (18)",
                          semdec=="19"~"Semaine du 4 mai (19)")) 

ages <- sort(as.numeric(unique(deces$agedec)))
x <- seq(from = 0, to = 100, by = 5)
list_ages <- as.data.frame(ages)

#### ann?e par ann?e ####

deces15f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15f=n()) #le nom de la variable est ? changer
deces15f <- merge(deces15f,list_ages,all=T,by.x="agedec",by.y="ages")
deces15f[is.na(deces15f)] <- 0

deces15e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15e=n()) #le nom de la variable est ? changer
deces15e <- merge(deces15e,list_ages,all=T,by.x="agedec",by.y="ages")
deces15e[is.na(deces15e)] <- 0


esmooth15 <- Mort1Dsmooth(deces15e$d15e, offsets$NETR2015)

deces16f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16f=n()) #le nom de la variable est ? changer
deces16f <- merge(deces16f,list_ages,all=T,by.x="agedec",by.y="ages")
deces16f[is.na(deces16f)] <- 0

deces16e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16e=n()) #le nom de la variable est ? changer
deces16e <- merge(deces16e,list_ages,all=T,by.x="agedec",by.y="ages")
deces16e[is.na(deces16e)] <- 0


esmooth16 <- Mort1Dsmooth(deces16e$d16e, offsets$NETR2016)

deces17f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17f=n()) #le nom de la variable est ? changer
deces17f <- merge(deces17f,list_ages,all=T,by.x="agedec",by.y="ages")
deces17f[is.na(deces17f)] <- 0

deces17e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17e=n()) #le nom de la variable est ? changer
deces17e <- merge(deces17e,list_ages,all=T,by.x="agedec",by.y="ages")
deces17e[is.na(deces17e)] <- 0


esmooth17 <- Mort1Dsmooth(deces17e$d17e, offsets$NETR2017)

deces18f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18f=n()) #le nom de la variable est ? changer
deces18f <- merge(deces18f,list_ages,all=T,by.x="agedec",by.y="ages")
deces18f[is.na(deces18f)] <- 0

deces18e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18e=n()) #le nom de la variable est ? changer
deces18e <- merge(deces18e,list_ages,all=T,by.x="agedec",by.y="ages")
deces18e[is.na(deces18e)] <- 0


esmooth18 <- Mort1Dsmooth(deces18e$d18e, offsets$NETR2018)

deces19f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19f=n()) #le nom de la variable est ? changer
deces19f <- merge(deces19f,list_ages,all=T,by.x="agedec",by.y="ages")
deces19f[is.na(deces19f)] <- 0

deces19e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19e=n()) #le nom de la variable est ? changer
deces19e <- merge(deces19e,list_ages,all=T,by.x="agedec",by.y="ages")
deces19e[is.na(deces19e)] <- 0


esmooth19 <- Mort1Dsmooth(deces19e$d19e, offsets$NETR2019)

deces20f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20f=n()) #le nom de la variable est ? changer
deces20f <- merge(deces20f,list_ages,all=T,by.x="agedec",by.y="ages")
deces20f[is.na(deces20f)] <- 0

deces20e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20e=n()) #le nom de la variable est ? changer
deces20e <- merge(deces20e,list_ages,all=T,by.x="agedec",by.y="ages")
deces20e[is.na(deces20e)] <- 0



#### Smooth? ####
tiff("Taux de mortalit? ?chelle log FR.tiff",width=40,height=30,units = "cm",res=780)

par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015
e15ul.num <- exp(ul)*offsets$NETR2015
e15logrates <- log(deces15e$d15e/offsets$NETR2015)

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e15smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015
f15ul.num <- exp(ul)*offsets$NFRA2015
f15logrates <- log(deces15f$d15f/offsets$NFRA2015)

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016
e16ul.num <- exp(ul)*offsets$NETR2016
e16logrates <- log(deces16e$d16e/offsets$NETR2016)

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e16smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016
f16ul.num <- exp(ul)*offsets$NFRA2016
f16logrates <- log(deces16f$d16f/offsets$NFRA2016)

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017
e17ul.num <- exp(ul)*offsets$NETR2017
e17logrates <- log(deces17e$d17e/offsets$NETR2017)

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e17smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017
f17ul.num <- exp(ul)*offsets$NFRA2017
f17logrates <- log(deces17f$d17f/offsets$NFRA2017)

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018
e18ul.num <- exp(ul)*offsets$NETR2018
e18logrates <- log(deces18e$d18e/offsets$NETR2018)

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e18smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018
f18ul.num <- exp(ul)*offsets$NFRA2018
f18logrates <- log(deces18f$d18f/offsets$NFRA2018)

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019
e19ul.num <- exp(ul)*offsets$NETR2019
e19logrates <- log(deces19e$d19e/offsets$NETR2019)

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e19smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019
f19ul.num <- exp(ul)*offsets$NFRA2019
f19logrates <- log(deces19f$d19f/offsets$NFRA2019)

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020
e20ul.num <- exp(ul)*offsets$NETR2020
e20logrates <- log(deces20e$d20e/offsets$NETR2020)

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020
f20ul.num <- exp(ul)*offsets$NFRA2020
f20logrates <- log(deces20f$d20f/offsets$NFRA2020)

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

tiff("Smooth2020FR.tiff",width=30,height=20,units = "cm",res=780)
plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "France",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020
f20ul.num <- exp(ul)*offsets$NFRA2020
f20logrates <- log(deces20f$d20f/offsets$NFRA2020)

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()

#les plots mais zoom?s aux grand ?ges####
tiff("Taux de mortalit? origine 50 ans et + GE.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015
e15ul.num <- exp(ul)*offsets$NETR2015
e15logrates <- deces15e$d15e/offsets$NETR2015*10000

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015
f15ul.num <- exp(ul)*offsets$NFRA2015
f15logrates <- deces15f$d15f/offsets$NFRA2015*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016
e16ul.num <- exp(ul)*offsets$NETR2016
e16logrates <- deces16e$d16e/offsets$NETR2016*10000

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016
f16ul.num <- exp(ul)*offsets$NFRA2016
f16logrates <- deces16f$d16f/offsets$NFRA2016*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017
e17ul.num <- exp(ul)*offsets$NETR2017
e17logrates <- deces17e$d17e/offsets$NETR2017*10000

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017
f17ul.num <- exp(ul)*offsets$NFRA2017
f17logrates <- deces17f$d17f/offsets$NFRA2017*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018
e18ul.num <- exp(ul)*offsets$NETR2018
e18logrates <- deces18e$d18e/offsets$NETR2018*10000

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018
f18ul.num <- exp(ul)*offsets$NFRA2018
f18logrates <- deces18f$d18f/offsets$NFRA2018*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019
e19ul.num <- exp(ul)*offsets$NETR2019
e19logrates <- deces19e$d19e/offsets$NETR2019*10000

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019
f19ul.num <- exp(ul)*offsets$NFRA2019
f19logrates <- deces19f$d19f/offsets$NFRA2019*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020
e20ul.num <- exp(ul)*offsets$NETR2020
e20logrates <- deces20e$d20e/offsets$NETR2020*10000

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020
f20ul.num <- exp(ul)*offsets$NFRA2020
f20logrates <- deces20f$d20f/offsets$NFRA2020*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
#les plots mais zoom?s aux grand ?ges avec ?chelle log####
tiff("Taux de mortalit? origine 50 ans et + LOG GE.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015
e15ul.num <- exp(ul)*offsets$NETR2015
e15logrates <- deces15e$d15e/offsets$NETR2015*10000

plot(ages, e15logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015
f15ul.num <- exp(ul)*offsets$NFRA2015
f15logrates <- deces15f$d15f/offsets$NFRA2015*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016
e16ul.num <- exp(ul)*offsets$NETR2016
e16logrates <- deces16e$d16e/offsets$NETR2016*10000

plot(ages, e16logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016
f16ul.num <- exp(ul)*offsets$NFRA2016
f16logrates <- deces16f$d16f/offsets$NFRA2016*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017
e17ul.num <- exp(ul)*offsets$NETR2017
e17logrates <- deces17e$d17e/offsets$NETR2017*10000

plot(ages, e17logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017
f17ul.num <- exp(ul)*offsets$NFRA2017
f17logrates <- deces17f$d17f/offsets$NFRA2017*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018
e18ul.num <- exp(ul)*offsets$NETR2018
e18logrates <- deces18e$d18e/offsets$NETR2018*10000

plot(ages, e18logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018
f18ul.num <- exp(ul)*offsets$NFRA2018
f18logrates <- deces18f$d18f/offsets$NFRA2018*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019
e19ul.num <- exp(ul)*offsets$NETR2019
e19logrates <- deces19e$d19e/offsets$NETR2019*10000

plot(ages, e19logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019
f19ul.num <- exp(ul)*offsets$NFRA2019
f19logrates <- deces19f$d19f/offsets$NFRA2019*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020
e20ul.num <- exp(ul)*offsets$NETR2020
e20logrates <- deces20e$d20e/offsets$NETR2020*10000

plot(ages, e20logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020
f20ul.num <- exp(ul)*offsets$NFRA2020
f20logrates <- deces20f$d20f/offsets$NFRA2020*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
#surmortalit? commune ####


dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(epcideces) %>% 
  summarise(dec20=n())

decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(epcideces) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(epcideces)

rm(decmoy,dec20)
cartesurmort <- merge(EPCI,indicesem,by.x="SIREN",by.y= "epcideces")
cartesurmort <- cartesurmort %>% 
  select(SIREN,dec20,decmoy,indice,geometry) %>% 
  arrange(SIREN)

plot(st_geometry(cartesurmort$geometry),col="#D5D5D5")

layoutLayer(bg="white",col=NULL,title = NULL)
choroLayer(cartesurmort,  var = "indice",method="fisher-jenks",nclass=6,border = "#595959",legend.title.txt = "Indice de surmotalit?", 
           legend.pos = "bottomleft", lwd=1.5, legend.values.rnd = 2,colNA="#D5D5D5",add=T,legend.nodata = NA ,legend.title.cex = 1.2,legend.values.cex = 1.2 ,legend.frame = T) 


cartesurmort$indice_std <- scale(cartesurmort$indice)
map_nb<-poly2nb(cartesurmort$geometry)
nbcheck <- neighborsDataFrame(map_nb)
nbcheck2 <- as.data.frame(sort(unique(as.numeric(nbcheck$id))))
map_nb_w<-nb2listw(map_nb)

cartesurmort$lag<-lag.listw(map_nb_w,cartesurmort$indice)
cartesurmort$lag_std<-lag.listw(map_nb_w,cartesurmort$indice_std)

cor.test(cartesurmort$lag,cartesurmort$lag_std)
locm<-localmoran(cartesurmort$indice,map_nb_w,alternative = "two.sided")
tab2<-as.data.frame(locm)
cartesurmorttest <- as.data.frame(cartesurmort) %>% 
  select(-geometry)

tabres<-cbind(cartesurmorttest,tab2)
tabres <- merge(EPCI,tabres,by.x="SIREN",by.y= "SIREN")
q1<-as.factor(tabres$indice_std>0)
levels(q1)<-c("Low","High")
q2<-as.factor(tabres$lag_std>0)
levels(q2)<-c("Low","High")
MapMoran<-paste0(q1,"-",q2)
MapMoran[abs(as.numeric(tabres$`Pr(z != 0)`))>0.05]<-"Non Sign."

labels=c("High-High","Low-Low","High-Low","Low-High","Non Sign.")
cartesurmort$Moran_type<-factor(MapMoran,levels=labels)
cartesurmort$Moran_color<-cartesurmort$Moran_type
colors=c("red","blue","lightpink","skyblue2","white")
levels(cartesurmort$Moran_color)<-colors
cartesurmort$Moran_color<-as.character(cartesurmort$Moran_color)

colors<-c("red","blue","lightpink","skyblue2","white")
tiff(paste0("moran1",".tiff"), units="in", width=10, height=10, res=1200)
plot(cartesurmort$geometry,col=cartesurmort$Moran_color)
  legend("topright",legend=labels, fill=colors,bty="n")
plot(DEP$geometry,col="transparent",bg="transparent",add=T,lwd=2,border="green")
dev.off()
####surmortalit? semaine####

dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,semdec)
write.csv2(indicesem,"semaineFR.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par semaine FR",".tiff"), units="in", width=28, height=10, res=780)

p <- ggplot(indicesem,aes(x=semdec, y=indice,linetype=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="R?gions",values=c("grey40","blue","red","#8AC926","#c42a19"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  ylab("Indice de surmortalit?")+
  scale_x_discrete(limits=c("Semaine du 2 mars (10)", 
                            "Semaine du 9 mars (11)",
                            "Semaine du 16 mars (12)",
                            "Semaine du 23 mars (13)",
                            "Semaine du 30 mars (14)",
                            "Semaine du 6 avril (15)",
                            "Semaine du 13 avril (16)",
                            "Semaine du 20 avril (17)",
                            "Semaine du 27 avril (18)",
                            "Semaine du 4 mai (19)"))+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(
    color=F,
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(p)

dev.off()

#surmortalit? ?ge####


dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,agedec)
write.csv2(indicesem,"ageFR.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par ?ge FR",".tiff"), units="in", width=28, height=10, res=780)

p <- ggplot(indicesem,aes(x=agedec, y=indice,linetype=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("grey40","blue","red","#8AC926","#c42a19"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  ylab("Indice de surmortalit?")+
  ylim(0,5)+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(
    color=F,
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(p)

dev.off()

#surmortalit? ?ge####


dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,agedec)
write.csv2(indicesem,"ageFR.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par ?ge FR",".tiff"), units="in", width=28, height=10, res=780)

p <- ggplot(indicesem,aes(x=agedec, y=indice,linetype=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("grey40","blue","red","#8AC926","#c42a19"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  ylab("Indice de surmortalit?")+
  ylim(0,5)+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(
    color=F,
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(p)

dev.off()

#Reprise de l'ensemble pour le Grand Est####
deces <- deces_sauv
offsets <- read_excel("C:/Users/irwin/OneDrive/Donn?es stats/deces/offsets.xlsx",sheet = 2)

deces <- deces %>% 
  mutate(anaiss=substr(datenaiss,1,4),
         adec=substr(datedeces,1,4),
         agedec=(as.numeric(adec)-as.numeric(anaiss)),
         agedec=ifelse(as.numeric(agedec)>=100,100,agedec),
         depdec=substr(lieudeces,1,2),
         depnaiss=substr(lieunaiss,1,2),
         moisnaiss=substr(datenaiss,5,6),
         moisdec=substr(datedeces,5,6),
         jdec=substr(datedeces,7,8),
         filtre=paste0(substr(moisdec,2,3),substr(jdec,1,3)),
         jnaiss=substr(datenaiss,7,8),
         datedeces=as.Date.character(datedeces,"%Y%m%d"),
         semdec=isoweek(datedeces),
         sexe=case_when(as.character(sexe)=="1"~"homme",
                        as.character(sexe)=="2"~"femme"),
         agedec=case_when(agedec>=100~100,
                          agedec<100~agedec),
         nomprenom=gsub("/","",nomprenom)) %>% 
  filter(semdec %in% c("10","11","12","13","14","15","16","17","18","19")&depdec %in% c("08","10","51","52","54","55","57","68","67","88")) %>% 
  select(nomprenom,sexe,agedec,datenaiss,anaiss,moisnaiss,jnaiss,lieunaiss,depnaiss,commnaiss,paysnaiss,datedeces,adec,moisdec,jdec,semdec,lieudeces,depdec,actedeces) %>% 
  ungroup()
deces$paysnaiss[is.na(deces$paysnaiss)] <- "FRANCE"
deces <- deces %>% 
  mutate(naiss=ifelse(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE"),"N?s en France","N?s ? l'?tranger"),
         semdec=case_when(semdec=="10"~"Semaine du 2 mars (10)", 
                          semdec=="11"~"Semaine du 9 mars (11)",
                          semdec=="12"~"Semaine du 16 mars (12)",
                          semdec=="13"~"Semaine du 23 mars (13)",
                          semdec=="14"~"Semaine du 30 mars (14)",
                          semdec=="15"~"Semaine du 6 avril (15)",
                          semdec=="16"~"Semaine du 13 avril (16)",
                          semdec=="17"~"Semaine du 20 avril (17)",
                          semdec=="18"~"Semaine du 27 avril (18)",
                          semdec=="19"~"Semaine du 4 mai (19)")) 
ages <- sort(as.numeric(unique(deces$agedec)))
x <- seq(from = 0, to = 100, by = 5)
list_ages <- as.data.frame(ages)

#### ann?e par ann?e ####

deces15f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15f=n()) #le nom de la variable est ? changer
deces15f <- merge(deces15f,list_ages,all=T,by.x="agedec",by.y="ages")
deces15f[is.na(deces15f)] <- 0

deces15e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15e=n()) #le nom de la variable est ? changer
deces15e <- merge(deces15e,list_ages,all=T,by.x="agedec",by.y="ages")
deces15e[is.na(deces15e)] <- 0


esmooth15 <- Mort1Dsmooth(deces15e$d15e, offsets$NETR2015GE)

deces16f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16f=n()) #le nom de la variable est ? changer
deces16f <- merge(deces16f,list_ages,all=T,by.x="agedec",by.y="ages")
deces16f[is.na(deces16f)] <- 0

deces16e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16e=n()) #le nom de la variable est ? changer
deces16e <- merge(deces16e,list_ages,all=T,by.x="agedec",by.y="ages")
deces16e[is.na(deces16e)] <- 0


esmooth16 <- Mort1Dsmooth(deces16e$d16e, offsets$NETR2016GE)

deces17f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17f=n()) #le nom de la variable est ? changer
deces17f <- merge(deces17f,list_ages,all=T,by.x="agedec",by.y="ages")
deces17f[is.na(deces17f)] <- 0

deces17e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17e=n()) #le nom de la variable est ? changer
deces17e <- merge(deces17e,list_ages,all=T,by.x="agedec",by.y="ages")
deces17e[is.na(deces17e)] <- 0


esmooth17 <- Mort1Dsmooth(deces17e$d17e, offsets$NETR2017GE)

deces18f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18f=n()) #le nom de la variable est ? changer
deces18f <- merge(deces18f,list_ages,all=T,by.x="agedec",by.y="ages")
deces18f[is.na(deces18f)] <- 0

deces18e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18e=n()) #le nom de la variable est ? changer
deces18e <- merge(deces18e,list_ages,all=T,by.x="agedec",by.y="ages")
deces18e[is.na(deces18e)] <- 0


esmooth18 <- Mort1Dsmooth(deces18e$d18e, offsets$NETR2018GE)

deces19f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19f=n()) #le nom de la variable est ? changer
deces19f <- merge(deces19f,list_ages,all=T,by.x="agedec",by.y="ages")
deces19f[is.na(deces19f)] <- 0

deces19e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19e=n()) #le nom de la variable est ? changer
deces19e <- merge(deces19e,list_ages,all=T,by.x="agedec",by.y="ages")
deces19e[is.na(deces19e)] <- 0


esmooth19 <- Mort1Dsmooth(deces19e$d19e, offsets$NETR2019GE)

deces20f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20f=n()) #le nom de la variable est ? changer
deces20f <- merge(deces20f,list_ages,all=T,by.x="agedec",by.y="ages")
deces20f[is.na(deces20f)] <- 0

deces20e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20e=n()) #le nom de la variable est ? changer
deces20e <- merge(deces20e,list_ages,all=T,by.x="agedec",by.y="ages")
deces20e[is.na(deces20e)] <- 0

#### Smooth? ####
tiff("Taux de mortalit? ?chelle log GE.tiff",width=40,height=30,units = "cm",res=780)

par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015GE),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015GE
e15ul.num <- exp(ul)*offsets$NETR2015GE
e15logrates <- log(deces15e$d15e/offsets$NETR2015GE)

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e15smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015GE),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015GE
f15ul.num <- exp(ul)*offsets$NFRA2015GE
f15logrates <- log(deces15f$d15f/offsets$NFRA2015GE)

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016GE),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016GE
e16ul.num <- exp(ul)*offsets$NETR2016GE
e16logrates <- log(deces16e$d16e/offsets$NETR2016GE)

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e16smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016GE),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016GE
f16ul.num <- exp(ul)*offsets$NFRA2016GE
f16logrates <- log(deces16f$d16f/offsets$NFRA2016GE)

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017GE),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017GE
e17ul.num <- exp(ul)*offsets$NETR2017GE
e17logrates <- log(deces17e$d17e/offsets$NETR2017GE)

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e17smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017GE),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017GE
f17ul.num <- exp(ul)*offsets$NFRA2017GE
f17logrates <- log(deces17f$d17f/offsets$NFRA2017GE)

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018GE),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018GE
e18ul.num <- exp(ul)*offsets$NETR2018GE
e18logrates <- log(deces18e$d18e/offsets$NETR2018GE)

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e18smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018GE),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018GE
f18ul.num <- exp(ul)*offsets$NFRA2018GE
f18logrates <- log(deces18f$d18f/offsets$NFRA2018GE)

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019GE),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019GE
e19ul.num <- exp(ul)*offsets$NETR2019GE
e19logrates <- log(deces19e$d19e/offsets$NETR2019GE)

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e19smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019GE),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019GE
f19ul.num <- exp(ul)*offsets$NFRA2019GE
f19logrates <- log(deces19f$d19f/offsets$NFRA2019GE)

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020GE),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020GE
e20ul.num <- exp(ul)*offsets$NETR2020GE
e20logrates <- log(deces20e$d20e/offsets$NETR2020GE)

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020GE),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020GE
f20ul.num <- exp(ul)*offsets$NFRA2020GE
f20logrates <- log(deces20f$d20f/offsets$NFRA2020GE)

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

#### ?chelle log propre ####
tiff("Smooth2020GE.tiff",width=30,height=20,units = "cm",res=780)

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020GE),method=3,lambda=15)
e20smo <- exp(e20smooth$logmortality)*10000
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ll <- exp(ll)*10000
ul <- pre$fit + 1.966*pre$se.fit
ul <- exp(ul)*10000
e20ll.num <- exp(ll)*offsets$NETR2020GE
e20ul.num <- exp(ul)*offsets$NETR2020GE
e20logrates <- deces20e$d20e/offsets$NETR2020GE*10000

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "Grand Est",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,xlab = "?ge",ylab = "Taux de mortalit?",log = "y",ylim = c(0.1,1200))
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020GE),method=3,lambda=15)
f20smo <- exp(f20smooth$logmortality)*10000
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ll <- exp(ll)*10000
ul <- pre$fit + 1.966*pre$se.fit
ul <- exp(ul)*10000
f20ll.num <- exp(ll)*offsets$NFRA2020
f20ul.num <- exp(ul)*offsets$NFRA2020
f20logrates <- deces20f$d20f/offsets$NFRA2020GE*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()

#les plots mais zoom?s aux grand ?ges####
tiff("Taux de mortalit? origine 50 ans et + GE.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015GE),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015GE
e15ul.num <- exp(ul)*offsets$NETR2015GE
e15logrates <- deces15e$d15e/offsets$NETR2015GE*10000

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015GE),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015GE
f15ul.num <- exp(ul)*offsets$NFRA2015GE
f15logrates <- deces15f$d15f/offsets$NFRA2015GE*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016GE),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016GE
e16ul.num <- exp(ul)*offsets$NETR2016GE
e16logrates <- deces16e$d16e/offsets$NETR2016GE*10000

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016GE),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016GE
f16ul.num <- exp(ul)*offsets$NFRA2016GE
f16logrates <- deces16f$d16f/offsets$NFRA2016GE*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017GE),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017GE
e17ul.num <- exp(ul)*offsets$NETR2017GE
e17logrates <- deces17e$d17e/offsets$NETR2017GE*10000

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017GE),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017GE
f17ul.num <- exp(ul)*offsets$NFRA2017GE
f17logrates <- deces17f$d17f/offsets$NFRA2017GE*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018GE),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018GE
e18ul.num <- exp(ul)*offsets$NETR2018GE
e18logrates <- deces18e$d18e/offsets$NETR2018GE*10000

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018GE),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018GE
f18ul.num <- exp(ul)*offsets$NFRA2018GE
f18logrates <- deces18f$d18f/offsets$NFRA2018GE*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019GE),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019GE
e19ul.num <- exp(ul)*offsets$NETR2019GE
e19logrates <- deces19e$d19e/offsets$NETR2019GE*10000

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019GE),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019GE
f19ul.num <- exp(ul)*offsets$NFRA2019GE
f19logrates <- deces19f$d19f/offsets$NFRA2019GE*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020GE),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020GE
e20ul.num <- exp(ul)*offsets$NETR2020GE
e20logrates <- deces20e$d20e/offsets$NETR2020GE*10000

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020GE),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020GE
f20ul.num <- exp(ul)*offsets$NFRA2020GE
f20logrates <- deces20f$d20f/offsets$NFRA2020GE*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
#les plots mais zoom?s aux grand ?ges avec ?chelle log####
tiff("Taux de mortalit? origine 50 ans et + LOG GE.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015GE),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015GE
e15ul.num <- exp(ul)*offsets$NETR2015GE
e15logrates <- deces15e$d15e/offsets$NETR2015GE*10000

plot(ages, e15logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015GE),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015GE
f15ul.num <- exp(ul)*offsets$NFRA2015GE
f15logrates <- deces15f$d15f/offsets$NFRA2015GE*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=75)
abline(lwd=1.5,col="#036b50",v=68)
abline(lwd=1.5,col="#782438",v=58-5)

abline(lwd=1.5,col="#782438",v=78-5)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016GE),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016GE
e16ul.num <- exp(ul)*offsets$NETR2016GE
e16logrates <- deces16e$d16e/offsets$NETR2016GE*10000

plot(ages, e16logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016GE),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016GE
f16ul.num <- exp(ul)*offsets$NFRA2016GE
f16logrates <- deces16f$d16f/offsets$NFRA2016GE*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=76)
abline(lwd=1.5,col="#036b50",v=69)
abline(lwd=1.5,col="#782438",v=58-4)

abline(lwd=1.5,col="#782438",v=78-4)
p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017GE),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017GE
e17ul.num <- exp(ul)*offsets$NETR2017GE
e17logrates <- deces17e$d17e/offsets$NETR2017GE*10000

plot(ages, e17logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017GE),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017GE
f17ul.num <- exp(ul)*offsets$NFRA2017GE
f17logrates <- deces17f$d17f/offsets$NFRA2017GE*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=77)
abline(lwd=1.5,col="#036b50",v=70)
abline(lwd=1.5,col="#782438",v=58-3)

abline(lwd=1.5,col="#782438",v=78-3)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018GE),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018GE
e18ul.num <- exp(ul)*offsets$NETR2018GE
e18logrates <- deces18e$d18e/offsets$NETR2018GE*10000

plot(ages, e18logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018GE),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018GE
f18ul.num <- exp(ul)*offsets$NFRA2018GE
f18logrates <- deces18f$d18f/offsets$NFRA2018GE*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=78)
abline(lwd=1.5,col="#036b50",v=71)
abline(lwd=1.5,col="#782438",v=58-2)

abline(lwd=1.5,col="#782438",v=78-2)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019GE),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019GE
e19ul.num <- exp(ul)*offsets$NETR2019GE
e19logrates <- deces19e$d19e/offsets$NETR2019GE*10000

plot(ages, e19logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019GE),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019GE
f19ul.num <- exp(ul)*offsets$NFRA2019GE
f19logrates <- deces19f$d19f/offsets$NFRA2019GE*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=79)
abline(lwd=1.5,col="#036b50",v=72)
abline(lwd=1.5,col="#782438",v=58-1)

abline(lwd=1.5,col="#782438",v=78-1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020GE),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020GE
e20ul.num <- exp(ul)*offsets$NETR2020GE
e20logrates <- deces20e$d20e/offsets$NETR2020GE*10000

plot(ages, e20logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020GE),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020GE
f20ul.num <- exp(ul)*offsets$NFRA2020GE
f20logrates <- deces20f$d20f/offsets$NFRA2020GE*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=80)
abline(lwd=1.5,col="#036b50",v=73)
abline(lwd=1.5,col="#782438",v=58)

abline(lwd=1.5,col="#782438",v=78)
p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

#surmortalit? semaine ####

dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,semdec)
write.csv2(indicesem,"semaineGE.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par semaine GE",".tiff"), units="in", width=28, height=10, res=780)

p <- ggplot(indicesem,aes(x=semdec, y=indice,linetype=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="R?gions",values=c("grey40","blue","red","#8AC926","#c42a19"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  scale_x_discrete(limits=c("Semaine du 2 mars (10)", 
                            "Semaine du 9 mars (11)",
                            "Semaine du 16 mars (12)",
                            "Semaine du 23 mars (13)",
                            "Semaine du 30 mars (14)",
                            "Semaine du 6 avril (15)",
                            "Semaine du 13 avril (16)",
                            "Semaine du 20 avril (17)",
                            "Semaine du 27 avril (18)",
                            "Semaine du 4 mai (19)"))+
  ylab("Indice de surmortalit?")+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(
    color=F,
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(p)

dev.off()

#surmortalit? ?ge####


dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,agedec)
write.csv2(indicesem,"ageGE.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par ?ge GE",".tiff"), units="in", width=28, height=10, res=780)

pagege <- ggplot(indicesem,aes(x=agedec, y=indice,color=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("#06D6A0","#ef476f"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  ggtitle("Grand Est")+
  ylab("Indice de surmortalit?")+
  ylim(0,4)+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(pagege)

dev.off()
####surmortalit? pays####
dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(paysnaiss) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(paysnaiss) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(paysnaiss)
write.csv2(indicesem,"paysGE.csv")
rm(decmoy,dec20)


#Reprise de l'ensemble pour l'Ile-de-France####
deces <- deces_sauv
offsets <- read_excel("C:/Users/irwin/OneDrive/Donn?es stats/deces/offsets.xlsx",sheet = 2)

deces <- deces %>% 
  mutate(anaiss=substr(datenaiss,1,4),
         adec=substr(datedeces,1,4),
         agedec=(as.numeric(adec)-as.numeric(anaiss)),
         agedec=ifelse(as.numeric(agedec)>=100,100,agedec),
         depdec=substr(lieudeces,1,2),
         depnaiss=substr(lieunaiss,1,2),
         moisnaiss=substr(datenaiss,5,6),
         moisdec=substr(datedeces,5,6),
         jdec=substr(datedeces,7,8),
         filtre=paste0(substr(moisdec,2,3),substr(jdec,1,3)),
         jnaiss=substr(datenaiss,7,8),
         datedeces=as.Date.character(datedeces,"%Y%m%d"),
         semdec=isoweek(datedeces),
         sexe=case_when(as.character(sexe)=="1"~"homme",
                        as.character(sexe)=="2"~"femme"),
         agedec=case_when(agedec>=100~100,
                          agedec<100~agedec),
         nomprenom=gsub("/","",nomprenom)) %>% 
  filter(semdec %in% c("10","11","12","13","14","15","16","17","18","19")&depdec %in% c("91",
                                                                                        "92",
                                                                                        "75",
                                                                                        "77",
                                                                                        "93",
                                                                                        "95",
                                                                                        "94",
                                                                                        "78")) %>% 
  select(nomprenom,sexe,agedec,datenaiss,anaiss,moisnaiss,jnaiss,lieunaiss,depnaiss,commnaiss,paysnaiss,datedeces,adec,moisdec,jdec,semdec,lieudeces,depdec,actedeces) %>% 
  ungroup()
deces$paysnaiss[is.na(deces$paysnaiss)] <- "FRANCE"
deces <- deces %>% 
  mutate(naiss=ifelse(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE"),"N?s en France","N?s ? l'?tranger"),
         semdec=case_when(semdec=="10"~"Semaine du 2 mars (10)", 
                          semdec=="11"~"Semaine du 9 mars (11)",
                          semdec=="12"~"Semaine du 16 mars (12)",
                          semdec=="13"~"Semaine du 23 mars (13)",
                          semdec=="14"~"Semaine du 30 mars (14)",
                          semdec=="15"~"Semaine du 6 avril (15)",
                          semdec=="16"~"Semaine du 13 avril (16)",
                          semdec=="17"~"Semaine du 20 avril (17)",
                          semdec=="18"~"Semaine du 27 avril (18)",
                          semdec=="19"~"Semaine du 4 mai (19)")) 
ages <- sort(as.numeric(unique(deces$agedec)))
x <- seq(from = 0, to = 100, by = 5)
list_ages <- as.data.frame(ages)

#### ann?e par ann?e ####

deces15f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15f=n()) #le nom de la variable est ? changer
deces15f <- merge(deces15f,list_ages,all=T,by.x="agedec",by.y="ages")
deces15f[is.na(deces15f)] <- 0

deces15e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15e=n()) #le nom de la variable est ? changer
deces15e <- merge(deces15e,list_ages,all=T,by.x="agedec",by.y="ages")
deces15e[is.na(deces15e)] <- 0


esmooth15 <- Mort1Dsmooth(deces15e$d15e, offsets$NETR2015IDF)

deces16f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16f=n()) #le nom de la variable est ? changer
deces16f <- merge(deces16f,list_ages,all=T,by.x="agedec",by.y="ages")
deces16f[is.na(deces16f)] <- 0

deces16e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16e=n()) #le nom de la variable est ? changer
deces16e <- merge(deces16e,list_ages,all=T,by.x="agedec",by.y="ages")
deces16e[is.na(deces16e)] <- 0


esmooth16 <- Mort1Dsmooth(deces16e$d16e, offsets$NETR2016IDF)

deces17f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17f=n()) #le nom de la variable est ? changer
deces17f <- merge(deces17f,list_ages,all=T,by.x="agedec",by.y="ages")
deces17f[is.na(deces17f)] <- 0

deces17e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17e=n()) #le nom de la variable est ? changer
deces17e <- merge(deces17e,list_ages,all=T,by.x="agedec",by.y="ages")
deces17e[is.na(deces17e)] <- 0


esmooth17 <- Mort1Dsmooth(deces17e$d17e, offsets$NETR2017IDF)

deces18f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18f=n()) #le nom de la variable est ? changer
deces18f <- merge(deces18f,list_ages,all=T,by.x="agedec",by.y="ages")
deces18f[is.na(deces18f)] <- 0

deces18e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18e=n()) #le nom de la variable est ? changer
deces18e <- merge(deces18e,list_ages,all=T,by.x="agedec",by.y="ages")
deces18e[is.na(deces18e)] <- 0


esmooth18 <- Mort1Dsmooth(deces18e$d18e, offsets$NETR2018IDF)

deces19f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19f=n()) #le nom de la variable est ? changer
deces19f <- merge(deces19f,list_ages,all=T,by.x="agedec",by.y="ages")
deces19f[is.na(deces19f)] <- 0

deces19e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19e=n()) #le nom de la variable est ? changer
deces19e <- merge(deces19e,list_ages,all=T,by.x="agedec",by.y="ages")
deces19e[is.na(deces19e)] <- 0


esmooth19 <- Mort1Dsmooth(deces19e$d19e, offsets$NETR2019IDF)

deces20f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20f=n()) #le nom de la variable est ? changer
deces20f <- merge(deces20f,list_ages,all=T,by.x="agedec",by.y="ages")
deces20f[is.na(deces20f)] <- 0

deces20e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20e=n()) #le nom de la variable est ? changer
deces20e <- merge(deces20e,list_ages,all=T,by.x="agedec",by.y="ages")
deces20e[is.na(deces20e)] <- 0

#### Smooth? ####
tiff("Taux de mortalit? ?chelle log IDF.tiff",width=40,height=30,units = "cm",res=780)

par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015IDF),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015IDF
e15ul.num <- exp(ul)*offsets$NETR2015IDF
e15logrates <- log(deces15e$d15e/offsets$NETR2015IDF)

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e15smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015IDF),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015IDF
f15ul.num <- exp(ul)*offsets$NFRA2015IDF
f15logrates <- log(deces15f$d15f/offsets$NFRA2015IDF)

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016IDF),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016IDF
e16ul.num <- exp(ul)*offsets$NETR2016IDF
e16logrates <- log(deces16e$d16e/offsets$NETR2016IDF)

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e16smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016IDF),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016IDF
f16ul.num <- exp(ul)*offsets$NFRA2016IDF
f16logrates <- log(deces16f$d16f/offsets$NFRA2016IDF)

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017IDF),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017IDF
e17ul.num <- exp(ul)*offsets$NETR2017IDF
e17logrates <- log(deces17e$d17e/offsets$NETR2017IDF)

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e17smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017IDF),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017IDF
f17ul.num <- exp(ul)*offsets$NFRA2017IDF
f17logrates <- log(deces17f$d17f/offsets$NFRA2017IDF)

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018IDF),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018IDF
e18ul.num <- exp(ul)*offsets$NETR2018IDF
e18logrates <- log(deces18e$d18e/offsets$NETR2018IDF)

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e18smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018IDF),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018IDF
f18ul.num <- exp(ul)*offsets$NFRA2018IDF
f18logrates <- log(deces18f$d18f/offsets$NFRA2018IDF)

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019IDF),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019IDF
e19ul.num <- exp(ul)*offsets$NETR2019IDF
e19logrates <- log(deces19e$d19e/offsets$NETR2019IDF)

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e19smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019IDF),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019IDF
f19ul.num <- exp(ul)*offsets$NFRA2019IDF
f19logrates <- log(deces19f$d19f/offsets$NFRA2019IDF)

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020IDF),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020IDF
e20ul.num <- exp(ul)*offsets$NETR2020IDF
e20logrates <- log(deces20e$d20e/offsets$NETR2020IDF)

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020IDF),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020IDF
f20ul.num <- exp(ul)*offsets$NFRA2020IDF
f20logrates <- log(deces20f$d20f/offsets$NFRA2020IDF)

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
tiff("Smooth2020IDF.tiff",width=30,height=20,units = "cm",res=780)

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020IDF),method=3,lambda=15)
e20smo <- exp(e20smooth$logmortality)*10000
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ll <- exp(ll)*10000
ul <- pre$fit + 1.966*pre$se.fit
ul <- exp(ul)*10000
e20ll.num <- exp(ll)*offsets$NETR2020IDF
e20ul.num <- exp(ul)*offsets$NETR2020IDF
e20logrates <- deces20e$d20e/offsets$NETR2020IDF*10000

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "?le-de-France",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,xlab = "?ge",ylab = "Taux de mortalit?",log = "y",ylim = c(0.1,1200))
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020IDF),method=3,lambda=15)
f20smo <- exp(f20smooth$logmortality)*10000
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ll <- exp(ll)*10000
ul <- pre$fit + 1.966*pre$se.fit
ul <- exp(ul)*10000
f20ll.num <- exp(ll)*offsets$NFRA2020
f20ul.num <- exp(ul)*offsets$NFRA2020
f20logrates <- deces20f$d20f/offsets$NFRA2020IDF*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()

#les plots mais zoom?s aux grand ?ges####
tiff("Taux de mortalit? origine 50 ans et + IDF.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015IDF),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015IDF
e15ul.num <- exp(ul)*offsets$NETR2015IDF
e15logrates <- deces15e$d15e/offsets$NETR2015IDF*10000

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015IDF),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015IDF
f15ul.num <- exp(ul)*offsets$NFRA2015IDF
f15logrates <- deces15f$d15f/offsets$NFRA2015IDF*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016IDF),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016IDF
e16ul.num <- exp(ul)*offsets$NETR2016IDF
e16logrates <- deces16e$d16e/offsets$NETR2016IDF*10000

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016IDF),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016IDF
f16ul.num <- exp(ul)*offsets$NFRA2016IDF
f16logrates <- deces16f$d16f/offsets$NFRA2016IDF*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017IDF),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017IDF
e17ul.num <- exp(ul)*offsets$NETR2017IDF
e17logrates <- deces17e$d17e/offsets$NETR2017IDF*10000

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017IDF),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017IDF
f17ul.num <- exp(ul)*offsets$NFRA2017IDF
f17logrates <- deces17f$d17f/offsets$NFRA2017IDF*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018IDF),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018IDF
e18ul.num <- exp(ul)*offsets$NETR2018IDF
e18logrates <- deces18e$d18e/offsets$NETR2018IDF*10000

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018IDF),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018IDF
f18ul.num <- exp(ul)*offsets$NFRA2018IDF
f18logrates <- deces18f$d18f/offsets$NFRA2018IDF*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019IDF),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019IDF
e19ul.num <- exp(ul)*offsets$NETR2019IDF
e19logrates <- deces19e$d19e/offsets$NETR2019IDF*10000

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019IDF),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019IDF
f19ul.num <- exp(ul)*offsets$NFRA2019IDF
f19logrates <- deces19f$d19f/offsets$NFRA2019IDF*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020IDF),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020IDF
e20ul.num <- exp(ul)*offsets$NETR2020IDF
e20logrates <- deces20e$d20e/offsets$NETR2020IDF*10000

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020IDF),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020IDF
f20ul.num <- exp(ul)*offsets$NFRA2020IDF
f20logrates <- deces20f$d20f/offsets$NFRA2020IDF*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
#les plots mais zoom?s aux grand ?ges avec ?chelle log####
tiff("Taux de mortalit? origine 50 ans et + LOG IDF.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015IDF),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015IDF
e15ul.num <- exp(ul)*offsets$NETR2015IDF
e15logrates <- deces15e$d15e/offsets$NETR2015IDF*10000

plot(ages, e15logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015IDF),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015IDF
f15ul.num <- exp(ul)*offsets$NFRA2015IDF
f15logrates <- deces15f$d15f/offsets$NFRA2015IDF*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=80-5)
abline(lwd=1.5,col="#036b50",v=73-5)
abline(lwd=1.5,col="#782438",v=58-5)
abline(lwd=1.5,col="#782438",v=78-5)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016IDF),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016IDF
e16ul.num <- exp(ul)*offsets$NETR2016IDF
e16logrates <- deces16e$d16e/offsets$NETR2016IDF*10000

plot(ages, e16logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016IDF),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016IDF
f16ul.num <- exp(ul)*offsets$NFRA2016IDF
f16logrates <- deces16f$d16f/offsets$NFRA2016IDF*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=80-4)
abline(lwd=1.5,col="#036b50",v=73-4)
abline(lwd=1.5,col="#782438",v=58-4)
abline(lwd=1.5,col="#782438",v=78-4)
p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017IDF),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017IDF
e17ul.num <- exp(ul)*offsets$NETR2017IDF
e17logrates <- deces17e$d17e/offsets$NETR2017IDF*10000

plot(ages, e17logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017IDF),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017IDF
f17ul.num <- exp(ul)*offsets$NFRA2017IDF
f17logrates <- deces17f$d17f/offsets$NFRA2017IDF*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=80-3)
abline(lwd=1.5,col="#036b50",v=73-3)
abline(lwd=1.5,col="#782438",v=58-3)
abline(lwd=1.5,col="#782438",v=78-3)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018IDF),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018IDF
e18ul.num <- exp(ul)*offsets$NETR2018IDF
e18logrates <- deces18e$d18e/offsets$NETR2018IDF*10000

plot(ages, e18logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)
abline(lwd=1.5,col="#036b50",v=80-2)
abline(lwd=1.5,col="#036b50",v=73-2)
abline(lwd=1.5,col="#782438",v=58-2)
abline(lwd=1.5,col="#782438",v=78-2)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018IDF),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018IDF
f18ul.num <- exp(ul)*offsets$NFRA2018IDF
f18logrates <- deces18f$d18f/offsets$NFRA2018IDF*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019IDF),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019IDF
e19ul.num <- exp(ul)*offsets$NETR2019IDF
e19logrates <- deces19e$d19e/offsets$NETR2019IDF*10000

plot(ages, e19logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)
abline(lwd=1.5,col="#036b50",v=80-1)
abline(lwd=1.5,col="#036b50",v=73-1)
abline(lwd=1.5,col="#782438",v=58-1)
abline(lwd=1.5,col="#782438",v=78-1)
f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019IDF),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019IDF
f19ul.num <- exp(ul)*offsets$NFRA2019IDF
f19logrates <- deces19f$d19f/offsets$NFRA2019IDF*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020IDF),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020IDF
e20ul.num <- exp(ul)*offsets$NETR2020IDF
e20logrates <- deces20e$d20e/offsets$NETR2020IDF*10000

plot(ages, e20logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020IDF),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020IDF
f20ul.num <- exp(ul)*offsets$NFRA2020IDF
f20logrates <- deces20f$d20f/offsets$NFRA2020IDF*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=80)
abline(lwd=1.5,col="#036b50",v=73)
abline(lwd=1.5,col="#782438",v=58)

abline(lwd=1.5,col="#782438",v=78)
p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

#faire hors GE et IDF
#c'est ? dire dans offsets faire pour ETR et FRA fracne -idf-ge chaque ann?e
#et dans le copier coller du code, changer les noms des tiff et changer le filtrer pour mettre !depidfetge
# et go sur la surmortalit?

#surmortalit? semaine ####

dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,semdec)
write.csv2(indicesem,"semaineIDF.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par semaine IDF",".tiff"), units="in", width=28, height=10, res=780)

p <- ggplot(indicesem,aes(x=semdec, y=indice,linetype=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("grey40","blue","red","#8AC926","#c42a19"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  scale_x_discrete(limits=c("Semaine du 2 mars (10)", 
                            "Semaine du 9 mars (11)",
                            "Semaine du 16 mars (12)",
                            "Semaine du 23 mars (13)",
                            "Semaine du 30 mars (14)",
                            "Semaine du 6 avril (15)",
                            "Semaine du 13 avril (16)",
                            "Semaine du 20 avril (17)",
                            "Semaine du 27 avril (18)",
                            "Semaine du 4 mai (19)"))+
  ylab("Indice de surmortalit?")+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(
    color=F,
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(p)

dev.off()

#surmortalit? ?ge####


dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,agedec)
write.csv2(indicesem,"ageIDF.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par ?ge IDF",".tiff"), units="in", width=28, height=10, res=780)

pageidf <- ggplot(indicesem,aes(x=agedec, y=indice,color=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("#06D6A0","#ef476f"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  ggtitle("?le-de-France")+
  ylab("Indice de surmortalit?")+
  ylim(0,4)+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(pageidf)

dev.off()
#### surmortalit? pays
dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(paysnaiss) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(paysnaiss) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(paysnaiss)
write.csv2(indicesem,"paysIDH.csv")
rm(decmoy,dec20)

#Reprise de l'ensemble pour la France hors R?gions du Grand Est et de l'Ile de France####
deces <- deces_sauv
offsets <- read_excel("C:/Users/irwin/OneDrive/Donn?es stats/deces/offsets.xlsx",sheet = 2)

deces <- deces %>% 
  mutate(anaiss=substr(datenaiss,1,4),
         adec=substr(datedeces,1,4),
         agedec=(as.numeric(adec)-as.numeric(anaiss)),
         agedec=ifelse(as.numeric(agedec)>=100,100,agedec),
         depdec=substr(lieudeces,1,2),
         depnaiss=substr(lieunaiss,1,2),
         moisnaiss=substr(datenaiss,5,6),
         moisdec=substr(datedeces,5,6),
         jdec=substr(datedeces,7,8),
         filtre=paste0(substr(moisdec,2,3),substr(jdec,1,3)),
         jnaiss=substr(datenaiss,7,8),
         datedeces=as.Date.character(datedeces,"%Y%m%d"),
         semdec=isoweek(datedeces),
         sexe=case_when(as.character(sexe)=="1"~"homme",
                        as.character(sexe)=="2"~"femme"),
         agedec=case_when(agedec>=100~100,
                          agedec<100~agedec),
         nomprenom=gsub("/","",nomprenom)) %>% 
  filter(semdec %in% c("10","11","12","13","14","15","16","17","18","19")& !(depdec %in% c("91",
                                                                                           "92",
                                                                                           "75",
                                                                                           "77",
                                                                                           "93",
                                                                                           "95",
                                                                                           "94",
                                                                                           "78","08","10","51","52","54","55","57","68","67","88"))) %>% 
  select(nomprenom,sexe,agedec,datenaiss,anaiss,moisnaiss,jnaiss,lieunaiss,depnaiss,commnaiss,paysnaiss,datedeces,adec,moisdec,jdec,semdec,lieudeces,depdec,actedeces) %>% 
  ungroup()
deces$paysnaiss[is.na(deces$paysnaiss)] <- "FRANCE"
deces <- deces %>% 
  mutate(naiss=ifelse(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE"),"N?s en France","N?s ? l'?tranger"),
         semdec=case_when(semdec=="10"~"Semaine du 2 mars (10)", 
                          semdec=="11"~"Semaine du 9 mars (11)",
                          semdec=="12"~"Semaine du 16 mars (12)",
                          semdec=="13"~"Semaine du 23 mars (13)",
                          semdec=="14"~"Semaine du 30 mars (14)",
                          semdec=="15"~"Semaine du 6 avril (15)",
                          semdec=="16"~"Semaine du 13 avril (16)",
                          semdec=="17"~"Semaine du 20 avril (17)",
                          semdec=="18"~"Semaine du 27 avril (18)",
                          semdec=="19"~"Semaine du 4 mai (19)")) 
ages <- sort(as.numeric(unique(deces$agedec)))
x <- seq(from = 0, to = 100, by = 5)
list_ages <- as.data.frame(ages)

#### ann?e par ann?e ####

deces15f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15f=n()) #le nom de la variable est ? changer
deces15f <- merge(deces15f,list_ages,all=T,by.x="agedec",by.y="ages")
deces15f[is.na(deces15f)] <- 0

deces15e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15e=n()) #le nom de la variable est ? changer
deces15e <- merge(deces15e,list_ages,all=T,by.x="agedec",by.y="ages")
deces15e[is.na(deces15e)] <- 0


esmooth15 <- Mort1Dsmooth(deces15e$d15e, offsets$NETR2015FRHDEP)

deces16f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16f=n()) #le nom de la variable est ? changer
deces16f <- merge(deces16f,list_ages,all=T,by.x="agedec",by.y="ages")
deces16f[is.na(deces16f)] <- 0

deces16e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16e=n()) #le nom de la variable est ? changer
deces16e <- merge(deces16e,list_ages,all=T,by.x="agedec",by.y="ages")
deces16e[is.na(deces16e)] <- 0


esmooth16 <- Mort1Dsmooth(deces16e$d16e, offsets$NETR2016FRHDEP)

deces17f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17f=n()) #le nom de la variable est ? changer
deces17f <- merge(deces17f,list_ages,all=T,by.x="agedec",by.y="ages")
deces17f[is.na(deces17f)] <- 0

deces17e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17e=n()) #le nom de la variable est ? changer
deces17e <- merge(deces17e,list_ages,all=T,by.x="agedec",by.y="ages")
deces17e[is.na(deces17e)] <- 0


esmooth17 <- Mort1Dsmooth(deces17e$d17e, offsets$NETR2017FRHDEP)

deces18f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18f=n()) #le nom de la variable est ? changer
deces18f <- merge(deces18f,list_ages,all=T,by.x="agedec",by.y="ages")
deces18f[is.na(deces18f)] <- 0

deces18e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18e=n()) #le nom de la variable est ? changer
deces18e <- merge(deces18e,list_ages,all=T,by.x="agedec",by.y="ages")
deces18e[is.na(deces18e)] <- 0


esmooth18 <- Mort1Dsmooth(deces18e$d18e, offsets$NETR2018FRHDEP)

deces19f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19f=n()) #le nom de la variable est ? changer
deces19f <- merge(deces19f,list_ages,all=T,by.x="agedec",by.y="ages")
deces19f[is.na(deces19f)] <- 0

deces19e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19e=n()) #le nom de la variable est ? changer
deces19e <- merge(deces19e,list_ages,all=T,by.x="agedec",by.y="ages")
deces19e[is.na(deces19e)] <- 0


esmooth19 <- Mort1Dsmooth(deces19e$d19e, offsets$NETR2019FRHDEP)

deces20f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20f=n()) #le nom de la variable est ? changer
deces20f <- merge(deces20f,list_ages,all=T,by.x="agedec",by.y="ages")
deces20f[is.na(deces20f)] <- 0

deces20e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20e=n()) #le nom de la variable est ? changer
deces20e <- merge(deces20e,list_ages,all=T,by.x="agedec",by.y="ages")
deces20e[is.na(deces20e)] <- 0

#### Smooth? ####
tiff("Taux de mortalit? ?chelle log FRHDEP.tiff",width=40,height=30,units = "cm",res=780)

par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015FRHDEP),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015FRHDEP
e15ul.num <- exp(ul)*offsets$NETR2015FRHDEP
e15logrates <- log(deces15e$d15e/offsets$NETR2015FRHDEP)

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e15smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015FRHDEP),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015FRHDEP
f15ul.num <- exp(ul)*offsets$NFRA2015FRHDEP
f15logrates <- log(deces15f$d15f/offsets$NFRA2015FRHDEP)

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016FRHDEP),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016FRHDEP
e16ul.num <- exp(ul)*offsets$NETR2016FRHDEP
e16logrates <- log(deces16e$d16e/offsets$NETR2016FRHDEP)

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e16smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016FRHDEP),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016FRHDEP
f16ul.num <- exp(ul)*offsets$NFRA2016FRHDEP
f16logrates <- log(deces16f$d16f/offsets$NFRA2016FRHDEP)

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017FRHDEP),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017FRHDEP
e17ul.num <- exp(ul)*offsets$NETR2017FRHDEP
e17logrates <- log(deces17e$d17e/offsets$NETR2017FRHDEP)

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e17smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017FRHDEP),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017FRHDEP
f17ul.num <- exp(ul)*offsets$NFRA2017FRHDEP
f17logrates <- log(deces17f$d17f/offsets$NFRA2017FRHDEP)

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018FRHDEP),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018FRHDEP
e18ul.num <- exp(ul)*offsets$NETR2018FRHDEP
e18logrates <- log(deces18e$d18e/offsets$NETR2018FRHDEP)

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e18smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018FRHDEP),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018FRHDEP
f18ul.num <- exp(ul)*offsets$NFRA2018FRHDEP
f18logrates <- log(deces18f$d18f/offsets$NFRA2018FRHDEP)

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019FRHDEP),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019FRHDEP
e19ul.num <- exp(ul)*offsets$NETR2019FRHDEP
e19logrates <- log(deces19e$d19e/offsets$NETR2019FRHDEP)

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e19smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019FRHDEP),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019FRHDEP
f19ul.num <- exp(ul)*offsets$NFRA2019FRHDEP
f19logrates <- log(deces19f$d19f/offsets$NFRA2019FRHDEP)

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020FRHDEP),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020FRHDEP
e20ul.num <- exp(ul)*offsets$NETR2020FRHDEP
e20logrates <- log(deces20e$d20e/offsets$NETR2020FRHDEP)

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020FRHDEP),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020FRHDEP
f20ul.num <- exp(ul)*offsets$NFRA2020FRHDEP
f20logrates <- log(deces20f$d20f/offsets$NFRA2020FRHDEP)

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
tiff("Smooth2020FRHDEP.tiff",width=30,height=20,units = "cm",res=780)

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020FRHDEP),method=3,lambda=15)
e20smo <- exp(e20smooth$logmortality)*10000
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ll <- exp(ll)*10000
ul <- pre$fit + 1.966*pre$se.fit
ul <- exp(ul)*10000
e20ll.num <- exp(ll)*offsets$NETR2020FRHDEP
e20ul.num <- exp(ul)*offsets$NETR2020FRHDEP
e20logrates <- deces20e$d20e/offsets$NETR2020FRHDEP*10000

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "Autres R?gions",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,xlab = "?ge",ylab = "Taux de mortalit?",log = "y",ylim = c(0.1,1200))
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020FRHDEP),method=3,lambda=15)
f20smo <- exp(f20smooth$logmortality)*10000
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ll <- exp(ll)*10000
ul <- pre$fit + 1.966*pre$se.fit
ul <- exp(ul)*10000
f20ll.num <- exp(ll)*offsets$NFRA2020
f20ul.num <- exp(ul)*offsets$NFRA2020
f20logrates <- deces20f$d20f/offsets$NFRA2020FRHDEP*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()
#les plots mais zoom?s aux grand ?ges####
tiff("Taux de mortalit? origine 50 ans et + FRHDEP.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015FRHDEP),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015FRHDEP
e15ul.num <- exp(ul)*offsets$NETR2015FRHDEP
e15logrates <- deces15e$d15e/offsets$NETR2015FRHDEP*10000

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015FRHDEP),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015FRHDEP
f15ul.num <- exp(ul)*offsets$NFRA2015FRHDEP
f15logrates <- deces15f$d15f/offsets$NFRA2015FRHDEP*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016FRHDEP),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016FRHDEP
e16ul.num <- exp(ul)*offsets$NETR2016FRHDEP
e16logrates <- deces16e$d16e/offsets$NETR2016FRHDEP*10000

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016FRHDEP),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016FRHDEP
f16ul.num <- exp(ul)*offsets$NFRA2016FRHDEP
f16logrates <- deces16f$d16f/offsets$NFRA2016FRHDEP*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017FRHDEP),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017FRHDEP
e17ul.num <- exp(ul)*offsets$NETR2017FRHDEP
e17logrates <- deces17e$d17e/offsets$NETR2017FRHDEP*10000

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017FRHDEP),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017FRHDEP
f17ul.num <- exp(ul)*offsets$NFRA2017FRHDEP
f17logrates <- deces17f$d17f/offsets$NFRA2017FRHDEP*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018FRHDEP),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018FRHDEP
e18ul.num <- exp(ul)*offsets$NETR2018FRHDEP
e18logrates <- deces18e$d18e/offsets$NETR2018FRHDEP*10000

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018FRHDEP),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018FRHDEP
f18ul.num <- exp(ul)*offsets$NFRA2018FRHDEP
f18logrates <- deces18f$d18f/offsets$NFRA2018FRHDEP*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019FRHDEP),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019FRHDEP
e19ul.num <- exp(ul)*offsets$NETR2019FRHDEP
e19logrates <- deces19e$d19e/offsets$NETR2019FRHDEP*10000

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019FRHDEP),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019FRHDEP
f19ul.num <- exp(ul)*offsets$NFRA2019FRHDEP
f19logrates <- deces19f$d19f/offsets$NFRA2019FRHDEP*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020FRHDEP),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020FRHDEP
e20ul.num <- exp(ul)*offsets$NETR2020FRHDEP
e20logrates <- deces20e$d20e/offsets$NETR2020FRHDEP*10000

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020FRHDEP),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020FRHDEP
f20ul.num <- exp(ul)*offsets$NFRA2020FRHDEP
f20logrates <- deces20f$d20f/offsets$NFRA2020FRHDEP*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
#les plots mais zoom?s aux grand ?ges avec ?chelle log####
tiff("Taux de mortalit? origine 50 ans et + LOG FRHDEP.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015FRHDEP),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015FRHDEP
e15ul.num <- exp(ul)*offsets$NETR2015FRHDEP
e15logrates <- deces15e$d15e/offsets$NETR2015FRHDEP*10000

plot(ages, e15logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)


f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015FRHDEP),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015FRHDEP
f15ul.num <- exp(ul)*offsets$NFRA2015FRHDEP
f15logrates <- deces15f$d15f/offsets$NFRA2015FRHDEP*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=80-5)
abline(lwd=1.5,col="#036b50",v=73-5)
abline(lwd=1.5,col="#782438",v=58-5)
abline(lwd=1.5,col="#782438",v=78-5)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016FRHDEP),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016FRHDEP
e16ul.num <- exp(ul)*offsets$NETR2016FRHDEP
e16logrates <- deces16e$d16e/offsets$NETR2016FRHDEP*10000

plot(ages, e16logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016FRHDEP),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016FRHDEP
f16ul.num <- exp(ul)*offsets$NFRA2016FRHDEP
f16logrates <- deces16f$d16f/offsets$NFRA2016FRHDEP*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=80-4)
abline(lwd=1.5,col="#036b50",v=73-4)
abline(lwd=1.5,col="#782438",v=58-4)
abline(lwd=1.5,col="#782438",v=78-4)
p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017FRHDEP),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017FRHDEP
e17ul.num <- exp(ul)*offsets$NETR2017FRHDEP
e17logrates <- deces17e$d17e/offsets$NETR2017FRHDEP*10000

plot(ages, e17logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017FRHDEP),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017FRHDEP
f17ul.num <- exp(ul)*offsets$NFRA2017FRHDEP
f17logrates <- deces17f$d17f/offsets$NFRA2017FRHDEP*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=80-3)
abline(lwd=1.5,col="#036b50",v=73-3)
abline(lwd=1.5,col="#782438",v=58-3)
abline(lwd=1.5,col="#782438",v=78-3)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018FRHDEP),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018FRHDEP
e18ul.num <- exp(ul)*offsets$NETR2018FRHDEP
e18logrates <- deces18e$d18e/offsets$NETR2018FRHDEP*10000

plot(ages, e18logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018FRHDEP),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018FRHDEP
f18ul.num <- exp(ul)*offsets$NFRA2018FRHDEP
f18logrates <- deces18f$d18f/offsets$NFRA2018FRHDEP*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=80-2)
abline(lwd=1.5,col="#036b50",v=73-2)
abline(lwd=1.5,col="#782438",v=58-2)
abline(lwd=1.5,col="#782438",v=78-2)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019FRHDEP),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019FRHDEP
e19ul.num <- exp(ul)*offsets$NETR2019FRHDEP
e19logrates <- deces19e$d19e/offsets$NETR2019FRHDEP*10000

plot(ages, e19logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019FRHDEP),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019FRHDEP
f19ul.num <- exp(ul)*offsets$NFRA2019FRHDEP
f19logrates <- deces19f$d19f/offsets$NFRA2019FRHDEP*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=80-1)
abline(lwd=1.5,col="#036b50",v=73-1)
abline(lwd=1.5,col="#782438",v=58-1)
abline(lwd=1.5,col="#782438",v=78-1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020FRHDEP),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020FRHDEP
e20ul.num <- exp(ul)*offsets$NETR2020FRHDEP
e20logrates <- deces20e$d20e/offsets$NETR2020FRHDEP*10000

plot(ages, e20logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020FRHDEP),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020FRHDEP
f20ul.num <- exp(ul)*offsets$NFRA2020FRHDEP
f20logrates <- deces20f$d20f/offsets$NFRA2020FRHDEP*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)
abline(lwd=1.5,col="#036b50",v=80)
abline(lwd=1.5,col="#036b50",v=73)
abline(lwd=1.5,col="#782438",v=58)
abline(lwd=1.5,col="#782438",v=78)
p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

#faire hors GE et FRHDEP
#c'est ? dire dans offsets faire pour ETR et FRA fracne -FRHDEP-ge chaque ann?e
#et dans le copier coller du code, changer les noms des tiff et changer le filtrer pour mettre !depFRHDEPetge
# et go sur la surmortalit?

#surmortalit? semaine ####

dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,semdec)
write.csv2(indicesem,"semaineFRHDEP.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par semaine FRHDEP",".tiff"), units="in", width=28, height=10, res=780)

p <- ggplot(indicesem,aes(x=semdec, y=indice,linetype=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("grey40","blue","red","#8AC926","#c42a19"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  scale_x_discrete(limits=c("Semaine du 2 mars (10)", 
                            "Semaine du 9 mars (11)",
                            "Semaine du 16 mars (12)",
                            "Semaine du 23 mars (13)",
                            "Semaine du 30 mars (14)",
                            "Semaine du 6 avril (15)",
                            "Semaine du 13 avril (16)",
                            "Semaine du 20 avril (17)",
                            "Semaine du 27 avril (18)",
                            "Semaine du 4 mai (19)"))+
  ylab("Indice de surmortalit?")+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(
    color=F,
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(p)

dev.off()

#surmortalit? ?ge####


dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,agedec)
write.csv2(indicesem,"ageFRHDEP.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par ?ge FRHDEP",".tiff"), units="in", width=28, height=10, res=780)

pagefrhdep <- ggplot(indicesem,aes(x=agedec, y=indice,color=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("#06D6A0","#ef476f"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  ylab("Indice de surmortalit?")+
  ylim(0,4)+
  ggtitle("Autres R?gions")+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(pagefrhdep)

dev.off()

tiff(paste0("grid surmortalit? ?ge",".tiff"), units="in", width=25, height=35, res=780)
grid.arrange(pagege,pageidf,pagefrhdep,ncol=1,nrow=3)
dev.off()

#surmortalit? par pays####
dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(paysnaiss) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(paysnaiss) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(paysnaiss)
write.csv2(indicesem,"paysFRHDEP.csv")
rm(decmoy,dec20)



  #### CHANGEMENT PARTIE SPECFIQUE LIEU DE NAISSANCE AF.NORD ####
#Importation et construction table####

deces <- deces_sauv
offsets <- read_excel("C:/Users/irwin/OneDrive/Donn?es stats/deces/offsets.xlsx",sheet = 2)

deces <- deces %>% 
  mutate(anaiss=substr(datenaiss,1,4),
         adec=substr(datedeces,1,4),
         agedec=(as.numeric(adec)-as.numeric(anaiss)),
         agedec=ifelse(as.numeric(agedec)>=100,100,agedec),
         depdec=substr(lieudeces,1,2),
         depnaiss=substr(lieunaiss,1,2),
         moisnaiss=substr(datenaiss,5,6),
         moisdec=substr(datedeces,5,6),
         jdec=substr(datedeces,7,8),
         filtre=paste0(substr(moisdec,2,3),substr(jdec,1,3)),
         jnaiss=substr(datenaiss,7,8),
         datedeces=as.Date.character(datedeces,"%Y%m%d"),
         semdec=isoweek(datedeces),
         sexe=case_when(as.character(sexe)=="1"~"homme",
                        as.character(sexe)=="2"~"femme"),
         agedec=case_when(agedec>=100~100,
                          agedec<100~agedec),
         nomprenom=gsub("/","",nomprenom)) %>% 
  filter(semdec %in% c("10","11","12","13","14","15","16","17","18","19")) %>% 
  select(nomprenom,sexe,agedec,datenaiss,anaiss,moisnaiss,jnaiss,lieunaiss,depnaiss,commnaiss,paysnaiss,datedeces,adec,moisdec,jdec,semdec,lieudeces,depdec,actedeces) %>% 
  ungroup()
deces$paysnaiss[is.na(deces$paysnaiss)] <- "FRANCE"
deces <- deces %>% 
  mutate(naiss=case_when(paysnaiss=="FRANCE"|paysnaiss=="REUNION"|paysnaiss=="GUADELOUPE"|paysnaiss=="MARTINIQUE"|paysnaiss=="GUYANE"~"N?s en France",
                         paysnaiss=="ALGERIE"|paysnaiss=="MAROC"|paysnaiss=="TUNISIE"|paysnaiss=="EGYPTE"|paysnaiss=="LYBIE"~"N?s en Afrique du Nord",
                        TRUE~"N?s ? l'?tranger (hors Afrique du Nord)"),
         semdec=case_when(semdec=="10"~"Semaine du 2 mars (10)", 
                          semdec=="11"~"Semaine du 9 mars (11)",
                          semdec=="12"~"Semaine du 16 mars (12)",
                          semdec=="13"~"Semaine du 23 mars (13)",
                          semdec=="14"~"Semaine du 30 mars (14)",
                          semdec=="15"~"Semaine du 6 avril (15)",
                          semdec=="16"~"Semaine du 13 avril (16)",
                          semdec=="17"~"Semaine du 20 avril (17)",
                          semdec=="18"~"Semaine du 27 avril (18)",
                          semdec=="19"~"Semaine du 4 mai (19)")) %>% 
  filter(!(naiss=="N?s en France"))

ages <- sort(as.numeric(unique(deces$agedec)))
x <- seq(from = 0, to = 100, by = 5)
list_ages <- as.data.frame(ages)
setwd("C:/Users/irwin/OneDrive/Donn?es stats/deces/afrique du nord")
#### ann?e par ann?e ####

deces15f <- deces %>% #nom de la table ? changer
  filter(naiss=="N?s en Afrique du Nord"&adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15f=n()) #le nom de la variable est ? changer
deces15f <- merge(deces15f,list_ages,all=T,by.x="agedec",by.y="ages")
deces15f[is.na(deces15f)] <- 0

deces15e <- deces %>% #nom de la table ? changer
  filter(naiss=="N?s ? l'?tranger (hors Afrique du Nord)"&adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15e=n()) #le nom de la variable est ? changer
deces15e <- merge(deces15e,list_ages,all=T,by.x="agedec",by.y="ages")
deces15e[is.na(deces15e)] <- 0


esmooth15 <- Mort1Dsmooth(deces15e$d15e, offsets$NETR2015)

deces16f <- deces %>% #nom de la table ? changer
  filter(naiss=="N?s en Afrique du Nord" &adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16f=n()) #le nom de la variable est ? changer
deces16f <- merge(deces16f,list_ages,all=T,by.x="agedec",by.y="ages")
deces16f[is.na(deces16f)] <- 0

deces16e <- deces %>% #nom de la table ? changer
  filter(!(naiss=="N?s ? l'?tranger (hors Afrique du Nord)") &adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16e=n()) #le nom de la variable est ? changer
deces16e <- merge(deces16e,list_ages,all=T,by.x="agedec",by.y="ages")
deces16e[is.na(deces16e)] <- 0


esmooth16 <- Mort1Dsmooth(deces16e$d16e, offsets$NETR2016)

deces17f <- deces %>% #nom de la table ? changer
  filter(naiss=="N?s en Afrique du Nord" &adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17f=n()) #le nom de la variable est ? changer
deces17f <- merge(deces17f,list_ages,all=T,by.x="agedec",by.y="ages")
deces17f[is.na(deces17f)] <- 0

deces17e <- deces %>% #nom de la table ? changer
  filter(!(naiss=="N?s ? l'?tranger (hors Afrique du Nord)") &adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17e=n()) #le nom de la variable est ? changer
deces17e <- merge(deces17e,list_ages,all=T,by.x="agedec",by.y="ages")
deces17e[is.na(deces17e)] <- 0


esmooth17 <- Mort1Dsmooth(deces17e$d17e, offsets$NETR2017)

deces18f <- deces %>% #nom de la table ? changer
  filter(naiss=="N?s en Afrique du Nord" &adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18f=n()) #le nom de la variable est ? changer
deces18f <- merge(deces18f,list_ages,all=T,by.x="agedec",by.y="ages")
deces18f[is.na(deces18f)] <- 0

deces18e <- deces %>% #nom de la table ? changer
  filter(!(naiss=="N?s ? l'?tranger (hors Afrique du Nord)") &adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18e=n()) #le nom de la variable est ? changer
deces18e <- merge(deces18e,list_ages,all=T,by.x="agedec",by.y="ages")
deces18e[is.na(deces18e)] <- 0


esmooth18 <- Mort1Dsmooth(deces18e$d18e, offsets$NETR2018)

deces19f <- deces %>% #nom de la table ? changer
  filter(naiss=="N?s en Afrique du Nord" &adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19f=n()) #le nom de la variable est ? changer
deces19f <- merge(deces19f,list_ages,all=T,by.x="agedec",by.y="ages")
deces19f[is.na(deces19f)] <- 0

deces19e <- deces %>% #nom de la table ? changer
  filter(!(naiss=="N?s ? l'?tranger (hors Afrique du Nord)") &adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19e=n()) #le nom de la variable est ? changer
deces19e <- merge(deces19e,list_ages,all=T,by.x="agedec",by.y="ages")
deces19e[is.na(deces19e)] <- 0


esmooth19 <- Mort1Dsmooth(deces19e$d19e, offsets$NETR2019)

deces20f <- deces %>% #nom de la table ? changer
  filter(naiss=="N?s en Afrique du Nord" &adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20f=n()) #le nom de la variable est ? changer
deces20f <- merge(deces20f,list_ages,all=T,by.x="agedec",by.y="ages")
deces20f[is.na(deces20f)] <- 0

deces20e <- deces %>% #nom de la table ? changer
  filter(!(naiss=="N?s ? l'?tranger (hors Afrique du Nord)") &adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20e=n()) #le nom de la variable est ? changer
deces20e <- merge(deces20e,list_ages,all=T,by.x="agedec",by.y="ages")
deces20e[is.na(deces20e)] <- 0



#### Smooth? ####
tiff("Taux de mortalit? ?chelle log FR.tiff",width=40,height=30,units = "cm",res=780)

par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015),method=3,lambda=20)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015
e15ul.num <- exp(ul)*offsets$NETR2015
e15logrates <- log(deces15e$d15e/offsets$NETR2015)

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e15smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f,method=3,lambda=20)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015
f15ul.num <- exp(ul)*offsets$NFRA2015
f15logrates <- log(deces15f$d15f/offsets$NFRA2015)

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016
e16ul.num <- exp(ul)*offsets$NETR2016
e16logrates <- log(deces16e$d16e/offsets$NETR2016)

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e16smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016
f16ul.num <- exp(ul)*offsets$NFRA2016
f16logrates <- log(deces16f$d16f/offsets$NFRA2016)

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017
e17ul.num <- exp(ul)*offsets$NETR2017
e17logrates <- log(deces17e$d17e/offsets$NETR2017)

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e17smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017
f17ul.num <- exp(ul)*offsets$NFRA2017
f17logrates <- log(deces17f$d17f/offsets$NFRA2017)

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018
e18ul.num <- exp(ul)*offsets$NETR2018
e18logrates <- log(deces18e$d18e/offsets$NETR2018)

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e18smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018
f18ul.num <- exp(ul)*offsets$NFRA2018
f18logrates <- log(deces18f$d18f/offsets$NFRA2018)

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019
e19ul.num <- exp(ul)*offsets$NETR2019
e19logrates <- log(deces19e$d19e/offsets$NETR2019)

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e19smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019
f19ul.num <- exp(ul)*offsets$NFRA2019
f19logrates <- log(deces19f$d19f/offsets$NFRA2019)

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020
e20ul.num <- exp(ul)*offsets$NETR2020
e20logrates <- log(deces20e$d20e/offsets$NETR2020)

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020
f20ul.num <- exp(ul)*offsets$NFRA2020
f20logrates <- log(deces20f$d20f/offsets$NFRA2020)

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

#les plots mais zoom?s aux grand ?ges####
tiff("Taux de mortalit? origine 50 ans et + GE.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015
e15ul.num <- exp(ul)*offsets$NETR2015
e15logrates <- deces15e$d15e/offsets$NETR2015*10000

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015
f15ul.num <- exp(ul)*offsets$NFRA2015
f15logrates <- deces15f$d15f/offsets$NFRA2015*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016
e16ul.num <- exp(ul)*offsets$NETR2016
e16logrates <- deces16e$d16e/offsets$NETR2016*10000

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016
f16ul.num <- exp(ul)*offsets$NFRA2016
f16logrates <- deces16f$d16f/offsets$NFRA2016*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017
e17ul.num <- exp(ul)*offsets$NETR2017
e17logrates <- deces17e$d17e/offsets$NETR2017*10000

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017
f17ul.num <- exp(ul)*offsets$NFRA2017
f17logrates <- deces17f$d17f/offsets$NFRA2017*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018
e18ul.num <- exp(ul)*offsets$NETR2018
e18logrates <- deces18e$d18e/offsets$NETR2018*10000

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018
f18ul.num <- exp(ul)*offsets$NFRA2018
f18logrates <- deces18f$d18f/offsets$NFRA2018*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019
e19ul.num <- exp(ul)*offsets$NETR2019
e19logrates <- deces19e$d19e/offsets$NETR2019*10000

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019
f19ul.num <- exp(ul)*offsets$NFRA2019
f19logrates <- deces19f$d19f/offsets$NFRA2019*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020
e20ul.num <- exp(ul)*offsets$NETR2020
e20logrates <- deces20e$d20e/offsets$NETR2020*10000

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020
f20ul.num <- exp(ul)*offsets$NFRA2020
f20logrates <- deces20f$d20f/offsets$NFRA2020*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
#les plots mais zoom?s aux grand ?ges avec ?chelle log####
tiff("Taux de mortalit? origine 50 ans et + LOG GE.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015
e15ul.num <- exp(ul)*offsets$NETR2015
e15logrates <- deces15e$d15e/offsets$NETR2015*10000

plot(ages, e15logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015
f15ul.num <- exp(ul)*offsets$NFRA2015
f15logrates <- deces15f$d15f/offsets$NFRA2015*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016
e16ul.num <- exp(ul)*offsets$NETR2016
e16logrates <- deces16e$d16e/offsets$NETR2016*10000

plot(ages, e16logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016
f16ul.num <- exp(ul)*offsets$NFRA2016
f16logrates <- deces16f$d16f/offsets$NFRA2016*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017
e17ul.num <- exp(ul)*offsets$NETR2017
e17logrates <- deces17e$d17e/offsets$NETR2017*10000

plot(ages, e17logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017
f17ul.num <- exp(ul)*offsets$NFRA2017
f17logrates <- deces17f$d17f/offsets$NFRA2017*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018
e18ul.num <- exp(ul)*offsets$NETR2018
e18logrates <- deces18e$d18e/offsets$NETR2018*10000

plot(ages, e18logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018
f18ul.num <- exp(ul)*offsets$NFRA2018
f18logrates <- deces18f$d18f/offsets$NFRA2018*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019
e19ul.num <- exp(ul)*offsets$NETR2019
e19logrates <- deces19e$d19e/offsets$NETR2019*10000

plot(ages, e19logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019
f19ul.num <- exp(ul)*offsets$NFRA2019
f19logrates <- deces19f$d19f/offsets$NFRA2019*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020
e20ul.num <- exp(ul)*offsets$NETR2020
e20logrates <- deces20e$d20e/offsets$NETR2020*10000

plot(ages, e20logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020
f20ul.num <- exp(ul)*offsets$NFRA2020
f20logrates <- deces20f$d20f/offsets$NFRA2020*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
#surmortalit? semaine ####

dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,semdec)
write.csv2(indicesem,"semaineFR.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par semaine FR",".tiff"), units="in", width=28, height=10, res=780)

p <- ggplot(indicesem,aes(x=semdec, y=indice,linetype=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="R?gions",values=c("grey40","blue","red","#8AC926","#c42a19"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  ylab("Indice de surmortalit?")+
  scale_x_discrete(limits=c("Semaine du 2 mars (10)", 
                            "Semaine du 9 mars (11)",
                            "Semaine du 16 mars (12)",
                            "Semaine du 23 mars (13)",
                            "Semaine du 30 mars (14)",
                            "Semaine du 6 avril (15)",
                            "Semaine du 13 avril (16)",
                            "Semaine du 20 avril (17)",
                            "Semaine du 27 avril (18)",
                            "Semaine du 4 mai (19)"))+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(
    color=F,
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(p)

dev.off()

#surmortalit? ?ge####


dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,agedec)
write.csv2(indicesem,"ageFR.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par ?ge FR",".tiff"), units="in", width=28, height=10, res=780)

p <- ggplot(indicesem,aes(x=agedec, y=indice,linetype=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("grey40","blue","red","#8AC926","#c42a19"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  ylab("Indice de surmortalit?")+
  ylim(0,5)+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(
    color=F,
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(p)

dev.off()

#Reprise de l'ensemble pour le Grand Est####

deces <- deces_sauv
offsets <- read_excel("C:/Users/irwin/OneDrive/Donn?es stats/deces/offsets.xlsx",sheet = 2)

deces <- deces %>% 
  mutate(anaiss=substr(datenaiss,1,4),
         adec=substr(datedeces,1,4),
         agedec=(as.numeric(adec)-as.numeric(anaiss)),
         agedec=ifelse(as.numeric(agedec)>=100,100,agedec),
         depdec=substr(lieudeces,1,2),
         depnaiss=substr(lieunaiss,1,2),
         moisnaiss=substr(datenaiss,5,6),
         moisdec=substr(datedeces,5,6),
         jdec=substr(datedeces,7,8),
         filtre=paste0(substr(moisdec,2,3),substr(jdec,1,3)),
         jnaiss=substr(datenaiss,7,8),
         datedeces=as.Date.character(datedeces,"%Y%m%d"),
         semdec=isoweek(datedeces),
         sexe=case_when(as.character(sexe)=="1"~"homme",
                        as.character(sexe)=="2"~"femme"),
         agedec=case_when(agedec>=100~100,
                          agedec<100~agedec),
         nomprenom=gsub("/","",nomprenom)) %>% 
  filter(semdec %in% c("10","11","12","13","14","15","16","17","18","19")&depdec %in% c("08","10","51","52","54","55","57","68","67","88")) %>% 
  select(nomprenom,sexe,agedec,datenaiss,anaiss,moisnaiss,jnaiss,lieunaiss,depnaiss,commnaiss,paysnaiss,datedeces,adec,moisdec,jdec,semdec,lieudeces,depdec,actedeces) %>% 
  ungroup()
deces$paysnaiss[is.na(deces$paysnaiss)] <- "FRANCE"
deces <- deces %>% 
  mutate(naiss=case_when(paysnaiss=="FRANCE"|paysnaiss=="REUNION"|paysnaiss=="GUADELOUPE"|paysnaiss=="MARTINIQUE"|paysnaiss=="GUYANE"~"N?s en France",
                         paysnaiss=="ALGERIE"|paysnaiss=="MAROC"|paysnaiss=="TUNISIE"|paysnaiss=="EGYPTE"|paysnaiss=="LYBIE"~"N?s en Afrique du Nord",
                         TRUE~"N?s ? l'?tranger (hors Afrique du Nord)"),
         semdec=case_when(semdec=="10"~"Semaine du 2 mars (10)", 
                          semdec=="11"~"Semaine du 9 mars (11)",
                          semdec=="12"~"Semaine du 16 mars (12)",
                          semdec=="13"~"Semaine du 23 mars (13)",
                          semdec=="14"~"Semaine du 30 mars (14)",
                          semdec=="15"~"Semaine du 6 avril (15)",
                          semdec=="16"~"Semaine du 13 avril (16)",
                          semdec=="17"~"Semaine du 20 avril (17)",
                          semdec=="18"~"Semaine du 27 avril (18)",
                          semdec=="19"~"Semaine du 4 mai (19)")) %>% 
  filter(!(naiss=="N?s en France"))

ages <- sort(as.numeric(unique(deces$agedec)))
x <- seq(from = 0, to = 100, by = 5)
list_ages <- as.data.frame(ages)
setwd("C:/Users/irwin/OneDrive/Donn?es stats/deces/afrique du nord")

#### ann?e par ann?e ####

deces15f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15f=n()) #le nom de la variable est ? changer
deces15f <- merge(deces15f,list_ages,all=T,by.x="agedec",by.y="ages")
deces15f[is.na(deces15f)] <- 0

deces15e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15e=n()) #le nom de la variable est ? changer
deces15e <- merge(deces15e,list_ages,all=T,by.x="agedec",by.y="ages")
deces15e[is.na(deces15e)] <- 0


esmooth15 <- Mort1Dsmooth(deces15e$d15e, offsets$NETR2015GE)

deces16f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16f=n()) #le nom de la variable est ? changer
deces16f <- merge(deces16f,list_ages,all=T,by.x="agedec",by.y="ages")
deces16f[is.na(deces16f)] <- 0

deces16e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16e=n()) #le nom de la variable est ? changer
deces16e <- merge(deces16e,list_ages,all=T,by.x="agedec",by.y="ages")
deces16e[is.na(deces16e)] <- 0


esmooth16 <- Mort1Dsmooth(deces16e$d16e, offsets$NETR2016GE)

deces17f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17f=n()) #le nom de la variable est ? changer
deces17f <- merge(deces17f,list_ages,all=T,by.x="agedec",by.y="ages")
deces17f[is.na(deces17f)] <- 0

deces17e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17e=n()) #le nom de la variable est ? changer
deces17e <- merge(deces17e,list_ages,all=T,by.x="agedec",by.y="ages")
deces17e[is.na(deces17e)] <- 0


esmooth17 <- Mort1Dsmooth(deces17e$d17e, offsets$NETR2017GE)

deces18f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18f=n()) #le nom de la variable est ? changer
deces18f <- merge(deces18f,list_ages,all=T,by.x="agedec",by.y="ages")
deces18f[is.na(deces18f)] <- 0

deces18e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18e=n()) #le nom de la variable est ? changer
deces18e <- merge(deces18e,list_ages,all=T,by.x="agedec",by.y="ages")
deces18e[is.na(deces18e)] <- 0


esmooth18 <- Mort1Dsmooth(deces18e$d18e, offsets$NETR2018GE)

deces19f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19f=n()) #le nom de la variable est ? changer
deces19f <- merge(deces19f,list_ages,all=T,by.x="agedec",by.y="ages")
deces19f[is.na(deces19f)] <- 0

deces19e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19e=n()) #le nom de la variable est ? changer
deces19e <- merge(deces19e,list_ages,all=T,by.x="agedec",by.y="ages")
deces19e[is.na(deces19e)] <- 0


esmooth19 <- Mort1Dsmooth(deces19e$d19e, offsets$NETR2019GE)

deces20f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20f=n()) #le nom de la variable est ? changer
deces20f <- merge(deces20f,list_ages,all=T,by.x="agedec",by.y="ages")
deces20f[is.na(deces20f)] <- 0

deces20e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20e=n()) #le nom de la variable est ? changer
deces20e <- merge(deces20e,list_ages,all=T,by.x="agedec",by.y="ages")
deces20e[is.na(deces20e)] <- 0

#### Smooth? ####
tiff("Taux de mortalit? ?chelle log GE.tiff",width=40,height=30,units = "cm",res=780)
ages <- sort(as.numeric(seq(0,100,1)))
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015GE),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015GE
e15ul.num <- exp(ul)*offsets$NETR2015GE
e15logrates <- log(deces15e$d15e/offsets$NETR2015GE)

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e15smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015GE),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015GE
f15ul.num <- exp(ul)*offsets$NFRA2015GE
f15logrates <- log(deces15f$d15f/offsets$NFRA2015GE)

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016GE),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016GE
e16ul.num <- exp(ul)*offsets$NETR2016GE
e16logrates <- log(deces16e$d16e/offsets$NETR2016GE)

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e16smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016GE),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016GE
f16ul.num <- exp(ul)*offsets$NFRA2016GE
f16logrates <- log(deces16f$d16f/offsets$NFRA2016GE)

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017GE),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017GE
e17ul.num <- exp(ul)*offsets$NETR2017GE
e17logrates <- log(deces17e$d17e/offsets$NETR2017GE)

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e17smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017GE),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017GE
f17ul.num <- exp(ul)*offsets$NFRA2017GE
f17logrates <- log(deces17f$d17f/offsets$NFRA2017GE)

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018GE),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018GE
e18ul.num <- exp(ul)*offsets$NETR2018GE
e18logrates <- log(deces18e$d18e/offsets$NETR2018GE)

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e18smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018GE),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018GE
f18ul.num <- exp(ul)*offsets$NFRA2018GE
f18logrates <- log(deces18f$d18f/offsets$NFRA2018GE)

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019GE),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019GE
e19ul.num <- exp(ul)*offsets$NETR2019GE
e19logrates <- log(deces19e$d19e/offsets$NETR2019GE)

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e19smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019GE),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019GE
f19ul.num <- exp(ul)*offsets$NFRA2019GE
f19logrates <- log(deces19f$d19f/offsets$NFRA2019GE)

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020GE),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020GE
e20ul.num <- exp(ul)*offsets$NETR2020GE
e20logrates <- log(deces20e$d20e/offsets$NETR2020GE)

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020GE),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020GE
f20ul.num <- exp(ul)*offsets$NFRA2020GE
f20logrates <- log(deces20f$d20f/offsets$NFRA2020GE)

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

#les plots mais zoom?s aux grand ?ges####
tiff("Taux de mortalit? origine 50 ans et + GE.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015GE),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015GE
e15ul.num <- exp(ul)*offsets$NETR2015GE
e15logrates <- deces15e$d15e/offsets$NETR2015GE*10000

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015GE),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015GE
f15ul.num <- exp(ul)*offsets$NFRA2015GE
f15logrates <- deces15f$d15f/offsets$NFRA2015GE*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016GE),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016GE
e16ul.num <- exp(ul)*offsets$NETR2016GE
e16logrates <- deces16e$d16e/offsets$NETR2016GE*10000

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016GE),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016GE
f16ul.num <- exp(ul)*offsets$NFRA2016GE
f16logrates <- deces16f$d16f/offsets$NFRA2016GE*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017GE),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017GE
e17ul.num <- exp(ul)*offsets$NETR2017GE
e17logrates <- deces17e$d17e/offsets$NETR2017GE*10000

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017GE),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017GE
f17ul.num <- exp(ul)*offsets$NFRA2017GE
f17logrates <- deces17f$d17f/offsets$NFRA2017GE*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018GE),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018GE
e18ul.num <- exp(ul)*offsets$NETR2018GE
e18logrates <- deces18e$d18e/offsets$NETR2018GE*10000

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018GE),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018GE
f18ul.num <- exp(ul)*offsets$NFRA2018GE
f18logrates <- deces18f$d18f/offsets$NFRA2018GE*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019GE),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019GE
e19ul.num <- exp(ul)*offsets$NETR2019GE
e19logrates <- deces19e$d19e/offsets$NETR2019GE*10000

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019GE),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019GE
f19ul.num <- exp(ul)*offsets$NFRA2019GE
f19logrates <- deces19f$d19f/offsets$NFRA2019GE*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020GE),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020GE
e20ul.num <- exp(ul)*offsets$NETR2020GE
e20logrates <- deces20e$d20e/offsets$NETR2020GE*10000

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020GE),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020GE
f20ul.num <- exp(ul)*offsets$NFRA2020GE
f20logrates <- deces20f$d20f/offsets$NFRA2020GE*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
#les plots mais zoom?s aux grand ?ges avec ?chelle log####
tiff("Taux de mortalit? origine 50 ans et + LOG GE.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015GE),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015GE
e15ul.num <- exp(ul)*offsets$NETR2015GE
e15logrates <- deces15e$d15e/offsets$NETR2015GE*10000

plot(ages, e15logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015GE),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015GE
f15ul.num <- exp(ul)*offsets$NFRA2015GE
f15logrates <- deces15f$d15f/offsets$NFRA2015GE*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016GE),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016GE
e16ul.num <- exp(ul)*offsets$NETR2016GE
e16logrates <- deces16e$d16e/offsets$NETR2016GE*10000

plot(ages, e16logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016GE),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016GE
f16ul.num <- exp(ul)*offsets$NFRA2016GE
f16logrates <- deces16f$d16f/offsets$NFRA2016GE*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017GE),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017GE
e17ul.num <- exp(ul)*offsets$NETR2017GE
e17logrates <- deces17e$d17e/offsets$NETR2017GE*10000

plot(ages, e17logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017GE),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017GE
f17ul.num <- exp(ul)*offsets$NFRA2017GE
f17logrates <- deces17f$d17f/offsets$NFRA2017GE*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018GE),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018GE
e18ul.num <- exp(ul)*offsets$NETR2018GE
e18logrates <- deces18e$d18e/offsets$NETR2018GE*10000

plot(ages, e18logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018GE),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018GE
f18ul.num <- exp(ul)*offsets$NFRA2018GE
f18logrates <- deces18f$d18f/offsets$NFRA2018GE*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019GE),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019GE
e19ul.num <- exp(ul)*offsets$NETR2019GE
e19logrates <- deces19e$d19e/offsets$NETR2019GE*10000

plot(ages, e19logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019GE),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019GE
f19ul.num <- exp(ul)*offsets$NFRA2019GE
f19logrates <- deces19f$d19f/offsets$NFRA2019GE*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020GE),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020GE
e20ul.num <- exp(ul)*offsets$NETR2020GE
e20logrates <- deces20e$d20e/offsets$NETR2020GE*10000

plot(ages, e20logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020GE),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020GE
f20ul.num <- exp(ul)*offsets$NFRA2020GE
f20logrates <- deces20f$d20f/offsets$NFRA2020GE*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

#surmortalit? semaine ####

dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,semdec)
write.csv2(indicesem,"semaineGE.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par semaine GE",".tiff"), units="in", width=28, height=10, res=780)

p <- ggplot(indicesem,aes(x=semdec, y=indice,linetype=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="R?gions",values=c("grey40","blue","red","#8AC926","#c42a19"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  scale_x_discrete(limits=c("Semaine du 2 mars (10)", 
                            "Semaine du 9 mars (11)",
                            "Semaine du 16 mars (12)",
                            "Semaine du 23 mars (13)",
                            "Semaine du 30 mars (14)",
                            "Semaine du 6 avril (15)",
                            "Semaine du 13 avril (16)",
                            "Semaine du 20 avril (17)",
                            "Semaine du 27 avril (18)",
                            "Semaine du 4 mai (19)"))+
  ylab("Indice de surmortalit?")+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(
    color=F,
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(p)

dev.off()

#surmortalit? ?ge####


dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,agedec)
write.csv2(indicesem,"ageGE.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par ?ge GE",".tiff"), units="in", width=28, height=10, res=780)

pagege <- ggplot(indicesem,aes(x=agedec, y=indice,color=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("#06D6A0","#ef476f"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  ggtitle("Grand Est")+
  ylab("Indice de surmortalit?")+
  ylim(0,5)+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2),
         size= FALSE)
print(pagege)

dev.off()


#Reprise de l'ensemble pour l'Ile-de-France####

deces <- deces_sauv

offsets <- read_excel("C:/Users/irwin/OneDrive/Donn?es stats/deces/offsets.xlsx",sheet = 2)

deces <- deces %>% 
  mutate(anaiss=substr(datenaiss,1,4),
         adec=substr(datedeces,1,4),
         agedec=(as.numeric(adec)-as.numeric(anaiss)),
         agedec=ifelse(as.numeric(agedec)>=100,100,agedec),
         depdec=substr(lieudeces,1,2),
         depnaiss=substr(lieunaiss,1,2),
         moisnaiss=substr(datenaiss,5,6),
         moisdec=substr(datedeces,5,6),
         jdec=substr(datedeces,7,8),
         filtre=paste0(substr(moisdec,2,3),substr(jdec,1,3)),
         jnaiss=substr(datenaiss,7,8),
         datedeces=as.Date.character(datedeces,"%Y%m%d"),
         semdec=isoweek(datedeces),
         sexe=case_when(as.character(sexe)=="1"~"homme",
                        as.character(sexe)=="2"~"femme"),
         agedec=case_when(agedec>=100~100,
                          agedec<100~agedec),
         nomprenom=gsub("/","",nomprenom)) %>% 
  filter(semdec %in% c("10","11","12","13","14","15","16","17","18","19")&depdec %in% c("91",
                                                                                        "92",
                                                                                        "75",
                                                                                        "77",
                                                                                        "93",
                                                                                        "95",
                                                                                        "94",
                                                                                        "78")) %>% 
  select(nomprenom,sexe,agedec,datenaiss,anaiss,moisnaiss,jnaiss,lieunaiss,depnaiss,commnaiss,paysnaiss,datedeces,adec,moisdec,jdec,semdec,lieudeces,depdec,actedeces) %>% 
  ungroup()
deces$paysnaiss[is.na(deces$paysnaiss)] <- "FRANCE"
deces <- deces %>% 
  mutate(naiss=case_when(paysnaiss=="FRANCE"|paysnaiss=="REUNION"|paysnaiss=="GUADELOUPE"|paysnaiss=="MARTINIQUE"|paysnaiss=="GUYANE"~"N?s en France",
                         paysnaiss=="ALGERIE"|paysnaiss=="MAROC"|paysnaiss=="TUNISIE"|paysnaiss=="EGYPTE"|paysnaiss=="LYBIE"~"N?s en Afrique du Nord",
                         TRUE~"N?s ? l'?tranger (hors Afrique du Nord)"),
         semdec=case_when(semdec=="10"~"Semaine du 2 mars (10)", 
                          semdec=="11"~"Semaine du 9 mars (11)",
                          semdec=="12"~"Semaine du 16 mars (12)",
                          semdec=="13"~"Semaine du 23 mars (13)",
                          semdec=="14"~"Semaine du 30 mars (14)",
                          semdec=="15"~"Semaine du 6 avril (15)",
                          semdec=="16"~"Semaine du 13 avril (16)",
                          semdec=="17"~"Semaine du 20 avril (17)",
                          semdec=="18"~"Semaine du 27 avril (18)",
                          semdec=="19"~"Semaine du 4 mai (19)")) %>% 
  filter(!(naiss=="N?s en France"))

ages <- sort(as.numeric(unique(deces$agedec)))
x <- seq(from = 0, to = 100, by = 5)
list_ages <- as.data.frame(ages)
setwd("C:/Users/irwin/OneDrive/Donn?es stats/deces/afrique du nord")
#### ann?e par ann?e ####

deces15f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15f=n()) #le nom de la variable est ? changer
deces15f <- merge(deces15f,list_ages,all=T,by.x="agedec",by.y="ages")
deces15f[is.na(deces15f)] <- 0

deces15e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15e=n()) #le nom de la variable est ? changer
deces15e <- merge(deces15e,list_ages,all=T,by.x="agedec",by.y="ages")
deces15e[is.na(deces15e)] <- 0


esmooth15 <- Mort1Dsmooth(deces15e$d15e, offsets$NETR2015IDF)

deces16f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16f=n()) #le nom de la variable est ? changer
deces16f <- merge(deces16f,list_ages,all=T,by.x="agedec",by.y="ages")
deces16f[is.na(deces16f)] <- 0

deces16e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16e=n()) #le nom de la variable est ? changer
deces16e <- merge(deces16e,list_ages,all=T,by.x="agedec",by.y="ages")
deces16e[is.na(deces16e)] <- 0


esmooth16 <- Mort1Dsmooth(deces16e$d16e, offsets$NETR2016IDF)

deces17f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17f=n()) #le nom de la variable est ? changer
deces17f <- merge(deces17f,list_ages,all=T,by.x="agedec",by.y="ages")
deces17f[is.na(deces17f)] <- 0

deces17e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17e=n()) #le nom de la variable est ? changer
deces17e <- merge(deces17e,list_ages,all=T,by.x="agedec",by.y="ages")
deces17e[is.na(deces17e)] <- 0


esmooth17 <- Mort1Dsmooth(deces17e$d17e, offsets$NETR2017IDF)

deces18f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18f=n()) #le nom de la variable est ? changer
deces18f <- merge(deces18f,list_ages,all=T,by.x="agedec",by.y="ages")
deces18f[is.na(deces18f)] <- 0

deces18e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18e=n()) #le nom de la variable est ? changer
deces18e <- merge(deces18e,list_ages,all=T,by.x="agedec",by.y="ages")
deces18e[is.na(deces18e)] <- 0


esmooth18 <- Mort1Dsmooth(deces18e$d18e, offsets$NETR2018IDF)

deces19f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19f=n()) #le nom de la variable est ? changer
deces19f <- merge(deces19f,list_ages,all=T,by.x="agedec",by.y="ages")
deces19f[is.na(deces19f)] <- 0

deces19e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19e=n()) #le nom de la variable est ? changer
deces19e <- merge(deces19e,list_ages,all=T,by.x="agedec",by.y="ages")
deces19e[is.na(deces19e)] <- 0


esmooth19 <- Mort1Dsmooth(deces19e$d19e, offsets$NETR2019IDF)

deces20f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20f=n()) #le nom de la variable est ? changer
deces20f <- merge(deces20f,list_ages,all=T,by.x="agedec",by.y="ages")
deces20f[is.na(deces20f)] <- 0

deces20e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20e=n()) #le nom de la variable est ? changer
deces20e <- merge(deces20e,list_ages,all=T,by.x="agedec",by.y="ages")
deces20e[is.na(deces20e)] <- 0

#### Smooth? ####
tiff("Taux de mortalit? ?chelle log IDF.tiff",width=40,height=30,units = "cm",res=780)

par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015IDF),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015IDF
e15ul.num <- exp(ul)*offsets$NETR2015IDF
e15logrates <- log(deces15e$d15e/offsets$NETR2015IDF)

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e15smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015IDF),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015IDF
f15ul.num <- exp(ul)*offsets$NFRA2015IDF
f15logrates <- log(deces15f$d15f/offsets$NFRA2015IDF)

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016IDF),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016IDF
e16ul.num <- exp(ul)*offsets$NETR2016IDF
e16logrates <- log(deces16e$d16e/offsets$NETR2016IDF)

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e16smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016IDF),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016IDF
f16ul.num <- exp(ul)*offsets$NFRA2016IDF
f16logrates <- log(deces16f$d16f/offsets$NFRA2016IDF)

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017IDF),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017IDF
e17ul.num <- exp(ul)*offsets$NETR2017IDF
e17logrates <- log(deces17e$d17e/offsets$NETR2017IDF)

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e17smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017IDF),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017IDF
f17ul.num <- exp(ul)*offsets$NFRA2017IDF
f17logrates <- log(deces17f$d17f/offsets$NFRA2017IDF)

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018IDF),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018IDF
e18ul.num <- exp(ul)*offsets$NETR2018IDF
e18logrates <- log(deces18e$d18e/offsets$NETR2018IDF)

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e18smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018IDF),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018IDF
f18ul.num <- exp(ul)*offsets$NFRA2018IDF
f18logrates <- log(deces18f$d18f/offsets$NFRA2018IDF)

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019IDF),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019IDF
e19ul.num <- exp(ul)*offsets$NETR2019IDF
e19logrates <- log(deces19e$d19e/offsets$NETR2019IDF)

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e19smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019IDF),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019IDF
f19ul.num <- exp(ul)*offsets$NFRA2019IDF
f19logrates <- log(deces19f$d19f/offsets$NFRA2019IDF)

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020IDF),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020IDF
e20ul.num <- exp(ul)*offsets$NETR2020IDF
e20logrates <- log(deces20e$d20e/offsets$NETR2020IDF)

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020IDF),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020IDF
f20ul.num <- exp(ul)*offsets$NFRA2020IDF
f20logrates <- log(deces20f$d20f/offsets$NFRA2020IDF)

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

#les plots mais zoom?s aux grand ?ges####
tiff("Taux de mortalit? origine 50 ans et + IDF.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015IDF),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015IDF
e15ul.num <- exp(ul)*offsets$NETR2015IDF
e15logrates <- deces15e$d15e/offsets$NETR2015IDF*10000

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015IDF),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015IDF
f15ul.num <- exp(ul)*offsets$NFRA2015IDF
f15logrates <- deces15f$d15f/offsets$NFRA2015IDF*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016IDF),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016IDF
e16ul.num <- exp(ul)*offsets$NETR2016IDF
e16logrates <- deces16e$d16e/offsets$NETR2016IDF*10000

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016IDF),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016IDF
f16ul.num <- exp(ul)*offsets$NFRA2016IDF
f16logrates <- deces16f$d16f/offsets$NFRA2016IDF*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017IDF),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017IDF
e17ul.num <- exp(ul)*offsets$NETR2017IDF
e17logrates <- deces17e$d17e/offsets$NETR2017IDF*10000

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017IDF),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017IDF
f17ul.num <- exp(ul)*offsets$NFRA2017IDF
f17logrates <- deces17f$d17f/offsets$NFRA2017IDF*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018IDF),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018IDF
e18ul.num <- exp(ul)*offsets$NETR2018IDF
e18logrates <- deces18e$d18e/offsets$NETR2018IDF*10000

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018IDF),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018IDF
f18ul.num <- exp(ul)*offsets$NFRA2018IDF
f18logrates <- deces18f$d18f/offsets$NFRA2018IDF*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019IDF),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019IDF
e19ul.num <- exp(ul)*offsets$NETR2019IDF
e19logrates <- deces19e$d19e/offsets$NETR2019IDF*10000

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019IDF),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019IDF
f19ul.num <- exp(ul)*offsets$NFRA2019IDF
f19logrates <- deces19f$d19f/offsets$NFRA2019IDF*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020IDF),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020IDF
e20ul.num <- exp(ul)*offsets$NETR2020IDF
e20logrates <- deces20e$d20e/offsets$NETR2020IDF*10000

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020IDF),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020IDF
f20ul.num <- exp(ul)*offsets$NFRA2020IDF
f20logrates <- deces20f$d20f/offsets$NFRA2020IDF*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
#les plots mais zoom?s aux grand ?ges avec ?chelle log####
tiff("Taux de mortalit? origine 50 ans et + LOG IDF.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015IDF),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015IDF
e15ul.num <- exp(ul)*offsets$NETR2015IDF
e15logrates <- deces15e$d15e/offsets$NETR2015IDF*10000

plot(ages, e15logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015IDF),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015IDF
f15ul.num <- exp(ul)*offsets$NFRA2015IDF
f15logrates <- deces15f$d15f/offsets$NFRA2015IDF*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016IDF),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016IDF
e16ul.num <- exp(ul)*offsets$NETR2016IDF
e16logrates <- deces16e$d16e/offsets$NETR2016IDF*10000

plot(ages, e16logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016IDF),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016IDF
f16ul.num <- exp(ul)*offsets$NFRA2016IDF
f16logrates <- deces16f$d16f/offsets$NFRA2016IDF*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017IDF),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017IDF
e17ul.num <- exp(ul)*offsets$NETR2017IDF
e17logrates <- deces17e$d17e/offsets$NETR2017IDF*10000

plot(ages, e17logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017IDF),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017IDF
f17ul.num <- exp(ul)*offsets$NFRA2017IDF
f17logrates <- deces17f$d17f/offsets$NFRA2017IDF*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018IDF),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018IDF
e18ul.num <- exp(ul)*offsets$NETR2018IDF
e18logrates <- deces18e$d18e/offsets$NETR2018IDF*10000

plot(ages, e18logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018IDF),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018IDF
f18ul.num <- exp(ul)*offsets$NFRA2018IDF
f18logrates <- deces18f$d18f/offsets$NFRA2018IDF*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019IDF),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019IDF
e19ul.num <- exp(ul)*offsets$NETR2019IDF
e19logrates <- deces19e$d19e/offsets$NETR2019IDF*10000

plot(ages, e19logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019IDF),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019IDF
f19ul.num <- exp(ul)*offsets$NFRA2019IDF
f19logrates <- deces19f$d19f/offsets$NFRA2019IDF*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020IDF),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020IDF
e20ul.num <- exp(ul)*offsets$NETR2020IDF
e20logrates <- deces20e$d20e/offsets$NETR2020IDF*10000

plot(ages, e20logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020IDF),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020IDF
f20ul.num <- exp(ul)*offsets$NFRA2020IDF
f20logrates <- deces20f$d20f/offsets$NFRA2020IDF*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

#faire hors GE et IDF
#c'est ? dire dans offsets faire pour ETR et FRA fracne -idf-ge chaque ann?e
#et dans le copier coller du code, changer les noms des tiff et changer le filtrer pour mettre !depidfetge
# et go sur la surmortalit?

#surmortalit? semaine ####

dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,semdec)
write.csv2(indicesem,"semaineIDF.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par semaine IDF",".tiff"), units="in", width=28, height=10, res=780)

p <- ggplot(indicesem,aes(x=semdec, y=indice,linetype=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("grey40","blue","red","#8AC926","#c42a19"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  scale_x_discrete(limits=c("Semaine du 2 mars (10)", 
                            "Semaine du 9 mars (11)",
                            "Semaine du 16 mars (12)",
                            "Semaine du 23 mars (13)",
                            "Semaine du 30 mars (14)",
                            "Semaine du 6 avril (15)",
                            "Semaine du 13 avril (16)",
                            "Semaine du 20 avril (17)",
                            "Semaine du 27 avril (18)",
                            "Semaine du 4 mai (19)"))+
  ylab("Indice de surmortalit?")+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(
    color=F,
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(p)

dev.off()

#surmortalit? ?ge####


dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,agedec)
write.csv2(indicesem,"ageIDF.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par ?ge IDF",".tiff"), units="in", width=28, height=10, res=780)

pageidf <- ggplot(indicesem,aes(x=agedec, y=indice,color=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("#06D6A0","#ef476f"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  ggtitle("?le-de-France")+
  ylab("Indice de surmortalit?")+
  ylim(0,5)+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2),
         size= FALSE)
print(pageidf)

dev.off()

#Reprise de l'ensemble pour la France hors R?gions du Grand Est et de l'Ile de France####

deces <- deces_sauv
offsets <- read_excel("C:/Users/irwin/OneDrive/Donn?es stats/deces/offsets.xlsx",sheet = 2)

deces <- deces %>% 
  mutate(anaiss=substr(datenaiss,1,4),
         adec=substr(datedeces,1,4),
         agedec=(as.numeric(adec)-as.numeric(anaiss)),
         agedec=ifelse(as.numeric(agedec)>=100,100,agedec),
         depdec=substr(lieudeces,1,2),
         depnaiss=substr(lieunaiss,1,2),
         moisnaiss=substr(datenaiss,5,6),
         moisdec=substr(datedeces,5,6),
         jdec=substr(datedeces,7,8),
         filtre=paste0(substr(moisdec,2,3),substr(jdec,1,3)),
         jnaiss=substr(datenaiss,7,8),
         datedeces=as.Date.character(datedeces,"%Y%m%d"),
         semdec=isoweek(datedeces),
         sexe=case_when(as.character(sexe)=="1"~"homme",
                        as.character(sexe)=="2"~"femme"),
         agedec=case_when(agedec>=100~100,
                          agedec<100~agedec),
         nomprenom=gsub("/","",nomprenom)) %>% 
  filter(semdec %in% c("10","11","12","13","14","15","16","17","18","19")& !(depdec %in% c("91",
                                                                                           "92",
                                                                                           "75",
                                                                                           "77",
                                                                                           "93",
                                                                                           "95",
                                                                                           "94",
                                                                                           "78","08","10","51","52","54","55","57","68","67","88"))) %>% 
  select(nomprenom,sexe,agedec,datenaiss,anaiss,moisnaiss,jnaiss,lieunaiss,depnaiss,commnaiss,paysnaiss,datedeces,adec,moisdec,jdec,semdec,lieudeces,depdec,actedeces) %>% 
  ungroup()
deces$paysnaiss[is.na(deces$paysnaiss)] <- "FRANCE"
deces <- deces %>% 
  mutate(naiss=case_when(paysnaiss=="FRANCE"|paysnaiss=="REUNION"|paysnaiss=="GUADELOUPE"|paysnaiss=="MARTINIQUE"|paysnaiss=="GUYANE"~"N?s en France",
                         paysnaiss=="ALGERIE"|paysnaiss=="MAROC"|paysnaiss=="TUNISIE"|paysnaiss=="EGYPTE"|paysnaiss=="LYBIE"~"N?s en Afrique du Nord",
                         TRUE~"N?s ? l'?tranger (hors Afrique du Nord)"),
         semdec=case_when(semdec=="10"~"Semaine du 2 mars (10)", 
                          semdec=="11"~"Semaine du 9 mars (11)",
                          semdec=="12"~"Semaine du 16 mars (12)",
                          semdec=="13"~"Semaine du 23 mars (13)",
                          semdec=="14"~"Semaine du 30 mars (14)",
                          semdec=="15"~"Semaine du 6 avril (15)",
                          semdec=="16"~"Semaine du 13 avril (16)",
                          semdec=="17"~"Semaine du 20 avril (17)",
                          semdec=="18"~"Semaine du 27 avril (18)",
                          semdec=="19"~"Semaine du 4 mai (19)")) %>% 
  filter(!(naiss=="N?s en France"))

ages <- sort(as.numeric(unique(deces$agedec)))
x <- seq(from = 0, to = 100, by = 5)
list_ages <- as.data.frame(ages)
setwd("C:/Users/irwin/OneDrive/Donn?es stats/deces/afrique du nord")
#### ann?e par ann?e ####

deces15f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15f=n()) #le nom de la variable est ? changer
deces15f <- merge(deces15f,list_ages,all=T,by.x="agedec",by.y="ages")
deces15f[is.na(deces15f)] <- 0

deces15e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2015")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d15e=n()) #le nom de la variable est ? changer
deces15e <- merge(deces15e,list_ages,all=T,by.x="agedec",by.y="ages")
deces15e[is.na(deces15e)] <- 0


esmooth15 <- Mort1Dsmooth(deces15e$d15e, offsets$NETR2015FRHDEP)

deces16f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16f=n()) #le nom de la variable est ? changer
deces16f <- merge(deces16f,list_ages,all=T,by.x="agedec",by.y="ages")
deces16f[is.na(deces16f)] <- 0

deces16e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2016")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d16e=n()) #le nom de la variable est ? changer
deces16e <- merge(deces16e,list_ages,all=T,by.x="agedec",by.y="ages")
deces16e[is.na(deces16e)] <- 0


esmooth16 <- Mort1Dsmooth(deces16e$d16e, offsets$NETR2016FRHDEP)

deces17f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17f=n()) #le nom de la variable est ? changer
deces17f <- merge(deces17f,list_ages,all=T,by.x="agedec",by.y="ages")
deces17f[is.na(deces17f)] <- 0

deces17e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2017")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d17e=n()) #le nom de la variable est ? changer
deces17e <- merge(deces17e,list_ages,all=T,by.x="agedec",by.y="ages")
deces17e[is.na(deces17e)] <- 0


esmooth17 <- Mort1Dsmooth(deces17e$d17e, offsets$NETR2017FRHDEP)

deces18f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18f=n()) #le nom de la variable est ? changer
deces18f <- merge(deces18f,list_ages,all=T,by.x="agedec",by.y="ages")
deces18f[is.na(deces18f)] <- 0

deces18e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2018")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d18e=n()) #le nom de la variable est ? changer
deces18e <- merge(deces18e,list_ages,all=T,by.x="agedec",by.y="ages")
deces18e[is.na(deces18e)] <- 0


esmooth18 <- Mort1Dsmooth(deces18e$d18e, offsets$NETR2018FRHDEP)

deces19f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19f=n()) #le nom de la variable est ? changer
deces19f <- merge(deces19f,list_ages,all=T,by.x="agedec",by.y="ages")
deces19f[is.na(deces19f)] <- 0

deces19e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2019")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d19e=n()) #le nom de la variable est ? changer
deces19e <- merge(deces19e,list_ages,all=T,by.x="agedec",by.y="ages")
deces19e[is.na(deces19e)] <- 0


esmooth19 <- Mort1Dsmooth(deces19e$d19e, offsets$NETR2019FRHDEP)

deces20f <- deces %>% #nom de la table ? changer
  filter(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE") ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20f=n()) #le nom de la variable est ? changer
deces20f <- merge(deces20f,list_ages,all=T,by.x="agedec",by.y="ages")
deces20f[is.na(deces20f)] <- 0

deces20e <- deces %>% #nom de la table ? changer
  filter(!(paysnaiss %in% c("FRANCE","REUNION","GUADELOUPE","MARTINIQUE","GUYANE")) ,adec %in% c("2020")) %>% #l'ann?e est ? changer
  arrange(agedec) %>% 
  group_by(agedec) %>% 
  summarise(d20e=n()) #le nom de la variable est ? changer
deces20e <- merge(deces20e,list_ages,all=T,by.x="agedec",by.y="ages")
deces20e[is.na(deces20e)] <- 0

#### Smooth? ####
tiff("Taux de mortalit? ?chelle log FRHDEP.tiff",width=40,height=30,units = "cm",res=780)

par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015FRHDEP),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015FRHDEP
e15ul.num <- exp(ul)*offsets$NETR2015FRHDEP
e15logrates <- log(deces15e$d15e/offsets$NETR2015FRHDEP)

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e15smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015FRHDEP),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015FRHDEP
f15ul.num <- exp(ul)*offsets$NFRA2015FRHDEP
f15logrates <- log(deces15f$d15f/offsets$NFRA2015FRHDEP)

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016FRHDEP),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016FRHDEP
e16ul.num <- exp(ul)*offsets$NETR2016FRHDEP
e16logrates <- log(deces16e$d16e/offsets$NETR2016FRHDEP)

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e16smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016FRHDEP),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016FRHDEP
f16ul.num <- exp(ul)*offsets$NFRA2016FRHDEP
f16logrates <- log(deces16f$d16f/offsets$NFRA2016FRHDEP)

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017FRHDEP),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017FRHDEP
e17ul.num <- exp(ul)*offsets$NETR2017FRHDEP
e17logrates <- log(deces17e$d17e/offsets$NETR2017FRHDEP)

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e17smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017FRHDEP),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017FRHDEP
f17ul.num <- exp(ul)*offsets$NFRA2017FRHDEP
f17logrates <- log(deces17f$d17f/offsets$NFRA2017FRHDEP)

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018FRHDEP),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018FRHDEP
e18ul.num <- exp(ul)*offsets$NETR2018FRHDEP
e18logrates <- log(deces18e$d18e/offsets$NETR2018FRHDEP)

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e18smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018FRHDEP),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018FRHDEP
f18ul.num <- exp(ul)*offsets$NFRA2018FRHDEP
f18logrates <- log(deces18f$d18f/offsets$NFRA2018FRHDEP)

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019FRHDEP),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019FRHDEP
e19ul.num <- exp(ul)*offsets$NETR2019FRHDEP
e19logrates <- log(deces19e$d19e/offsets$NETR2019FRHDEP)

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e19smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019FRHDEP),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019FRHDEP
f19ul.num <- exp(ul)*offsets$NFRA2019FRHDEP
f19logrates <- log(deces19f$d19f/offsets$NFRA2019FRHDEP)

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020FRHDEP),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020FRHDEP
e20ul.num <- exp(ul)*offsets$NETR2020FRHDEP
e20logrates <- log(deces20e$d20e/offsets$NETR2020FRHDEP)

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlab = "?ge",ylab = "Taux de mortalit?")
lines(ages, e20smo, col="#036b50",lwd=1)
lines(ages, ll, col="#06D6A0",lwd=1)
lines(ages, ul, col="#06D6A0",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020FRHDEP),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020FRHDEP
f20ul.num <- exp(ul)*offsets$NFRA2020FRHDEP
f20logrates <- log(deces20f$d20f/offsets$NFRA2020FRHDEP)

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20smo, col="#782438",lwd=1)
lines(ages, ll, col="#ef476f",lwd=1)
lines(ages, ul, col="#ef476f",lwd=1)
p2020 <- recordPlot()
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

#les plots mais zoom?s aux grand ?ges####
tiff("Taux de mortalit? origine 50 ans et + FRHDEP.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015FRHDEP),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015FRHDEP
e15ul.num <- exp(ul)*offsets$NETR2015FRHDEP
e15logrates <- deces15e$d15e/offsets$NETR2015FRHDEP*10000

plot(ages, e15logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015FRHDEP),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015FRHDEP
f15ul.num <- exp(ul)*offsets$NFRA2015FRHDEP
f15logrates <- deces15f$d15f/offsets$NFRA2015FRHDEP*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016FRHDEP),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016FRHDEP
e16ul.num <- exp(ul)*offsets$NETR2016FRHDEP
e16logrates <- deces16e$d16e/offsets$NETR2016FRHDEP*10000

plot(ages, e16logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016FRHDEP),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016FRHDEP
f16ul.num <- exp(ul)*offsets$NFRA2016FRHDEP
f16logrates <- deces16f$d16f/offsets$NFRA2016FRHDEP*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017FRHDEP),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017FRHDEP
e17ul.num <- exp(ul)*offsets$NETR2017FRHDEP
e17logrates <- deces17e$d17e/offsets$NETR2017FRHDEP*10000

plot(ages, e17logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017FRHDEP),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017FRHDEP
f17ul.num <- exp(ul)*offsets$NFRA2017FRHDEP
f17logrates <- deces17f$d17f/offsets$NFRA2017FRHDEP*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018FRHDEP),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018FRHDEP
e18ul.num <- exp(ul)*offsets$NETR2018FRHDEP
e18logrates <- deces18e$d18e/offsets$NETR2018FRHDEP*10000

plot(ages, e18logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018FRHDEP),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018FRHDEP
f18ul.num <- exp(ul)*offsets$NFRA2018FRHDEP
f18logrates <- deces18f$d18f/offsets$NFRA2018FRHDEP*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019FRHDEP),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019FRHDEP
e19ul.num <- exp(ul)*offsets$NETR2019FRHDEP
e19logrates <- deces19e$d19e/offsets$NETR2019FRHDEP*10000

plot(ages, e19logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019FRHDEP),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019FRHDEP
f19ul.num <- exp(ul)*offsets$NFRA2019FRHDEP
f19logrates <- deces19f$d19f/offsets$NFRA2019FRHDEP*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020FRHDEP),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020FRHDEP
e20ul.num <- exp(ul)*offsets$NETR2020FRHDEP
e20logrates <- deces20e$d20e/offsets$NETR2020FRHDEP*10000

plot(ages, e20logrates,col="black",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),ylim=c(0,500),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020FRHDEP),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020FRHDEP
f20ul.num <- exp(ul)*offsets$NFRA2020FRHDEP
f20logrates <- deces20f$d20f/offsets$NFRA2020FRHDEP*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020
#les plots mais zoom?s aux grand ?ges avec ?chelle log####
tiff("Taux de mortalit? origine 50 ans et + LOG FRHDEP.tiff",width=30,height=20,units = "cm",res=780)
par(mfrow = c(2, 3))
e15smooth <- Mort1Dsmooth(x = ages, y = deces15e$d15e, offset  = log(offsets$NETR2015FRHDEP),method=3,lambda=15)
e15smo <- e15smooth$logmortality
num <- e15smooth$fitted.values
pre <- predict(e15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e15ll.num <- exp(ll)*offsets$NETR2015FRHDEP
e15ul.num <- exp(ul)*offsets$NETR2015FRHDEP
e15logrates <- deces15e$d15e/offsets$NETR2015FRHDEP*10000

plot(ages, e15logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2015",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e15logrates, col="#036b50",lwd=1)

f15smooth <- Mort1Dsmooth(x = ages, y = deces15f$d15f, offset  = log(offsets$NFRA2015FRHDEP),method=3,lambda=15)
f15smo <- f15smooth$logmortality
num <- f15smooth$fitted.values
pre <- predict(f15smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f15ll.num <- exp(ll)*offsets$NFRA2015FRHDEP
f15ul.num <- exp(ul)*offsets$NFRA2015FRHDEP
f15logrates <- deces15f$d15f/offsets$NFRA2015FRHDEP*10000

points(ages, f15logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f15logrates, col="#782438",lwd=1)
p2015 <- recordPlot()

e16smooth <- Mort1Dsmooth(x = ages, y = deces16e$d16e, offset  = log(offsets$NETR2016FRHDEP),method=3,lambda=15)
e16smo <- e16smooth$logmortality
num <- e16smooth$fitted.values
pre <- predict(e16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e16ll.num <- exp(ll)*offsets$NETR2016FRHDEP
e16ul.num <- exp(ul)*offsets$NETR2016FRHDEP
e16logrates <- deces16e$d16e/offsets$NETR2016FRHDEP*10000

plot(ages, e16logrates,log="y",col="black",type = "p",pch=21,bg="#06D6A0",main = "2016",xlim = c(50,90) ,xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e16logrates, col="#036b50",lwd=1)

f16smooth <- Mort1Dsmooth(x = ages, y = deces16f$d16f, offset  = log(offsets$NFRA2016FRHDEP),method=3,lambda=15)
f16smo <- f16smooth$logmortality
num <- f16smooth$fitted.values
pre <- predict(f16smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f16ll.num <- exp(ll)*offsets$NFRA2016FRHDEP
f16ul.num <- exp(ul)*offsets$NFRA2016FRHDEP
f16logrates <- deces16f$d16f/offsets$NFRA2016FRHDEP*10000

points(ages, f16logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f16logrates, col="#782438",lwd=1)

p2016 <- recordPlot()

e17smooth <- Mort1Dsmooth(x = ages, y = deces17e$d17e, offset  = log(offsets$NETR2017FRHDEP),method=3,lambda=15)
e17smo <- e17smooth$logmortality
num <- e17smooth$fitted.values
pre <- predict(e17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e17ll.num <- exp(ll)*offsets$NETR2017FRHDEP
e17ul.num <- exp(ul)*offsets$NETR2017FRHDEP
e17logrates <- deces17e$d17e/offsets$NETR2017FRHDEP*10000

plot(ages, e17logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2017",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e17logrates, col="#036b50",lwd=1)

f17smooth <- Mort1Dsmooth(x = ages, y = deces17f$d17f, offset  = log(offsets$NFRA2017FRHDEP),method=3,lambda=15)
f17smo <- f17smooth$logmortality
num <- f17smooth$fitted.values
pre <- predict(f17smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f17ll.num <- exp(ll)*offsets$NFRA2017FRHDEP
f17ul.num <- exp(ul)*offsets$NFRA2017FRHDEP
f17logrates <- deces17f$d17f/offsets$NFRA2017FRHDEP*10000

points(ages, f17logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f17logrates, col="#782438",lwd=1)
p2017 <- recordPlot()

e18smooth <- Mort1Dsmooth(x = ages, y = deces18e$d18e, offset  = log(offsets$NETR2018FRHDEP),method=3,lambda=15)
e18smo <- e18smooth$logmortality
num <- e18smooth$fitted.values
pre <- predict(e18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e18ll.num <- exp(ll)*offsets$NETR2018FRHDEP
e18ul.num <- exp(ul)*offsets$NETR2018FRHDEP
e18logrates <- deces18e$d18e/offsets$NETR2018FRHDEP*10000

plot(ages, e18logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2018",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e18logrates, col="#036b50",lwd=1)

f18smooth <- Mort1Dsmooth(x = ages, y = deces18f$d18f, offset  = log(offsets$NFRA2018FRHDEP),method=3,lambda=15)
f18smo <- f18smooth$logmortality
num <- f18smooth$fitted.values
pre <- predict(f18smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f18ll.num <- exp(ll)*offsets$NFRA2018FRHDEP
f18ul.num <- exp(ul)*offsets$NFRA2018FRHDEP
f18logrates <- deces18f$d18f/offsets$NFRA2018FRHDEP*10000

points(ages, f18logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f18logrates, col="#782438",lwd=1)
p2018 <- recordPlot()

e19smooth <- Mort1Dsmooth(x = ages, y = deces19e$d19e, offset  = log(offsets$NETR2019FRHDEP),method=3,lambda=15)
e19smo <- e19smooth$logmortality
num <- e19smooth$fitted.values
pre <- predict(e19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e19ll.num <- exp(ll)*offsets$NETR2019FRHDEP
e19ul.num <- exp(ul)*offsets$NETR2019FRHDEP
e19logrates <- deces19e$d19e/offsets$NETR2019FRHDEP*10000

plot(ages, e19logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2019",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e19logrates, col="#036b50",lwd=1)

f19smooth <- Mort1Dsmooth(x = ages, y = deces19f$d19f, offset  = log(offsets$NFRA2019FRHDEP),method=3,lambda=15)
f19smo <- f19smooth$logmortality
num <- f19smooth$fitted.values
pre <- predict(f19smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f19ll.num <- exp(ll)*offsets$NFRA2019FRHDEP
f19ul.num <- exp(ul)*offsets$NFRA2019FRHDEP
f19logrates <- deces19f$d19f/offsets$NFRA2019FRHDEP*10000

points(ages, f19logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f19logrates, col="#782438",lwd=1)
p2019 <- recordPlot()

e20smooth <- Mort1Dsmooth(x = ages, y = deces20e$d20e, offset  = log(offsets$NETR2020FRHDEP),method=3,lambda=15)
e20smo <- e20smooth$logmortality
num <- e20smooth$fitted.values
pre <- predict(e20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
e20ll.num <- exp(ll)*offsets$NETR2020FRHDEP
e20ul.num <- exp(ul)*offsets$NETR2020FRHDEP
e20logrates <- deces20e$d20e/offsets$NETR2020FRHDEP*10000

plot(ages, e20logrates,col="black",log="y",type = "p",pch=21,bg="#06D6A0",main = "2020",xlim=c(50,90),xlab = "?ge",ylab = "Taux de mortalit? (pour 10 000)")
lines(ages, e20logrates, col="#036b50",lwd=1)

f20smooth <- Mort1Dsmooth(x = ages, y = deces20f$d20f, offset  = log(offsets$NFRA2020FRHDEP),method=3,lambda=15)
f20smo <- f20smooth$logmortality
num <- f20smooth$fitted.values
pre <- predict(f20smooth, se.fit=TRUE)
ll <- pre$fit - 1.966*pre$se.fit
ul <- pre$fit + 1.966*pre$se.fit
f20ll.num <- exp(ll)*offsets$NFRA2020FRHDEP
f20ul.num <- exp(ul)*offsets$NFRA2020FRHDEP
f20logrates <- deces20f$d20f/offsets$NFRA2020FRHDEP*10000

points(ages, f20logrates,col="black",type = "p",pch=21,bg="#ef476f")
lines(ages, f20logrates, col="#782438",lwd=1)

p2020 <- recordPlot()
print(p2020)
dev.off()

print(p2015)
# dev.off()
p2016
#dev.off()
p2017
#dev.off()
p2018
#dev.off()
p2019
# dev.off()
p2020

#faire hors GE et FRHDEP
#c'est ? dire dans offsets faire pour ETR et FRA fracne -FRHDEP-ge chaque ann?e
#et dans le copier coller du code, changer les noms des tiff et changer le filtrer pour mettre !depFRHDEPetge
# et go sur la surmortalit?

#surmortalit? semaine ####

dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,semdec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,semdec)
write.csv2(indicesem,"semaineFRHDEP.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par semaine FRHDEP",".tiff"), units="in", width=28, height=10, res=780)

p <- ggplot(indicesem,aes(x=semdec, y=indice,linetype=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("grey40","blue","red","#8AC926","#c42a19"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  scale_x_discrete(limits=c("Semaine du 2 mars (10)", 
                            "Semaine du 9 mars (11)",
                            "Semaine du 16 mars (12)",
                            "Semaine du 23 mars (13)",
                            "Semaine du 30 mars (14)",
                            "Semaine du 6 avril (15)",
                            "Semaine du 13 avril (16)",
                            "Semaine du 20 avril (17)",
                            "Semaine du 27 avril (18)",
                            "Semaine du 4 mai (19)"))+
  ylab("Indice de surmortalit?")+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(
    color=F,
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    size= FALSE)
print(p)

dev.off()

#surmortalit? ?ge####


dec20 <-  deces %>% 
  filter(adec %in% c("2020")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(dec20=n())


decmoy <-  deces %>% 
  filter(adec %in% c("2015","2016","2017","2018","2019")) %>% 
  group_by(naiss,agedec) %>% 
  summarise(decmoy=n()) %>% 
  mutate(decmoy=as.numeric(decmoy)/5)

decmoy <- merge(decmoy,dec20)
indicesem<- decmoy %>% 
  mutate(indice=dec20/decmoy) %>%
  arrange(naiss,agedec)
write.csv2(indicesem,"ageFRHDEP.csv")
rm(decmoy,dec20)

tiff(paste0("surmortalit? par ?ge FRHDEP",".tiff"), units="in", width=28, height=10, res=780)

pagefrhdep <- ggplot(indicesem,aes(x=agedec, y=indice,color=naiss,group=naiss)) + 
  geom_line(size=1.5) +
  scale_colour_manual(name="",values=c("#06D6A0","#ef476f"))+
  scale_linetype_manual(name=" ",values=c("solid","dotdash")) +
  theme_bw() +
  xlab(" ")+
  ylab("Indice de surmortalit?")+
  ylim(0,5)+
  ggtitle("Autres R?gions")+
  theme(text=element_text(family="Calibri"),legend.position = c(0.8, 0.72),panel.background = element_rect(fill="white", colour=NA),panel.grid.major.y = element_line(color="gray80"),legend.direction = "vertical",
        legend.text=element_text(family="Calibri",size = 22),         plot.margin = unit(c(1,1,1,1),"cm"),
        legend.key.width=unit(1.4, "cm"),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(family="Calibri",size = 20),
        plot.title = element_text(family="Calibri",size = 22, face = "bold"),
        axis.title = element_text(family="Calibri",face = "bold", color = "black",
                                  size = 20),
        axis.text.x = element_text(family="Calibri",face="bold",color = "black",
                                   size = 18),
        axis.text.y = element_text(family="Calibri",face="bold",color = "black",
                                   size = 25)) +
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2),
         size= FALSE)
print(pagefrhdep)

dev.off()

tiff(paste0("grid surmortalit? ?ge",".tiff"), units="in", width=25, height=35, res=780)
grid.arrange(pagege,pageidf,pagefrhdep,ncol=1,nrow=3)
dev.off()


# Fin