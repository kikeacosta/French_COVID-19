#1# #Creation of the database

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

options(OutDec=",") #On choisit la virgule comme separateur des decimales
options(digits = 4) #On choisit d arrondir les resultats a un chiffre apres la virgule
options(scipen = 999) #On choisit d'empecher l affichage des resultats en ecriture scientifique

deces2020M1 <- read_delim("Data/deces_2020_M01.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M2 <- read_delim("Data/deces_2020_M02.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M3 <- read_delim("Data/deces_2020_M03.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M4 <- read_delim("Data/deces_2020_M04.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M5 <- read_delim("Data/deces_2020_M05.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M6 <- read_delim("Data/deces_2020_M06.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M7 <- read_delim("Data/deces_2020_M07.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M8 <- read_delim("Data/deces_2020_M08.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces2020M9 <- read_delim("Data/deces_2020_M09.csv", ";", escape_double = FALSE, trim_ws = TRUE)
deces_2015 <- read_delim("Data/deces-2015.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
deces_2016 <- read_delim("Data/deces-2016.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
deces_2017 <- read_delim("Data/deces-2017.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
deces_2018 <- read_delim("Data/deces-2018.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
deces_2019 <- read_delim("Data/deces-2019.csv",  ";", escape_double = FALSE, trim_ws = TRUE)

deaths <- rbind(deces2020M1,
               deces2020M2,
               deces2020M3,
               deces2020M4,
               deces2020M5,
               deces2020M6,
               deces2020M7,
               deces2020M8,
               deces2020M9,
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
   deces2020M6,
   deces2020M7,
   deces2020M8,
   deces2020M9,
   deces_2015,
   deces_2016,
   deces_2017,
   deces_2018,
   deces_2019)

write_rds(deaths,"Data/1_deaths.rds")
