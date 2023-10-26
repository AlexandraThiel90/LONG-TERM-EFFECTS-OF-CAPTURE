rm(list=ls()) ##remove all files in R
library(lubridate)
library(lattice)
library(dplyr)
library(ggplot2)
library(nlme)
library(lme4)
library(sf)
library(leaflet)
library(tidyverse)
library(rgdal)
library(ggpubr)
library(sp)
library(geosphere)

options(max.print=500)
options(tibble.print_max = 50, tibble.print_min = 30)

##################   Billberies ##########################
fuchsfile <- ("C:/Users/borisf/Dropbox/LeadIsotopes/data/berries/Blabaer22.csv")

bdata <- read.csv(fuchsfile,header=F,sep=";",skip=2)

mbdata <- bdata  %>%  dplyr::select("BillBerrySqm"="V85","Date"="V12") %>% 
            dplyr::mutate(Year=(as.numeric(year(dmy(Date))))) %>% 
            dplyr::filter(!is.na(BillBerrySqm),Year %in% c(2009:2022))


ggplot(mbdata,aes(x=Year,y=BillBerrySqm))+
  geom_point()+geom_smooth(method="lm",se=F)
library(nlme)
m1 <- gls(BillBerrySqm ~ Year,data=mbdata, method="REML")
summary(m1)
offset <- data.frame(e = resid(m1))
offset <- offset %>% mutate(BillIndex=(e-min(e))/(max(e)-min(e)))
                    

Billberries <- cbind(mbdata,offset)      


####################   Lingonberries #######################################

fuchsfile <- ("C:/Users/borisf/Dropbox/LeadIsotopes/data/berries/Lingon22.csv")

bdata <- read.csv(fuchsfile,header=F,sep=";",skip=2)

mbdata <- bdata  %>%  dplyr::select("LingBerrySqm"="V85","Date"="V12") %>% 
  dplyr::mutate(Year=(as.numeric(year(dmy(Date))))) %>% 
  dplyr::filter(!is.na(LingBerrySqm),Year %in% c(2009:2022))


ggplot(mbdata,aes(x=Year,y=LingBerrySqm))+
  geom_point()+geom_smooth(method="lm",se=F)
library(nlme)
m1 <- gls(LingBerrySqm ~ Year,data=mbdata, method="REML")
summary(m1)
offset <- data.frame(e = resid(m1))
offset <- offset %>% mutate(LingIndex=(e-min(e))/(max(e)-min(e)))


Lingberries <- cbind(mbdata,offset)  

Lingberries[14,] <- c("Na","Na",2018,"Na","Na")
Lingberries[,3] <- as.numeric(Lingberries[,3])

############################################################################

berries <- Billberries %>% right_join(Lingberries, by="Year") %>% 
  dplyr::select(Year,BillIndex,LingIndex)


write.table(berries, "C:/Users/borisf/Dropbox/LeadIsotopes/data/berries/scaled.dat",
            row.names = F, sep=";" )                  
