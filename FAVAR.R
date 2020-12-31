########################################################################################
##                                                                
## Factor-Augmented VAR Models - Otto Dahlin
##
########################################################################################

library(writexl)
library(ggplot2)
library(dplyr)
library(AER)
library(lmtest)
library(tseries)
library(urca)
library(dynlm)
library(sandwich)
library(readxl)
library(xts)
library(vars)
library(zoo)
library(timeSeries)
library(quantmod)
library(mFilter)
library(seasonal)
library(lubridate)
library(dplyr)


########################################################################################
getwd()
data1 <- read_excel("cleandata2.xlsx")
View(data1)
str(data1)

CPI<- data1[,"CPI"]
class(CPI)
CPI_ts <- ts(CPI, frequency = 4, start=c(1996,1), end=c(2019,4))
CPI_ts
class(CPI_ts)
ts.plot(CPI_ts, col="darkgreen", lwd=2)

CPI_ts

CPI_ts_season = ts(CPI_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(CPI_ts_season, main="CPI")

decompose_CPI <- decompose(CPI_ts_season, "multiplicative")
decompose_CPI
adjust_CPI <- CPI_ts/decompose_CPI$seasonal
plot(adjust_CPI , col="red", lwd=1, main="Seasonally Adjusted")

adjust.CPI

d.log.adjust_CPI <- diff(log(adjust_CPI))
ur.kpss(d.log.adjust_CPI, type = "tau")@teststat
ur.kpss(d.log.adjust_CPI, type ="tau")@cval

#FINAL: 
CPI <- ts(d.log.adjust_CPI, frequency = 4, start=c(1996,1), end=c(2019,4))
CPI



########################################################################################



CPIDRYCK<- data1[,"CPIDRYCK"]
class(CPIDRYCK)
CPIDRYCK_ts <- ts(CPIDRYCK, start=c(1996,1), end=c(2019,4), frequency = 4)
CPIDRYCK_ts
class(CPIDRYCK_ts)
ts.plot(CPIDRYCK_ts, col="darkgreen", lwd=2)


CPIDRYCK_ts_season = ts(CPIDRYCK_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(CPIDRYCK_ts_season, main="CPIDRYCK")

decompose_CPIDRYCK <- decompose(CPIDRYCK_ts_season, "multiplicative")
decompose_CPIDRYCK
adjust_CPIDRYCK <- CPIDRYCK_ts/decompose_CPIDRYCK$seasonal
plot(adjust_CPIDRYCK , col="red", lwd=1, main="Seasonally Adjusted")

adjust_CPIDRYCK

d.log.adjust_CPIDRYCK <- diff(log(adjust_CPIDRYCK))
ur.kpss(d.log.adjust_CPIDRYCK, type = "tau")@teststat
ur.kpss(d.log.adjust_CPIDRYCK, type ="tau")@cval

#FINAL: 
CPIDRYCK <- ts(d.log.adjust_CPIDRYCK, frequency = 4, start=c(1996,1), end=c(2019,4))
CPIDRYCK




########################################################################################

CPIBOENDE<- data1[,"CPIBOENDE"]
class(CPIBOENDE)
CPIBOENDE_ts <- ts(CPIBOENDE, start=c(1996,1), end=c(2019,4), frequency = 4)
CPIBOENDE_ts
class(CPIBOENDE_ts)
ts.plot(CPIBOENDE_ts, col="darkgreen", lwd=2)


CPIBOENDE_ts_season = ts(CPIBOENDE_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(CPIBOENDE_ts_season, main="CPIBOENDE")

decompose_CPIBOENDE <- decompose(CPIBOENDE_ts_season, "multiplicative")
decompose_CPIBOENDE
adjust_CPIBOENDE <- CPIBOENDE_ts/decompose_CPIBOENDE$seasonal
plot(adjust_CPIBOENDE , col="red", lwd=1, main="Seasonally Adjusted")

adjust_CPIBOENDE

d.log.adjust_CPIBOENDE <- diff(log(adjust_CPIBOENDE))
ur.kpss(d.log.adjust_CPIBOENDE, type = "tau")@teststat
ur.kpss(d.log.adjust_CPIBOENDE, type ="tau")@cval

#FINAL: 
CPIBOENDE <- ts(d.log.adjust_CPIBOENDE, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(CPIBOENDE)


########################################################################################
CPIINVENT<- data1[,"CPIINVENT"]
CPIINVENT_ts <- ts(CPIINVENT, start=c(1996,1), end=c(2019,4), frequency = 4)
CPIINVENT_ts
class(CPIINVENT_ts)
ts.plot(CPIINVENT_ts, col="darkgreen", lwd=2)


CPIINVENT_ts_season = ts(CPIINVENT_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(CPIINVENT_ts_season, main="CPIBOENDE")

decompose_CPIINVENT <- decompose(CPIINVENT_ts_season, "multiplicative")
decompose_CPIINVENT
adjust_CPIINVENT <- CPIINVENT_ts/decompose_CPIINVENT$seasonal
plot(adjust_CPIINVENT , col="red", lwd=1, main="Seasonally Adjusted")

adjust_CPIINVENT

d.log.adjust_CPIINVENT<- diff(log(adjust_CPIINVENT))
ur.kpss(d.log.adjust_CPIINVENT, type = "tau")@teststat
ur.kpss(d.log.adjust_CPIINVENT, type ="tau")@cval

#FINAL: 
CPIINVENT <- ts(d.log.adjust_CPIINVENT, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(CPIINVENT)

########################################################################################

GRUVOR<- data1[,"GRUVOR"]
GRUVOR_ts <- ts(GRUVOR, start=c(1996,1), end=c(2019,4), frequency = 4)


GRUVOR_ts_season = ts(GRUVOR_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(GRUVOR_ts_season, main="GRUVOR")

decompose_GRUVOR <- decompose(GRUVOR_ts_season, "multiplicative")
decompose_GRUVOR
adjust_GRUVOR <- GRUVOR_ts/decompose_GRUVOR$seasonal
plot(adjust_GRUVORT , col="red", lwd=1, main="Seasonally Adjusted")

adjust_GRUVOR

log.adjust_GRUVOR<- log(adjust_GRUVOR)
ur.kpss(log.adjust_GRUVOR, type = "tau")@teststat
ur.kpss(log.adjust_GRUVOR, type ="tau")@cval

#FINAL: 
GRUVOR <- ts(log.adjust_GRUVOR, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(GRUVOR)
GRUVOR

#######################################################################################

INSATSVAROR<- data1[,"INSATSVAROR"]
INSATSVAROR_ts <- ts(INSATSVAROR, start=c(1996,1), end = c(2019,4), frequency = 4)
INSATSVAROR_ts

INSATSVAROR_ts_season = ts(INSATSVAROR_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(INSATSVAROR_ts_season, main="INSATSVAROR")

decompose_INSATSVAROR <- decompose(INSATSVAROR_ts_season, "multiplicative")
decompose_INSATSVAROR
adjust_INSATSVAROR<- INSATSVAROR_ts/decompose_INSATSVAROR$seasonal
plot(adjust_INSATSVAROR , col="red", lwd=1, main="Seasonally Adjusted")

adjust_INSATSVAROR

d.log.adjust_INSATSVAROR<- diff(log(adjust_INSATSVAROR))
ur.kpss(d.log.adjust_INSATSVAROR, type = "tau")@teststat
ur.kpss(d.log.adjust_INSATSVAROR, type ="tau")@cval

#FINAL: 
INSATSVAROR <- ts(d.log.adjust_INSATSVAROR, frequency = 4, start=c(1994,1), end=c(2019,4))
ts.plot(INSATSVAROR)
INSATSVAROR

#######################################################################################

INDUSTRI <- data1[,"INDUSTRI"]
INDUSTRI_ts <- ts(INDUSTRI, start=c(1995,1), end = c(2019,4), frequency = 4)
INDUSTRI_ts

INDUSTRI_ts_season = ts(INDUSTRI_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(INDUSTRI_ts_season, main="INSATSVAROR")


decompose_INDUSTRI <- decompose(INDUSTRI_ts_season, "multiplicative")
decompose_INDUSTRI
adjust_INDUSTRI<- INDUSTRI_ts/decompose_INDUSTRI$seasonal
plot(adjust_INDUSTRI , col="red", lwd=1, main="Seasonally Adjusted")

adjust_INDUSTRI

d.log.adjust_INDUSTRI<- diff(log(adjust_INDUSTRI))
ur.kpss(d.log.adjust_INDUSTRI, type = "tau")@teststat
ur.kpss(d.log.adjust_INDUSTRI, type ="tau")@cval

#FINAL: 
INDUSTRI<- ts(d.log.adjust_INDUSTRI, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(INSATSVAROR)
INSATSVAROR



#######################################################################################

DRYCK <- data1[,"DRYCK"]
DRYCK_ts <- ts(DRYCK , start=c(1996,1), end = c(2019,4), frequency = 4)
DRYCK_ts


DRYCK_ts_season = ts(DRYCK_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(DRYCK_ts_season, main="DRYCK")


decompose_DRYCK<- decompose(DRYCK_ts_season, "multiplicative")
decompose_DRYCK
adjust_DRYCK<- DRYCK_ts/decompose_DRYCK$seasonal
plot(adjust_DRYCK , col="red", lwd=1, main="Seasonally Adjusted")

adjust_DRYCK

d.log.adjust_DRYCK<- diff(log(adjust_DRYCK))
ur.kpss(d.log.adjust_DRYCK, type = "tau")@teststat
ur.kpss(d.log.adjust_DRYCK, type ="tau")@cval

#FINAL: 
DRYCK<- ts(d.log.adjust_DRYCK, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(DRYCK)



#######################################################################################

MASSAINDUSTRI <- data1[,"MASSAINDUSTRI"]
MASSAINDUSTRI_ts <- ts(MASSAINDUSTRI , start=c(1996,1), end = c(2019,4), frequency = 4)


MASSAINDUSTRI_ts_season = ts(MASSAINDUSTRI_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(MASSAINDUSTRI_ts_season, main="DRYCK")


decompose_MASSAINDUSTRI<- decompose(MASSAINDUSTRI_ts_season, "multiplicative")
decompose_MASSAINDUSTRI
adjust_MASSAINDUSTRI<- MASSAINDUSTRI_ts/decompose_MASSAINDUSTRI$seasonal
plot(adjust_MASSAINDUSTRI , col="red", lwd=1, main="Seasonally Adjusted")

adjust_MASSAINDUSTRI

log.adjust_MASSAINDUSTRI<- log(adjust_MASSAINDUSTRI)
ur.kpss(log.adjust_MASSAINDUSTRI, type = "tau")@teststat
ur.kpss(log.adjust_MASSAINDUSTRI, type ="tau")@cval

#FINAL: 
MASSAINDUSTRI<- ts(log.adjust_MASSAINDUSTRI, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(MASSAINDUSTRI)

#######################################################################################

M1 <- data1[,"M1"]
M1_ts <- ts(M1, start=c(1996,1), end = c(2019,4), frequency = 4)

M1_ts_season = ts(M1_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(M1_ts_season, main="M1")


decompose_M1<- decompose(M1_ts_season, "multiplicative")
decompose_M1
adjust_M1<- M1_ts/decompose_M1$seasonal
plot(adjust_M1 , col="red", lwd=1, main="Seasonally Adjusted")

adjust_M1

d.log.adjust_M1<- diff(log(adjust_MASSAINDUSTRI))
ur.kpss(d.log.adjust_M1, type = "tau")@teststat
ur.kpss(d.log.adjust_M1, type ="tau")@cval

#FINAL: 
M1<- ts(d.log.adjust_M1, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(M1)

#######################################################################################

M3 <- data1[,"M3"]
M3_ts <- ts(M3, start=c(1996,1), end = c(2019,4), frequency = 4)
M3_ts


M3_ts_season = ts(M3_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(M3_ts_season, main="M3")


decompose_M3<- decompose(M3_ts_season, "multiplicative")
decompose_M3
adjust_M3<- M3_ts/decompose_M3$seasonal
plot(adjust_M3 , col="red", lwd=1, main="Seasonally Adjusted")

adjust_M3

d.log.adjust_M3<- diff(log(adjust_M3))
ur.kpss(d.log.adjust_M3, type = "tau")@teststat
ur.kpss(d.log.adjust_M3, type ="tau")@cval

#FINAL: 
M3<- ts(d.log.adjust_M3, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(M3)

#######################################################################################

MFIFOREIGN <- data1[,"MFIFOREIGN"]
MFIFOREIGN_ts <- ts(MFIFOREIGN, start=c(1996,1), end = c(2019,4), frequency = 4)
MFIFOREIGN_ts



MFIFOREIGN_ts_season = ts(MFIFOREIGN_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(MFIFOREIGN_ts_season, main="M3")


decompose_MFIFOREIGN<- decompose(MFIFOREIGN_ts_season, "multiplicative")
decompose_MFIFOREIGN
adjust_MFIFOREIGN<- MFIFOREIGN_ts/decompose_MFIFOREIGN$seasonal
plot(adjust_MFIFOREIGN , col="red", lwd=1, main="Seasonally Adjusted")

adjust_MFIFOREIGN

d.log.adjust_MFIFOREIGN <- diff(log(adjust_MFIFOREIGN))
ur.kpss(d.log.adjust_MFIFOREIGN, type = "tau")@teststat
ur.kpss(d.log.adjust_MFIFOREIGN, type ="tau")@cval

#FINAL: 
M3<- ts(d.log.adjust_MFIFOREIGN, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(MFIFOREIGN)


#######################################################################################


MFIUTLANING <- data1[,"MFIUTLANING"]
MFIUTLANING_ts <- ts(MFIUTLANING, start=c(1996,1), end = c(2019,4), frequency = 4)
MFIUTLANING_ts

MFIUTLANING_ts_season = ts(MFIUTLANING_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(MFIUTLANING_ts_season, main="MFIUTLANING")


decompose_MFIUTLANING<- decompose(MFIUTLANING_ts_season, "multiplicative")
decompose_MFIUTLANING
adjust_MFIUTLANING<- MFIUTLANING_ts/decompose_MFIUTLANING$seasonal
plot(adjust_MFIUTLANING, col="red", lwd=1, main="Seasonally Adjusted")

adjust_MFIUTLANING

d.log.adjust_MFIUTLANING <- diff(log(adjust_MFIUTLANING))
ur.kpss(d.log.adjust_MFIUTLANING, type = "tau")@teststat
ur.kpss(d.log.adjust_MFIUTLANING, type ="tau")@cval

#FINAL: 
MFIUTLANING<- ts(d.log.adjust_MFIUTLANING, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(MFIUTLANING)



#######################################################################################

MFIINLANING <- data1[,"MFIINLANING"]
MFIINLANING_ts <- ts(MFIINLANING, start=c(1996,1), end = c(2019,4), frequency = 4)
MFIINLANING_ts

MFIINLANING_ts_season = ts(MFIINLANING_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(MFIINLANING_ts_season, main="MFIINLANING")


decompose_MFIINLANING <- decompose(MFIINLANING_ts_season, "multiplicative")
decompose_MFIINLANING
adjust_MFIINLANING <- MFIINLANING_ts/decompose_MFIINLANING$seasonal
plot(adjust_MFIINLANING, col="red", lwd=1, main="Seasonally Adjusted")

adjust_MFIINLANING

d.log.adjust_MFIINLANING <- diff(log(adjust_MFIINLANING))
ur.kpss(d.log.adjust_MFIINLANING, type = "tau")@teststat
ur.kpss(d.log.adjust_MFIINLANING, type ="tau")@cval

#FINAL: 
MFIINLANING<- ts(d.log.adjust_MFIINLANING, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(MFIINLANING)

#######################################################################################

MFIOFFENTLIG <- data1[,"MFIOFFENTLIG"]
MFIOFFENTLIG_ts <- ts(MFIOFFENTLIG, start=c(1996,1), end = c(2019,4), frequency = 4)
MFIOFFENTLIG_ts



MFIOFFENTLIG_ts_season = ts(MFIOFFENTLIG_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(MFIOFFENTLIG_ts_season, main="MFIOFFENTLIG")


decompose_MFIOFFENTLIG <- decompose(MFIOFFENTLIG_ts_season, "multiplicative")
decompose_MFIOFFENTLIG
adjust_MFIOFFENTLIG <- MFIOFFENTLIG_ts/decompose_MFIOFFENTLIG$seasonal
plot(adjust_MFIOFFENTLIG, col="red", lwd=1, main="Seasonally Adjusted")

adjust_MFIOFFENTLIG

d.log.adjust_MFIOFFENTLIG <- diff(log(adjust_MFIOFFENTLIG))
ur.kpss(d.log.adjust_MFIOFFENTLIG, type = "tau")@teststat
ur.kpss(d.log.adjust_MFIOFFENTLIG, type ="tau")@cval

#FINAL: 
MFIOFFENTLIG <- ts(d.log.adjust_MFIOFFENTLIG, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(MFIOFFENTLIG)


#######################################################################################

TCW <- data1[,"TCW"]

TCW_ts <- ts(TCW, start=c(1996,1), end=c(2019,4), frequency = 4)
TCW_ts


TCW_ts_season = ts(TCW_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(TCW_ts_season)

decompose_TCW<- decompose(TCW_ts_season, "multiplicative")
decompose_TCW
adjust_TCW<- TCW_ts/decompose_TCW$seasonal
plot(adjust_TCW , col="red", lwd=1, main="Seasonally Adjusted")

adjust_TCW

d.log.adjust_TCW<- diff(log(adjust_TCW))
ur.kpss(d.log.adjust_TCW, type = "tau")@teststat
ur.kpss(d.log.adjust_TCW, type ="tau")@cval

#FINAL: 
TCW <- ts(d.log.adjust_TCW, frequency = 4, start=c(1996,1), end=c(2019,4))
TCW

#######################################################################################

SDR <- data1[,"SDR"]

SDR_ts <- ts(SDR, start=c(1996,1), end=c(2019,4), frequency = 4)
SDR_ts


SDR_ts_season = ts(SDR_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(SDR_ts_season)

decompose_SDR<- decompose(SDR_ts_season, "multiplicative")
decompose_SDR
adjust_SDR<- SDR_ts/decompose_SDR$seasonal
plot(adjust_SDR , col="red", lwd=1, main="Seasonally Adjusted")

adjust_SDR

d.log.adjust_SDR<- diff(log(adjust_SDR))
ur.kpss(d.log.adjust_SDR, type = "tau")@teststat
ur.kpss(d.log.adjust_SDR, type ="tau")@cval

#FINAL: 
SDR<- ts(d.log.adjust_SDR, frequency = 4, start=c(1996,1), end=c(2019,4))
SDR

#######################################################################################

KIX <- data1[,"KIX"]

KIX_ts <- ts(KIX, start=c(1996,1), end=c(2019,4), frequency = 4)
KIX_ts


KIX_ts_season = ts(KIX_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(KIX_ts_season)

decompose_KIX<- decompose(KIX_ts_season, "multiplicative")
decompose_KIX
adjust_KIX<- KIX_ts/decompose_KIX$seasonal
plot(adjust_KIX , col="red", lwd=1, main="Seasonally Adjusted")

adjust_KIX

d.log.adjust_KIX<- diff(log(adjust_KIX))
ur.kpss(d.log.adjust_KIX, type = "tau")@teststat
ur.kpss(d.log.adjust_KIX, type ="tau")@cval

#FINAL: 
KIX<- ts(d.log.adjust_KIX, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(KIX)


#######################################################################################
SEKUSD1MAN<- data1[,"SEKUSD1MAN"]

SEKUSD1MAN_ts <- ts(SEKUSD1MAN, start=c(1996,1), end=c(2019,4), frequency = 4)
SEKUSD1MAN_ts
class(SEKUSD1MAN_ts)
ts.plot(SEKUSD1MAN_ts, col="darkgreen", lwd=2)
SEKUSD1MAN_ts


SEKUSD1MAN_ts_season = ts(SEKUSD1MAN_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(SEKUSD1MAN_ts_season)

decompose_SEKUSD1MAN<- decompose(SEKUSD1MAN_ts_season, "multiplicative")
decompose_SEKUSD1MAN
adjust_SEKUSD1MAN<- SEKUSD1MAN_ts/decompose_SEKUSD1MAN$seasonal
plot(adjust_SEKUSD1MAN , col="red", lwd=1, main="Seasonally Adjusted")

adjust_SEKUSD1MAN


#FINAL: 
SEKUSD1MAN<- ts(adjust_SEKUSD1MAN, frequency = 4, start=c(1996,1), end=c(2019,4))
SEKUSD1MAN

#######################################################################################

US3MAN<- data1[,"US3MAN"]


US3MAN_ts <- ts(US3MAN, start=c(1996,1), end=c(2019,4), frequency = 4)
US3MAN_ts
class(US3MAN_ts)
ts.plot(US3MAN_ts, col="darkgreen", lwd=2)
US3MAN_ts


US3MAN_ts_season = ts(US3MAN_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(US3MAN_ts_season)

decompose_US3MAN<- decompose(US3MAN_ts_season, "multiplicative")
decompose_US3MAN
adjust_US3MAN<- US3MAN_ts/decompose_US3MAN$seasonal
plot(adjust_US3MAN , col="red", lwd=1, main="Seasonally Adjusted")

adjust_US3MAN


#FINAL: 
US3MAN<- ts(adjust_US3MAN, frequency = 4, start=c(1996,1), end=c(2019,4))
US3MAN


#######################################################################################
GB3MAN<- data1[,"GB3MAN"]


GB3MAN_ts <- ts(GB3MAN, start=c(1996,1), end=c(2019,4), frequency = 4)
GB3MAN_ts
class(GB3MAN_ts)
ts.plot(GB3MAN_ts, col="darkgreen", lwd=2)
GB3MAN_ts


GB3MAN_ts_season = ts(US3MAN_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(GB3MAN_ts_season)

decompose_GB3MAN<- decompose(GB3MAN_ts_season, "multiplicative")
decompose_GB3MAN
adjust_GB3MAN<- GB3MAN_ts/decompose_GB3MAN$seasonal
plot(adjust_GB3MAN , col="red", lwd=1, main="Seasonally Adjusted")

adjust_GB3MAN


#FINAL: 
GB3MAN <- ts(adjust_GB3MAN, frequency = 4, start=c(1996,1), end=c(2019,4))
GB3MAN


#######################################################################################

US5Y<- data1[,"US5Y"]

US5Y_ts <- ts(US5Y, start=c(1996,1), end=c(2019,4), frequency = 4)
US5Y_ts
class(US5Y_ts)
ts.plot(US5Y_ts, col="darkgreen", lwd=2)
US5Y_ts

US5Y_ts_season = ts(US5Y_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(US5Y_ts_season)

decompose_US5Y<- decompose(US5Y_ts_season, "multiplicative")
decompose_US5Y
adjust_US5Y<- US5Y_ts/decompose_US5Y$seasonal
plot(adjust_US5Y , col="red", lwd=1, main="Seasonally Adjusted")

adjust_US5Y

#FINAL: 
US5Y <- ts(adjust_US5Y, frequency = 4, start=c(1996,1), end=c(2019,4))
US5Y

#######################################################################################
DE5Y<- data1[,"DE5Y"]


DE5Y_ts <- ts(DE5Y, start=c(1996,1), end=c(2019,4), frequency = 4)
DE5Y_ts
class(DE5Y_ts)
ts.plot(DE5Y_ts, col="darkgreen", lwd=2)
DE5Y_ts


DE5Y_ts_season = ts(DE5Y_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(DE5Y_ts_season)

decompose_DE5Y<- decompose(DE5Y_ts_season, "multiplicative")
decompose_DE5Y
adjust_DE5Y<- DE5Y_ts/decompose_DE5Y$seasonal
plot(adjust_DE5Y , col="red", lwd=1, main="Seasonally Adjusted")

adjust_DE5Y

#FINAL: 
DE5Y <- ts(adjust_DE5Y, frequency = 4, start=c(1996,1), end=c(2019,4))
DE5Y

#######################################################################################

STIBOR1M <- data1[,"STIBOR1M"]


STIBOR1M_ts <- ts(STIBOR1M, start=c(1996,1), end=c(2019,4), frequency = 4)
STIBOR1M_ts
class(STIBOR1M_ts)
ts.plot(STIBOR1M_ts, col="darkgreen", lwd=2)
STIBOR1M_ts


STIBOR1M_ts_season = ts(STIBOR1M_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(STIBOR1M_ts_season)

decompose_STIBOR1M<- decompose(STIBOR1M_ts_season, "multiplicative")
decompose_STIBOR1M
adjust_STIBOR1M<- STIBOR1M_ts/decompose_STIBOR1M$seasonal
plot(adjust_STIBOR1M , col="red", lwd=1, main="Seasonally Adjusted")

adjust_STIBOR1M

#FINAL: 
STIBOR1M <- ts(adjust_STIBOR1M, frequency = 4, start=c(1996,1), end=c(2019,4))
STIBOR1M


#######################################################################################

STIBOR3M <- data1[,"STIBOR3M"]


STIBOR3M_ts <- ts(STIBOR3M, start=c(1996,1), end=c(2019,4), frequency = 4)
STIBOR3M_ts
class(STIBOR3M_ts)
ts.plot(STIBOR3M_ts, col="darkgreen", lwd=2)
STIBOR3M_ts


STIBOR3M_ts_season = ts(STIBOR3M_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(STIBOR3M_ts_season)

decompose_STIBOR3M<- decompose(STIBOR3M_ts_season, "multiplicative")
decompose_STIBOR3M
adjust_STIBOR3M<- STIBOR3M_ts/decompose_STIBOR3M$seasonal
plot(adjust_STIBOR3M , col="red", lwd=1, main="Seasonally Adjusted")

adjust_STIBOR3M

#FINAL: 
STIBOR3M <- ts(adjust_STIBOR3M, frequency = 4, start=c(1996,1), end=c(2019,4))
STIBOR3M
#######################################################################################

SSVX1M <- data1[,"SSVX1M"]


SSVX1M_ts <- ts(SSVX1M, start=c(1996,1), end=c(2019,4), frequency = 4)
SSVX1M_ts
class(SSVX1M_ts)
ts.plot(SSVX1M_ts, col="darkgreen", lwd=2)
SSVX1M_ts



SSVX1M_ts_season = ts(SSVX1M_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(SSVX1M_ts_season)

decompose_SSVX1M<- decompose(SSVX1M_ts_season, "multiplicative")
decompose_SSVX1M
adjust_SSVX1M<- SSVX1M_ts/decompose_SSVX1M$seasonal
plot(adjust_SSVX1M , col="red", lwd=1, main="Seasonally Adjusted")

adjust_SSVX1M

#FINAL: 
SSVX1M <- ts(adjust_SSVX1M, frequency = 4, start=c(1996,1), end=c(2019,4))
SSVX1M

#######################################################################################

SSVX6M <- data1[,"SSVX6M"]


SSVX6M_ts <- ts(SSVX6M, start=c(1996,1), end=c(2019,4), frequency = 4)
SSVX6M_ts
class(SSVX6M_ts)
ts.plot(SSVX6M_ts, col="darkgreen", lwd=2)
SSVX6M_ts


SSVX6M_ts_season = ts(SSVX6M_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(SSVX6M_ts_season)

decompose_SSVX6M<- decompose(SSVX6M_ts_season, "multiplicative")
decompose_SSVX6M
adjust_SSVX6M<- SSVX6M_ts/decompose_SSVX6M$seasonal
plot(adjust_SSVX6M , col="red", lwd=1, main="Seasonally Adjusted")

adjust_SSVX6M

#FINAL: 
SSVX6M <- ts(adjust_SSVX6M, frequency = 4, start=c(1996,1), end=c(2019,4))
SSVX6M
#######################################################################################

SEGVB2Y <- data1[,"SEGVB2Y"]


SEGVB2Y_ts <- ts(SEGVB2Y,  start=c(1996,1), end=c(2019,4), frequency = 4)
SEGVB2Y_ts
class(SEGVB2Y_ts)
ts.plot(SEGVB2Y_ts, col="darkgreen", lwd=2)
SEGVB2Y_ts


SEGVB2Y_ts_season = ts(SEGVB2Y_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(SEGVB2Y_ts_season)

decompose_SEGVB2Y<- decompose(SEGVB2Y_ts_season, "multiplicative")
decompose_SEGVB2Y
adjust_SEGVB2Y<- SEGVB2Y_ts/decompose_SEGVB2Y$seasonal
plot(adjust_SEGVB2Y , col="red", lwd=1, main="Seasonally Adjusted")

adjust_SEGVB2Y

#FINAL: 
SEGVB2Y <- ts(adjust_SEGVB2Y, frequency = 4, start=c(1996,1), end=c(2019,4))
SEGVB2Y

#######################################################################################

SEGVB10Y <- data1[,"SEGVB10Y"]


SEGVB10Y_ts <- ts(SEGVB10Y, start=c(1996,1), end=c(2019,4), frequency = 4)
SEGVB10Y_ts
class(SEGVB10Y_ts)
ts.plot(SEGVB10Y_ts, col="darkgreen", lwd=2)
SEGVB10Y_ts



SEGVB10Y_ts_season = ts(SEGVB10Y_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(SEGVB10Y_ts_season)

decompose_SEGVB10Y<- decompose(SEGVB10Y_ts_season, "multiplicative")
decompose_SEGVB10Y
adjust_SEGVB10Y<- SEGVB10Y_ts/decompose_SEGVB10Y$seasonal
plot(adjust_SEGVB10Y , col="red", lwd=1, main="Seasonally Adjusted")

adjust_SEGVB10Y

#FINAL: 
SEGVB10Y <- ts(adjust_SEGVB10Y, frequency = 4, start=c(1996,1), end=c(2019,4))
SEGVB10Y

#######################################################################################

STFIX3M <- data1[,"STFIX3M"]


STFIX3M_ts <- ts(STFIX3M, start=c(1996,1), end=c(2019,4), frequency = 4)
STFIX3M_ts
class(STFIX3M_ts)
ts.plot(STFIX3M_ts, col="darkgreen", lwd=2)
STFIX3M_ts


STFIX3M_ts_season = ts(STFIX3M_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(STFIX3M_ts_season)

decompose_STFIX3M<- decompose(STFIX3M_ts_season, "multiplicative")
decompose_STFIX3M
adjust_STFIX3M<- STFIX3M_ts/decompose_STFIX3M$seasonal
plot(adjust_STFIX3M , col="red", lwd=1, main="Seasonally Adjusted")

adjust_STFIX3M

#FINAL: 
STFIX3M<- ts(adjust_STFIX3M, frequency = 4, start=c(1996,1), end=c(2019,4))
STFIX3M


######################################################################################

STFIX6M <- data1[,"STFIX6M"]


STFIX6M_ts <- ts(STFIX6M, start=c(1996,1), end=c(2019,4), frequency = 4)
STFIX6M_ts
class(STFIX6M_ts)
ts.plot(STFIX6M_ts, col="darkgreen", lwd=2)
STFIX6M_ts

STFIX6M_ts_season = ts(STFIX6M_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(STFIX6M_ts_season)

decompose_STFIX6M<- decompose(STFIX6M_ts_season, "multiplicative")
decompose_STFIX6M
adjust_STFIX6M<- STFIX6M_ts/decompose_STFIX6M$seasonal
plot(adjust_STFIX6M , col="red", lwd=1, main="Seasonally Adjusted")

adjust_STFIX6M

#FINAL: 
STFIX6M<- ts(adjust_STFIX6M, frequency = 4, start=c(1996,1), end=c(2019,4))
STFIX6M


######################################################################################

EU5Y <- data1[,"EU5Y"]


EU5Y_ts <- ts(EU5Y, start=c(1996,1), end=c(2019,4), frequency = 4)
EU5Y_ts
class(EU5Y_ts)
ts.plot(EU5Y_ts, col="darkgreen", lwd=2)
EU5Y_ts


EU5Y_ts_season = ts(EU5Y_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(EU5Y_ts_season)

decompose_EU5Y<- decompose(EU5Y_ts_season, "multiplicative")
decompose_EU5Y
adjust_EU5Y<- EU5Y_ts/decompose_EU5Y$seasonal
plot(adjust_EU5Y, col="red", lwd=1, main="Seasonally Adjusted")

adjust_EU5Y

#FINAL: 
EU5Y<- ts(adjust_EU5Y, frequency = 4, start=c(1996,1), end=c(2019,4))
EU5Y

######################################################################################

REPORANTA <- data1[,"REPORANTA"]


REPORANTA_ts <- ts(REPORANTA, start=c(1996,1), end=c(2019,4), frequency = 4)
REPORANTA_ts
class(REPORANTA_ts)
ts.plot(REPORANTA_ts, col="darkgreen", lwd=2)
REPORANTA_ts


REPORANTA_ts_season = ts(REPORANTA_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(REPORANTA_ts_season)

decompose_REPORANTA<- decompose(REPORANTA_ts_season, "multiplicative")
decompose_REPORANTA
adjust_REPORANTA<- REPORANTA_ts/decompose_REPORANTA$seasonal
plot(adjust_REPORANTA, col="red", lwd=1, main="Seasonally Adjusted")

adjust_REPORANTA

#FINAL: 
REPORANTA<- ts(adjust_REPORANTA, frequency = 4, start=c(1996,1), end=c(2019,4))
REPORANTA



######################################################################################


# INDIKATORER från KI:

Insatsvaruindustri  <- data1[,"Insatsvaruindustri"]

Insatsvaruindustri_ts  <- ts(Insatsvaruindustri , start=c(1996,1), end=c(2019,4), frequency = 4)
Insatsvaruindustri_ts
ts.plot(Insatsvaruindustri_ts, col="darkgreen", lwd=2)



Insatsvaruindustri_ts_season = ts(Insatsvaruindustri_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(Insatsvaruindustri_ts_season)

decompose_Insatsvaruindustri<- decompose(Insatsvaruindustri_ts_season, "multiplicative")
decompose_Insatsvaruindustri
adjust_Insatsvaruindustri<- Insatsvaruindustri_ts/decompose_Insatsvaruindustri$seasonal
plot(adjust_Insatsvaruindustri, col="red", lwd=1, main="Seasonally Adjusted")

adjust_Insatsvaruindustri

d.log.adjust_Insatsvaruindustri<- diff(log(adjust_Insatsvaruindustri))
ur.kpss(d.log.adjust_Insatsvaruindustri, type = "tau")@teststat
ur.kpss(d.log.adjust_KIX, type ="tau")@cval

#FINAL: 
Insatsvaruindustri<- ts(d.log.adjust_Insatsvaruindustri, frequency = 4, start=c(1996,1), end=c(2019,4))
Insatsvaruindustri


##################################################################################


Investeringsvaruindustri  <- data1[,"Investeringsvaruindustri"]

Investeringsvaruindustri_ts  <- ts(Investeringsvaruindustri , start=c(1996,1), end=c(2019,4), frequency = 4)
Investeringsvaruindustri_ts
ts.plot(Investeringsvaruindustri_ts, col="darkgreen", lwd=2)



Investeringsvaruindustri_ts_season = ts(Investeringsvaruindustri_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(Investeringsvaruindustri_ts_season)

decompose_Investeringsvaruindustri<- decompose(Investeringsvaruindustri_ts_season, "multiplicative")
decompose_Investeringsvaruindustri
adjust_Investeringsvaruindustri <- Investeringsvaruindustri_ts/decompose_Investeringsvaruindustri$seasonal
plot(adjust_Investeringsvaruindustri, col="red", lwd=1, main="Seasonally Adjusted")

adjust_Investeringsvaruindustri


#FINAL: 
Investeringsvaruindustri<- ts(adjust_Investeringsvaruindustri, frequency = 4, start=c(1996,1), end=c(2019,4))
Investeringsvaruindustri

##################################################################################


Konsumtionsvaruindustri  <- data1[,"Konsumtionsvaruindustri"]

Konsumtionsvaruindustri_ts  <- ts(Konsumtionsvaruindustri , start=c(1996,1), end=c(2019,4), frequency = 4)
Konsumtionsvaruindustri_ts
ts.plot(Konsumtionsvaruindustri_ts, col="darkgreen", lwd=2)



Konsumtionsvaruindustri_ts_season = ts(Konsumtionsvaruindustri_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(Konsumtionsvaruindustri_ts_season)



#FINAL: 
Konsumtionsvaruindustri<- ts(Konsumtionsvaruindustri_ts_season, frequency = 4, start=c(1996,1), end=c(2019,4))
Konsumtionsvaruindustri

##################################################################################


Varaktigakonsumtionsvaror <- data1[,"Varaktigakonsumtionsvaror"]

Varaktigakonsumtionsvaror_ts  <- ts(Varaktigakonsumtionsvaror , start=c(1996,1), end=c(2019,4), frequency = 4)
Varaktigakonsumtionsvaror_ts
ts.plot(Varaktigakonsumtionsvaror_ts, col="darkgreen", lwd=2)


Varaktigakonsumtionsvaror_ts_season = ts(Varaktigakonsumtionsvaror_ts, frequency=4, start=c(1996,1), end=c(2019,4))
plot(Varaktigakonsumtionsvaror_ts_season)

decompose_Varaktigakonsumtionsvaror<- decompose(Varaktigakonsumtionsvaror_ts_season, "multiplicative")
decompose_Varaktigakonsumtionsvaror
adjust_Varaktigakonsumtionsvaror<- Varaktigakonsumtionsvaror_ts/decompose_Varaktigakonsumtionsvaror$seasonal
plot(adjust_Varaktigakonsumtionsvaror, col="red", lwd=1, main="Seasonally Adjusted")

adjust_Varaktigakonsumtionsvaror


#FINAL: 
Varaktigakonsumtionsvaror <- ts(adjust_Varaktigakonsumtionsvaror, frequency = 4, start=c(1996,1), end=c(2019,4))
Varaktigakonsumtionsvaror

##################################################################################


Livsmedelsindustri  <- data1[,"Livsmedelsindustri"]

Livsmedelsindustri  <- ts(Livsmedelsindustri  , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Livsmedelsindustri)
ts.plot(Livsmedelsindustri_ts, col="darkgreen", lwd=2)

##################################################################################


Textilindustri  <- data1[,"Textilindustri"]

Textilindustri   <- ts(Textilindustri   , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Textilindustri )

##################################################################################


Trävaruindustri  <- data1[,"Trävaruindustri"]

Trävaruindustri    <- ts(Trävaruindustri   , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Trävaruindustri  )

##################################################################################


Sågverk <- data1[,"Sågverk"]

Sågverk    <- ts(Sågverk  , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Sågverk)

##################################################################################


Massapappersindustri  <- data1[,"Massapappersindustri"]

Massapappersindustri    <- ts(Massapappersindustri   , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Massapappersindustri )

##################################################################################


Massaindustri   <- data1[,"Massaindustri"]

Massaindustri    <- ts(Massaindustri    , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Massaindustri )

##################################################################################


Pappersindustri    <- data1[,"Pappersindustri"]

Pappersindustri     <- ts(Pappersindustri     , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Pappersindustri )

##################################################################################


Grafiskindustri    <- data1[,"Grafiskindustri"]

Grafiskindustri     <- ts(Grafiskindustri     , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Grafiskindustri )

##################################################################################


Petroleumindustri    <- data1[,"Petroleumindustri"]

Petroleumindustri  <- ts(Petroleumindustri    , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Petroleumindustri )

##################################################################################


Kemiskindustri     <- data1[,"Kemiskindustri"]

Kemiskindustri   <- ts(Kemiskindustri    , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Kemiskindustri)

##################################################################################


Läkemedelsindustri     <- data1[,"Läkemedelsindustri"]

Läkemedelsindustri    <- ts(Läkemedelsindustri    , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Läkemedelsindustri )


##################################################################################


Stålmetallindustri     <- data1[,"Stålmetallindustri"]

Stålmetallindustri    <- ts(Stålmetallindustri, start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Stålmetallindustri)

##################################################################################


Metallvaruindustri      <- data1[,"Metallvaruindustri"]

Metallvaruindustri     <- ts(Metallvaruindustri , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Metallvaruindustri)

##################################################################################


Elektronikindustri      <- data1[,"Elektronikindustri"]

Elektronikindustri    <- ts(Elektronikindustri , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Elektronikindustri)


##################################################################################


Maskinindustri     <- data1[,"Maskinindustri"]

Maskinindustri    <- ts(Maskinindustri  , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Maskinindustri)

##################################################################################


Detaljhandel <- data1[,"Detaljhandel"]

Detaljhandel   <- ts(Detaljhandel  , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Detaljhandel)


##################################################################################


Partihandel <- data1[,"Partihandel"]

Partihandel    <- ts(Partihandel , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Partihandel)

##################################################################################


Barometerindikatorn <- data1[,"Barometerindikatorn"]

Barometerindikatorn    <- ts(Barometerindikatorn , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Barometerindikatorn)


Barometerindikatorn_ts_season = ts(Barometerindikatorn, frequency=4, start=c(1996,1), end=c(2019,4))
plot(Barometerindikatorn_ts_season)

decompose_Barometerindikatorn<- decompose(Barometerindikatorn_ts_season , "multiplicative")
decompose_Barometerindikatorn
adjust_Barometerindikatorn<- Barometerindikatorn/decompose_Barometerindikatorn$seasonal
plot(adjust_Barometerindikatorn , col="red", lwd=1, main="Seasonally Adjusted")

adjust_Barometerindikatorn

d.log.adjust_Barometerindikatorn<- diff(log(adjust_Barometerindikatorn))
ur.kpss(d.log.adjust_Barometerindikatorn, type = "tau")@teststat
ur.kpss(log.Kemiskindustri, type ="tau")@cval


#FINAL: 
Barometerindikatorn<- ts(d.log.adjust_Barometerindikatorn, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(Barometerindikatorn)


##################################################################################


Husbyggande <- data1[,"Husbyggande"]

Husbyggande    <- ts(Husbyggande , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Husbyggande)


Husbyggande_ts_season = ts(Husbyggande, frequency=4, start=c(1996,1), end=c(2019,4))
plot(Husbyggande_ts_season)

decompose_Husbyggande<- decompose(Husbyggande_ts_season , "multiplicative")
decompose_Husbyggande
adjust_Husbyggande<- Husbyggande/decompose_Husbyggande$seasonal
plot(adjust_Husbyggande , col="red", lwd=1, main="Seasonally Adjusted")

adjust_Husbyggande

d.log.adjust_Husbyggande <- diff(log(adjust_Husbyggande))
ur.kpss(d.log.adjust_Husbyggande, type = "tau")@teststat
ur.kpss(log.Kemiskindustri, type ="tau")@cval


#FINAL: 
Husbyggande <- ts(d.log.adjust_Husbyggande, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(Husbyggande)


##################################################################################


Konfidensindikator <- data1[,"Konfidensindikator hushåll"]

Konfidensindikator    <- ts(Konfidensindikator , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Konfidensindikator)


Konfidensindikator_ts_season = ts(Konfidensindikator, frequency=4, start=c(1996,1), end=c(2019,4))
plot(Konfidensindikator_ts_season)

decompose_Konfidensindikator <- decompose(Konfidensindikator_ts_season , "multiplicative")
decompose_Konfidensindikator
adjust_Konfidensindikator <- Konfidensindikator/decompose_Konfidensindikator$seasonal
plot(adjust_Konfidensindikator , col="red", lwd=1, main="Seasonally Adjusted")

adjust_Konfidensindikator

d.log.adjust_Konfidensindikator <- diff(log(adjust_Konfidensindikator))
ur.kpss(d.log.adjust_Konfidensindikator, type = "tau")@teststat
ur.kpss(log.Kemiskindustri, type ="tau")@cval


#FINAL: 
Konfidensindikator <- ts(d.log.adjust_Konfidensindikator, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(Konfidensindikator)



##################################################################################


Makroindexhushåll <- data1[,"Makroindex hushåll"]

Makroindexhushåll    <- ts(Makroindexhushåll , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Makroindexhushåll)


Makroindexhushåll_ts_season = ts(Makroindexhushåll, frequency=4, start=c(1996,1), end=c(2019,4))
plot(Makroindexhushåll_ts_season)

decompose_Makroindexhushåll<- decompose(Makroindexhushåll_ts_season , "multiplicative")
decompose_Makroindexhushåll

adjust_Makroindexhushåll<- Makroindexhushåll/decompose_Makroindexhushåll$seasonal
plot(adjust_Makroindexhushåll , col="red", lwd=1, main="Seasonally Adjusted")

adjust_Makroindexhushåll

d.log.adjust_Makroindexhushåll <- diff(log(adjust_Konfidensindikator))
ur.kpss(d.log.adjust_Makroindexhushåll, type = "tau")@teststat
ur.kpss(log.Kemiskindustri, type ="tau")@cval


#FINAL: 
Makroindexhushåll <- ts(d.log.adjust_Makroindexhushåll, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(Makroindexhushåll)


##################################################################################


Mikroindexhushåll <- data1[,"Mikroindex hushåll"]

Mikroindexhushåll    <- ts(Mikroindexhushåll , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Mikroindexhushåll)


Mikroindexhushåll_ts_season = ts(Mikroindexhushåll, frequency=4, start=c(1996,1), end=c(2019,4))
plot(Mikroindexhushåll_ts_season)

decompose_Mikroindexhushåll <- decompose(Mikroindexhushåll_ts_season , "multiplicative")
decompose_Mikroindexhushåll

adjust_Mikroindexhushåll <- Mikroindexhushåll/decompose_Mikroindexhushåll$seasonal
plot(adjust_Mikroindexhushåll , col="red", lwd=1, main="Seasonally Adjusted")

adjust_Mikroindexhushåll

d.log.adjust_Mikroindexhushåll <- diff(log(adjust_Mikroindexhushåll))
ur.kpss(d.log.adjust_Mikroindexhushåll, type = "tau")@teststat
ur.kpss(log.Kemiskindustri, type ="tau")@cval


#FINAL: 
Mikroindexhushålll <- ts(d.log.adjust_Mikroindexhushåll, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(Mikroindexhushåll)





##################################################################################



Sysselsättning <- data1[,"Sysselsättning"]

Sysselsättning    <- ts(Sysselsättning  , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Sysselsättning )


Sysselsättning_ts_season = ts(Sysselsättning , frequency=4, start=c(1996,1), end=c(2019,4))
plot(Sysselsättning_ts_season)

decompose_Sysselsättning  <- decompose(Sysselsättning_ts_season , "multiplicative")
decompose_Sysselsättning

adjust_Sysselsättning <- Sysselsättning/decompose_Sysselsättning$seasonal
plot(adjust_Sysselsättning , col="red", lwd=1, main="Seasonally Adjusted")

adjust_Sysselsättning

d.log.adjust_Sysselsättning <- diff(log(adjust_Sysselsättning))
ur.kpss(d.log.adjust_Sysselsättning, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning
        , type ="tau")@cval


#FINAL: 
Sysselsättning <- ts(d.log.adjust_Sysselsättning, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(Sysselsättning)




##################################################################################


Export <- data1[,"Export"]

Export    <- ts(Export  , start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Export)


Export_ts_season = ts(Export , frequency=4, start=c(1996,1), end=c(2019,4))
plot(Export_ts_season)

decompose_Export  <- decompose(Export_ts_season , "multiplicative")
decompose_Export

adjust_Export <- Export/decompose_Export$seasonal
plot(adjust_Export , col="red", lwd=1, main="Seasonally Adjusted")

adjust_Export

d.log.adjust_Export <- diff(log(adjust_Export))
ur.kpss(d.log.adjust_Export, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
Export <- ts(d.log.adjust_Export, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(Export)


##################################################################################


Import  <- data1[,"Import"]

Import  <- ts(Import, start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(Import )


Import_ts_season = ts(Import  , frequency=4, start=c(1996,1), end=c(2019,4))
plot(Import_ts_season)

decompose_Import   <- decompose(Import_ts_season , "multiplicative")
decompose_Import 

adjust_Import  <- Import /decompose_Import$seasonal
plot(adjust_Import  , col="red", lwd=1, main="Seasonally Adjusted")

adjust_Import 

d.log.adjust_Import  <- diff(log(adjust_Import))
ur.kpss(d.log.adjust_Import, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
Import <- ts(d.log.adjust_Import, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(Import)

##################################################################################


BNPMarknadspris  <- data1[,"BNPMarknadspris"]

BNPMarknadspris  <- ts(BNPMarknadspris, start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(BNPMarknadspris )


BNPMarknadspris <- ts(BNPMarknadspris, frequency=4, start=c(1996,1), end=c(2019,4))
BNPMarknadspris
ts.plot(BNPMarknadspris)
decompose_BNPMarknadspris <- decompose(BNPMarknadspris, "additive")
decompose_BNPMarknadspris
adjust_BNPMarknadspris <- BNPMarknadspris - decompose_BNPMarknadspris$seasonal
plot(adjust_BNPMarknadspris)

d.log.adjust_BNPMarknadspris  <- diff(log(adjust_BNPMarknadspris))
ur.kpss(d.log.adjust_BNPMarknadspris, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
BNPMarknadspris <- ts(d.log.adjust_BNPMarknadspris, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(BNPMarknadspris)

##################################################################################

# PRISER:

KPILivsmedel  <- data1[,"KPILivsmedel"]

KPILivsmedel <- ts(KPILivsmedel, start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(KPILivsmedel )


KPILivsmedel <- ts(KPILivsmedel, frequency=4, start=c(1996,1), end=c(2019,4))
KPILivsmedel
ts.plot(KPILivsmedel)
decompose_KPILivsmedel <- decompose(KPILivsmedel, "additive")
decompose_KPILivsmedel
adjust_KPILivsmedel <- KPILivsmedel - decompose_KPILivsmedel$seasonal
plot(adjust_KPILivsmedel)

d.log.adjust_KPILivsmedel <- diff(log(adjust_BNPMarknadspris))
ur.kpss(d.log.adjust_KPILivsmedel, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
KPILivsmedel <- ts(d.log.adjust_KPILivsmedel, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(KPILivsmedel)


##################################################################################

# PRISER:

KPIBoende  <- data1[,"KPIBoende"]

KPIBoende <- ts(KPIBoende, start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(KPIBoende)


KPIBoende <- ts(KPIBoende, frequency=4, start=c(1996,1), end=c(2019,4))
KPIBoende
ts.plot(KPIBoende)
decompose_KPIBoende<- decompose(KPIBoende, "additive")
decompose_KPIBoende
adjust_KPIBoende <- KPIBoende - decompose_KPIBoende$seasonal
plot(adjust_KPIBoende)

d.log.adjust_KPIBoende <- diff(log(adjust_KPIBoende))
ur.kpss(d.log.adjust_KPIBoende, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
KPIBoende <- ts(d.log.adjust_KPIBoende, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(KPIBoende)


##################################################################################

# PRISER:

KPIInventarier  <- data1[,"KPIInventarier"]

KPIInventarier <- ts(KPIInventarier, start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(KPIInventarier)


KPIInventarier <- ts(KPIInventarier, frequency=4, start=c(1996,1), end=c(2019,4))
KPIInventarier
ts.plot(KPIBoende)
decompose_KPIInventarier<- decompose(KPIInventarier, "additive")
decompose_KPIInventarier
adjust_KPIInventarier <- KPIInventarier - decompose_KPIInventarier$seasonal
plot(adjust_KPIInventarier)

d.log.adjust_KPIInventarier <- diff(log(adjust_KPIInventarier))
ur.kpss(d.log.adjust_KPIInventarier, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
KPIInventarier <- ts(d.log.adjust_KPIInventarier, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(KPIInventarier)


##################################################################################

# PRISER:

KPIHälsa  <- data1[,"KPIHälsa"]

KPIHälsa <- ts(KPIHälsa, start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(KPIHälsa)


KPIHälsa <- ts(KPIHälsa, frequency=4, start=c(1996,1), end=c(2019,4))
KPIHälsa
ts.plot(KPIHälsa)
decompose_KPIHälsa<- decompose(KPIHälsa, "additive")
decompose_KPIHälsa
adjust_KPIHälsa <- KPIHälsa - decompose_KPIHälsa$seasonal
plot(adjust_KPIHälsa)

d.log.adjust_KPIHälsa <- diff(log(adjust_KPIHälsa))
ur.kpss(d.log.adjust_KPIHälsa, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
KPIHälsa<- ts(d.log.adjust_KPIHälsa, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(KPIHälsa)

##################################################################################

# PRISER:

KPITransport  <- data1[,"KPITransport"]

KPITransport <- ts(KPITransport, start=c(1996,1), end=c(2019,4), frequency = 4)
ts.plot(KPITransport)


KPITransport <- ts(KPITransport, frequency=4, start=c(1996,1), end=c(2019,4))
KPITransport
ts.plot(KPITransport)
decompose_KPITransport<- decompose(KPITransport, "additive")
decompose_KPITransport
adjust_KPITransport <- KPITransport - decompose_KPITransport$seasonal
plot(adjust_KPITransport)

d.log.adjust_KPITransport <- diff(log(adjust_KPITransport))
ur.kpss(d.log.adjust_KPITransport, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
KPITransport<- ts(d.log.adjust_KPITransport, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(KPITransport)


##################################################################################

# PRISER:

KPIKultur  <- data1[,"KPIKultur"]

KPIKultur <- ts(KPIKultur, start=c(1996,1), end=c(2019,4), frequency = 4)

KPIKultur <- ts(KPIKultur, frequency=4, start=c(1996,1), end=c(2019,4))
KPIKultur
ts.plot(KPIKultur)
decompose_KPIKultur<- decompose(KPITransport, "additive")
decompose_KPIKultur
adjust_KPIKultur<- KPIKultur - decompose_KPIKultur$seasonal
plot(adjust_KPIKultur)

d.log.adjust_KPIKultur <- diff(log(adjust_KPIKultur))
ur.kpss(d.log.adjust_KPIKultur, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
KPIKultur<- ts(d.log.adjust_KPIKultur, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(KPIKultur)



##################################################################################

# PRISER:

KPILogistik  <- data1[,"KPILogistik"]

KPILogistik <- ts(KPILogistik, start=c(1996,1), end=c(2019,4), frequency = 4)

decompose_KPILogistik<- decompose(KPILogistik, "additive")
decompose_KPILogistik
adjust_KPILogistik<- KPILogistik - decompose_KPILogistik$seasonal
plot(adjust_KPILogistik)

d.log.adjust_KPILogistik <- diff(log(adjust_KPILogistik))
ur.kpss(d.log.adjust_KPILogistik, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
KPILogistik <- ts(d.log.adjust_KPILogistik, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(KPILogistik)


##################################################################################

# PRISER:

KPIVarorochtjänster <- data1[,"KPIVarorochtjänster"]

KPIVarorochtjänster <- ts(KPIVarorochtjänster, start=c(1996,1), end=c(2019,4), frequency = 4)

decompose_KPIVarorochtjänster <- decompose(KPIVarorochtjänster, "additive")
decompose_KPIVarorochtjänster
adjust_KPIVarorochtjänster <- KPIVarorochtjänster - decompose_KPILogistik$seasonal
plot(adjust_KPIVarorochtjänster)

d.log.adjust_KPIVarorochtjänster <- diff(log(adjust_KPIVarorochtjänster))
ur.kpss(d.log.adjust_KPIVarorochtjänster, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
KPIVarorochtjänster <- ts(d.log.adjust_KPIVarorochtjänster, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(KPILogistik)


##################################################################################

#

ProductionIndustry <- data1[,"ProductionIndustry"]

ProductionIndustry<- ts(ProductionIndustry, start=c(1996,1), end=c(2019,4), frequency = 4)

decompose_ProductionIndustry <- decompose(ProductionIndustry, "additive")
decompose_ProductionIndustry
adjust_ProductionIndustry <- ProductionIndustry - decompose_ProductionIndustry$seasonal
plot(adjust_ProductionIndustry)

d.log.adjust_ProductionIndustry <- diff(log(adjust_ProductionIndustry))
ur.kpss(adjust_ProductionIndustry, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
ProductionIndustry <- ts(d.log.adjust_ProductionIndustry, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(ProductionIndustry)

##################################################################################

#

Manufacturing <- data1[,"Manufacturing"]

Manufacturing<- ts(Manufacturing, start=c(1996,1), end=c(2019,4), frequency = 4)

decompose_Manufacturing <- decompose(ProductionIndustry, "additive")
decompose_Manufacturing
adjust_Manufacturing <- Manufacturing - decompose_Manufacturing$seasonal
plot(adjust_Manufacturing)

d.log.adjust_Manufacturing <- diff(log(adjust_Manufacturing))
ur.kpss(d.log.adjust_Manufacturing, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
Manufacturing <- ts(d.log.adjust_Manufacturing, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(Manufacturing)

##################################################################################

ProductionNondurable <- data1[,"ProductionNondurable"]

ProductionNondurable <- ts(ProductionNondurable, start=c(1996,1), end=c(2019,4), frequency = 4)

decompose_ProductionNondurable <- decompose(ProductionNondurable, "additive")
decompose_ProductionNondurable
adjust_ProductionNondurable <- ProductionNondurable - decompose_ProductionNondurable$seasonal
plot(adjust_ProductionNondurable)

d.log.adjust_ProductionNondurable <- diff(log(adjust_ProductionNondurable))
ur.kpss(d.log.adjust_ProductionNondurable, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
ProductionNondurable <- ts(d.log.adjust_ProductionNondurable, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(ProductionNondurable)


##################################################################################

ProductionEnergy <- data1[,"ProductionEnergy"]

ProductionEnergy <- ts(ProductionEnergy, start=c(1996,1), end=c(2019,4), frequency = 4)

decompose_ProductionEnergy <- decompose(ProductionEnergy, "additive")
decompose_ProductionEnergy
adjust_ProductionEnergy <- ProductionEnergy - decompose_ProductionEnergy$seasonal
plot(adjust_ProductionEnergy)

d.log.adjust_ProductionEnergy <- diff(log(adjust_ProductionEnergy))
ur.kpss(d.log.adjust_ProductionEnergy, type = "tau")@teststat
ur.kpss(adjust_Sysselsättning, type ="tau")@cval


#FINAL: 
ProductionEnergy <- ts(d.log.adjust_ProductionEnergy, frequency = 4, start=c(1996,1), end=c(2019,4))
ts.plot(ProductionEnergy)


############################################################################################
# ALLA Serier
############################################################################################


###########################################################################
# TESTAR FÖRST ENLIGT SVAR() paketet.
############################################################################


################################################################################
# GJORD ENLIGT SVARS paketet med Bias-adjusted bootstrapping med svars() paketet.
###################################################################################

# Serier: BNPMarknadspris, husbyggandet, Sysselsättning, KPIvarorochtjänster


ts.plot(BNPMarknadspris) #d.log
ts.plot(Manufacturing) # d.log
ts.plot(KPIVarorochtjänster) # d.log
ts.plot(STIBOR1M) # nivå
ts.plot(Sysselsättning) #d.log
###################################################
# Standardisera pga konstiga IRF utan standardisering i svars() paketet. 
# dvs standardisering efter difflog transformationer.

# 1.
BNPMarknadspris.st <- scale(BNPMarknadspris, scale=TRUE, center=TRUE)
BNPMarknadspris.st
BNPMarknadspris.st.serie <- ts(BNPMarknadspris.st, start=c(1996,1), end = c(2019,4), frequency = 4)
ts.plot(BNPMarknadspris.st.serie)
class(BNPMarknadspris.st.serie)


## 2.
ts.plot(KPIVarorochtjänster)
KPIvaror.st <- scale(KPIVarorochtjänster)
KPIvaror.st
KPIvaror.st.serie <- ts(KPIvaror.st, start=c(1996,1), end = c(2019,4), frequency = 4)
ts.plot(KPIvaror.st.serie)

# 3.
STIBOR1M.st <- scale(STIBOR1M, scale=TRUE, center=TRUE)
STIBOR1M.st
STIBOR1M.st.serie <- ts(STIBOR1M.st, start=c(1996,1), end = c(2019,4), frequency = 4)
class(STIBOR1M.st.serie)

#4.
Sysselsättning.st <- scale(Sysselsättning, scale=TRUE, center=TRUE)
Sysselsättning.st
Sysselsättning.st.serie <- ts(Sysselsättning.st, start=c(1996,1), end = c(2019,4), frequency = 4)
class(Sysselsättning.st.serie)

# För model spec 2
Konfidens.st <- scale(Konfidensindikator, scale=TRUE, center=TRUE)
Konfidens.st

Konfidens.st.serie <- ts(Konfidens.st, start=c(1996,1), end = c(2019,4), frequency = 4)
ts.plot(Konfidens.st.serie)
class(Konfidens.st.serie)

# För model spec 2
Husbyggande.st <- scale(Husbyggande, scale=TRUE, center=TRUE)
Husbyggande.st
Husbyggande.st.serie <- ts(Husbyggande.st, start=c(1996,1), end = c(2019,4), frequency = 4)
ts.plot(Husbyggande.st.serie)
class(Husbyggande.st.serie)


ts.plot(BNPMarknadspris)
ts.plot(Konfidens.st.serie)
###########################################################

# GENOMFÖRD UTEFTER (SVARS) PAKETET.
# Konfidens.st / KPIvaror.st / Sysselsättning.st / STIBOR1M.st

# Installera paketet "svars"
install.packages("svars")
library(svars)

svars.test <- cbind(Konfidens.st.serie, KPIvaror.st.serie, Sysselsättning.st.serie, STIBOR1M.st.serie)
svars.test

class(svars.test) #ok = matrix, ts


VARselect(svars.test, type ="const") # SC/BIC sÃƒÆ’ger 2 lags
#VAR system v1
svars.test.1 <- vars::VAR(svars.test, lag.max=2, ic="SC")
class(svars.test.1)
svars.test.v1 <- id.chol(svars.test.1)


svars.test.2 <- id.chol(svars.test.1, order_k = c("Konfidens.st.serie", "KPIvaror.st.serie", "Sysselsättning.st.serie", "STIBOR1M.st.serie"))
summary(svars.test.2) 
#ser i summary B0 matrisen med noll-restriktioner.


# Utan konfidensband
irf.svars.test.2 <- irf(svars.test.2, n.ahead=30, nboot=100, bootstrap=TRUE)
plot(irf.svars.test.2)

irf.svars.test.2.boot <- mb.boot(svars.test.2, nboot=1000, n.ahead=30, nc=1)
summary(irf.svars.test.2.boot)

plot(irf.svars.test.2.boot,lowerq = c(0.05, 0.16), upperq = c(0.95, 0.84))

# Bias adjusted Bootstraps: Bernanke et al. (2005) used such bias-adjusted bootstraps following Kilian (1998)
irf.svars.test.2.boot2 <- ba.boot(irf.svars.test.2.boot, nc=1)
plot(irf.svars.test.2.boot2 , scales='free',lowerq = c(0.05,  0.16), upperq = c(0.95, 0.84))
# Olika konfidensintervaller, 90%, 80% och 68%!
# Eller så kör vi bara 90% konvidensintervaller som nedan:
plot(irf.svars.test.2.boot2, scales='free',lowerq = c(0.05), upperq = c(0.95))

# EXPORT TO PDFs:
test.pdf.IRF.ba <- plot(irf.MODELL2.boot2, scales='free',lowerq = c(0.05,  0.16), upperq = c(0.95, 0.84))
####################################################################################



###################################################################################
###################################################################################
# 1) MICKES METOD med vars() paketet.
###################################################################################
###################################################################################

# BNPMarknadspris.st / KPIvaror.st / Sysselsättning.st / STIBOR1M.st


var.st.micke2 <- cbind(window(Konfidens.st.serie, start = c(1996,1)),
                       window(Sysselsättning.st.serie, start = c(1996,1)),
                       window(Husbyggande.st.serie, start = c(1996,1)),
                       window(STIBOR1M.st.serie, start = c(1996,1)))

#Variabel namn:
dimnames(var.st.micke2)[[2]] <- c("Konfidens","Syss","Hus","STIBOR")

var.st.micke2

#nr of lags
VARselect(var.st.micke2, type ="const")

# Construct VAR
var.st.micke.2 <- VAR(var.st.micke2, p = 2, type ="const") 
summary(var.st.micke.2)
serial.test(var.st.micke.2) 

roots(var.st.micke.2) # stabilt.

################################################
# IRF: var.st.micke2
################################################
irf(var.st.micke.2, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, boot=TRUE)
plot(irf(var.st.micke.2, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, runs=100))

irf(var.st.micke.2, impulse ="Hus", response = "Syss", n.ahead = 16, seed = 4654, boot=TRUE)
plot(irf(var.st.micke.2, impulse ="Hus", response = "Syss", n.ahead = 16, seed = 4654, runs=100))

irf(var.st.micke.2, impulse ="Konfidens", response = "Hus", n.ahead = 32, seed = 4654, boot=TRUE)
plot(irf(var.st.micke.2, impulse ="Konfidens", response = "Hus", n.ahead = 32, seed = 4654, runs=100))

irf(var.st.micke.2, impulse ="STIBOR", response = "Syss", n.ahead = 32, seed = 4654)
plot(irf(var.st.micke.2, impulse ="STIBOR", response = "Syss", n.ahead = 36, seed = 4654, runs=100, ortho=TRUE))


p1.micke2 <- irf(var.st.micke.2, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, boot=TRUE)
plot(p1.micke2)


# Impulse storlek /magnitud "e" = repo

summary(var.st.micke.2)$varresult$STIBOR$sigma

summary(var.st.micke.2)$varresult$Syss$sigma

summary(var.st.micke.2)$varresult$Hus$sigma

summary(var.st.micke.2)$varresult$Konfidens$sigma


# Alla ihop vars() paketet.
irf2 <- plot(irf(var.st.micke.2, impulse ="Konfidens", response = c("Hus", "Syss", "STIBOR"), n.ahead = 36, seed = 4654, runs=100))

# Ändra limiterna:
plot(irf(var.result.st, impulse ="Repo.st", response = "Skuld.st", n.ahead = 16, seed = 4654), ylim=c(-0.003,0.003), ylab="Debt growth rate (%)", main="Baseline Model; Orthogonal Impulse Response From Repo Rate")




###################################################################################
# 2-modell spec: MICKES METOD med vars() paketet.
###################################################################################


# KOnfidens, Hus, Syss, KPI, STIBOR1M
var.st.micke3 <- cbind(window(Konfidens.st.serie, start = c(1996,1)),
                       window(Husbyggande.st.serie, start = c(1996,1)),
                       window(Sysselsättning.st.serie, start = c(1996,1)),
                       window(KPIvaror.st.serie, start = c(1996,1)),
                       window(STIBOR1M.st.serie, start = c(1996,1)))


dimnames(var.st.micke3)[[2]] <- c("Konfidens","Hus","Syss","KPI","STIBOR")

var.st.micke3

VARselect(var.st.micke3, type ="const")

var.st.micke.3 <- VAR(var.st.micke3, p = 2, type ="const") 
summary(var.st.micke.3)
serial.test(var.st.micke.3) 

roots(var.st.micke.3) # stabilt.

################################################
# IRF
################################################
irf(var.st.micke.3, impulse ="Konfidens", response = "Hus", n.ahead = 16, seed = 4654, boot=TRUE)
plot(irf(var.st.micke.3, impulse ="Konfidens", response = "Hus", n.ahead = 36, seed = 4654, runs=100))

irf(var.st.micke.3, impulse ="Hus", response = "Konfidens", n.ahead = 16, seed = 4654, boot=TRUE)
plot(irf(var.st.micke.2, impulse ="Hus", response = "Konfidens", n.ahead = 16, seed = 4654, runs=100))


# Impulse storlek /magnitud "e" = repo
#

# That is, if the SD is .04 and if peak response is .09, you can interpret that as a 1% shock in x producing a 2.25% (=.09/.04) response in y.

summary(var.st.micke.2)$varresult$Konfidens$sigma
# Standardavvikelse storlek = 
summary(var.st.micke.2)$varresult$Syss$sigma
#Standardavvikelse storlek = 0

summary(var.st.micke.2)$varresult$FPI$sigma
#Standardavvikelse storlek  

summary(var.st.micke.2)$varresult$Skuld$sigma
#Standardavvikelse storlek 



# Ändra limiterna:
plot(irf(var.result.st, impulse ="Repo.st", response = "Skuld.st", n.ahead = 16, seed = 4654), ylim=c(-0.003,0.003), ylab="Debt growth rate (%)", main="Baseline Model; Orthogonal Impulse Response From Repo Rate")

##########################################################
# Vill samköra responsen av HUS till följd av shock till SYSS.

#### PACKAGES ####
library(forecast) # For autoplot
library(gridExtra) # For grid
library(ggplot2)
library(tidyverse)
install.packages("tsDyn")
library(tsDyn)
library(vars)
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(dplyr)

# KOnfidens, Hus, Syss, KPI, STIBOR1M
var.st.micke.3 <- VAR(var.st.micke3, p = 2, type ="const") 

RESPONSE = c("Hus", "Syss", "KPI")
IMPULSE = "Konfidens"

fits = lapply(IMPULSE,function(i){
  irf(var.st.micke.3,response=RESPONSE,impulse=i,
      n.ahead=16,ortho=TRUE,boot=TRUE)
})
names(fits) = IMPULSE


library(cowplot)

P = lapply(fits,function(i)as_grob(~plot(i,cex.main=0.7,mar=c(0.5,0.5,0.5,0.5))))
grid.arrange(grobs=P,ncol=3)

plotdf = lapply(names(fits),function(i){
  data.frame(
    index = 1:nrow(fits[[i]]$irf[[1]]),
    value=fits[[i]]$irf[[1]][,1],
    Lower=fits[[i]]$Lower[[1]][,1],
    Upper=fits[[i]]$Upper[[1]][,1],
    Impulse = i)
})
plotdf=do.call(rbind,plotdf)

ggplot(plotdf,aes(x=index,y=value)) + 
  geom_line() +facet_wrap(~Impulse) + 
  geom_ribbon(aes(ymin=Lower,ymax=Upper),fill=NA,col="salmon",linetype="dashed") + 
  geom_hline(yintercept=0,col="salmon") + theme_bw()


#########################################################################################################
# Modell 1
irf(var.st.micke.2, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, boot=TRUE)
p1 <- irf(var.st.micke.2, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, boot=TRUE, runs=100)
plot(p1)

# Modell 2
irf(var.st.micke.3, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, boot=TRUE)
p2 <- irf(var.st.micke.3, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, boot=TRUE)
plot(p2)
plot(irf(var.st.micke.3, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, runs=100))
##########################################################################################################





################################################
# IRF: IMpulse SYSS response: Hus och Konfidens
################################################


irf(var.st.micke.3, impulse ="Syss", response = "Konfidens", n.ahead = 16, seed = 4654, boot=TRUE)
plot(irf(var.st.micke.3, impulse ="Syss", response = "Konfidens", n.ahead = 16, seed = 4654, runs=100))




p1 <- irf(var.st.micke.2, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, boot=TRUE, ci=0.90)
plot(p1)


p2 <- irf(var.st.micke.3, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, boot=TRUE, ci=0.90)
plot(p2)








# Response HUS/Konfidens
# Impulse SYSS
library(cowplot)
library(gridExtra)


RESPONSE = c("Hus", "Konfidens")
IMPULSE = "Syss"

fits = lapply(IMPULSE,function(i){
  irf(var.st.micke.3, response=RESPONSE,impulse=i,
      n.ahead=40,ortho=TRUE,boot=TRUE)
})
names(fits) = IMPULSE

P = lapply(fits,function(i)as_grob(~plot(i,cex.main=0.7,mar=c(0.5,0.5,0.5,0.5))))
grid.arrange(grobs=P,ncol=2)






t###########################################################################
# MODELL 1 splittar up
p1$Lower #lower band
p1$Upper # upper band
p1$irf # punkt estimaten median responserna


# Modell 2 splittar upp
p2$Lower #lower band
p2$Upper # upper band
p2$irf # punkt estimaten median responserna

xl <- p2$Lower
xu <- p2$Upper
x1 <- p2$irf

# TS - objects - MODEL 1
xl.ts <- ts(xl)
xl.ts
class(xl.ts)

xu.ts <- ts(xu)
xu.ts
class(xu.ts)

x1.ts<- ts(x1)
x1.ts
class(x1.ts)

#########################################################






#########################################################

# Modell 2 splittar upp
p2$Lower #lower band
p2$Upper # upper band
p2$irf # punkt estimaten median responserna

xl2 <- p2$Lower
xu2 <- p2$Upper
x2 <- p2$irf

# TS - objects - MODEL 2
xl2.ts <- ts(xl2)
xl2.ts
class(xl2.ts)

xu2.ts <- ts(xu2)
xu2.ts
class(xu2.ts)

x2.ts<- ts(x2)
x2.ts
class(x2.ts)


p4 <- autoplot(x2.ts) +
  autolayer(xu2.ts, series = "Upper limit") +
  autolayer(xl2.ts, series = "Lower limit") +
  theme(legend.position="bottom") +
  guides(colour=guide_legend(title="")) +
  ggtitle("Income vs. Consumption") +
  xlab("Year") +
  ylab("$ billion")





####################################################################

# Clean console output
cat("")
# Clean environment 
rm(list=ls())
#### PACKAGES ####
library(forecast) # For autplot
library(gridExtra) # For grid

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# sample size
n <- 80
# Generate some data
y1 <- 1 + 1 * (1:n) + rnorm(n)
y2 <- 1 + 0.5 * (1:n) + rnorm(n)
y3 <- 10 + 1 * (1:n) + rnorm(n)
y4 <- 1 - 0.5 * (1:n) + rnorm(n)
y5 <- 1 + 1 * (1:n) + rnorm(n)
y6 <- 1 + 0.5 * (1:n) + rnorm(n)
y7 <- 10 + 1 * (1:n) + rnorm(n)
y8 <- 1 - 0.5 * (1:n) + rnorm(n)
# Make ts object, easier with x-axes labels
y1 <- ts(y1, start=1990, frequency=4)
y2 <- ts(y2, start=1990, frequency=4)
y3 <- ts(y3, start=1990, frequency=4)
y4 <- ts(y4, start=1990, frequency=4)
y5 <- ts(y5, start=1990, frequency=4)
y6 <- ts(y6, start=1990, frequency=4)
y7 <- ts(y7, start=1990, frequency=4)
y8 <- ts(y8, start=1990, frequency=4)


p1 <- autoplot(y1) +
  autolayer(y1, series = "US Income") +
  autolayer(y2, series = "US Consumption") +
  theme(legend.position="bottom") +
  guides(colour=guide_legend(title="")) +
  ggtitle("Income vs. Consumption") +
  xlab("Year") +
  ylab("$ billion")

p2 <- autoplot(y3) +
  autolayer(y3, series = "UK Income") +
  autolayer(y4, series = "UK Consumption") +
  theme(legend.position="bottom") +
  guides(colour=guide_legend(title="")) +
  ggtitle("Income vs. Consumption") +
  xlab("Year") +
  ylab("$ billion")

p3 <- autoplot(y5) +
  autolayer(y5, series = "EU Income") +
  autolayer(y6, series = "EU Consumption") +
  theme(legend.position="bottom") +
  guides(colour=guide_legend(title="")) +
  ggtitle("Income vs. Consumption") +
  xlab("Year") +
  ylab("$ billion")

p4 <- autoplot(y7) +
  autolayer(y7, series = "AUS Income") +
  autolayer(y8, series = "AUS Consumption") +
  theme(legend.position="bottom") +
  guides(colour=guide_legend(title="")) +
  ggtitle("Income vs. Consumption") +
  xlab("Year") +
  ylab("$ billion")

grid.arrange(p1, p2,p3,p4, ncol=2)

#####################################################################






RESPONSE = "Hus"
IMPULSE = c("Syss","Konfidens")

library(vars)
library(cowplot)
library(gridExtra)

RESPONSE = "prod"
IMPULSE = c("e","U","rw")

fits = lapply(IMPULSE,function(i){
  irf(var.st.micke.2,response=RESPONSE,impulse=IMPULSE,
      n.ahead=16,ortho=TRUE,boot=TRUE)
})
names(fits) = IMPULSE

P = lapply(fits,function(i)as_grob(~plot(i,cex.main=0.7,mar=c(0.5,0.5,0.5,0.5))))
grid.arrange(grobs=P,ncol=2)




plotdf = lapply(names(fits),function(IMPULSE){
  data.frame(
    index = 1:nrow(fits[[i]]$irf[[1]]),
    value=fits[[p1]]$irf[[1]][,1],
    Lower=fits[[p1]]$Lower[[1]][,1],
    Upper=fits[[p1]]$Upper[[1]][,1],
    Impulse = p1)
})
plotdf=do.call(rbind,plotdf)

ggplot(plotdf,aes(x=index,y=value)) + 
  geom_line() +facet_wrap(~Impulse) + 
  geom_ribbon(aes(ymin=Lower,ymax=Upper),fill=NA,col="salmon",linetype="dotdash") + 
  geom_hline(yintercept=0,col="darkgreen") + theme_bw()







###################################

# Behöver IRF som i dataframes:

install.packages("devtools")
library(devtools)
extract_varirf
single_p1 <- extract_varirf(p1)
#################################################################################



var.st.micke3 <- cbind(window(Konfidens.st.serie, start = c(1996,1)),
                       window(Husbyggande.st.serie, start = c(1996,1)),
                       window(Sysselsättning.st.serie, start = c(1996,1)),
                       window(KPIvaror.st.serie, start = c(1996,1)),
                       window(STIBOR1M.st.serie, start = c(1996,1)))


dimnames(var.st.micke3)[[2]] <- c("Konfidens","Hus","Syss","KPI","STIBOR")

var.st.micke3

VARselect(var.st.micke3, type ="trend")

var.st.micke.3 <- VAR(var.st.micke3, p = 2, type ="const") 
summary(var.st.micke.3)
serial.test(var.st.micke.3) 

roots(var.st.micke.3) # stabilt.

################################################
# IRF
################################################
irf(var.st.micke.3, impulse ="Konfidens", response = "Hus", n.ahead = 16, seed = 4654, boot=TRUE)
plot(irf(var.st.micke.3, impulse ="Konfidens", response = "Hus", n.ahead = 36, seed = 4654, runs=100))

irf(var.st.micke.3, impulse ="Hus", response = "Konfidens", n.ahead = 16, seed = 4654, boot=TRUE)
plot(irf(var.st.micke.2, impulse ="Hus", response = "Konfidens", n.ahead = 16, seed = 4654, runs=100))

irf(var.st.micke.3, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, boot=TRUE)
plot(irf(var.st.micke.3, impulse ="Syss", response = "Hus", n.ahead = 16, seed = 4654, runs=100))

irf(var.st.micke.2, impulse ="STIBOR", response = "Syss", n.ahead = 16, seed = 4654)
plot(irf(var.st.micke.2, impulse ="STIBOR", response = "Syss", n.ahead = 16, seed = 4654, runs=100, ortho=TRUE))




############################################################################


install.packages("gridGraphics")
library(gridGraphics)
install.packages("grid")
install.packages("ggplot2")
library(ggplot2)


# IMPULS till HUS
# RESPONSER: KONFIDENS; SYSS, KPI!
# 3 figurer ställt mot varandra.
var.st.micke.3
class(var.st.micke.3)

RESPONSE = "Konfidens"
IMPULSE = c("Hus","Syss","KPI")

fits = lapply(IMPULSE,function(i){
  irf(var.st.micke.3,response=RESPONSE,impulse=i,
      n.ahead=16,ortho=TRUE,boot=TRUE)
})
names(fits) = IMPULSE

P = lapply(fits,function(i)as_grob(~plot(i,cex.main=0.9,mar=c(0.05,0.05,0.05,0.05))))
grid.arrange(grobs=P,ncol=3)
# n.col= 3 då tre st impulser

plotdf = lapply(names(fits),function(i){
  data.frame(
    index = 1:nrow(fits[[i]]$irf[[1]]),
    value=fits[[i]]$irf[[1]][,1],
    Lower=fits[[i]]$Lower[[1]][,1],
    Upper=fits[[i]]$Upper[[1]][,1],
    Impulse = i)
})
plotdf=do.call(rbind,plotdf)

ggplot(plotdf,aes(x=index,y=value)) + 
  geom_line() + facet_wrap(~Impulse) + 
  geom_ribbon(aes(ymin=Lower,ymax=Upper),fill=NA,col="green",linetype= "twodotted") + 
  geom_hline(yintercept=0,col="blue") + theme_bw()

# https://stackoverflow.com/questions/59335309/plotting-impulse-response-functions-in-grid-format

plot(p1)
########################################################################################

# 1)      Kör modellerna
# 2)      Beräkna impulsresponserna
# 3)      Spara dessa med olika namn
# 4)      Om du använder min kod är det bara att ange de på rätt ställe.
# 5)      Om du använder koden nedan, gör en data frame per figur som innehåller de impulse responser du vill ha i figuren



#######################################################################################







###################################################################################
###################################################################################
#
#  PCA FACTORS:
#
###################################################################################
###################################################################################

getwd()
pcadata <- read_excel("smallpcadata.xlsx")
str(pcadata)
#Dataframe:
PCA <- pcadata[,c(2:70)]
PCA
head(PCA)
View(PCA)

# FROM CHARACTER TO NUMERIC CONVERSION:
is.numeric(PCA)
str(PCA)
class(PCA)


sapply(PCA, class)

PCA$CPI <- as.numeric(PCA$CPI)
PCA$CPIDRYCK <- as.numeric(PCA$CPIDRYCK)
PCA$CPIBOENDE  <- as.numeric(PCA$CPIBOENDE)
PCA$GRUVOR   <- as.numeric(PCA$GRUVOR)
PCA$INSATSVAROR   <- as.numeric(PCA$INSATSVAROR)
PCA$INDUSTRI   <- as.numeric(PCA$INDUSTRI)
PCA$MASSAINDUSTRI   <- as.numeric(PCA$MASSAINDUSTRI)
PCA$M1   <- as.numeric(PCA$M1)
PCA$M3   <- as.numeric(PCA$M3)
PCA$MFIUTLANING   <- as.numeric(PCA$MFIUTLANING)
PCA$MFIOFFENTLIG    <- as.numeric(PCA$MFIOFFENTLIG  )
PCA$TCW    <- as.numeric(PCA$TCW  )
PCA$SDR    <- as.numeric(PCA$SDR  )
PCA$KIX    <- as.numeric(PCA$KIX  )
PCA$SEKUSD1MAN    <- as.numeric(PCA$SEKUSD1MAN  )

PCA$US5Y     <- as.numeric(PCA$US5Y  )
PCA$DE5Y     <- as.numeric(PCA$DE5Y  )
PCA$STIBOR1M     <- as.numeric(PCA$STIBOR1M   )
PCA$STIBOR3M     <- as.numeric(PCA$STIBOR1M   )
PCA$SSVX1M     <- as.numeric(PCA$SSVX1M   )

PCA$SSVX6M     <- as.numeric(PCA$SSVX6M   )
PCA$SEGVB2Y     <- as.numeric(PCA$SEGVB2Y   )
PCA$SEGVB10Y     <- as.numeric(PCA$SEGVB10Y  )
PCA$STFIX3M       <- as.numeric(PCA$STFIX3M   )
PCA$STFIX6M       <- as.numeric(PCA$STFIX6M   )
PCA$EU5Y        <- as.numeric(PCA$EU5Y    )
PCA$Investeringsvaruindustri         <- as.numeric(PCA$Investeringsvaruindustri   )
PCA$Konsumtionsvaruindustri         <- as.numeric(PCA$Konsumtionsvaruindustri   )

PCA$Livsmedelsindustri         <- as.numeric(PCA$Livsmedelsindustri   )
PCA$Trävaruindustri         <- as.numeric(PCA$Trävaruindustri   )
PCA$Massaindustri         <- as.numeric(PCA$Massaindustri    )

PCA$Pappersindustri         <- as.numeric(PCA$Pappersindustri   )
PCA$Grafiskindustri         <- as.numeric(PCA$Grafiskindustri   )

PCA$Petroleumindustri         <- as.numeric(PCA$Petroleumindustri   )
PCA$Kemiskindustri         <- as.numeric(PCA$Kemiskindustri  )
PCA$Stålmetallindustri          <- as.numeric(PCA$Stålmetallindustri  )
PCA$Metallvaruindustri          <- as.numeric(PCA$Metallvaruindustri  )

PCA$Elektronikindustri           <- as.numeric(PCA$Elektronikindustri   )
PCA$Maskinindustri           <- as.numeric(PCA$Maskinindustri   )
PCA$Detaljhandel           <- as.numeric(PCA$Detaljhandel   )

PCA$Partihandel           <- as.numeric(PCA$Partihandel  )
PCA$Barometerindikatorn           <- as.numeric(PCA$Barometerindikatorn  )
PCA$Husbyggande            <- as.numeric(PCA$Husbyggande   )
PCA$Sysselsättning           <- as.numeric(PCA$Sysselsättning  )
PCA$Export           <- as.numeric(PCA$Export   )
PCA$Import         <- as.numeric(PCA$Import   )

PCA$BNPMarknadspris        <- as.numeric(PCA$BNPMarknadspris   )
PCA$KPILivsmedel        <- as.numeric(PCA$KPILivsmedel  )
PCA$KPIBoende         <- as.numeric(PCA$KPIBoende  )
PCA$KPIInventarier          <- as.numeric(PCA$KPIInventarier   )

PCA$KPITransport          <- as.numeric(PCA$KPITransport   )
PCA$KPIKultur           <- as.numeric(PCA$KPIKultur   )
PCA$KPILogistik          <- as.numeric(PCA$KPILogistik   )
PCA$KPIVarorochtjänster           <- as.numeric(PCA$KPIVarorochtjänster   )
PCA$ProductionIndustry           <- as.numeric(PCA$ProductionIndustry  )
PCA$Manufacturing           <- as.numeric(PCA$Manufacturing   )
PCA$ProductionEnergy           <- as.numeric(PCA$ProductionEnergy )



PCA$MFIINLANING          <- as.numeric(PCA$MFIINLANING  )
PCA$CPIBOENDE         <- as.numeric(PCA$CPIBOENDE  )
PCA$CPIINVENT         <- as.numeric(PCA$CPIINVENT  )
PCA$REPORANTA         <- as.numeric(PCA$REPORANTA )

PCA$Konfidensindikatorhushall         <- as.numeric(PCA$Konfidensindikatorhushall )
PCA$Mikroindexhushall         <- as.numeric(PCA$Mikroindexhushall )
PCA$US3MAN       <- as.numeric(PCA$US3MAN )
PCA$GB3MAN       <- as.numeric(PCA$GB3MAN )
PCA$Insatsvaruindustri       <- as.numeric(PCA$Insatsvaruindustri   )
PCA$KPIHälsa     <- as.numeric(PCA$KPIHälsa  )
PCA$ProductionNondurable      <- as.numeric(PCA$ProductionNondurable   )


class(PCA$CPI) # numeric
class(PCA$REPORANTA) # numeric
class(PCA$CPIBOENDE) # numeric

# Stäm av att alla numeriska
str(PCA)

#The base R function prcomp() is used to perform PCA. By default, 
#it centers the variable to have mean equals to zero. With parameter scale. = T, 
#we normalize the variables to have standard deviation equals to 1.
# https://www.analyticsvidhya.com/blog/2016/03/pca-practical-guide-principal-component-analysis-python/

# JÄTTE VIKTIGT ATT SE ÖVER för ev. missing values/<NA>. Går ej att köra PRCOMP annars.
FAVAR.PCA <- prcomp(PCA, scale.=TRUE, center=TRUE)
FAVAR.PCA # 3 faktorer/komponenter pga 3 variabler.

mean(cor(PCA)) # Correlation
str(PCA)
PCA
summary(FAVAR.PCA) # proportion of variation explained by variables.

FAVAR.PCA$rotation # rotationer.
# PC 1 = CPIDRYCK/CPI
# PC2 = CPIBOENDE

FAVAR.PCA

screeplot(FAVAR.PCA, type="line")
screeplot(FAVAR.PCA, main="Scree Plot Bars", xlab = "Components")

FAVAR.PCA$rotation # loadings or denoted rotations. get correlation contributio of each variable to each and every component.

# Rotations=loadings.

FAVAR.PCA1 <- FAVAR.PCA$sdev^2
FAVAR.PCA1
FAVAR.PCA2 <- round(FAVAR.PCA1/sum(FAVAR.PCA1)*100,1)
FAVAR.PCA2 # contribution of each component in percentage form.

# GET SCREE PLOT Disection in PERCENT form!
barplot(FAVAR.PCA2, main ="Scree Plot", xlab ="Principal Components", ylab ="Percent Variation")

FAVAR.PCA$sdev

FAVAR.loadings <- FAVAR.PCA$rotation
FAVAR.loadings


### --- OR EXTRACING THIS WAY:


# If say we have 10 variables and that is we get 10 components. However we realize that only the first 2 or first 3 components
# explain sufficiently much. THen we can extract the first two components like the below:

# SCREE-PLOT Show the percentage of variances explained by each principal component.
fviz_screeplot(FAVAR.PCA)
get_eig(FAVAR.PCA)

# VERY NICE SCREE PLOT with PERCENTAGE:
# SCREE PLOT:
fviz_eig(FAVAR.PCA, addlabels = TRUE, ylim=c(0,100), 
         main="Scree Plot of Variation Explained By PCs")


FAVAR.PCA$x # get the scores
FAVAR.PCA$importance

# Extracting first 5 components, and rounding them down.
round(FAVAR.loadings,2)[,1:5]

FAVAR.PCA$x # scores

PC1 <- FAVAR.PCA$x[,1]
PC1 # extracing PC1


PC2 <- FAVAR.PCA$x[,2]
PC2


biplot(FAVAR.PCA, scale=0)
#The parameter scale = 0 ensures that arrows are scaled to represent the loadings. 
#To make inference from image above, focus on the extreme ends (top, bottom, left, right) of this graph.

##############################################
aload <- abs(FAVAR.PCA$rotation) # absolute value if needed.
aload

std.dev.FAVAR.PCA <- FAVAR.PCA$sdev
std.dev.FAVAR.PCA

#Computing variance:
FAVAR.PCA.var <- std.dev.FAVAR.PCA^2 # this is same as eigenvalues.
FAVAR.PCA.var


# proportion of variance explained:
prop.varex.FAVAR.PCA <- FAVAR.PCA.var/sum(FAVAR.PCA.var)
prop.varex.FAVAR.PCA

# Another nice scree plot in descenting order:
plot(prop.varex.FAVAR.PCA, xlab="Principal Component", ylab="Proportion of Variance Explained", type="b")



###############################################
# END 
##############################################

