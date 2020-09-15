setwd("F:/R")
EPI_data<-read.csv("EPI_data.csv")
EPI_data
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI
tf<-is.na(EPI)
E <- EPI[!tf]
summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(EPI); qqline(EPI)
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
boxplot(EPI,DALY)
boxplot(EPI,WATER_H)
help(distributions)
EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
EPISW<-EPI[!No_surface_water]
ESW <- EPISW[!is.na(EPISW)]
hist(ESW)
hist(ESW, seq(30., 95., 1.0), prob=TRUE)
EPIDesert<-EPI[!Desert]
EDesert<- EPIDesert[!is.na(EPIDesert)]
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob=TRUE)
EPIHPD<-EPI[!High_Population_Density]
EHPD<- EPIHPD[!is.na(EPIHPD)]
hist(EHPD)
hist(EHPD, seq(30., 95., 1.0), prob=TRUE)
EPI_South_Asia <- EPI[EPI_regions=="South Asia"]
E_South_Asia<- EPI_South_Asia[!is.na(EPI_South_Asia)]
hist(E_South_Asia)
hist(E_South_Asia, seq(30., 95., 1.0), prob=TRUE)


GRUMP_data <-read.csv('GPW3_GRUMP_SummaryInformation_2010.csv')
GRUMP_data
View(GRUMP_data)
attach(GRUMP_data)
fix(GRUMP_data)
PopulationPerUnit
tf<-is.na(PopulationPerUnit)
E <- PopulationPerUnit[!tf]
summary(PopulationPerUnit)
fivenum(PopulationPerUnit,na.rm=TRUE)
stem(PopulationPerUnit)
hist(PopulationPerUnit)
hist(PopulationPerUnit, seq(0, 400, 1.0), prob=TRUE)
lines(density(PopulationPerUnit,na.rm=TRUE,bw=1.))
rug(PopulationPerUnit)
plot(ecdf(PopulationPerUnit), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(PopulationPerUnit); qqline(PopulationPerUnit)

x <- rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
boxplot(PopulationPerUnit,ContinentName)
PoCode<-PopulationPerUnit[!CountryCode]
PCode <- PoCode[!is.na(PoCode)]
hist(PCode)
hist(PCode, seq(0, 300, 1.0), prob=TRUE)



