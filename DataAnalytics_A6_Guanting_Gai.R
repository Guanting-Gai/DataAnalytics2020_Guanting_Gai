setwd('F:/R')
data <- read.csv('dataset.csv')
head(data)
View(data)
tail(data)
summary(data)
# find null
help(ncol)
ncol(data)
colnames(data[1])
ls1<-list()
str(data)
d1 <- data[-c(1:6)]
str(d1)
# replace faults with average
for (i in (1:ncol(d1))) {
  if (nrow(d1[which(d1[i] < 0),][i]) > 0)
  {
    ave <- d1[-which(d1[i] < 0),][i]
    ave1 <- as.numeric(unlist(ave[1]))
    ave2 <- mean(ave1)
    d1[which(d1[i] < 0),][i] <- ave2
  }
  
}
head(d1)
##cor
library(corrplot)
par(mfrow = c(1,1))
data_cor<-cor(d1)
corrplot(data_cor) 
##plot
library(grid)
library(ggplot2)
library(ggthemes) 
library(gridExtra)
summary(d1$ALE)
summary(d1$Health_Status)
theme_set(theme_classic())
names(d1)
summary(d1$Age_19_Under)
p1 <- ggplot(d1, aes(x = cut(Age_19_Under,breaks = c(0,10,20,30,40,50)),y = ALE,fill=cut(Health_Status,breaks = c(0,10,20,30,40,50)))) + 
  geom_bar(stat='identity',position= position_dodge())+
  coord_cartesian(ylim=c(65, 85))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#009E73", "#F0E442"))+
  xlab('Age_19_Under')+labs(fill='Health_Status')
p1
summary(d1$Age_19_64)
p2 <- ggplot(d1, aes(x = cut(Age_19_64,breaks = c(40,50,60,70,80,90)),y = ALE,fill=cut(Health_Status,breaks = c(0,10,20,30,40,50)))) + 
  geom_bar(stat='identity',position= position_dodge())+
  coord_cartesian(ylim=c(65, 85))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#009E73", "#F0E442"))+
  xlab('Age_19_64')+labs(fill='Health_Status')
p2
summary(d1$Age_65_84)
p3 <- ggplot(d1, aes(x = cut(Age_65_84,breaks = c(1,5,10,15,20,25,30)),y = ALE,fill=cut(Health_Status,breaks = c(0,10,20,30,40,50)))) + 
  geom_bar(stat='identity',position= position_dodge())+
  coord_cartesian(ylim=c(65, 85))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#009E73", "#F0E442"))+
  xlab('Age_65_84')+ labs(fill='Health_Status')
p3

summary(d1$Age_85_and_Over)
p4 <- ggplot(d1, aes(x = cut(Age_85_and_Over,breaks = c(0.01,2,4,6,8)),y = ALE,fill=cut(Health_Status,breaks = c(0,10,20,30,40,50)))) + 
  geom_bar(stat='identity',position= position_dodge())+
  coord_cartesian(ylim=c(65, 85))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#009E73", "#F0E442"))+
  xlab('Age_85_and_Over')+ labs(fill='Health_Status')
p4
grid.arrange(p1,p2,p3,p4)

p5 <- ggplot(d1,aes(x = cut(Obesity,breaks = c(0,10,20,30,40,50)),y=No_Exercise))+ geom_boxplot()+xlab('Obesity')
p6 <- ggplot(d1,aes(x = cut(Obesity,breaks = c(0,10,20,30,40,50)),y=Few_Fruit_Veg))+ geom_boxplot()+xlab('Obesity')

summary(d1$Diabetes)
p7 <- ggplot(d1,aes(x = cut(High_Blood_Pres,breaks = c(0,10,20,30,40,50)),y=No_Exercise))+ geom_boxplot()+xlab('High_Blood_Pres')
p8 <- ggplot(d1,aes(x = cut(High_Blood_Pres,breaks = c(0,10,20,30,40,50)),y=Few_Fruit_Veg))+ geom_boxplot()+xlab('High_Blood_Pres')
p9 <- ggplot(d1,aes(x = cut(Smoker,breaks = c(0,10,20,30,40,50)),y=No_Exercise))+ geom_boxplot()+xlab('Smoker')
p10 <- ggplot(d1,aes(x = cut(Smoker,breaks = c(0,10,20,30,40,50)),y=Few_Fruit_Veg))+ geom_boxplot()+xlab('Smoker')
p11 <- ggplot(d1,aes(x = cut(Diabetes,breaks = c(0.5,5,10,15,20)),y=No_Exercise))+ geom_boxplot()+xlab('Diabetes')
p12 <- ggplot(d1,aes(x = cut(Diabetes,breaks = c(0.5,5,10,15,20)),y=Few_Fruit_Veg))+ geom_boxplot()+xlab('Diabetes')
grid.arrange(p5,p6,p7,p8,p9,p10,p11,p12)
#scale
df1 <- scale(d1)
df1 <- as.data.frame(df1)
class(df1)
head(df1)

# PCA
library("factoextra")
df2 <- df1[-1]
df3 <- df1[1]
df.pca <- prcomp(df2)

fviz_eig(df.pca, addlabels = TRUE, ylim = c(0, 30)) # 
var <- get_pca_var(df.pca)
corrplot(var$cos2, is.corr=FALSE)
fviz_pca_var(df.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
fviz_contrib(df.pca, choice = "var", axes = 1:3)
View(df.pca$x)
df_new <- df.pca$x
df_new <- cbind(df3,df_new)
View(df_new)
df_new <- df_new[1:7]

# split
library(caTools)
spl = sample.split(df_new$ALE, 0.8)
train = subset(df_new, spl == TRUE)
test=subset(df_new,spl==FALSE)
names(train)

#Model:
#Model1 Linear Regression 
lr <- lm(ALE~., data=train)
summary(lr)
lr_prediction = predict(lr, test)
View(lr_prediction)
plot(test$ALE,type="l",lty=2,col="green")
lines(lr_prediction,col="blue")
library(caret)
postResample(pred = lr_prediction, obs = test$ALE)

#Model2 Random Forest
library(randomForest)
rf<-randomForest(ALE~., data=train, ntree=100)
rf_prediction = predict(rf, test)
plot(test$ALE,type="l",lty=2,col="green")
lines(rf_prediction,col="blue")
postResample(pred = rf_prediction, obs = test$ALE)

#Model3 Decision Tree
library(rpart)
library(rpart.plot)
dt = rpart(ALE ~ ., data = train, method = "anova")
rpart.plot(dt)
dt_prediction = predict(dt, test)
plot(test$ALE,type="l",lty=2,col="green")
lines(dt_prediction,col="blue")
postResample(pred = dt_prediction, obs = test$ALE)
