movie.df<-read.csv("FinalProject.csv",header=TRUE,)
dim(movie.df)
View(movie.df)
summary(movie.df)
install.packages("ggpubr")
library("ggpubr")
##is this genre related to profitability##calculating coefficient correlation between two variables
c<-as.numeric(movie.df$Genre)# converting character to numeric

library(tidyr)
library(dplyr)
zero<-movie.df %>% replace_na(list(Profitability = 0))##replacing NA in variable Profitability with '0' value
u<-as.character(zero$Profitability)
t<-as.numeric(u)
cor(c,t,  method = "pearson")#cofficient correlation formula
##scatter plot
plot(c,t,xlab="genre",ylab="profit")
abline(lm(c~ t,data=movie.df),col='red')

newdata <- movie.df[which(movie.df$Genre=="Drama"),]
a<-as.numeric(newdata$Genre)
b<-as.numeric(newdata$Profitability)
plot(a,b,xlab="Drama genre",ylab="profit")
abline(lm(a~ b,data=newdata),col='red')



newdata <- movie.df[which(movie.df$Genre=="Comedy"),]
p<-as.numeric(newdata$Genre)
zero1<-newdata %>% replace_na(list(Profitability = 0))
q<-as.numeric(zero1$Profitability)
plot(p,q,xlab="Comedy genre",ylab="profit")
abline(lm(p~ q,data=newdata),col='red')
sd(zero1$Profitability)

newdata <- movie.df[which(movie.df$Genre=="Romance"),]
x<-as.numeric(newdata$Genre)
zero2<-newdata %>% replace_na(list(Profitability = 0))
y<-as.numeric(zero2$Profitability)
plot(x,y,xlab="genre",ylab="profit")
abline(lm(x~ y,data=newdata),col='red')


newdata <- movie.df[which(movie.df$Genre=="Fantasy"),]
k<-as.numeric(newdata$Genre)
zero3<-newdata %>% replace_na(list(Profitability = 0))
l<-as.numeric(zero3$Profitability)
plot(k,l,xlab="genre",ylab="profit")



newdata <- movie.df[which(movie.df$Genre=="Animation"),]
ox<-as.numeric(newdata$Genre)
zero4<-newdata %>% replace_na(list(Profitability = 0))
oy<-as.numeric(zero4$Profitability)
plot(ox,oy,xlab="genre",ylab="profit")
abline(lm(ox~ oy,data=newdata),col='red')


newdata <- movie.df[which(movie.df$Genre=="Action"),]
oa<-as.numeric(newdata$Genre)
zero5<-newdata %>% replace_na(list(Profitability = 0))
ob<-as.numeric(zero5$Profitability)
plot(oa,ob,xlab="genre",ylab="profit")
abline(lm(oa~ ob,data=newdata),col='red')
cor(x,y,  method = "pearson")


########
##is there any relationship between audience score and rotten tomatoes score?
cor(movie.df$Audience..score..,movie.df$Rotten.Tomatoes..,  method = "pearson", use = "complete.obs")
library("ggpubr")
ggscatter(movie.df, x = "Audience..score..", y ="Rotten.Tomatoes..", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "audience", ylab = "rotten tomatoes")





