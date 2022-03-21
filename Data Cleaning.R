# Credit Cards Assignment
# Data Cleaning 

setwd("D:/Credit card Assignment")

library(dplyr)
library (readr)

# Load the data 
client_data<- read.csv('client_data.csv', header = T, sep = ",")
dim(client_data)                        # 30001 * 25
head(client_data)
header_client <- client_data[1,2:25]
client_data <- client_data[2:30001,2:25]
names(client_data) <- header_client

str(client_data)
dim(client_data)

# Convert characters to numeric and factor 
client_data <- client_data %>% 
  mutate_at(c(1,5,12:23), as.numeric)

client_data <- client_data %>% 
  mutate_at(c(2:4,6:11,24), as.factor)


# Change the name of the target 
colnames(client_data)[24] <- "DEFAULT"   

# Obtain the summary of the data 
summary(client_data)
table(client_data$DEFAULT)


# Convert the numerical values to categorical values 
# Credit Limit
client_data$LIMIT_BAL<- cut(client_data$LIMIT_BAL,breaks=c(0,10000,110000,210000,310000,410000,510000,1000000),
             labels = c("<=10000","10001-110000","110001-210000", "210001-310000","310001-410000","410001-510000",
                         ">510000"))
client_data$LIMIT_BAL[1:10]

# age
client_data$AGE <- cut(client_data$AGE,breaks=c(20,30,40,50,60,70,80), right=F,
           labels = c("20-29","30-39","40-49", "50-59","60-69","70-79"))

client_data$AGE[1:15]
summary(client_data)

# Education -> combine 5 and 6
levels(client_data$EDUCATION) <- c(0,1,2,3,4,5,5)

# Data Imputation on missing Values 
# Using polyreg (multinominal logistic regression)
levels(client_data$EDUCATION) <- c(NA,1,2,3,4,5,5)
levels(client_data$MARRIAGE)  <- c(NA,1,2,3)

# install.packages("backports")
library(mice)
impute <- mice(client_data[,3:4],m=3,seed=123)
print(impute)

client_data[,3:4] <- complete(impute,1)  # choose the first imputation

# Look at the distributions 
stripplot(impute,pch=20,cex=2)

summary(client_data)
# write.csv(client_data,'client_data_afterPreprocess_ver1.csv',row.names = F, col.names=F)
read.csv('client_data_afterPreprocess_ver1.csv', header=T, sep=',')

# Draw the bar chart for each repayment status 
# Compute each percentage
par(mar=c(4,4,4,4))
plot_1 <- ggplot(client_data, aes(factor(PAY_0))) +
  geom_bar(fill="red")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="September Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")

plot_2 <- ggplot(client_data, aes(factor(PAY_2))) +
  geom_bar(fill="orange")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="August Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")

plot_3 <- ggplot(client_data, aes(factor(PAY_3))) +
  geom_bar(fill="yellow")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="July Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")

plot_4 <- ggplot(client_data, aes(factor(PAY_4))) +
  geom_bar(fill="green")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="June Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")

plot_5 <- ggplot(client_data, aes(factor(PAY_5))) +
  geom_bar(fill="blue")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="May Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")

plot_6 <- ggplot(client_data, aes(factor(PAY_6))) +
  geom_bar(fill="purple")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="April Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")

library(cowplot)
plot_grid(plot_1, plot_2, plot_3,plot_4,nrow = 2,
          ncol = 2)
plot_grid(plot_5, plot_6,nrow = 2,
          ncol = 2)


# Relevel the Repayment status to -2,-1,0,1(1-3months delay),
# 2(4-6moths delay),3(>6months delay)
levels(client_data$PAY_0) <- c(-2,-1,0,1,1,1,2,2,2,3,3)
levels(client_data$PAY_2) <- c(-2,-1,0,1,1,1,2,2,2,3,3)
levels(client_data$PAY_3) <- c(-2,-1,0,1,1,1,2,2,2,3,3)
levels(client_data$PAY_4) <- c(-2,-1,0,1,1,1,2,2,2,3,3)
levels(client_data$PAY_5) <- c(-2,-1,0,1,1,1,2,2,2,3,3)
levels(client_data$PAY_6) <- c(-2,-1,0,1,1,1,2,2,2,3,3)

summary(client_data)
str(client_data)
# saveRDS(client_data,'client_data_afterPreprocess_ver4.csv')

client_data <- readRDS('client_data_afterPreprocess_ver4.csv')
summary(client_data)
str(client_data)

################################################################################################
# Bill Statement  
# y-axis is the bill need to pay 
par(mfrow = c(2,3))
plot(client_data$BILL_AMT1,col="red",ylab="Amount(NT Dollar)",main="September Bill Statement")
plot(client_data$BILL_AMT2,col="orange",ylab="Amount(NT Dollar)",main="August Bill Statement")
plot(client_data$BILL_AMT3,col="yellow",ylab="Amount(NT Dollar)",main="July Bill Statement")
plot(client_data$BILL_AMT4,col="green",ylab="Amount(NT Dollar)",main="June Bill Statement")
plot(client_data$BILL_AMT5,col="blue",ylab="Amount(NT Dollar)",main="May Bill Statement")
plot(client_data$BILL_AMT6,col="purple",ylab="Amount(NT Dollar)",main="April Bill Statement")

par(mfrow = c(1,1))
boxplot(client_data$BILL_AMT1,client_data$BILL_AMT2,
        client_data$BILL_AMT3,client_data$BILL_AMT4,
        client_data$BILL_AMT5,client_data$BILL_AMT6,
        xaxt="n",
        xlab="Months",
        ylab="Amount of bill payments (NT Dollars)",
        main="Boxplots of of amount of bill payments in 6 months",
        col=c("red", "orange","yellow", "green", "blue", "purple"))

axis(1,
     at=1:6,
     labels=c("September","August","July","June","May", "April"))


####################################################################################################
# Amount of previous statement
# side = 1,2,3,4 (bottom, left, top, right)
# line (which margin line)
par(mfrow = c(2,3))
plot(client_data$PAY_AMT1,col="red",ylab="Amount(NT Dollar)",main="Previous Payment in September")
abline(h=mean(client_data$PAY_AMT1), col="pink")
mtext(round(mean(client_data$PAY_AMT1),2), side=1, line=-2,cex = .6)

plot(client_data$PAY_AMT2,col="orange",ylab="Amount(NT Dollar)",main="Previous Payment in August")
abline(h=mean(client_data$PAY_AMT2), col="pink")
mtext(round(mean(client_data$PAY_AMT2),2), side=1, line=-2,cex = .6)

plot(client_data$PAY_AMT3,col="yellow",ylab="Amount(NT Dollar)",main="Previous Payment in July")
abline(h=mean(client_data$PAY_AMT3), col="pink")
mtext(round(mean(client_data$PAY_AMT3),2), side=1, line=-2,cex = .6)

plot(client_data$PAY_AMT4,col="green",ylab="Amount(NT Dollar)",main="Previous Payment in June")
abline(h=mean(client_data$PAY_AMT4), col="pink")
mtext(round(mean(client_data$PAY_AMT4),2), side=1, line=-2,cex = .6)

plot(client_data$PAY_AMT5,col="blue",ylab="Amount(NT Dollar)",main="Previous Payment in April")
abline(h=mean(client_data$PAY_AMT5), col="pink")
mtext(round(mean(client_data$PAY_AMT5),2), side=1, line=-2,cex = .6)

plot(client_data$PAY_AMT6,col="purple",ylab="Amount(NT Dollar)",main="Previous Payment in May")
abline(h=mean(client_data$PAY_AMT6), col="pink")
mtext(round(mean(client_data$PAY_AMT6),2), side=1, line=-2,cex = .6)


######################################################################################
# continous vs categorical 
par(mfrow = c(1,3))
plot(BILL_AMT1~PAY_0, data=client_data)


# continous vs continous
# Scatter plot 
plot(client_data$BILL_AMT1,client_data$PAY_AMT1,col='red')

# Scatter plot of the amount of bill statement 
plot(client_data[12:17],col='red')


# Scatter plot of the amount of previous payback 
plot(client_data[18:23],col='red')


# skewness 
library(e1071) 
skewness(client_data$BILL_AMT1)# 2.663595
skewness(client_data$BILL_AMT2)
skewness(client_data$BILL_AMT3)
skewness(client_data$BILL_AMT4)
skewness(client_data$BILL_AMT5)
skewness(client_data$BILL_AMT6)

skewness(client_data$PAY_AMT1)
skewness(client_data$PAY_AMT2)
skewness(client_data$PAY_AMT3)
skewness(client_data$PAY_AMT4)
skewness(client_data$PAY_AMT5)
skewness(client_data$PAY_AMT6)

# Pairwise-t-test
bb <- as.numeric(client_data$DEFAULT)-1
client_data$BILL_AMT1
t.test(BILL_AMT1~bb, client_data, paired = TRUE, alternative = "two.sided")
pairwise.t.test(p.adjust="bonferroni")

sum(client_data$PAY_AMT6)
sum(client_data$PAY_AMT1)
sum(client_data$PAY_AMT1)-sum(client_data$PAY_AMT6)

