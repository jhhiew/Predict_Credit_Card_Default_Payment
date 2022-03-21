# Graph Representation 1

setwd("D:/Credit card Assignment")

client_data <- readRDS('client_data_afterPreprocess_ver4.csv')
summary(client_data)
str(client_data)


library(ggplot2)
library(dplyr)
library(plotrix)  # 3D pie chart


# 3D Pie Chart - SEX
count_SEX <- table(client_data$SEX)
pie_labels <- paste0(round(100 * count_SEX/sum(count_SEX), 2), "%")

pie3D(count_SEX,labels=pie_labels,explode=0.1,
      cex=1.5,
      col =  c("#b1c7fc", "#ffaca6"),
      main="Sex Distribution")

legend("topleft", legend = c("Male", "Female"),
       fill =  c("#b1c7fc", "#ffaca6"),
       cex=0.8,box.lty=0)


# 3D Pie Chart - MARRIAGE 
count_marriage <- table(client_data$MARRIAGE)
pie_labels_marriage <- paste0(round(100 * count_marriage/sum(count_marriage), 2), "%")

pie3D(count_marriage,labels=pie_labels_marriage,explode=0.1,
      cex=1.5,
      col =  c("#ffeeb0", "#f6d4ff","#b0ffcd"),
      main="Marital Status Distribution")

legend("topleft", legend = c("Married", "Single", "Others"),
       fill =  c("#ffeeb0", "#f6d4ff","#b0ffcd"),
       cex=0.65,box.lty=0)


# Bar Chart for Education
ggplot(client_data, aes(factor(EDUCATION))) +
        geom_bar(fill=c("#f9e1e0","#feadb9","#bc85a3","#9799ba","#4a7ba6"))+
        labs(x = "Educational Levels",y = "Numbers of clients",title ="Education Level Distribution")+
        scale_x_discrete(labels=c("Graduate School","University","High School", "Others", "Unknown")) +
        stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
                   vjust = -0.1, geom = "text", position = "stack", color ="black")

# Bar Chart for Age
ggplot(client_data, aes(factor(AGE))) +
        geom_bar(fill=c("#F8CA9D","#8EC9BB","#C5D7C0","#FB8E7E","#F2CF59","#FA6E4F"))+
        labs(x = "Age",y = "Numbers of clients",title ="Age Distribution")+
        scale_x_discrete(labels=c("20-29","30-39","40-49", "50-59", "60-69","70-79")) +
        stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
                   hjust=-0.1, geom = "text", position = "stack", color ="black") +
        coord_flip()
        
       
#############################################################################
# Compare between demographics and the the default payment 
# Mosaic Graph with p-value (using chi-square test)
# between categorical values 
# install.packages("vcd")

library(vcd)
tb2 <- structable(SEX~DEFAULT,client_data)
plot(tb2,gp=shading_hcl)

# Marriage
tb1 <- structable(MARRIAGE~DEFAULT,client_data)
plot(tb1,gp=shading_hcl)

# Education
tb3 <- structable(EDUCATION~DEFAULT,client_data)
plot(tb3,gp=shading_hcl)

# Age
tb4 <- structable(AGE~DEFAULT,client_data)
plot(tb4,gp=shading_hcl,cex.axis=0.5)

# Credit Balance
tb5 <- structable(LIMIT_BAL~DEFAULT,client_data)
plot(tb5,gp=shading_hcl,cex.axis=0.5)

#######################################################################################
# SEX
number_SEX <- ifelse(client_data$SEX == 2,1,1)
name_SEX <- ifelse(client_data$SEX == 2,"Female","Male")
name_SEX <- factor(name_SEX,levels=c("Male", "Female"))
default_payment <- ifelse(client_data$DEFAULT=='1', "Yes", "No")
data_SEX <- data.frame(name_SEX,number_SEX,default_payment)

ggplot(data_SEX, aes(x=name_SEX, y=number_SEX))+
  geom_col(aes(fill =default_payment), width = 0.7) + 
  labs(x = "SEX",y = "Numbers of clients",title ="Status of default payment by different sex") 
  # geom_text(size = 3, position = position_stack(vjust = 0.5))


# Martial Status
number_martial <- ifelse(client_data$MARRIAGE == 2,1,1)
# name_martial <- ifelse(client_data$SEX == 2,"Female","Male")
levels(client_data$MARRIAGE) <- c("Married", "Single", "Others")
name_martial <- factor(client_data$MARRIAGE,levels=c("Married", "Single", "Others"))
default_payment <- ifelse(client_data$DEFAULT=='1', "Yes", "No")
data_martial <- data.frame(name_martial,number_martial,default_payment)

ggplot(data_martial, aes(x=name_martial, y=number_martial))+
  geom_col(aes(fill =default_payment), width = 0.7) + 
  labs(x = "MARTIAL STATUS",y = "Numbers of clients",title ="Status of Default Payment by Different Martial Status") 
# geom_text(size = 3, position = position_stack(vjust = 0.5))

# Education
number_edu <- ifelse(client_data$EDUCATION == 2,1,1)
levels(client_data$EDUCATION) <- c("Graduate School", "University", "High School", "Others", "Unknown")
name_edu <- factor(client_data$EDUCATION,levels=c("Graduate School", "University", "High School", "Others", "Unknown"))
default_payment <- ifelse(client_data$DEFAULT=='1', "Yes", "No")
data_edu <- data.frame(name_edu,number_edu,default_payment)

ggplot(data_edu, aes(x=name_edu, y=number_edu))+
  geom_col(aes(fill =default_payment), width = 0.7) + 
  labs(x = "EDUCATION LEVELS",y = "Numbers of clients",title ="Status of Default Payment by Different Education Levels") 

# Age 
number_age <- ifelse(client_data$AGE == 2,1,1)
levels(client_data$AGE) <- c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79")
name_age <- factor(client_data$AGE,levels=c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79"))
default_payment <- ifelse(client_data$DEFAULT=='1', "Yes", "No")
data_age <- data.frame(name_age,number_age,default_payment)

ggplot(data_age, aes(x=name_age, y=number_age))+
  geom_col(aes(fill =default_payment), width = 0.7) + 
  labs(x = "AGE",y = "Numbers of clients",title ="Status of Default Payment by Different Age Group")


# Credit_Limit
number_limit <- ifelse(client_data$LIMIT_BAL == 2,1,1)
levels(client_data$LIMIT_BAL) <- c("<=10000", "10001-110000", "110001-210000", "210001-310000", "310001-410000", "410001-510000", ">510000")
name_limit <- factor(client_data$LIMIT_BAL,levels=c("<=10000", "10001-110000", "110001-210000", "210001-310000", "310001-410000", "410001-510000", ">510000"))
default_payment <- ifelse(client_data$DEFAULT=='1', "Yes", "No")
data_limit <- data.frame(name_limit,number_limit,default_payment)

ggplot(data_limit, aes(x=name_limt, y=number_limit))+
  geom_col(aes(fill =default_payment), width = 0.7) + 
  labs(x = "Amount of Given Credits (NT Dollar)",y = "Numbers of clients",title ="Status of Default Payment by Amount of Given Credits")



########################################################################################
# Repayment Status in 6 months
# Draw the bar chart for each repayment status 
# Compute each percentage

library(ggplot2)
library(dplyr)
par(mar=c(4,4,4,4))

plot_6 <- ggplot(client_data, aes(factor(PAY_6))) +
  geom_bar(fill="purple")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="April Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")


plot_5 <- ggplot(client_data, aes(factor(PAY_5))) +
  geom_bar(fill="blue")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="May Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")

plot_4 <- ggplot(client_data, aes(factor(PAY_4))) +
  geom_bar(fill="green")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="June Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")

plot_3 <- ggplot(client_data, aes(factor(PAY_3))) +
  geom_bar(fill="yellow")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="July Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")


plot_2 <- ggplot(client_data, aes(factor(PAY_2))) +
  geom_bar(fill="orange")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="August Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")

plot_1 <- ggplot(client_data, aes(factor(PAY_0))) +
  geom_bar(fill="red")+
  labs(x = "Repayment Status",y = "Numbers of clients",title ="September Payment Status")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.1, geom = "text", position = "stack", color ="black")


library(cowplot)
plot_grid(plot_6, plot_5, plot_4,plot_3,nrow = 2,
          ncol = 2)
plot_grid(plot_2, plot_1,nrow = 2,
          ncol = 2)

######################################################################################
# Decreasing/ Increasing Repayment Status
# April 
april_repay <- as.data.frame(table(client_data$PAY_6))
may_repay <- as.data.frame(table(client_data$PAY_5))
june_repay <- as.data.frame(table(client_data$PAY_4))
july_repay <- as.data.frame(table(client_data$PAY_3))
aug_repay <- as.data.frame(table(client_data$PAY_2))
sep_repay <- as.data.frame(table(client_data$PAY_0))
repay_whole <- cbind(april_repay,may_repay[,2],june_repay[,2],july_repay[,2],
                     aug_repay[,2],sep_repay[,2])
names(repay_whole) <- c("repayment","April", "May", "June", "July", "August", "September")

ggplot(data=repay_whole, aes(x=repayment,group=1)) +
  geom_line(aes(y=April), color="purple") +
  geom_line(aes(y=May), color="blue") + 
  geom_line(aes(y=June), color="green") +
  geom_line(aes(y=July), color="yellow") + 
  geom_line(aes(y=August), color="orange") +
  geom_line(aes(y=September), color="red") + 
  labs(x = "Repayment Status",y = "Numbers of clients",title ="Repayment Status Across 6 Months")


#################################################################################
# Transpose the graph Repayment
repay_whole_new <- data.frame(t(repay_whole))
colnames(repay_whole_new) <- repay_whole[, 1]
names(repay_whole_new) <- NULL
rownames(repay_whole_new) <- NULL
names(repay_whole) <- c("Months","April", "May", "June", "July", "August", "September")
repay_whole_new <- cbind(names(repay_whole),repay_whole_new)
colnames(repay_whole_new) <- NULL
rownames_repay <- repay_whole_new[,1]
repay_whole_new<- repay_whole_new[2:7,]
names(repay_whole_new) <- c("Months","a", "b", "c", "d", "e", "f")

# Use Probability 
prob_a <- as.numeric(repay_whole_new$a)
prob_a <- round((prob_a/30000 * 100),2)

prob_b <- as.numeric(repay_whole_new$b)
prob_b <- round((prob_b/30000 * 100),2)

prob_c <- as.numeric(repay_whole_new$c)
prob_c <- round((prob_c/30000 * 100),2)

prob_d <- as.numeric(repay_whole_new$d)
prob_d <- round((prob_d/30000 * 100),2)

prob_e <- as.numeric(repay_whole_new$e)
prob_e <- round((prob_e/30000 * 100),2)

prob_f <- as.numeric(repay_whole_new$f)
prob_f <- round((prob_f/30000 * 100),2)

months<- c("April", "May", "June", "July", "August", "September")
repay_data <- cbind(months,prob_a,prob_b,prob_c,prob_d,prob_e,prob_f)
repay_data <- as.data.frame(repay_data)

repay_the_new <- cbind(repay_data,repay_whole_new[,2:7])
repay_the_new[,1]<- factor(months,levels=c("April", "May", "June", "July", "August","September"))

# Line Graph for repayment status
par(mar=c(4,4,4,4))
gg <- ggplot(data=repay_the_new)
gg <- gg + geom_line(aes(x=months, y=as.numeric(prob_a),group=1), color="purple",size=1)
gg <- gg + geom_text(data=repay_the_new,aes(x=months, y=as.numeric(prob_a),label=prob_a), vjust=0.05,size=3.5) 

gg <- gg + geom_line(aes(x=months, y=as.numeric(prob_b),group=2), color="blue",size=1)
gg <- gg + geom_text(data=repay_the_new,aes(x=months, y=as.numeric(prob_b),label=prob_b), vjust=-0.1,size=3.5) 

gg <- gg + geom_line(aes(x=months, y=as.numeric(prob_c),group=3), color="green",size=1)
gg <- gg + geom_text(data=repay_the_new,aes(x=months, y=as.numeric(prob_c),label=prob_c), vjust=0.05,size=3.5) 

gg <- gg + geom_line(aes(x=months, y=as.numeric(prob_d),group=4), color="yellow",size=1)
gg <- gg + geom_text(data=repay_the_new,aes(x=months, y=as.numeric(prob_d),label=prob_d), vjust=1.8,size=3.5) 

gg <- gg + geom_line(aes(x=months, y=as.numeric(prob_e),group=5), color="orange",size=1)
gg <- gg + geom_text(data=repay_the_new,aes(x=months, y=as.numeric(prob_e),label=prob_e), vjust=0.01,size=3.5) 

gg <- gg + geom_line(aes(x=months, y=as.numeric(prob_f),group=6), color="red",size=1)
gg <- gg + geom_text(data=repay_the_new,aes(x=months, y=as.numeric(prob_f),label=prob_f), vjust=1.4,size=3.5) 

gg <- gg +   ylim(0, 60) + labs(x = "Months",y = "Percentage of clients (%) ",title ="Repayment Status Across 6 Months")
gg <- gg+ annotate("text", x=6.35, y=19, label= "-2:Paid in full, Inactive", cex=2.5) # pur
gg <- gg+ annotate("text", x=6.35, y=10, label= "-1:Paid in full", cex=2.5)           # blue
gg <- gg+ annotate("text", x=6.35, y=50, label= " 0:Paid min amount", cex=2.5)        # green
gg <- gg+ annotate("text", x=6.35, y=23, label= " 1:Delay 1-3 Months", cex=2.5)       # yellow
gg <- gg+ annotate("text", x=6.35, y= 1, label= " 2:Delay 4-5 Months", cex=2.5)       # orange
gg <- gg+ annotate("text", x=6.35, y= 0.1, label= " 3:Delay >6 Months", cex=2.5)        # red
gg


########################################################################################
# The bar charts 
delay_data <- client_data[,6:11]

levels(delay_data$PAY_0) <- c("N","N","N", "Y","Y","Y")
levels(delay_data$PAY_2) <- c("N","N","N", "Y","Y","Y")
levels(delay_data$PAY_3) <- c("N","N","N", "Y","Y","Y")
levels(delay_data$PAY_4) <- c("N","N","N", "Y","Y","Y")
levels(delay_data$PAY_5) <- c("N","N","N", "Y","Y","Y")
levels(delay_data$PAY_6) <- c("N","N","N", "Y","Y","Y")

sep_data <- delay_data$PAY_0
aug_data <- delay_data$PAY_2
july_data <-delay_data$PAY_3
june_data <-delay_data$PAY_4
may_data <- delay_data$PAY_5
april_data <-delay_data$PAY_6

name_SEX # from above 

no_sep_data <- ifelse(delay_data$PAY_0 == "N",1,1)
no_aug_data <- ifelse(delay_data$PAY_0 == "N",1,1)
no_july_data <- ifelse(delay_data$PAY_0 == "N",1,1)
no_june_data <- ifelse(delay_data$PAY_0 == "N",1,1)
no_may_data <- ifelse(delay_data$PAY_0 == "N",1,1)
no_april_data <- ifelse(delay_data$PAY_0 == "N",1,1)


delay_data_new <- data.frame(name_SEX,sep_data,aug_data,july_data,june_data,may_data,april_data,
                        no_sep_data,no_aug_data,no_july_data ,no_june_data ,no_may_data ,no_april_data )
class(delay_data_new)

ggplot(delay_data_new, aes(x=april_data, y=no_april_data))+
  geom_col(aes(fill = name_SEX), width = 0.7) + 
  labs(x = "Delay",y = "Numbers of clients",title ="Status of payment by different sex in April") 


###########################################################################################
# The bar charts 
delay_data <- client_data[,6:11]

levels(delay_data$PAY_0) <- c("N","N","N", "Y","Y","Y")
levels(delay_data$PAY_2) <- c("N","N","N", "Y","Y","Y")
levels(delay_data$PAY_3) <- c("N","N","N", "Y","Y","Y")
levels(delay_data$PAY_4) <- c("N","N","N", "Y","Y","Y")
levels(delay_data$PAY_5) <- c("N","N","N", "Y","Y","Y")
levels(delay_data$PAY_6) <- c("N","N","N", "Y","Y","Y")

sep_data <- delay_data$PAY_0
aug_data <- delay_data$PAY_2
july_data <-delay_data$PAY_3
june_data <-delay_data$PAY_4
may_data <- delay_data$PAY_5
april_data <-delay_data$PAY_6

# from above 
name_SEX 
name_martial
name_edu
name_age
name_limit

delay_data_new <- data.frame(name_SEX,name_martial,name_edu,name_age,name_limit,sep_data,aug_data,july_data,june_data,may_data,april_data)
class(delay_data_new)

##############################################################################################
# By Sex
sep_data_delay  <- delay_data_new[,1:6]
sep_data_delay_new <-sep_data_delay[!(sep_data_delay$sep_data =="N"),]  
dim(sep_data_delay_new)
a<- as.matrix(table(sep_data_delay_new$name_SEX))

aug_data_delay  <- delay_data_new[,1:7]
aug_data_delay_new <-aug_data_delay[!(aug_data_delay$aug_data =="N"),]  
dim(aug_data_delay_new)
b <- as.matrix(table(aug_data_delay_new$name_SEX))

july_data_delay  <- delay_data_new[,1:8]
july_data_delay_new <-july_data_delay[!(july_data_delay$july_data =="N"),]  
dim(july_data_delay_new)
c <- as.matrix(table(july_data_delay_new$name_SEX))

june_data_delay  <- delay_data_new[,1:9]
june_data_delay_new <-june_data_delay[!(june_data_delay$june_data =="N"),] 
dim(june_data_delay_new)
d <-as.matrix(table(june_data_delay_new$name_SEX)) 

may_data_delay  <- delay_data_new[,1:10]
may_data_delay_new <-may_data_delay[!(may_data_delay$may_data =="N"),] 
dim(may_data_delay_new)
e <- as.matrix(table(may_data_delay_new$name_SEX))

april_data_delay  <- delay_data_new[,1:11]
april_data_delay_new <-april_data_delay[!(april_data_delay$april_data =="N"),]  
dim(april_data_delay_new)
f <- as.matrix(table(april_data_delay_new$name_SEX))

only_delay_data <- cbind(f[,1],e[,1],d[,1],c[,1],b[,1],a[,1])
colnames(only_delay_data) <-c("April","May","June","July","August","September")
class(only_delay_data)


bar <- barplot(only_delay_data, beside = T,
        xlab = "Months",
        ylab = "Numbers of clients",
        main="Delay Payment Across 6 months by Sex",
        ylim=c(0,4500),
        col=c("#b1c7fc", "#ffaca6"))

y <- as.matrix(only_delay_data)
text(bar,y,labels=as.character(y),cex=1.0, pos=3)
# text(bar,y,labels=paste(as.character(round(prop.table(y,margin=2)*100,2)),"%"),cex=1.0, pos=1)

legend("top", legend = c("Male", "Female"),
       fill =  c("#b1c7fc", "#ffaca6"),
       cex=0.65,box.lty=0)


##################################
# By Martial Status
sep_data_delay  <- delay_data_new[,2:6]
sep_data_delay_new <-sep_data_delay[!(sep_data_delay$sep_data =="N"),]  
dim(sep_data_delay_new)
a<- as.matrix(table(sep_data_delay_new$name_martial))

aug_data_delay  <- delay_data_new[,2:7]
aug_data_delay_new <-aug_data_delay[!(aug_data_delay$aug_data =="N"),]  
dim(aug_data_delay_new)
b <- as.matrix(table(aug_data_delay_new$name_martial))

july_data_delay  <- delay_data_new[,2:8]
july_data_delay_new <-july_data_delay[!(july_data_delay$july_data =="N"),]  
dim(july_data_delay_new)
c <- as.matrix(table(july_data_delay_new$name_martial))

june_data_delay  <- delay_data_new[,2:9]
june_data_delay_new <-june_data_delay[!(june_data_delay$june_data =="N"),] 
dim(june_data_delay_new)
d <-as.matrix(table(june_data_delay_new$name_martial)) 

may_data_delay  <- delay_data_new[,2:10]
may_data_delay_new <-may_data_delay[!(may_data_delay$may_data =="N"),]  
dim(may_data_delay_new)
e <- as.matrix(table(may_data_delay_new$name_martial))

april_data_delay  <- delay_data_new[,2:11]
april_data_delay_new <-april_data_delay[!(april_data_delay$april_data =="N"),]  
dim(april_data_delay_new)
f <- as.matrix(table(april_data_delay_new$name_martial))

only_delay_data <- cbind(f[,1],e[,1],d[,1],c[,1],b[,1],a[,1])
colnames(only_delay_data) <-c("April","May","June","July","August","September")
class(only_delay_data)


bar <- barplot(only_delay_data, beside = T,
               xlab = "Months",
               ylab = "Numbers of clients",
               main="Delay Payment Across 6 Months by Martial Status",
               ylim=c(0,4000),
               col=c("#ffeeb0", "#f6d4ff","#b0ffcd"))

y <- as.matrix(only_delay_data)
text(bar,y,labels=as.character(y),cex=1.0, pos=3)
# text(bar,y,labels=paste(as.character(round(prop.table(y,margin=2)*100,2)),"%"),cex=0.65, pos=4)

legend("top", legend = c("Married", "Single", "Others"),
       fill =  c("#ffeeb0", "#f6d4ff","#b0ffcd"),
       cex=0.65,box.lty=0)


##################################
# By Education Level 
sep_data_delay  <- delay_data_new[,3:6]
sep_data_delay_new <-sep_data_delay[!(sep_data_delay$sep_data =="N"),]  
dim(sep_data_delay_new)
a<- as.matrix(table(sep_data_delay_new$name_edu))

aug_data_delay  <- delay_data_new[,3:7]
aug_data_delay_new <-aug_data_delay[!(aug_data_delay$aug_data =="N"),]  
dim(aug_data_delay_new)
b <- as.matrix(table(aug_data_delay_new$name_edu))

july_data_delay  <- delay_data_new[,3:8]
july_data_delay_new <-july_data_delay[!(july_data_delay$july_data =="N"),]  
dim(july_data_delay_new)
c <- as.matrix(table(july_data_delay_new$name_edu))

june_data_delay  <- delay_data_new[,3:9]
june_data_delay_new <-june_data_delay[!(june_data_delay$june_data =="N"),] 
dim(june_data_delay_new)
d <-as.matrix(table(june_data_delay_new$name_edu)) 

may_data_delay  <- delay_data_new[,3:10]
may_data_delay_new <-may_data_delay[!(may_data_delay$may_data =="N"),]  
dim(may_data_delay_new)
e <- as.matrix(table(may_data_delay_new$name_edu))

april_data_delay  <- delay_data_new[,3:11]
april_data_delay_new <-april_data_delay[!(april_data_delay$april_data =="N"),]  
dim(april_data_delay_new)
f <- as.matrix(table(april_data_delay_new$name_edu))

only_delay_data <- cbind(f[,1],e[,1],d[,1],c[,1],b[,1],a[,1])
colnames(only_delay_data) <-c("April","May","June","July","August","September")
class(only_delay_data)


bar <- barplot(only_delay_data, beside = T,
               xlab = "Months",
               ylab = "Numbers of clients",
               main="Delay Payment Across 6 Months by Education Levels",
               ylim=c(0,4000),
               col=c("#f9e1e0","#feadb9","#bc85a3","#9799ba","#4a7ba6"))

y <- as.matrix(only_delay_data)
text(bar,y,labels=as.character(y),cex=1.0, pos=3)
# text(bar,y,labels=paste(as.character(round(prop.table(y,margin=2)*100,2)),"%"),cex=0.65, pos=4)

legend("top", legend = c("Graduate School","University","High School", "Others", "Unknown"),
       fill =  c("#f9e1e0","#feadb9","#bc85a3","#9799ba","#4a7ba6"),
       cex=0.55,box.lty=0)


##################################
# By Education Level 

sep_data_delay  <- delay_data_new[,4:6]
sep_data_delay_new <-sep_data_delay[!(sep_data_delay$sep_data =="N"),]  
dim(sep_data_delay_new)
a<- as.matrix(table(sep_data_delay_new$name_age))

aug_data_delay  <- delay_data_new[,4:7]
aug_data_delay_new <-aug_data_delay[!(aug_data_delay$aug_data =="N"),]  
dim(aug_data_delay_new)
b <- as.matrix(table(aug_data_delay_new$name_age))

july_data_delay  <- delay_data_new[,4:8]
july_data_delay_new <-july_data_delay[!(july_data_delay$july_data =="N"),]  
dim(july_data_delay_new)
c <- as.matrix(table(july_data_delay_new$name_age))

june_data_delay  <- delay_data_new[,4:9]
june_data_delay_new <-june_data_delay[!(june_data_delay$june_data =="N"),] 
dim(june_data_delay_new)
d <-as.matrix(table(june_data_delay_new$name_age)) 

may_data_delay  <- delay_data_new[,4:10]
may_data_delay_new <-may_data_delay[!(may_data_delay$may_data =="N"),]  
dim(may_data_delay_new)
e <- as.matrix(table(may_data_delay_new$name_age))

april_data_delay  <- delay_data_new[,4:11]
april_data_delay_new <-april_data_delay[!(april_data_delay$april_data =="N"),]  
dim(april_data_delay_new)
f <- as.matrix(table(april_data_delay_new$name_age))

only_delay_data <- cbind(f[,1],e[,1],d[,1],c[,1],b[,1],a[,1])
colnames(only_delay_data) <-c("April","May","June","July","August","September")
class(only_delay_data)


bar <- barplot(only_delay_data, beside = T,
               xlab = "Months",
               ylab = "Numbers of clients",
               main="Delay Payment Across 6 Months by Age",
               ylim=c(0,4000),
               col=c("#F8CA9D","#8EC9BB","#C5D7C0","#FB8E7E","#F2CF59","#FA6E4F"))

y <- as.matrix(only_delay_data)
text(bar,y,labels=as.character(y),cex=1.0, pos=3)
# text(bar,y,labels=paste(as.character(round(prop.table(y,margin=2)*100,2)),"%"),cex=0.65, pos=4)

legend("top", legend = c("20-29","30-39","40-49", "50-59", "60-69","70-79"),
       fill =  c("#F8CA9D","#8EC9BB","#C5D7C0","#FB8E7E","#F2CF59","#FA6E4F"),
       cex=0.55,box.lty=0)

######################################################################

# By CREDIT LIMIT

sep_data_delay  <- delay_data_new[,5:6]
sep_data_delay_new <-sep_data_delay[!(sep_data_delay$sep_data =="N"),]  
dim(sep_data_delay_new)
a<- as.matrix(table(sep_data_delay_new$name_limit))

aug_data_delay  <- delay_data_new[,5:7]
aug_data_delay_new <-aug_data_delay[!(aug_data_delay$aug_data =="N"),]  
dim(aug_data_delay_new)
b <- as.matrix(table(aug_data_delay_new$name_limit))

july_data_delay  <- delay_data_new[,4:8]
july_data_delay_new <-july_data_delay[!(july_data_delay$july_data =="N"),]  
dim(july_data_delay_new)
c <- as.matrix(table(july_data_delay_new$name_limit))

june_data_delay  <- delay_data_new[,5:9]
june_data_delay_new <-june_data_delay[!(june_data_delay$june_data =="N"),] 
dim(june_data_delay_new)
d <-as.matrix(table(june_data_delay_new$name_limit)) 

may_data_delay  <- delay_data_new[,5:10]
may_data_delay_new <-may_data_delay[!(may_data_delay$may_data =="N"),]  
dim(may_data_delay_new)
e <- as.matrix(table(may_data_delay_new$name_limit))

april_data_delay  <- delay_data_new[,5:11]
april_data_delay_new <-april_data_delay[!(april_data_delay$april_data =="N"),]  
dim(april_data_delay_new)
f <- as.matrix(table(april_data_delay_new$name_limit))

only_delay_data <- cbind(f[,1],e[,1],d[,1],c[,1],b[,1],a[,1])
colnames(only_delay_data) <-c("April","May","June","July","August","September")
class(only_delay_data)

par(mar=c(4,4,4,4))
bar <- barplot(only_delay_data, beside = T,
               xlab = "Months",
               ylab = "Numbers of clients",
               main="Delay Payment Across 6 Months by Amount of Given Credits",
               ylim=c(0,4000),
               col=c("#E0BBE4","#957DAD","#D291BC","#FEC8D8","#FFDFD3","#FFF0F3", "#E6F7F1"))

y <- as.matrix(only_delay_data)
text(bar,y,labels=as.character(y),cex=1.0, pos=3)
# text(bar,y,labels=paste(as.character(round(prop.table(y,margin=2)*100,2)),"%"),cex=0.65, pos=4)

legend("topleft", legend = c("<=10000", "10001-110000", "110001-210000", "210001-310000", "310001-410000", "410001-510000", ">510000"),
       fill =  c("#E0BBE4","#957DAD","#D291BC","#FEC8D8","#FFDFD3","#FFF0F3", "#E6F7F1"),
       cex=0.65,box.lty=0)
