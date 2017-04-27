####################### Data Science Insurance Project #######################

##################Data : Swedish Motor Insurance Data ########################

######### Install and Activate Required Libraries  ####################

######### Import the Required Data #######################
Insurance_Data <- read.csv(file.choose())
######### Check and Validate Data Import #################

View(Insurance_Data)
head(Insurance_Data)
tail(Insurance_Data)
summary(Insurance_Data)
str(Insurance_Data)

################## Few Plots to visualize the data #########
factor(Insurance_Data$Make)
plot(data=Insurance_Data,(Claims) ~ as.factor(Insurance_Data$Make))


################## Finding Correlation between the Payment and Insurance criteria####
cor(Insurance_Data$Claims,Insurance_Data$Payment)
cor(Insurance_Data$Insured,Insurance_Data$Payment)
plot(Insurance_Data$Insured,Insurance_Data$Payment,
     xlab="Insured Amount",ylab="Payment",cex=0.6,
     main="Insured Amount Vs.Payment",cex.main=0.8,cex.axis=0.6)
plot(Insurance_Data$Claims,Insurance_Data$Payment,
     xlab="Claims",ylab="Payment",cex=0.6,
     main="Claims Vs.Payment",cex.main=0.8,cex.axis=0.6)


############# Building a Linear Regression Model #################

Reg_model<-lm(data=Insurance_Data,Payment~Insured+Claims+Make+Bonus+Zone+Kilometres)

summary(Reg_model)

############# Grouping and Aggregation ####################

Zone_Analysis<-apply(Insurance_Data[,c(5,6,7)], 2, function(x) tapply(x, Insurance_Data$Zone, mean))
Zone_Analysis
KM_Analysis<-apply(Insurance_Data[,c(5,6,7)], 2, function(x) tapply(x, Insurance_Data$Kilometres, mean))
KM_Analysis
Bonus_Analysis<-apply(Insurance_Data[,c(5,6,7)], 2, function(x) tapply(x, Insurance_Data$Bonus, mean))
Bonus_Analysis

####################### Claims Model ##########################

Claims_Reg_Model <-lm(data=Insurance_Data,Claims~Kilometres+Zone+Bonus+Make+Insured)
summary(Claims_Reg_Model )