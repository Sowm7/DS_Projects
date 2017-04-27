####################### Data Science Hospital Project #######################

##################Data : DataDB.com data ########################

######### Install and Activate Required Libraries  ####################

######### Import the Required Data #######################
Hospital_Data <- read.csv(file.choose())
######### Check and Validate Data Import #################

View(Hospital_Data)
head(Hospital_Data)
tail(Hospital_Data)
summary(Hospital_Data)
str(Hospital_Data)

################## Plots to visualize the data ############
plot.new()
hist(Hospital_Data$AGE,breaks=max(Hospital_Data$AGE),xlab="Age Group ",ylab="No.of Patients/Visits",
     main="Children Hospital Visit Analysis",cex.main=0.8,cex=0.5,col="gray")
 
summary(as.factor(Hospital_Data$AGE))
##################  Finding Maximium Expenditure by Age Group #########
Exp_Analysis <-aggregate(TOTCHG ~ AGE, FUN = sum, data = Hospital_Data)

######### To Find the Maximum Expenditure ################
Exp_Analysis[which.max(Exp_Analysis$TOTCHG),]

##########Finding Maximum Expenditure by Type of Treatment and Hospitalization Cost ###############

which.max(summary(as.factor(Hospital_Data$APRDRG)))
Treatment_Analysis <- aggregate(TOTCHG ~ APRDRG, FUN = sum, data = Hospital_Data)
######### To Find the Maximum Expenditure ################
Treatment_Analysis[which.max(Treatment_Analysis$TOTCHG),]

############# Race Based Analysis ###############
Hospital_Data$RACE <- as.factor(Hospital_Data$RACE)
summary(Hospital_Data$RACE)
Hospital_Data <- na.omit(Hospital_Data)
Check_for_Variance <- aov(TOTCHG ~ RACE, data = Hospital_Data)
summary(Check_for_Variance)

################### Utilization Analysis ################
Hospital_Data$FEMALE <- as.factor(Hospital_Data$FEMALE)
summary(Hospital_Data$FEMALE)
 
Check_for_Utilization <- t.test(TOTCHG ~ FEMALE, data = Hospital_Data)
Check_for_Utilization 

Utilization_Model <- lm(TOTCHG ~ AGE + FEMALE, data = Hospital_Data)
summary(Utilization_Model)

############# Analyze the Length of Stay #################

Length_of_Stay_Model <- lm(LOS ~ AGE + FEMALE + RACE, data = Hospital_Data)
summary(Length_of_Stay_Model)

############# Analyze the Total Charges  #################

All_Features_Model <- lm(TOTCHG ~ ., data = Hospital_Data)
summary(All_Features_Model)