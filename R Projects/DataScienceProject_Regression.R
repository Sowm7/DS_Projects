############################## Data Science Project Regression ######################

#### Activate Required Libraries ####################

library(car)  

## Import the Flight Data #######################
#### Data :
Flight_data <- read.csv(file.choose())

########## Check Imported Data ##############
head(Flight_data)
sum(Flight_data)
Flight_data[1:3,]
## Define hours of departure
Flight_data$sched=factor(floor(Flight_data$schedtime/100))

table(Flight_data$carrier)
table(Flight_data$dest)
table(Flight_data$origin)
table(Flight_data$weather)
table(Flight_data$dayweek)
table(Flight_data$daymonth)
table(Flight_data$delay)
Flight_data$delay=recode(Flight_data$delay,"'delayed'=1;else=0")
Flight_data$delay=as.numeric(levels(Flight_data$delay)[Flight_data$delay])
table(Flight_data$delay)
##Delay: 1=Monday; 2=Tuesday; 3=Wednesday; 4=Thursday;
## 5=Friday; 6=Saturday; 7=Sunday
## 7=Sunday and 1=Monday coded as 1
Flight_data$dayweek=recode(Flight_data$dayweek,"c(1,7)=1;else=0")
table(Flight_data$dayweek)
## Omit unused variables
Flight_data=Flight_data[,c(-1,-3,-5,-6,-7,-11,-12)]
Flight_data[1:3,]
############# Compute Size of Training and Test Datasets #################
Tot_Size=length(Flight_data$delay)
Training_Size=floor(Tot_Size*(0.6))
Test_Size=Tot_Size-Training_Size


set.seed(1)
Train=sample(1:Tot_Size,Training_Size)
## estimation of the logistic regression moFlight_data
## explanatory variables: carrier, destination, origin, weather, day of week
## (weekday/weekend), scheduled hour of departure
## create design matrix; indicators for categorical variables (factors)
XFlight_data <- model.matrix(delay~.,data=Flight_data)[,-1]
XFlight_data[1:3,]
X_Train <- XFlight_data[Train,]
X_Test <- XFlight_data[-Train,]
Y_Train <- Flight_data$delay[Train]
Y_Test<- Flight_data$delay[-Train]
Model_1=glm(delay~.,family=binomial,data=data.frame(delay=Y_Train,X_Train))
summary(Model_1)
## prediction: predicted default probabilities for cases in test set
ptest_result <- predict(Model_1,newdata=data.frame(X_Test),type="response")
data.frame(Y_Test,ptest)[1:10,]
## first column in list represents the case number of the test element
plot(Y_Test~ptest_result)
 
## coding as 1 if probability 0.5 or larger
gg1=floor(ptest+0.5) ## floor function; see help command
ttt=table(ynew,gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error
## coding as 1 if probability 0.3 or larger
gg2=floor(ptest+0.7)
ttt=table(ynew,gg2)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error
bb=cbind(ptest,ynew)
bb
bb1=bb[order(ptest,decreasing=TRUE),]
bb1
## order cases in test set according to their success prob
## actual outcome shown next to it
## overall success (Flight_dataay) prob in the evaluation data set
xbar=mean(ynew)
xbar
## calculating the lift
## cumulative 1's sorted by predicted values
## cumulative 1's using the average success prob from evaluation set
axis=dim(n2)
ax=dim(n2)
ay=dim(n2)
axis[1]=1
ax[1]=xbar
ay[1]=bb1[1,2]
for (i in 2:n2) {
  axis[i]=i
  ax[i]=xbar*i
  ay[i]=ay[i-1]+bb1[i,2]
}
aaa=cbind(bb1[,1],bb1[,2],ay,ax)
aaa[1:100,]
plot(axis,ay,xlab="number of cases",ylab="number of successes",main="Lift:
     Cum successes sorted by pred val/success prob")
points(axis,ax,type="l")