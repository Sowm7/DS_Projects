####################### Data Science Internet Project #######################

##################Data : DataDB.com data ########################

######### Install and Activate Required Libraries  ####################

######### Import the Required Data #######################
Internet_Data <- read.csv(file.choose())
######### Check and Validate Data Import #################

View(Internet_Data)
head(Internet_Data)
tail(Internet_Data)
summary(Internet_Data)
str(Internet_Data)

##################  Unique Page Visits #########
Unique_Page_Visit_Analysis<-aov(Uniquepageviews~Visits, data=Internet_Data)
summary(Unique_Page_Visit_Analysis) 


################ Exits Analysis ###############

Exit_Analysis<-aov(Exits~Timeinpage+Continent+Sourcegroup+Bounces+
           Uniquepageviews+Visits, data=Internet_Data)
summary(Exit_Analysis)

############# Time Spent in Page Analysis ###############

Time_in_Page_Analysis<-aov(Timeinpage~Exits+Continent+Sourcegroup+Bounces+
                     Uniquepageviews+Visits, data=Internet_Data)
summary(Time_in_Page_Analysis)


############## High Bounce Rate Analysis##############
Internet_Data$BouncesNew <- Internet_Data$BouncesNew*0.01
Bounce_rate_Model<-glm(BouncesNew~Timeinpage+Continent+Exits+Sourcegroup+
                         Uniquepageviews+Visits, data=Internet_Data,family="binomial")
summary(Bounce_rate_Model)


