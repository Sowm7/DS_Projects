################## Retail Project  ##################################

##To automate the process of recommendations, the store needs to analyze the 
##given attributes of the product, like style, season, etc.,
##and come up with a model to predict the recommendation of products (in binary output - 0 or 1) accordingly

install.packages("forecast")
install.packages("imputeTS")
library(stats)
library(forecast)
library(imputeTS)

Dress_attributes <- readxl::read_excel(file.choose())
################### Check the file Import ##########################
head(Dress_attributes)
tail(Dress_attributes)
colnames(Dress_attributes)
View(Dress_attributes)
########### Rename the pattern_Type Column to avoid Errors ##############
colnames(Dress_attributes)[13] <- "Pattern_Type"
colnames(Dress_attributes)[15] <- "Total_Sales"
colnames(Dress_attributes)
Recommendation_Model <- glm(Recommendation ~ Style + Price + Rating + Size + 
                        Season + NeckLine +SleeveLength + waiseline + Material +
                        FabricType + Decoration + Pattern_Type,
                        data = Dress_attributes)
################### View the Recommendation Model Output ##################

summary(Recommendation_Model)
seq
###############################################################################
##In order to stock the inventory, the store wants to analyze the sales data
##and predict the trend of total sales for each dress for an extended period of
##three or more alternative days.
##############################################################################.

Dress_Sales <- readxl::read_excel(file.choose())


summary(Dress_Sales)
dim(Dress_Sales)
Dress_Sales_Ed <- Dress_Sales[,-c(25:35)]
View(Dress_Sales_Ed)
dim(Dress_Sales_Ed)
summary(Dress_Sales_Ed)
tail(Dress_Sales_Ed)
############ Compute Total Sales as Column Sums ###########
Totals<-colSums(Dress_Sales_Ed[,c(2:24)],na.rm=TRUE)
is.vector(Totals)
Totals
Timeseries_Model <- ts(Totals, start = 1, frequency = 7)
summary(Timeseries_Model)
#******************************************
##timeseries <- na.interpolation(timeseries)
#*******************************************
TS_Fit_Model <- auto.arima(Timeseries_Model)
summary(TS_Fit_Model)
predict(TS_Fit_Model,3)

forecast(TS_Fit_Model,3)
plot(forecast(TS_Fit_Model,3))

###############################################################################
##To decide the pricing for various upcoming clothes, the store wishes to find 
##how the style, season, and material affect the sales of a dress and 
##if the style of the dress is more influential than its price.
##############################################################################.

Dress_Sales_Ed$Total_Sales_by_ID <- rowSums(Dress_Sales_Ed[c(2:501),c(2:24)],na.rm=TRUE) 
View(Dress_Sales_Ed)
#*****************************************************************************
Merged_Attribute_Sales_Data <- merge(Dress_attributes,Dress_Sales_Ed,by="Dress_ID")
View(Merged_Attribute_Sales_Data)
#*****************************************************************************
#attach(Dress_attributes)
#************* To Analyse the Impact of each variable using the ANOVA Method
AOV_Style <- aov(Total_Sales ~Style,data=Dress_attributes)
AOV_Season<- aov(Total_Sales ~Season,data=Dress_attributes)
AOV_Material <- aov(Total_Sales ~Material,data=Dress_attributes)

summary(AOV_Style)
summary.aov(AOV_Season)
summary.aov(AOV_Material)


LR_Model_SSM <- lm(Total_Sales~Style + Season + Material, data = Dress_attributes)
summary(LR_Model_SSM)

LR_Price_Style<- lm(Total_Sales ~ Style+Price,data=Dress_attributes)
summary(LR_Price_Style)
###########################################################
##Also, to increase the sales, the management wants to analyze
##the attributes of dresses and find which are the leading factors 
##affecting the sales of a dress

##########################################################

All_Feature_LM_Model <- lm(Total_Sales~Style + Price + Rating + Size + Recommendation+
                             Season + NeckLine +SleeveLength + waiseline + Material +
                             FabricType + Decoration + Pattern_Type, data = Dress_attributes)
summary(All_Feature_LM_Model)
#######################################################

##To regularize the rating procedure and find its efficiency, 
##the store wants to find if the rating of the dress affects the total sales.
##To find the relation between rating and total sales (both are numerical variables), 
##perform a correlation of the two attributes.
 
########################################################  

cor.test(Dress_attributes$Total_Sales, Dress_attributes$Rating)
