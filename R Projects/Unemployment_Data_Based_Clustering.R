####################### Data Science Project Clustering     ##############################

#######################  Data : Monthly US Unemp_Dataloyment Rates###############################
install.packages("ggrepel")
########## Import the data     ########################
Unemp_Data <- read.csv(file.choose())
############ Inspect Data #############################
head(Unemp_Data)
Unemp_Data[1:50,1]
str(Unemp_Data)

## k-means clustering on 2 dimensions (mean, stddev)

set.seed(1)
grpUnemp_Data <- kmeans(Unemp_Data[,c("mean","stddev")], centers=3, nstart=10)
## list of cluster assignments
o=order(grpUnemp_Data$cluster)
data.frame(Unemp_Data$state[o],grpUnemp_Data$cluster[o])
plot.new()
plot(Unemp_Data$mean,Unemp_Data$stddev,type="n",xlab="Mean", ylab="Stddev",main="Plot of Unemployment Rates Mean Vs.Std Deviation for 50 States",cex.main=0.8,xlim=c(3, 9),ylim = c(0.5,3.5))
text(x=Unemp_Data$mean,y=Unemp_Data$stddev,labels=Unemp_Data$state,
     fg = gray(0.7),cex=0.6, col=grpUnemp_Data$cluster+1)

?plot


##############################################################################################

## Trial with GG Plot and GG text Repel to display states more clearly ############################
##library(ggplot2)
##library(ggrepel)
##ggplot(data=Unemp_Data, x=Unemp_Data$mean,y=Unemp_Data$stddev,xlab="Mean", ylab="Stddev",xlim=c(3, 9),ylim = c(0.5,3.5))+
##geom_text_repel(cex=0.5,data=Unemp_Data,x=Unemp_Data$mean,y=Unemp_Data$stddev,aes(label=Unemp_Data$state))
