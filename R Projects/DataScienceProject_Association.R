################### Data Science Project ###############################
########################################################################
############### Song Recommendation Rules ##############################


### ***  Import Data Play counts *** ###
LastFM_data <- read.csv(file.choose())

########## Check Data Import ###########
head(LastFM_data,n=20)
tail(LastFM_data,n=20)
str(LastFM_data)
LastFM_data[1:10,]

#### Factorize the User variable to obtain list of Songs by User ######
length(LastFM_data$user) ## 289,955 records in the file
LastFM_data$user <- factor(LastFM_data$user)
levels(LastFM_data$user) ## 15,000 users
levels(LastFM_data$artist) ## 1,004 artists

############# Activate Library ########################################
library(arules) ## a-rules package for association rules
 
## Split Playlist into a list of users

User_Playlist <- split(x=LastFM_data[,"artist"],f=LastFM_data$user) 
## Remove artist duplicates
 
User_Playlist <- lapply(User_Playlist,unique) 
User_Playlist <- as(User_Playlist,"transactions")
## view this as a list of "transactions"
## transactions is a data class defined in arules
itemFrequency(User_Playlist)
## Lists the support of the 1,004 bands
## Number of times band is listed by 15,000 users
## Computes the  freq for each artist mentioned by the 15,000 users
?itemFrequencyPlot
## Plots the item frequencies (Only Bands with > 70% support)
itemFrequencyPlot(User_Playlist,support=.07,cex.names=0.45,ylab="Band",xlab="Relative Frequency",popCol="Black",horiz=TRUE)
## Plots the item frequencies (Only Top 20 Bands with > 70% support)
itemFrequencyPlot(User_Playlist,support=.07,cex.names=0.45,ylab="Band",xlab="Relative Frequency",topN=20,popCol="Black",horiz=TRUE)

## Finally, we build the association rules
## Only rules with support > 0.01 and confidence > .50
## Implies it can't be a super rare band

Playlist_Rules <- apriori(User_Playlist,parameter=list(support=.01,confidence=.5))
inspect(Playlist_Rules)
## Let's filter by lift > 5.
## Among those associations with support > 0.01 and confidence > .50, only show those with lift > 5
inspect(subset(Playlist_Rules, subset=lift > 5))
## Lastly, order by confidence to make it easier to understand
inspect(sort(subset(Playlist_Rules, subset=lift > 5), by="confidence"))