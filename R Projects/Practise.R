x <- 0:6
as.numeric(x)
seq

x <-c("a", "b", "c") 
as.logical(x)

m <-matrix(1:6, nrow=2, ncol=3)
m

m <-1:10 
m

x <-1:3 
y <-10:12 
cbind(x, y)

x <-1:3
y <-10:12 
rbind(x, y)

x <-list(1, "a", TRUE, 1+ 4i) 
x

x <-factor(c("yes", "yes", "no", "yes", "no"))
x
x <-factor(c("yes", "yes", "no", "yes", "no")) 
table(x)
x <-vector("list", length=5) 

x<- c(1, 2, NA, 10, 3)
x

x <-c(1, 2, NaN, NA, 4) 
is.na(x)

x <-data.frame(foo =1:4, bar =c(T, T, F, F)) 
x

name(x)<-c("New York", "Seattle", "Los Angeles")
x

?unserialize
?load
?read.csv
?save
 
x <-1:3 

?attri

y <-data.frame(a =1, b ="a") 
dput(y, file="y.R") 
new.y<-dget("y.R") 
?stats
for(i in 1:10){ 
  print(i) 
} 

m <-matrix(nrow=2, ncol=3.5)
m

m <-mat(nrow=2, ncol=3)
m <-matrix(nrow=2, ncol=3.5) 
dim(m)

swirl()


?copy()
?bind
?paste()


?unserialize
?dump
?text

?sort

install.packages("swirl",dependencies=TRUE)
library(swirl)
names(x)<-c("New York", "Seattle", "Los Angeles")
x <-vector("numeric", length=10) 
x
new.y
names(x)

class(names)
attr(names)
