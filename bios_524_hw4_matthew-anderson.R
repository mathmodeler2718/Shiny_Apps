#Matthew Anderson
#Homework #4  BIOS 524 
#12.7.2018


# Question 1 ###################################################################################################################### 

#t interval
sample_1<-rnorm(30,0,1)
t.test(sample_1)


#my function for the t interval called random_normal

random_normal<-function(n,mu,sigma,alpha){
  sample_vector<-rnorm(n,mu,sigma)
  lower_bound<-mean(sample_vector)- abs(qt((alpha/2), n-1))*sd(sample_vector)/sqrt(n)
  upper_bound<-mean(sample_vector)+ abs(qt((alpha/2), n-1))*sd(sample_vector)/sqrt(n)
  output<-c(lower_bound, upper_bound)
  return(output)
}

random_normal(30,0,1,.05)

#similar results to the t.test function the random number generator will give slightly different answers

# Question 2 ###################################################################################################################### 

# a #
x<-as.vector(iris$Sepal.Length)
x<-sort(x)

#this function uniqueness will return the number of unique values in the Sepal.Length data
uniqueness<-function(x){
  sort(x)
  y<-c()
  
  for (i in 2:(length(x)-1) ){
    
    if (x[i]!=x[i+1] & x[i]!=x[i-1]){
      
         y[i]<-1
      
    } 
    
    else {
      y[i]<-0
     }
    
   if (x[1] !=x[2]){
     y[1]<-1
    }
    
     else {
       y[i]<-0
     }
  
     if (x[length(x)] !=x[length(x)-1]){
       y[length(x)]<-1
     } 
    
     else {
       y[i]<-0
     }
     
   }
  
  output<-sum(y)
  return(output)
}

uniqueness(x)
x

# the function works and returns the number of unique values, 9

########## b #

#this is performing the function on each column 
z<-as.matrix(iris[,1:5])

unique_per_column<-c()

  for (i in 1:ncol(z)){
    z<-apply(z, 2, sort)
    
    unique_per_column[i]<-uniqueness(z[,i])
    
  }

unique_per_column
#there are no unique entry's in column 4 and 5

##########  c #

#the apply function is good for this
apply(z,2, uniqueness)


# Question 3 ###################################################################################################################### 

x<-1:10
trim<-function(x,s,l){
  if (length(x)<(s+l+1)){
    stop("length of x not enough arguments")
  }
  sort(x)
  x<-mean(x[(1+s):(length(x)-l)])
  output<-x
  return(output)
}

trim(x,1,3)

#my trim function returns the correct mean

# Question 4 ###################################################################################################################### 


trig<-function(a,b,c){
  

  if(missing(a)){
      a<-sqrt(c^2-b^2)
  return(a)
  }
  
  if (missing(b)){
  b<-sqrt(c^2-a^2)
  return(b)
  }
  if (missing(c)){
  
    c<-sqrt(a^2+b^2)
    return(c)
  }
  
   missing[]
}

trig(3,4)
trig(3,4,5)
trig(3,,)
trig(,4,5)
trig(3,,5)

# Question 5 ###################################################################################################################### 
library(MASS) 
x<-Insurance

#first I need to manipulate the data slightly to calculate the number of holders per age
#then I use the tapply function to sum over each district

##########  a #
age1<-x[seq(1, 61, 4),c(1:5)]
age_less_25<-tapply(age1$Holders, age1$District, FUN=sum)

age2<-x[seq(2, 62, 4),c(1:5)]
age_25_29<-tapply(age2$Holders, age2$District, FUN=sum)

age3<-x[seq(3, 63, 4),c(1:5)]
age_30_35<-tapply(age3$Holders, age3$District, FUN=sum)


age4<-x[seq(4, 64, 4),c(1:5)]
age_more_35<-tapply(age4$Holders, age4$District, FUN=sum)

holders<-as.table(cbind(age_less_25,age_25_29, age_30_35, age_more_35))
holders

##########  b #
age1<-x[seq(1, 61, 4),c(1:5)]
age_less_25<-tapply(age1$Claims, age1$District, FUN=sum)

age2<-x[seq(2, 62, 4),c(1:5)]
age_25_29<-tapply(age2$Claims, age2$District, FUN=sum)

age3<-x[seq(3, 63, 4),c(1:5)]
age_30_35<-tapply(age3$Claims, age3$District, FUN=sum)


age4<-x[seq(4, 64, 4),c(1:5)]
age_more_35<-tapply(age4$Claims, age4$District, FUN=sum)

claims<-as.table(cbind(age_less_25,age_25_29, age_30_35, age_more_35))
claims

##########  c #
ratio<-claims/holders
ratio

########## d #
district_1<-ratio[1,]
district_2<-ratio[2,]
district_3<-ratio[3,]
district_4<-ratio[4,]


plot(district_1,xaxt="n", ann = FALSE, type="l",col="red",main="Ratio Claims per Holder by Age and District", 
     xlab="Age Group", ylab="Ratio of Claims per Holder", ylim=c(0,.3 ) )

lines(district_2,col="green")
lines(district_3,col="purple")
lines(district_4,col="blue")
axis(1, at=c(1:4), labels=c("under 25", "25 to 19 ","30 to 35", "over 35"))

legend(1, .1, legend=c("District 1","District 2","District 3", "District 4"), fill=c("red","green","purple","blue"))

title(main="Ratio Claims per Person by Age and District", xlab="Age Group", ylab="Ratio of Claims per Holder")


#It looks like as the age increases the number of claims  slightly  decreases. This makes sense since the data is 
#of car insurance claims 


# Question 6 ######################################################################################################################

diet_1_weight<-ChickWeight[which(ChickWeight$Diet==1),1]
diet_1_time<-ChickWeight[which(ChickWeight$Diet==1),2]

diet_2_weight<-ChickWeight[which(ChickWeight$Diet==2),1]
diet_2_time<-ChickWeight[which(ChickWeight$Diet==2),2]

diet_3_weight<-ChickWeight[which(ChickWeight$Diet==3),1]
diet_3_time<-ChickWeight[which(ChickWeight$Diet==3),2]

diet_4_weight<-ChickWeight[which(ChickWeight$Diet==4),1]
diet_4_time<-ChickWeight[which(ChickWeight$Diet==4),2]

par(mfrow=c(2,2))
plot(diet_1_time,diet_1_weight, main="Time vs Weight Diet 1")
plot(diet_2_time,diet_2_weight, main="Time vs Weight Diet 2")
plot(diet_3_time,diet_3_weight, main="Time vs Weight Diet 3")
plot(diet_4_time,diet_4_weight, main="Time vs Weight Diet 4")


# Question 7 ###################################################################################################################### 

########## a #

#Here I am making a mixture normal from two samples
mix_sample<-function(n,p,mu1,mu2,sigma1,sigma2){
  normal_1<-rnorm(n, mu1,sigma1)
  normal_2<-rnorm(n, mu2,sigma2)
  u<-rbinom(n, 1, p)
  y<-(1-u)*normal_1+ u*normal_2
  output<-list(y)
  return(output)
}

#drawing 2 samples y1 and y2 from the mixture of normal distributions generated by the functions
y1<-(mix_sample(1000,.5,-2,3,2,1.5))
y2<-(mix_sample(1000, 0.2,-2,3,2,1.5))
y1<-unlist(y1, recursive = TRUE, use.names = TRUE)
y2<-unlist(y2, recursive = TRUE, use.names = TRUE)


##########  b and C #

par(mfrow=c(1,2))
hist(y1, freq = FALSE)
lines(density(y1), col = "red")
legend(-9, .11, legend=c("y1 density"), fill=c("red"))

hist(y2, freq = FALSE)
lines(density(y2), col = "blue")
legend(0,.15, legend=c("y2 density"), fill=c("blue"))






