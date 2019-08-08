##Tony Zheng
## Project #4 Twitter Data

df<-read.csv("~/Business Analytics/FinalProject/twitter_data.csv")
View(df)

str(df)
##Changing the categorical into numeric for analysis
##I want to use verified and media_type as 2 variables of interest so these 2 will be changed


#dx<-as.logical(df$verified)
#View(dx)
#I verified that this makes the verified FALSE equal to 0 and TRUE equal to 1

df$verified<-as.numeric(df$verified)
#View(df)

#dx1<-as.numeric(df$media_type)
#View(dx1)
#This shows that photo would be =1, but there's a bunch of NA

#dx1[is.na(dx1)] <- 0
#View(dx1)
#this makes NA = 0


df$media_type<--as.numeric(df$media_type)
df[is.na(df)] <- 0
View(df)

dffactors<-df
##I want to convert the df back to factors since I fixed the NA and converted logic to numeric
#View(dffactors)
dffactors$media_type[dffactors$media_type==-1]<-"Photo"
dffactors$media_type[dffactors$media_type==0]<-"None"
dffactors$media_type<-factor(dffactors$media_type)

dffactors$verified[dffactors$verified==1]<-"Verified"
dffactors$verified[dffactors$verified==0]<-"Not Verified"
dffactors$verified<-factor(dffactors$verified)

str(dffactors)

boxplot(dffactors$retweet_count~dffactors$media_type,ylab="retweets",xlab="Photo Attached", main = "Tweets Based on Photo")
##This boxplot doesn't seem to yield much insight aside from the outliers in having a photo
##attempting to find mean to use to gain insight

#library(dplyr)
#dffactors %>% group_by(dffactors$media_type) %>% summarize(Average=mean(dffactors$retweet_count)) 
#library(plyr)

#x1<-ddply(dffactors,.(factor),summarize,mean=mean(dffactors$retweet_count))
#x1


mean(dffactors$retweet_count)
#Did not factor correctly the means, need to figure out another method 

mean(dffactors$retweet_count[dffactors$media_type=="Photo"])
mean(dffactors$retweet_count[dffactors$media_type=="None"])
##This shows us that with a photot he average number of retweets is 21,307 vs 17,014 which is significant.

mean(dffactors$retweet_count[dffactors$verified=="Verified"])
mean(dffactors$retweet_count[dffactors$verified=="Not Verified"])
##This shows us that being not verified actually yielded on average more retweets.

###Lets see if this holds true for favorites.

mean(dffactors$favorite_count[dffactors$media_type=="Photo"])
mean(dffactors$favorite_count[dffactors$media_type=="None"])

#the average is 53,984 vs 53,266, the values are too close for any significant insight. meaning there may not be a difference.
#this is a significant difference when compared to retweet counts.


mean(dffactors$retweet_count[dffactors$verified=="Verified"])
mean(dffactors$retweet_count[dffactors$verified=="Not Verified"])
#This follows the trend of not verified having more retweets, means more favorites as well.

#alternative way using t-test

test <- t.test(retweet_count ~ media_type, data= dffactors, var.equal = FALSE, paired=FALSE ,conf.level = .95)
test
#p-value = 6.945e-09, reject null hypothesis, there is significance between retweet count & media type

test2 <- t.test(retweet_count ~ verified, data= dffactors, var.equal = FALSE, paired=FALSE ,conf.level = .95)
test2
#p-value < 2.2e-16, reject null hypothesis, there is significance between retweet count & being verified

test3 <- t.test(favorite_count ~ media_type, data= dffactors, var.equal = FALSE, paired=FALSE ,conf.level = .95)
test3
#p-value = 0.6314, fail to reject null hypothesis, there ISN'T significance between favorite count & media type

test4 <- t.test(favorite_count ~ verified, data= dffactors, var.equal = FALSE, paired=FALSE ,conf.level = .95)
test4
#p-value < 2.2e-16, reject null hypothesis, there is significance between favorite count & being verified

#from the above I obtained the following variables that should be added to the model for:
#retweets_count~mediatype+verified
#favorite_count~verified
###favorite_count should not have mediatype

model1<-lm(df$display_text_width~df$favorite_count)
model2<-lm(df$display_text_width~df$retweet_count)
model3<-lm(df$display_text_width~df$favorite_count+df$retweet_count)
summary(model1)
summary(model2)
summary(model3)

#install.packages("lmtest")
library(lmtest)
##lrtest can only be used for nested models meaning that one model is a subset of another
lrtest(model1,model3)
lrtest(model2,model3)

anova(model1,model3)
anova(model2,model3)

tweets<-df[,c("display_text_width","favorite_count","retweet_count")]
modelA<-lm(display_text_width~favorite_count,data=tweets)
modelB<-lm(display_text_width~retweet_count,data=tweets)
modelC<-lm(display_text_width~favorite_count+retweet_count,data=tweets)


lm(display_text_width~favorite_count+retweet_count,data=tweets)

a<-coef(modelA)
print(a)
b<-coef(modelB)
print(b)
c<-coef(modelC)
print(c)

##Trying to figure out the optimal posting strategy:
optimal4retweets<-lm(df$retweet_count~df$hashtag_count+df$listed_count+df$followers_count+df$account_age+df$verified+df$media_type)
retweetcoef<-coef(optimal4retweets)
print(retweetcoef)

library(ggplot2)

ggplot(df,aes(x=retweet_count,y=followers_count))+geom_point(color='blue',fill='blue')
ggplot(df,aes(y=retweet_count,x=followers_count))+geom_point(color='blue',fill='blue')


library(psych)
library(lmtest)

attach(df)
plot(Jockers, retweet_count)

Jockers_c<-cut(Jockers,breaks=seq(-20,20,0.5))
plot(Jockers_c,retweet_count,outline=FALSE)

Jockers2<-Jockers^2
ols<-lm(retweet_count~Jockers+Jockers2)
summary(ols)

#can't find evidence that Jockers has an effect
#Jockers^2 = evidence it has negative effect.

#R-squared is trying to compare the fits (if you increase # of variable, R square goes up)



attach(df)
plot(Jockers, retweet_count)

Jockers_c<-cut(Jockers,breaks=seq(-20,20,0.5))
plot(Jockers_c,retweet_count,outline=FALSE)

Jockers2<-Jockers^2
ols<-lm(retweet_count~Jockers+Jockers2)
summary(ols)
##p-value is 0.06



attach(df)
plot(McDonald, retweet_count)

McDonald_c<-cut(McDonald,breaks=seq(-20,20,0.5))
plot(McDonald_c,retweet_count,outline=FALSE)

McDonald2<-McDonald^2
ols<-lm(retweet_count~McDonald+McDonald2)
summary(ols)
##p-value is 0.00059

attach(df)
plot(Sentiword, retweet_count)

Sentiword_c<-cut(Sentiword,breaks=seq(-20,20,0.5))
plot(Sentiword_c,retweet_count,outline=FALSE)

Sentiword2<-Sentiword^2
ols<-lm(retweet_count~Sentiword+Sentiword2)
summary(ols)
##p-value 7.824e-06



attach(df)
plot(Senticnet, retweet_count)

Senticnet_c<-cut(Senticnet,breaks=seq(-20,20,0.5))
plot(Senticnet_c,retweet_count,outline=FALSE)

Senticnet2<-Senticnet^2
ols<-lm(retweet_count~Senticnet+Senticnet2)
summary(ols)
##p-value 0.2632


favoriteT<-lm(favorite_count~retweet_count+display_text_width+
                followers_count+account_age+hashtag_count+
                hashtag_sentiment,data=df)
summary(favoriteT)

#the intercept is 2.678e+04, coeff for retweetcount 1.748,
#display_text_width: -3.376e+01, followers_count: 6.127e-04
#account_age: -9.712e-01, hashtag_count: -5.220e+03, hashtag_senti: 5.227e+02

#Therefore the equation would be
#Favorite_Count= 26780+(1.748*retweet_count)+(-33.76*display_text_width)+
#(.0006127*followers_count)+(-97.12*account_age)+ (-5220*hashtag_count)+
#(hashtag_sentiment*522.7)

##From this we can see the following would be a positive to favorite_counts
## positives:: retweet_count and hashtag_sentiment
##negatives:: display_textwidth, account_age, hashtag_count
#insigificance:: followers_count

favoriteTweets<-lm(favorite_count~retweet_count+display_text_width+
                     hashtag_count+
                hashtag_sentiment,data=df)
summary(favoriteTweets)

favoriteT2<-lm(favorite_count~Jockers+McDonald+Senticnet+
                 Sentiword + friends_count +listed_count +
                 statuses_count,data=df)
##viewing the coefficients
##positives are Jockers, McDonald
#insigificant: Senticent, Sentiword, friends_count
##negative: listed_count, and statuses_count

##Taking the positives from both to make the empirical model we will make the following
#retweet_count, hashtag_sentiment, Jockers and McDonald

emp.favoriteTweets<-lm(favorite_count~retweet_count+hashtag_sentiment+
                         Jockers+McDonald,data=df)

##The Formula would be the following:
##FavoriteCount= 20425.596 + (1.782*retweet_count)+(-1034.179*hashtag_sentiment)+(926.099*Jockers)+(681.609*McDonald)
#The strategy would be to have high retweet counts, with low value for hashtag_sentiment, and high values for Jockers and McDonald
bptest(emp.favoriteTweets)

#the p-value is 2.2e-16, we conclude there is heteroscedastcity

#install.packages("sandwich")
library("sandwich") ##needed to use vcovHC
coeftest(emp.favoriteTweets,vcov = vcovHC(emp.favoriteTweets,"HC1"))
summary(emp.favoriteTweets)

##I notice the Std. Error values using coeftest are roughly 100 smaller in comparison
# for hashtag_sentiment, Jockers and McDonald


Retweet<-lm(retweet_count~favorite_count+display_text_width+
              followers_count+account_age+hashtag_count+
              hashtag_sentiment,data=df)

##postitive:hashtag_count
#negatives: display_text_width, hashtag_sentiment
#insigificant: favorite_count, followers count, account_age

Retweet2<-lm(retweet_count~Jockers+McDonald+Senticnet+
                 Sentiword + friends_count +listed_count +
                 statuses_count,data=df)

#positives: Jockers, McDonald
#negative: Senticent, Sentiword
#insigificant: friends_count, statuses_count, listed_count


#Taking the positive from both results would be the following:
## hashtag_count, Jockers and McDonald

emp.Retweet<-lm(retweet_count~hashtag_count+Jockers+McDonald, data=df)

##The formula would be:
##RetweetCount= 19008.06 + (hashtag_count*-2291.79)+(Jockers*70.69)+(McDonald*1140.83)
#The strategey would be less hashtags, and high values for Jockers and McDonald


bptest(emp.Retweet)

#the p-value is 0.1706, we conclude there is no heteroscedastcity

#trying correction for hetero just to see the values
coeftest(emp.Retweet,vcov = vcovHC(emp.Retweet,"HC1"))
summary(emp.Retweet)

##I am not sure that I understand the results, since they also seemed to have changed.