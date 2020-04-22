
##______________________Question 2.1_________________________________

#' load the smoking data
#+ smokeData
smokeUrl = 'http://pbrown.ca/teaching/appliedstats/data/smoke.RData'
(smokeFile = tempfile(fileext='.RData'))
download.file(smokeUrl, smokeFile)
(load(smokeFile))
dim(smoke)

#'
#'
# Let's look at and isolate the values that we care about
smoke[1:5,c('Age','Sex','Grade','RuralUrban','Race', 'chewing_tobacco_snuff_or', 'ever_tobacco_hookah_or_wa')]

# Let's take a look at the grades and ages of the smokers
table(smoke$Grade, smoke$Age, exclude=NULL)

table(smoke$Race, smoke$chewing_tobacco_snuff_or, exclude = c(NULL, NA))
table(smoke$RuralUrban, smoke$chewing_tobacco_snuff_or, exclude = c(NULL, NA))
table(smoke$Sex, smoke$ever_tobacco_hookah_or_wa, exclude = c(NULL, NA))


#' nine year olds look suspicious
#' get rid of missings and age 9
smokeSub = smoke[smoke$Age != 9 & !is.na(smoke$Race) &
                   !is.na(smoke$RuralUrban) & !is.na(smoke$chewing_tobacco_snuff_or) & !is.na(smoke$ever_tobacco_hookah_or_wa), ]
dim(smokeSub)

smokeSub[1:10,c('Age','Sex','Grade','RuralUrban','Race', 'chewing_tobacco_snuff_or', 'ever_tobacco_hookah_or_wa')]


# let's just look at a table of race vs. chewing tobacco
smokeAggRaceChew = reshape2::dcast(smokeSub, Race ~ chewing_tobacco_snuff_or,length)
smokeAggRaceChew = na.omit(smokeAggRaceChew)
smokeAggRaceChew$total = smokeAggRaceChew$'FALSE' + smokeAggRaceChew$'TRUE'
smokeAggRaceChew$prop = smokeAggRaceChew$'TRUE' / smokeAggRaceChew$total
smokeAggRaceChew[,c("Race", "FALSE", "TRUE", "total", "prop")]



#We center the data around the median age, which is 14
smokeSub$AgeC = smokeSub$Age - 14


interaction.plot(smokeSub$Age,smokeSub$Sex,smokeSub$chewing_tobacco_snuff_or)
interaction.plot(smokeSub$Age,smokeSub$RuralUrban,smokeSub$chewing_tobacco_snuff_or)
interaction.plot(smokeSub$Sex,smokeSub$RuralUrban,smokeSub$chewing_tobacco_snuff_or)

interaction.plot(x.factor =smokeSub$Age,    # variable to plot on x-axis
                 trace.factor = smokeSub$Sex, # variable to specify "traces"; here, lines
                 response = smokeSub$chewing_tobacco_snuff_or,    # variable to plot on y-axis
                 fun = mean,  # summary statistic to be plotted for response variable
                 type = "l",     # type of plot, here "l" for lines
                 ylab = "Chewing Tobacco Use",
                 xlab = "Age",
                 col = c("blue4", "red4"),
                 lty = 1,  # line type
                 lwd = 2,  # line width
                 trace.label = "Sex",  # label for legend
                 xpd = FALSE) #,  # 'clip' legend at border

#We fit a model
#the odds of black smoking is exp(-1.49797), 
#the odds of white people smoking is exp(-3.49371 )/exp(-3.49371 -1.49797) higher than black people of similar demograpic
#the odds of white people smoking is exp(-3.49371 )/exp(-3.49371 -0.73433) higher than hispanic people of similar demograpic



smokeFit = glm(chewing_tobacco_snuff_or ~ Race + RuralUrban + AgeC + Sex + AgeC*Sex, 
               family=binomial(link='logit'), data=smokeSub)

table <- summary(smokeFit)$coef
conf_int <- confint(smokeFit)
table2<- cbind(table, conf_int)
knitr::kable(table2, digits=3)


#We also try the model
#smokeFit2 = glm(chewing_tobacco_snuff_or ~ Race * RuralUrban, 
#               family=binomial(link='logit'), data=smokeSub)
#table2 <- summary(smokeFit2)$coef
#conf_int2 <- confint(smokeFit2)
#table3<- cbind(table2, conf_int2)
#knitr::kable(table3[c(1:3,7),], digits=3)
# But see that it is much more complicated, and the AIC is only 2 higher. Furthermore, the confidence interval contains 0 
# for Raceblack:RuralUrbanRural indicating it may not be a helpful parameter, we decide to stick with the simpler model, as
# it appears to be doing a fine job predicting


#creating a confidence interval 
#look T PVALUE AND CONFIDENCE INTERVALS DOESNT INCLUDE 0 - LOG(0)=1 
#getting the Estimates for who we care about, mainly white, black, hispanic
#these are the log odds of what we are trying to study
smoke_coeffs <- summary(smokeFit)[['coefficients']][c(1:3,7:9)]

coefs_2.5 <- conf_int[c(1:3,7), 1]
coefs_97.5 <- conf_int[c(1:3,7), 2]

#urban odds
a<- exp(smoke_coeffs[1]) #white urban
b<- exp(smoke_coeffs[1] + smoke_coeffs[2]) #black urban
c <- exp(smoke_coeffs[1] + smoke_coeffs[3]) #hispanic urban

urban <- c(a,b,c)
#urban lower CI
al<-exp(coefs_2.5[1])
bl<- exp(coefs_2.5[1] + coefs_2.5[2]) #black urban
cl <- exp(coefs_2.5[1] + coefs_2.5[3]) #hispanic urban
urbanl <-c(al,bl,cl)
#urban upper CI
au<-exp(coefs_97.5[1])
bu<- exp(coefs_97.5[1] + coefs_97.5[2]) #black urban
cu <- exp(coefs_97.5[1] + coefs_97.5[3]) #hispanic urban
urbanu <- c(au,bu,cu)

#rural odds
d <- exp(smoke_coeffs[1]+smoke_coeffs[4]) #white rural
e<- exp(smoke_coeffs[1]+smoke_coeffs[2]+ smoke_coeffs[4]) #black rural
f<- exp(smoke_coeffs[1]+smoke_coeffs[3]+ smoke_coeffs[4]) #hispanic rural

rural <- c(d,e,f)

#rural lower CI
dl <- exp(coefs_2.5[1]+coefs_2.5[4]) #white rural
el<- exp(coefs_2.5[1]+coefs_2.5[2]+ coefs_2.5[4]) #black rural
fl<- exp(coefs_2.5[1]+coefs_2.5[3]+ coefs_2.5[4]) #hispanic rural

rurall <- c(dl,el,fl)
#rural odds upper CI
du <- exp(coefs_97.5[1]+coefs_97.5[4]) #white rural
eu<- exp(coefs_97.5[1]+coefs_97.5[2]+ coefs_97.5[4]) #black rural
fu<- exp(coefs_97.5[1]+coefs_97.5[3]+ coefs_97.5[4]) #hispanic rural

ruralu <- c(du,eu,fu)


results_odds <- data.frame((matrix(c(urban,urbanl,urbanu,rural,rurall,ruralu),nrow=3,ncol=6)),
                           row.names=c("White Odds","Black Odds","Hispanic Odds"))
names(results_odds) <- c("Urban Est.", "Urban Lower", "Urban Upper", "Rural Est.", "Rural Lower", "Rural Upper")


#___________QUESTION 2.2_____________________

#CENTER AGE AT 15
#The likelihood of having used a hookah or waterpipe on at least one occasion is the same for two
#individuals of the different sexes, provided their age, ethnicity, and other demographic characteristics
#are similar


model2 <- glm(ever_tobacco_hookah_or_wa ~ Sex+ RuralUrban + Race + AgeC,
    family=binomial, data=smokeSub)
summary(model2)

#b2 
#The odds of the female smoking is 1.04 times higher than makes given that they are of the same background 
#this is insignificant - pvalue 

smokefitsum <- summary(model2)$coef
somefit_conf_int <- confint(model2)
new_tab<- cbind(smokefitsum, somefit_conf_int)
knitr::kable(new_tab[c(1:5,9),], digits=3)

hookah_coeffs <- summary(model2)[['coefficients']][c(1:5,9)]
confint_coefs_2.5 <- somefit_conf_int[c(1:5,9), 1]
confint_coefs_97.5 <- somefit_conf_int[c(1:5,9), 2]

#calcilate odds
#female odds
female_odds <- exp(hookah_coeffs[1]+hookah_coeffs[2])
male_odds <- exp(hookah_coeffs[1])
female_odds_confint_2.5 <- exp(confint_coefs_2.5[1]+confint_coefs_2.5[2])
female_odds_confint_97.5<- exp(confint_coefs_97.5[1]+confint_coefs_97.5[2])
male_odds_confint_2.5 <- exp(confint_coefs_2.5[1])
male_odds_confint_97.5<-exp(confint_coefs_97.5[1])


woman <- c(female_odds,female_odds_confint_2.5,female_odds_confint_97.5)
man<- c(male_odds,male_odds_confint_2.5,male_odds_confint_97.5)


hookah_odds <- data.frame(t(matrix(c(woman,man),nrow=3,ncol=2,byrow=F)), row.names = c("Female Odds", "Male Odds"))

names(hookah_odds) <- c("Estimate", "Lower Bound", "Upper Bound")
knitr::kable(hookah_odds, digits=3)


#------------------QUESTION 1-------------------------------
# Use a Gamma generalized linear model to model the lifetimes as a function of the thorax length and activity.
# Write a brief report (a half to one page of writing) summarizing the problem and the model used, and
# interpreting the coefficients in your model in terms of their effect on expected lifetime. Write a one-paragraph,
# non-technical, summary of the results, that might appear in a “Research News” media article about the
# laboratory in question.


# Gamma: a gamma distribution for positive continuous data, lifetime in days is positive and continuous
data('fruitfly', package='faraway')
summary(fruitfly)

library(ggplot2)
ggplot(fruitfly, aes(x = activity, y = longevity)) + geom_point(aes(colour = thorax))  +
  stat_summary(fun.y = mean,
               geom = "point",
               shape = 18,
               size = 3.5,
               color="yellow") +
  xlab("Sexually Active") +
  ylab("Longevity (days)")

fruitfly$thoraxC <- fruitfly$thorax - mean(fruitfly$thorax)
fruitfly$thoraxC1 <-fruitfly$thoraxC/ sd(fruitfly$thoraxC)

lifetime_model <- glm(longevity ~ thoraxC1 + activity, family=Gamma(link= "log"), data=fruitfly)
summary(lifetime_model)


#one standard deviation increase in thorax length leads to a (e^(.20433263)-1)100,  22.67% increase
#in lifespan in isolated flies

#Interceot interpretation: exp(intercept) an isolated fly at the mean thorax 
#length has an expected lifespan of 60 days

# activitylow: (exp(-0.11646)-1)*100 - activity low fly at the mean thorax level 
#have a 10.99% decrease in lifespan compared to an isolated fly at the mean thorax length 

#activityhigh: (exp(-0.41465896)-1)*100 decrease in expected lifetime compared to isolated

#lifetime_model2 <- glm(longevity ~ thorax + activity + thorax * activity, family=Gamma(link= "log"), data=fruitfly)
#summary(lifetime_model2) #this is a bad model, we have no indication that activity is related to life


#Make a histogram of the gamma model 


par(mfrow=c(2,3))

shape = 1/summary(lifetime_model)$dispersion
hist(fruitfly[fruitfly$activity == 'low','longevity'], prob=TRUE,
     xlab='low',main = "Low Baseline Dist.")
xSeq = seq(par('usr')[1], par('usr')[2], len=200)
lines(xSeq, 
      dgamma(xSeq, shape=shape, 
             scale = exp(lifetime_model$coef['(Intercept)'])/shape), #baseline intercept divided by shape
      col='red', lwd=2
)

shape = 1/summary(lifetime_model)$dispersion
hist(fruitfly[fruitfly$activity == 'high','longevity'], prob=TRUE,
     xlab='low')
xSeq = seq(par('usr')[1], par('usr')[2], len=200)
lines(xSeq, 
      dgamma(xSeq, shape=shape, 
             scale = exp(lifetime_model$coef['(Intercept)'])/shape), #baseline intercept divided by shape
      col='red', lwd=2
)

shape = 1/summary(lifetime_model)$dispersion
hist(fruitfly[fruitfly$activity == 'isolated','longevity'], prob=TRUE,
     xlab='low')
xSeq = seq(par('usr')[1], par('usr')[2], len=200)
lines(xSeq, 
      dgamma(xSeq, shape=shape, 
             scale = exp(lifetime_model$coef['(Intercept)'])/shape), #baseline intercept divided by shape
      col='red', lwd=2
)

shape = 1/summary(lifetime_model)$dispersion
hist(fruitfly[fruitfly$activity == 'one','longevity'], prob=TRUE,
     xlab='low')
xSeq = seq(par('usr')[1], par('usr')[2], len=200)
lines(xSeq, 
      dgamma(xSeq, shape=shape, 
             scale = exp(lifetime_model$coef['(Intercept)'])/shape), #baseline intercept divided by shape
      col='red', lwd=2
)

shape = 1/summary(lifetime_model)$dispersion
hist(fruitfly[fruitfly$activity == 'many','longevity'], prob=TRUE,
     xlab='low')
xSeq = seq(par('usr')[1], par('usr')[2], len=200)
lines(xSeq, 
      dgamma(xSeq, shape=shape, 
             scale = exp(lifetime_model$coef['(Intercept)'])/shape), #baseline intercept divided by shape
      col='red', lwd=2
)



