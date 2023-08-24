###### some code for project ##########


setwd("/Users/yang/Documents/course/stat403/2023 - Bingham, Derek /project")

gh=read.table(file="data/design(1).csv",header=TRUE,sep=",")
gh=as.data.frame(gh)
gh

#make factors
gh$greenhouse=factor(gh$greenhouse)
gh$humidity=factor(gh$humidity)
gh$pot=factor(gh$pot)
gh$plant=factor(gh$plant)



gh$Trt <- with(gh, interaction(humidity,plant))
gh[1:5,]
library(ggplot2)
plot.dotplot <- ggplot(data=gh, aes(x=Trt, y=yield))+
  ggtitle("Dot plots to check for outliers")+ 
  xlab("Treatment (humidity.plant)")+
  ylab("yield")+ 
  geom_point(position=position_jitter(height=0.2, width=0.2), size=2)+ 
  geom_boxplot(alpha=0.1)
plot.dotplot



library(tidyverse)
library(plyr)
report <- ddply(gh, "Trt", summarize,
                n.Trt = length(yield),
                mean.Trt = mean(yield),
                sd.Trt = sd(yield))

report

# The standard deviations are all about the same. 
# There is no evidence of a increase in the standard
# deviations with the mean.


str(gh)

report2 <- gh %>%
  group_by(plant) %>%
  summarise(mean = mean(yield), sd = sd(yield))
report2




gh %>%
  group_by(humidity) %>%
  summarise(n = n(), mean = mean(yield), sd = sd(yield))


plot.interaction <- ggplot(data=report2, aes(x=plant, y=mean, group=humidity, color=humidity, linetype=humidity, shape=humidity ))+
  ggtitle("Interaction plot based on raw data")+ xlab("Plant")+ ylab("Mean Yield")+ geom_point(size=3)+ geom_line()
plot.interaction

plot.interaction <- ggplot(data=report2, aes(x=humidity, y=mean, group=plant, color=plant, linetype=plant, shape=plant ))+
  ggtitle("Interaction plot based on raw data")+ xlab("humidity")+ ylab("Mean Yield")+ geom_point(size=3)+ geom_line()
plot.interaction

interaction.plot(gh$humidity, gh$plant, gh$yield, type = "b", legend = TRUE)





boxplot(yield ~ humidity:plant, data=gh, range=0,
        notch=FALSE, # helps to compare pop means
        main="Biomass of seedlings",
        sub="Whiskers extend to range of data",
        xlab="humidity:plant", ylab="yield")

stripchart(yield ~ humidity:plant, data=gh, add=TRUE,
           vertical=TRUE, method="jitter", jitter=.1)




library(doBy) # We don’t compute standard errors because design is NOT a CRD 
report<- summaryBy(yield ~ humidity+plant, data=gh, FUN=c(length,mean,sd))





interaction.plot(gh$plant, gh$humidity, gh$yield, type = "b", legend = TRUE)

segments(as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"), x$"Lower CI", as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"), x$"Upper CI")
})

gh






library(lmerTest)
fit.lmerTest <- lmerTest::lmer(yield ~ humidity + plant + humidity:plant +
                                          (1 | greenhouse), data=gh)


fit.lmerTest <- lmerTest::lmer(yield ~ humidity + plant + humidity:plant +
                                 (1 | greenhouse), data=gh)
gh

anova(fit.lmerTest) # uses Satterhwaite approximation 
anova(fit.lmerTest, ddf="Kenward-Roger") 
anova(fit.lmerTest, ddf="lme4")
# the table from the original lmer() without p-values.


# 求各个treatment level 的mean的 CI 
# Notice that the lmerTest package and the lsmeans package BOTH provide lsmeans() # functions so you have to be careful that you get the correct function! 
temp.lsmo <- lsmeans::lsmeans(fit.lmerTest, ~humidity)
temp.lsmo
temp.lsmo2 <- lsmeans::lsmeans(fit.lmerTest, ~plant)
temp.lsmo2

temp.lsmo3 <- lsmeans::lsmeans(fit.lmerTest, ~humidity+plant)
temp.lsmo3


# interaction plot with error bar:
temp.lsmo3 # values
with(gh, interaction.plot(humidity, plant, yield, ylim=c(5.5,8)))
points(1,6.62, pch=9, col="red")
segments(1,5.79 ,1,7.46, col="red")
points(2,7.42, pch=9, col="blue")
segments(2, 6.58 ,2,8.25, col="blue")
points(1.05,6.19, pch=9, col="green")
segments(1.05,5.36 ,1.05,7.03, col="green")
points(2.05,6.89, pch=9, col="orange")
segments(2.05, 6.06 ,2.05,7.73, col="orange")



# 求各个treatment level 之间difference of mean的 CI: Hypothesis testing 
temp.pairs <- pairs(temp.lsmo, adjust='tukey') 
summary(temp.pairs, infer=TRUE)

temp.pairs2 <- pairs(temp.lsmo2, adjust='tukey') 
summary(temp.pairs2, infer=TRUE)

# The effect test for humidity failed to find a statistically 
# significant difference in the mean yield so the Tukey comparisons 
# also cannot distinguish the means among the humidity.


# Extract the variance components 
vc <- VarCorr(fit.lmerTest)
vc

#This indicates that the greenhouse-to-greenhouse variance (1.13917^2) 
# is larger in size to the sub-greenhouse- to-sub-greenhouse variation (0.46405^2). 
# This is useful information for planning future experiments.
sqrt(5.406)
sqrt(0.215)



str(gh)

#library(lattice)
#with(gh, xyplot(yield ~ humidity | plant, groups = Trt))

#layout(matrix(1:4, 2,2)) 
qqnorm(resid(fit.lmerTest),main="Q-Q plot for residuals")
plot(resid(fit.lmerTest) ~ fitted(fit.lmerTest),main="residual plot") 
abline(h=0)
ranef(fit.lmerTest)
qqnorm(ranef(fit.lmerTest)$greenhouse$"(Intercept)", main="Q-Q plot for the random effect" ) 




#### code I can not run 
library(lmerTest)
library(ggplot2)
library(broom)
library(broom.mixed)
fit.lmerTest <- lmerTest::lmer(yield ~ humidity + plant + humidity:plant +
                                 (1 | greenhouse), data=gh)

df <- broom::augment(fit.lmerTest)

diag.qqplot <- ggplot(df, aes(sample=.resid)) +
  stat_qq()+
  ggtitle("Q-Q plot for residuals")
diag.qqplot


diag.resplot <- ggplot(df, aes(x=.fitted, y=.resid))+
  ggtitle("Residual vs Predicted plot")+
  geom_point() + geom_hline(yintercept=0)
diag.resplot

diag.qqplot.batch <- ggplot(data.frame(batch.eff=ranef(fit.lmerTest)$greenhouse$"(Intercept)") , aes(sample=batch.eff))+
  stat_qq()+
  ggtitle("Q-Q plot for greenhouse effect")
diag.qqplot.batch


###







result <- aov(yield ~ humidity * plant + Error(greenhouse:humidity), data = gh)
summary(result)


result <- aov(yield ~ humidity * plant + Error(greenhouse:humidity), data = gh)
summary(result)






#segments(as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"), x$"Lower CI", as.numeric(as.character(x$Temp))+.5*(x$Gender=="Female"), x$"Upper CI")
})

#ylim=range(GT.means$lsmeans.table[,c("Lower CI","Upper CI")]



