install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
crop.data <- read.csv("/Users/kaikukaholoaa/Desktop/Vital_Rates_KUR-PHR-LIS/crop.data.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric"))
summary(crop.data)
##########################################
#one way anova example

one.way <- aov(yield ~ fertilizer, data = crop.data)
#Density (Categorical) = # of observations = 48 obs for density 1 and 48 obs for density 2
#Block (Categorical) = # of observations = 24 obs for block 1, 2, 3, and 4
#Fertilizer (Categorical) = # of observations = 32 obs for fertilizer 1, 2, and 3
#Yield (Numerical) = numeric summary (min. median. mode. IQR.)

#summary of one way anova:
summary(one.way)
#df = df for the independent variable (the number of levels in the variable minus 1),
#df residuls = df for the residuals (the total number of observations minus one and minus the number of levels in the independent variables).
#sumsq = the total variation between the group means and the overall mean).
#mean sq = Mean Sq column is the mean of the sum of square
#f-value = from f test, The larger the F value, the more likely it is that the variation caused by the independent variable is real and not due to chance.
#Pr(>F) = p-value of f stat. how likely it is that the F-value calculated from the test would have occurred if the null hypothesis of no difference among group means were true.

##########################################

#two way anova, incorporating density AND fertilizer
two.way <- aov(yield ~ fertilizer + density, data = crop.data)

summary(two.way)
#when adding the density to it, we noticed that the sum sq went down, meaning that there is now less
#variation between group means and the overall mean. 

##########################################

#testing interaction
interaction <- aov(yield ~ fertilizer*density, data = crop.data)

summary(interaction)
#after testing the interaction between our two variables, we noticed that the sum sq is really low with a low f value
# I would hypothesize that this means that density has no effect on fertilizer. 
#not much variation that can be explained by the interaction between fertilizer and planting density.

##########################################

#adding a blocking/ confounding variable (variable that cannot be explained)
blocking <- aov(yield ~ fertilizer + density + block, data = crop.data)

summary(blocking)
#we notice a low f-value and a low pvalue for our f statistic, meaning that there probably arent any confounding variables
#not affecting how much variation in the dependent variable they explain.

##########################################

#finding the best fit model using AIC... lower AIC the better!

library(AICcmodavg)

model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names)
#looks like two way anova is the best

##########################################

#check for homoscedasticity using plot function
par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#plots are normal!
#If your model doesn’t fit the assumption of homoscedasticity, you can try the Kruskall-Wallis test instead.

##########################################

#Post-Hoc

#ANOVA tells us if there are differences among group means, but not what the differences are. 
#To find out which groups are statistically different from one another,  perform a tukey post-hoc test for pairwise comparisons:

tukey.two.way<-TukeyHSD(two.way)

tukey.two.way

#differences seem to be mostly between fertilizer treatments 3 and 1 (makes sense)
#3-1 and 3-2 are statistically significant (p-value > 0.05)
#2-1 are not statistically significant (p=0.44)

##########################################

#plot them!!
#When plotting the results of a model, it is important to display:

#the raw data
#summary information (mean and standard error)
#letters or symbols above each group being compared to indicate the groupwise differences.

#from anova, we know density and fertilizer are sig. in plant size
#To display this information on a graph, 
#we need to show which of the combinations of fertilizer type + planting density are statistically different from one another.

tukey.plot.aov<-aov(yield ~ fertilizer:density, data=crop.data)

tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#differences are mostly between 3 and 1
#p-value for these pairwise differences is < 0.05 because confidence intervals dont include 0. 

##########################################

#making a dataframe

mean.yield.data <- crop.data %>%
  group_by(fertilizer, density) %>%
  summarise(
    yield = mean(yield)
  )

#Next, add the group labels as a new variable in the data frame.

mean.yield.data$group <- c("a","b","b","b","b","c")

mean.yield.data

#plot the raw data

two.way.plot <- ggplot(crop.data, aes(x = density, y = yield, group=fertilizer)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

two.way.plot

#Add the means and standard errors to the graph

two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.yield.data, aes(x=density, y=yield))

two.way.plot

#Split up the data (so you can see better)

two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$group, vjust = -8, size = 5) +
  facet_wrap(~ fertilizer)

two.way.plot

#wow! density 2 is sig higher in fert 1, but not in 2 and 3

#make ready for publication

two.way.plot <- two.way.plot +
  theme_classic2() +
  labs(title = "Crop yield in response to fertilizer mix and planting density",
       x = "Planting density (1=low density, 2=high density)",
       y = "Yield (bushels per acre)")

two.way.plot

##########################################
#Report results!

#We found a statistically-significant difference in average crop yield by 
#both fertilizer type (f(2)=9.018, p < 0.001) and 
#by planting density (f(1)=15.316, p<0.001).

#A Tukey post-hoc test revealed that fertilizer mix 3 resulted in a higher yield on 
#average than fertilizer mix 1 (0.59 bushels/acre), and a higher yield on average than 
#fertilizer mix 2 (0.42 bushels/acre). Planting density was also significant, with planting 
#density 2 resulting in an higher yield on average of 0.46 bushels/acre over planting density 1.

#A subsequent groupwise comparison showed the strongest yield gains at planting density 2, 
#fertilizer mix 3, suggesting that this mix of treatments was most advantageous for crop growth 
#under our experimental conditions.


