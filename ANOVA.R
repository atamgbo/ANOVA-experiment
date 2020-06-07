#Exercise 4.2
#data <- read.csv("data.csv", header=TRUE,sep=",")
library(PMCMRplus)
data<- c(3129,3000,2865,2890,3200,3300,2975,3150,2800,2900,2985,3050,2600,2700,2600,2765)
mixing_techniques<- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
anova_data<-data.frame(STRENGTH=data,TECHNIQUE=mixing_techniques)
view(anova_data)
#Test the hypothesis that mixing technique affects the strenght of the cement.
model<-aov(STRENGTH~factor(TECHNIQUE),data=anova_data)
summary(model) 
#boxplot
boxplot(STRENGTH~factor(TECHNIQUE),data=anova_data)
#pairwise t test
pairwise.t.test(anova_data$STRENGTH,factor(anova_data$TECHNIQUE),p.adjust.method ="bonferroni" )
#Arlison-Darlin
out<-adAllPairsTest(STRENGTH~factor(TECHNIQUE),data=anova_data)
summary(out)
#Analyze residuals
#Normal assumption
residuals(model)
qqnorm(residuals(model))
qqline(residuals(model))
#Constant variance assumption
residuals(model)
plot(residuals(model))
qqline(residuals(model))
plot(anova_data$TECHNIQUE,residuals(model))
#Independence assumption
plot(residuals(model))

plot(anova_data$TECHNIQUE,anova_data$STRENGTH)
