library(lattice)
# data summaries
df <- stat_rp
df1 <- df
df1 <- df1[, c('age', 'male', 'fav_sub', 'predisposition', 'att_test_missed', 'iq', 'practiced', 'mental_arithm')]
#df1 <- df1[complete.cases(df1), ]
dim(df1)
head(df1)
View(df1)


#Summary of columns
summary(df1)

# STEM + fav_sub
install.packages("vtree")
library(vtree)
vtree(df1, c("predisposition", "fav_sub"), 
      fillcolor = c( predisposition = "#e7d4e8", fav_sub = "#99d8c9"),
      horiz = FALSE, pruneNA = TRUE)
# Male/female
gender <- as.data.frame(table(df$male))              
gender
install.packages("ggplot2")
library(ggplot2)
ggplot(gender, aes(x = Var1, y = Freq, fill = Var1), size = 1) +  
  geom_bar(stat = "identity", width = 0.3) +
  geom_text(aes(label = Freq), vjust = 0)

practiced <- as.data.frame(table(df$practiced))              
practiced

# age
df1 %>% ggplot(aes(x = age)) + geom_histogram(bins=20)
qplot(df1$age,
      geom="histogram",
      binwidth = 5,  
      main = "Histogram for Age", 
      xlab = "Age",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(10,90))

# tests
df1 %>% ggplot(aes(x = att_test_missed)) + geom_histogram(bins=10)
df1 %>% ggplot(aes(x = iq)) + geom_histogram(bins=10)
df1 %>% ggplot(aes(x = mental_arithm)) + geom_histogram(bins=10)
qplot(df1$att_test_missed, geom="histogram", binwidth = 5, main = "Histogram for Attention test scores", xlab = "Scores",  
      fill=I("green"), col=I("red"), alpha=I(.2), xlim=c(0,80))
qplot(df1$iq, geom="histogram", binwidth = 5, main = "Histogram for IQ test scores", xlab = "Scores",  
      fill=I("green"), col=I("red"), alpha=I(.2), xlim=c(60,150))
qplot(df1$mental_arithm, geom="histogram", binwidth = 5, main = "Histogram for Mental Arithmetic test scores", xlab = "Scores",  
      fill=I("green"), col=I("red"), alpha=I(.2), xlim=c(0,50))




#########################################################
# Correlation of columns 
#numerical vars
install.packages("gridExtra")
library(gridExtra)
sc_plots = list()
sc_plots$sc1 = ggplot(df1, aes(x= age, y=mental_arithm)) +
  geom_point() + geom_rug()
sc_plots$sc2 = ggplot(df1, aes(x=sqrt(att_test_missed), y= mental_arithm))+ 
  geom_point() + geom_rug() + geom_smooth(method = "lm", se=FALSE) 
  #+geom_smooth(method = "lm", se=FALSE, formula = "y~log(x)", color="orange")
sc_plots$sc3 = ggplot(df1, aes(x= sqrt(iq), y=mental_arithm))+
  geom_point() + geom_rug() + geom_smooth(method = "lm", se=FALSE) 
  #+geom_smooth(method = "lm", se=FALSE, formula = "y~poly(x,2)", color="orange")
grid.arrange(sc_plots$sc1, sc_plots$sc2, sc_plots$sc3,
             ncol = 3)
ggplot(df1, aes(x= log(att_test_missed), y=mental_arithm)) +
  geom_point() + geom_rug()

xyplot(log(att_test_missed, 5)~mental_arithm, df1)
xyplot(log(att_test_missed)~log(mental_arithm), df1)

#categorical vars
boxplot(df1$mental_arithm ~ df1$male , 
        ylab="Score" , xlab="Gender")
boxplot(df1$mental_arithm ~ df1$predisposition , 
        ylab="Score" , xlab="Predisposition")
boxplot(df1$mental_arithm ~ df1$practiced, 
        ylab="Score" , xlab="Practiced")



##########################################################
# LR model
lm_test = lm(df1$mental_arithm ~ sqrt(df1$att_test_missed) + df1$iq + df1$predisposition + df1$male, na.action=na.omit)
summary(lm_test)
confint.lm(lm_test, "predisposition", level=0.95)

#############################################################
#checking model assumptions
# linearity
# constant var
# normality
qqmath(residuals(lm_test))
xyplot(residuals(lm_test)~df1$iq)
xyplot(residuals(lm_test)~df1$att_test_missed)
xyplot(residuals(lm_test)~df1$STEM)
xyplot(residuals(lm_test)~df1$male)
# residuals mean = 0
# residuals have constant variance
# residuals are normally distributed
## plot residuals against the predicted value OR against the explanatory variables 
## - to check the linearity assumption (residuals should evenly spread above and below zero)
plot(lm_test)
#plotting residuals against fitted values
xyplot(residuals(lm_test)~fitted(lm_test), ylab = 'residual')
xyplot(residuals(lm_test)~fitted(lm_test), type=c('p', 'r'), xlab = 'Fitted', ylab = 'Residual')#regression line for residuals
xyplot(residuals(lm_test)~fitted(lm_test), type=c('p', 'smooth'), xlab = 'Fitted values', ylab = 'Residuals')
xyplot(residuals(lm_test)~fitted(lm_test),groups=df1$predisposition, auto.key = TRUE, xlab = 'Fitted values', ylab = 'Residuals', main = "Groups: Predisposition")
xyplot(residuals(lm_test)~fitted(lm_test),groups=df1$male, auto.key = TRUE, xlab = 'Fitted values', ylab = 'Residuals', main = "Groups: Gender")

#plotting residuals against explanatory variables
xyplot(residuals(lm_test)~sqrt(df1$att_test_missed), type=c('p', 'smooth'),xlab = 'SQRT(attention test results)', ylab = 'Residual', main="Residuals VS SQRT(attention test results)") #regression line for residuals
xyplot(residuals(lm_test)~sqrt(df1$iq), groups=df1$male, auto.key = TRUE, type=c('p', 'smooth'),xlab = 'IQ', ylab = 'Residual', main="Residuals VS IQ") #regression line for residuals

##constant variance assumption:
## also using plots above (does the spread of residuals change as fitted values change?)
## if I have very few observations for a certain region of fitted(expl) var., then there will be less variation


######################################################
#R^2 - how much the variation of response var is explained by the LR model 
#[0,1]: 1=all the variation is explained by LR
# example for R^2=0.145: about 15% of variation is explained by our LR model => there is still a lot of variation that is not explained by the model


###normality assumptions:
#qqplot
qqmath(residuals(lm_test))
# if we don't have large deviations from that straight line, then this assumption is probably not violated
lm_iq = lm(df1$iq ~ df1$att_test_missed + df1$mental_arithm + df1$STEM + df1$male)
summary(lm_iq)

lm_att = lm(df1$att_test_missed ~ df1$iq + df1$mental_arithm + df1$STEM + df1$male)
summary(lm_att)

#checking assumptions:
qqmath(residuals(lm_test))

xyplot(residuals(lm_test)~df1$iq)
xyplot(residuals(lm_test)~df1$att_test_missed) #make transformation 
####################
xyplot(log(df$mental_arithm)~log(df$att_test_missed), auto.key = TRUE)
xyplot(df$att_test_missed~df$mental_arithm, groups=df1$predisposition, auto.key = TRUE)
xyplot(df$att_test_missed~df$mental_arithm, groups=df1$male, auto.key = TRUE)
trellis.focus()
panel.identify()
trellis.unfocus()
df1[df1$att_test_missed == 62]
######################
xyplot(residuals(lm_test)~df1$iq, groups=df1$STEM, auto.key = TRUE)
xyplot(residuals(lm_test)~df1$STEM)


# maybe, make CIs for estimates


