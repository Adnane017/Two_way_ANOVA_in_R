###############################################################
# Two-way ANOVA tutorial in R 
#
# File Created on: 16th July, 2020
# Created by: Adnane Ez-zizi
#             a.ez-zizi@bham.ac.uk
# 
# Most of the R outputs have been copied and pasted as 
# comments below their corresponding command lines. 
###############################################################

###############################################################
# Preliminary steps
###############################################################

# Load the necessary packages
library(ggplot2) # To make nice plots
library(car) # To be able to run Levene test

# Setting the working directory (DON'T FORGET TO MODIFY THE WORKING DIRECTORY PATH) 
setwd("~/Teaching/Two_way_ANOVA_in_R")

###############################################################
# Data preparation
###############################################################

### Load the advertising dataset 
Marks = read.csv("marks.csv")
head(Marks, n=3)
#   marks course qual
# 1    55      1    2
# 2    48      2    2
# 3    69      3    1

### Check the types of variables
str(Marks)
# 'data.frame':	300 obs. of  3 variables:
# $ marks : int  76 80 51 80 67 67 70 81 51 85 ...
# $ course: int  1 1 1 1 1 1 1 1 1 1 ...
# $ qual  : int  1 1 1 1 1 1 1 1 1 1 ...
# => COURSE AND QUAL SHOULD BE CONVERTED TO FACTOR VARIABLES BEFORE APPLYING ANOVA

### Convert 'course' and 'qual' into factor variables
Marks$course = factor(Marks$course, levels = c(1,2,3), labels = c('A', 'B', 'C'))
Marks$qual = factor(Marks$qual, levels = c(1,2), labels = c('A level', 'BTEC'))

summary(Marks)
#     marks       course       qual    
# Min.   :27.00   A:100   A_level:150  
# 1st Qu.:55.00   B:100   BTEC   :150  
# Median :62.00   C:100                
# Mean   :62.35                        
# 3rd Qu.:70.00                        
# Max.   :93.00   

### We have a balanced design: same number of observations per group combination
xtabs( ~ course + qual, data = Marks)
#       qual
# course A level BTEC
#      A      50   50
#      B      50   50
#      C      50   50

###############################################################
# Data exploration
###############################################################

### Note: https://www.r-graph-gallery.com/ contains graph templates that you can use
### to make some nice plots

### Effect of course
p1 = ggplot(Marks, aes(x = course, y = marks)) + 
        stat_summary(fun.y = mean, geom = "point", size=4) +
        stat_summary(fun.y = mean, geom = "line", aes(group = 1), linetype = "dashed", size=1) + 
        stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.15) +
        theme(legend.title = element_text(face="bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold"),
              plot.title = element_text(size = 14, face = "bold"),
              strip.text = element_text(size = 12, face = "bold")) 
plot(p1)

### Effect of qualification
p2 = ggplot(Marks, aes(x = qual, y = marks)) + 
        stat_summary(fun.y = mean, geom = "point", size=4) +
        stat_summary(fun.y = mean, geom = "line", aes(group = 1), linetype = "dashed", size=1) + 
        stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.15) + 
        xlab("qualification") +
        theme(legend.title = element_text(face="bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold"),
              plot.title = element_text(size = 14, face = "bold"),
              strip.text = element_text(size = 12, face = "bold")) 
plot(p2)


### Combined effects
pd = position_dodge(width = 0.2) # So that the error bars and points don't overlap 
p3 = ggplot(Marks, aes(x = course, y = marks, colour = qual)) + 
        stat_summary(fun.y = mean, geom = "point", size=4, position = pd) +
        stat_summary(fun.y = mean, geom = "line", aes(group = qual), linetype = "dashed", size=1, position = pd) + 
        stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.15, position = pd) + 
        labs(x = "course", y = "marks", colour = "qualification") + 
        scale_color_manual(breaks = c("A level", "BTEC"), values=c("red", "blue")) +
        theme(legend.position = "top",
              legend.title = element_text(face="bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold"),
              plot.title = element_text(size = 14, face = "bold"),
              strip.text = element_text(size = 12, face = "bold")) 
plot(p3)

###############################################################
# Two-way ANOVA without interaction
###############################################################

### Fit a two-way ANOVA model 
aov_2way = aov(marks ~ course + qual, data = Marks)
summary(aov_2way)
#              Df Sum Sq Mean Sq F value Pr(>F)  
# course        2    304   151.8   1.363 0.2575  
# qual          1    437   436.8   3.921 0.0486 *
# Residuals   296  32972   111.4  

# Model assumptions will be checked for the final model

###############################################################
# Adding an interaction term
###############################################################

### Add an interaction term between course and qualification to the previous model 
aov_inter = aov(marks ~ course + qual + course:qual, data = Marks)
summary(aov_inter)
#              Df Sum Sq Mean Sq F value  Pr(>F)
# course        2    304   151.8   1.430 0.24101
# qual          1    437   436.8   4.113 0.04345 *
# course:qual   2   1750   875.0   8.239 0.00033 ***
# Residuals   294  31222   106.2

### Check model assumptions

# Assumption 1: residuals are normally distributed
plot(aov_inter, 2)
# All points fall approximately along the reference line, suggesting that the residuals 
# are normally distributed.

# This is confirmed by running Shapiro-Wilk test
resid_aov = residuals(aov_inter) 
shapiro.test(resid_aov)
#         Shapiro-Wilk normality test
# 
# data:  resid_aov
# W = 0.99713, p-value = 0.8763

# Assumption 2: Homogeneity of the group variances (No need as we used Welsh's test)
plot(aov_inter, 1)
# There is no evident relationship between residuals and fitted values (i.e. the mean of 
# each group), suggesting that the variance doesn't differ between groups

# This is confirmed by running Levene test
leveneTest(marks ~ course * qual, data = Marks)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   5  1.2416 0.2896
#       294








