#Jackson Anderson
#Computational Biology
#Assignment 8


library(tidyverse)
library(ggplot2)
library(wesanderson)
library("grid")
library("gridExtra")
library(lme4)
library(nlme)

#Setting Working Directory
setwd("/Users/jacksonanderson/Desktop/EBIO4420/CompBioLabsAndHomework/Labs/Assignment8/Data_and_Scripts")

#Reading data in as a tibble
transDat <- read_csv("TransplantBaeleDataCSV.csv")


#Below, I subset the data for brief visualizations. Thes visualizations were done in the console, and were purely meant to give me some general insights on the distrubtion's of the data within each treatment.


#Subsetting data by PoolType
createdDat <- transDat %>% 
  filter( PoolType == "Created")

naturalDat <- transDat %>% 
  filter( PoolType == "Natural")

#Plotting Functions: These Functions are to be used throughout the rest of the script to efficiently plot the data

residFunc <- function(model) {
  p1<-ggplot(model, aes(.fitted, .resid))  
  p1 <- p1 + geom_jitter(width = .008, aes(col = factor(.fitted)), size = 2, alpha = .5, show.legend = F)
  p1 <- p1 + scale_color_manual(values = wes_palette("FantasticFox1"))
  p1<-p1+geom_hline(yintercept=0, col="darkslategrey", linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
}

qqFunc <- function(model) {
  y <- quantile(model$resid[!is.na(model$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(model, aes(sample=.resid)) +
    stat_qq( color = "#46ACC8") +
    geom_abline(slope = slope, intercept = int, color="#E58601", alpha = .75) +
    xlab("Theoretical Quantiles")+ylab("Standardized Residuals") + ggtitle("Normal Q-Q") +
    theme_bw()
  
  return(p)
}

cooksFunc <- function(model) {
  p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity", aes(fill = "#E58601"), show.legend = F)
  p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
  p4<-p4+ggtitle("Cook's distance")+theme_bw()
  return(p4)
}

BoxPlotFuncNoOrder <- function( expData, treatment, response, ExpName, yLabel ) {
  prelimGraph <- ggplot( data = expData, aes( x = treatment, y = response) ) +
    geom_jitter(width = .02, aes(col = factor(treatment)), size = 2, alpha = .5, show.legend = F) +
    geom_boxplot( aes( col = factor(treatment)), 
                  show.legend = F,
                  fill = NA,
                  outlier.color = "Black",
                  outlier.shape = 1,
                  outlier.size = 1.5) +
    scale_color_manual(values = wes_palette("FantasticFox1")) +
    labs(title= ExpName, 
         x= "Treatment Category",
         y= yLabel,
         caption = "- (o) indicates outlier data point") +
    theme_bw()
  return(prelimGraph)
}

myPlotsNoOrder <- function(model, expData, treatment, response, ExpLabel, respLabel, Exper ) {
  
  p1 <- residFunc(model)
  
  p2 <- BoxPlotFuncNoOrder( expData, treatment, response, ExpLabel, respLabel )
  
  p3 <- qqFunc(model)
  
  p4 <- cooksFunc(model)
  
  gridPlot <- grid.arrange(p2, p1, p3, p4, nrow = 2, top = Exper)
  return(gridPlot)
}

#Initial Models, Graphs, and Data/Results Summary

#Models

fullLinModel <- lm(transDat$ReproWt ~ transDat$PoolType)#model for graphics
ttest1 <- t.test(ReproWt ~ PoolType, data = transDat)
ttest1

#Graphs
myPlotsNoOrder( fullLinModel, transDat, transDat$PoolType, transDat$ReproWt, "Reproductive Effort in Natural vs Created Pools", "Reproductive Output (g)", "Baele Transplant Experiment")

#result of a t.test suggest that there is a significant difference in mean fitness between the two groups. Plants in natural pools seem to have a reproductive biomass 2.4 grams heavier than that of plants in artificially created pools. This test does NOT account for random effects due to Transect and PoolID. As well, residual heteroskedasticity (as seen in the resid. vs fitted plot) suggests that the data does not meet the assumptions of normalcy required by the model.






#Further Models, Graphs, and Data/Results Summaries

#One way to account for heteroskedasticity in residual variance is to log transform the response variabel. I will create a new variable, LogReproWt, or a log transformed version of the variable ReproWt

transDat$LogReproWt <- log1p(transDat$ReproWt)


#I can now do the same models, graphs, and summaries as done before. This time I will do it on the log transformed data


#Models
fullLinModelLog <- lm(transDat$LogReproWt ~ transDat$PoolType)#model for graphics
ttest1 <- t.test(LogReproWt ~ PoolType, data = transDat)
ttest1

#Graphs
myPlotsNoOrder( fullLinModelLog, transDat, transDat$PoolType, transDat$LogReproWt, "Reproductive Effort in Natural vs Created Pools", "Reproductive Output (g), Log Transformed", "Baele Transplant Experiment: Log Transformed Response")

#t test and data visualizations suggests that there is still a significant difference in response variable means to treatment effect. As well, the data is not as heteroskedastic, and thus I feel more comfortable with the knowldge that the data is meeting assumptions of normalcy required for the model. In this case, when the response variable has been log transformed, the difference in means between treatments for LogReproWt is ~0.30, where plants in natural pools can be seen to have a higher fitness (using LogReproWt as a measure). Next, I must account for the random effects of PoolID and Transect location!

#Running a mixed model with PoolID and nested transect as random effects

transDat <- transDat[-c(65),] #first, removing NA row; lme function was having trouble computing without doing so. na.action option in lme function was giving me some major issues.
m1 <- lme(LogReproWt~PoolType , random = ~1 | PoolID/Trans, data = transDat)
summary(m1)

#Analyzying the results of this model suggest that, when accounting for random effects, there is no significant difference in response means by treatment. As well, it would seem that PoolID accounts for more variance in response variable distribution than Trans (0.4096561 vs 0.0001026181 StdDev., respectively). Thus, I will only use the fitted values for PoolID, and not transect, when making subsequent graphs

#Creating a data frame with values from the m1 model; done for graphing ease of use in diagnostics plots
m1DataFrame <- data.frame(m1$fitted[,2], m1$residuals[,2], m1$fitted[,1])

#Adding fitted values from m1 to transDat for ease of use when plotting. Taking the inverse log, such that values plotted represent fitted values that are untransformed, thus indicating what the actual predicted reproductive biomass might be from the model
transDat$Fitted <- expm1(m1$fitted[,2])



#Mixed Model Plotting: Given the strange syntax of the lme model, I was not able to use the plotting functions I used before; doing so would have required significant changes and specifications to each graph being made. Consequently, I made each graph seperately and stored it in an object to then be called when making the grid plot. 


#Diagnsotics Plot Mixed Model

MMDiagPlot <-ggplot(m1DataFrame, aes(m1DataFrame$m1.fitted...2., m1DataFrame$m1.residuals...2.))   + geom_jitter(width = .008, aes(col = factor(m1DataFrame$m1.fitted...1.)), size = 2, alpha = .5, show.legend = F) + 
  scale_color_manual(values = wes_palette("FantasticFox1")) + 
  geom_hline(yintercept=0, col="darkslategrey", linetype="dashed") +
  xlab("Fitted values")+
  ylab("Residuals") + 
  ggtitle("Figure 4: Residual vs Fitted Plot (using PoolID)")+theme_bw() + 
  labs( caption = "Brown = Created, Yellow = Natural ")
MMDiagPlot


#Boxplot mixedModel

MMBoxplot <- ggplot( transDat, aes( x = PoolType, y = Fitted),  ) +
  geom_count( aes( col = factor(PoolType)), show.legend = F, alpha =.75) +
  geom_boxplot( aes( col = factor(PoolType)), 
                show.legend = F,
                fill = NA) +
  scale_color_manual(values = wes_palette("FantasticFox1")) +
  labs(title= "Figure 2: Mixed Model Fitted Values By Pool For Each Treatment", 
       x= "Treatment Category",
       y= "Fitted Reproductive Biomass (g)",
       caption = "Letters are significance indicators") +
  theme_bw() +
  annotate("text", x=1, y=3.8, label= "a") + 
  annotate("text", x=2, y=8, label= "a")
MMBoxplot


#QQ Plot Mixed Model

y <- quantile(m1DataFrame$m1.residuals...2.[!is.na(m1DataFrame$m1.residuals...2.)], c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
MMQQplot <- ggplot(m1DataFrame, aes(sample=m1.residuals...2.)) +
  stat_qq( color = "#46ACC8") +
  geom_abline(slope = slope, intercept = int, color="#E58601", alpha = .75) +
  xlab("Theoretical Quantiles")+ylab("Standardized Residuals") + ggtitle(" Figure 3: Normal Q-Q") +
  theme_bw()
MMQQplot

#Box Plot Non-LME

NMMBoxplot <- ggplot( transDat, aes( x = PoolType, y = ReproWt),  ) +
  geom_jitter(width = .02, aes(col = factor(PoolType)), size = 2, alpha = .5, show.legend = F ) +
  geom_boxplot( aes( col = factor(PoolType)), 
                show.legend = F,
                fill = NA) +
  scale_color_manual(values = wes_palette("FantasticFox1")) +
  labs(title= "Figure 1: Fitness by Pool Type", 
       x= "Treatment Category",
       y= "Reproductive Biomass (g)") +
  theme_bw() +
  annotate("text", x=1, y=18, label= "a") + 
  annotate("text", x=2, y=41, label= "b")
NMMBoxplot

#Grid Plot Mixed Model Results: This is the same plot as in the Readme file for this assignment
gridPlotFinal <- grid.arrange(NMMBoxplot, MMBoxplot, MMQQplot, MMDiagPlot,  top = "Baele Transplant Experiment Graphs and Models", bottom = "Significant difference in response variable means by treatment found when not accounting for random effects (Figure 1). 
No significant difference in predicted response variable means by treatment when accounting 
for random effects of individual pool (Figure 2)." )



