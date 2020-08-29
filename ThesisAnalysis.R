#' Jackson Anderson
#' Thesis Analysis
#' Department of Ecology and Evolutionary Biology, CU Boulder
#' Fall 2020

#### Package Calls ####

library(ggplot2)
library(tidyverse)
library(lme4)
library(nlme)
library(lattice)
library(wesanderson)
library(grid)
library(gridExtra)
library(knitr)


#### Questions, Hypothesis, Prediction ####

#' __Questions__
#' Do plants living in created or natural pools experience different levels of environmental stress? 
#' If there does appear to be a treatment effect, when using pool type as a predictor, how does the variation within each treatment differ
#' What are the potential effects on the outcome of the study due to experimental design?
#' 
#' __Hypothesis__
#' Pool type (natural vs created) will have an effect on plant biomass and reproductive biomass
#' 
#' __Predictions__
#' mean plant biomass will be higher in artificially created pools than naturally created pools because of the habitat homogeneity present in the artificially created pools. This is based on the assumption that the habitat in the artificially created pools is suited for l. fremontii. 
#' 
#' 
#### Data ####

#' __Reading in Data__

bealeDat <- Beale2015_transplantdata_forJackson

view(bealeDat)

#### Primary Visualizations ####

#' __Weight by PoolType__
#' 

weightDat <- bealeDat %>%
  filter(!is.na(VegWt))

ggplot(weightDat, aes(PoolType, VegWt)) +
  geom_boxplot()
#' The above visualization is interesting: It would seem that the weight of plants within the created pools are less than those in the natural pool. If weight is to be used as a metric of stress, those that weigh less are considered to be more stressed (cite)
#' 
#' __Reproductive Biomass by PoolType__
#' 

reproDat <- bealeDat %>%
  filter(!is.na(ReproWt))

ggplot(reproDat, aes(PoolType, ReproWt)) +
  geom_boxplot()
#' The above visualization further confirms the notion that plants in the created pools as their reproductive biomass is less than that in the natural pool, if we are to take reproductive biomass as an indicator of reproductive effort. 
#' 
#' __Inflorescence Biomass by PoolType__
#' 
#' Vis. One: Inflorescense Weight alone
infloDat <- bealeDat %>%
  filter(!is.na(FullInflorWt))

ggplot(infloDat, aes(PoolType, FullInflorWt)) +
  geom_boxplot()

#' Vis. Two: #percentage of total energy investment given to reproductive growth 
#' 

infloDat <- infloDat %>%
  filter(!is.na(VegWt))

infloDat$reproPropn<- (infloDat$ReproWt + infloDat$VegWt) / infloDat$ReproWt #percentage of total energy investment given to reproductive growth 

ggplot(infloDat, aes(PoolType, reproPropn)) +
  geom_boxplot()

#' The above graph suggests that, of the total energy invested into growth, more is invested into reproduction in the natural pools

#### Response Variable Analysis and Behavior ####

#### Vegitative Biomass Modeling ####

#' __Stress Test: Veg. Biomass__

#' modeling stress

veglme <- lme(VegWt~PoolType , random = ~1 | PoolID/Trans, data = weightDat)
summary(veglme)

plot.lme(veglme)

qqnorm(veglme, ~resid(., type = "p"), abline = c(0,1))

#response variable distribution is heteroskedastic; will try an unequal variances model to see if it solves the issue.

veglme_uv <- lme(VegWt~PoolType , random = ~1 | PoolID/Trans, data = weightDat, weights = varIdent(form = ~1|PoolType))

summary(veglme_uv)

plot.lme(veglme_uv) 

qqnorm(veglme_uv, ~resid(., type = "p"), abline = c(0,1))

#somewhat better on the residual distribution front, but the qqplot does not look ideal. Will log transform the response variable and see if that helps

weightDat$logVegWt <- log1p(weightDat$VegWt)

veglme_log <- lme(logVegWt~PoolType , random = ~1 | PoolID/Trans, data = weightDat)
summary(veglme)

plot.lme(veglme_log)

qqnorm(veglme_log, ~resid(., type = "p"), abline = c(0,1))

#this is much better! Will try and unequal variances model to see if we can improve it further

veglme_log_uv <- lme(logVegWt~PoolType , random = ~1 | PoolID/Trans, data = weightDat, weights = varIdent(form = ~1|PoolType))

summary(veglme_log_uv)

plot.lme(veglme_log_uv) 

qqnorm(veglme_log_uv, ~resid(., type = "p"), abline = c(0,1))

#' Performing anova on equal and unequal variance models of log transformed data

anova(veglme_log, veglme_log_uv)#suggests that uv model is worth using

#' __Results of Model: Final Answer__
#' The above model suggests that there is no difference in mean veg biomass between created and natural pools. This suggests that, when using vegetative biomass as a predictor of stress, the two habitats do not affect the plants deferentially.
summary_lme_veg_uv_model <- summary(veglme_log_uv) 
#' 
#' __Plotting the Results__

#fitting same model from final answer with no intercept for plotting:

veglme_log_uv_noint <- lme(logVegWt~ 0 + PoolType , random = ~1 | PoolID/Trans, data = weightDat, weights = varIdent(form = ~1|PoolType))

veglme_noint_summary <- summary(veglme_log_uv_noint)

#creating data frame for plotting:
vegOutput_df <- data.frame(poolmean = veglme_noint_summary$tTable[,1],
                   poolse = veglme_noint_summary$tTable[,2],
                   trtCat = levels(as.factor(weightDat$PoolType)))
#Final Plot

vegPlot <- ggplot(vegOutput_df, aes(trtCat, poolmean)) +
  geom_point() +
  geom_errorbar(aes(ymin=poolmean-poolse, ymax=poolmean+poolse, col = factor(trtCat)), width = 0.4, show.legend = F) +
  geom_jitter(data = weightDat, aes(PoolType, logVegWt, col = factor(PoolType)), width = 0.07, alpha = 0.4, show.legend = F) +
  scale_color_manual(values = wes_palette("GrandBudapest1")) +
  xlab("Pool Type") +
  ylab("Vegitative Biomass (g) - log transformed") +
  ggtitle("Stress Response to Habitat Type") +
  theme_light() +
  annotate("text", x= 1, y = 3.2, label = "a") +
  annotate("text", x= 2, y = 4.2, label = "a")
vegPlot  

#### Reproductive Output Modeling ####

#' __Reproductive Biomass__

#' modeling stress

reprolme <- lme(ReproWt~PoolType , random = ~1 | PoolID/Trans, data = reproDat)
summary(reprolme)

plot.lme(reprolme)

qqnorm(reprolme, ~resid(., type = "p"), abline = c(0,1))

#response variable distribution is heteroskedastic; will try an unequal variances model to see if it solves the issue.

reprolme_uv <- lme(ReproWt~PoolType , random = ~1 | PoolID/Trans, data = reproDat, weights = varIdent(form = ~1|PoolType))

summary(reprolme_uv)

plot.lme(reprolme_uv) 

qqnorm(reprolme_uv, ~resid(., type = "p"), abline = c(0,1))

#somewhat better on the residual distribution front, but the qqplot does not look ideal. Will log transform the response variable and see if that helps

reproDat$logReproWt <- log1p(reproDat$ReproWt)

reprolme_log <- lme(logReproWt~PoolType , random = ~1 | PoolID/Trans, data = reproDat)
summary(reprolme_log)

plot.lme(reprolme_log)

qqnorm(reprolme_log, ~resid(., type = "p"), abline = c(0,1))

#this is much better! Will try and unequal variances model to see if we can improve it further

reprolme_log_uv <- lme(logReproWt~PoolType , random = ~1 | PoolID/Trans, data = reproDat, weights = varIdent(form = ~1|PoolType))

summary(reprolme_log_uv)

plot.lme(reprolme_log_uv) 

qqnorm(reprolme_log_uv, ~resid(., type = "p"), abline = c(0,1))

#' Performing anova on equal and unequal variance models of log transformed data

anova(reprolme_log, reprolme_log_uv)#suggests that uv model is worth using

#' __Results of Model: Final Answer__
#' The above model suggests that there is no difference in mean veg biomass between created and natural pools. This suggests that, when using vegetative biomass as a predictor of stress, the two habitats do not affect the plants deferentially.
summary_lme_repro_uv_model <- summary(reprolme_log_uv) 
#' 
#' __Plotting the Results__

#fitting same model from final answer with no intercept for plotting:

reprolme_log_uv_noint <- lme(logReproWt~ 0 + PoolType , random = ~1 | PoolID/Trans, data = reproDat, weights = varIdent(form = ~1|PoolType))

reprolme_noint_summary <- summary(reprolme_log_uv_noint)

#creating data frame for plotting:
reproOutput_df <- data.frame(poolmean = reprolme_noint_summary$tTable[,1],
                           poolse = reprolme_noint_summary$tTable[,2],
                           trtCat = levels(as.factor(reproDat$PoolType)))
#Final Plot

reproPlot <- ggplot(reproOutput_df, aes(trtCat, poolmean)) +
  geom_point() +
  geom_errorbar(aes(ymin=poolmean-poolse, ymax=poolmean+poolse, col = factor(trtCat)), width = 0.4, show.legend = F) +
  geom_jitter(data = reproDat, aes(PoolType, logReproWt, col = factor(PoolType)), width = 0.07, alpha = 0.4, show.legend = F) +
  scale_color_manual(values = wes_palette("GrandBudapest1")) +
  xlab("Pool Type") +
  ylab("Reproductive Biomass (g) - log transformed") +
  ggtitle("Reproductive Output Response to Habitat Type") +
  theme_light() +
  annotate("text", x= 1, y = 3.2, label = "a") +
  annotate("text", x= 2, y = 4.2, label = "a")
reproPlot 

#### Percentage Investment in Reproduction Modeling ####

#' __Percentage Investment Modeling__

#' modeling the ratio of energy investment into reproduction proportional to total biomass

investlme <- lme(reproPropn~PoolType , random = ~1 | PoolID/Trans, data = infloDat)
summary(investlme) #implies that there was significantly more energy invested into the growth of repoductive structures relative to the total biomass of the plant in the natural pool type

plot.lme(investlme)

qqnorm(investlme, ~resid(., type = "p"), abline = c(0,1))

#response variable distribution is heteroskedastic; will try an unequal variances model to see if it solves the issue.

investlme_uv <- lme(reproPropn~PoolType , random = ~1 | PoolID/Trans, data = infloDat, weights = varIdent(form = ~1|PoolType))

summary(investlme_uv)

plot.lme(investlme_uv) 

qqnorm(investlme_uv, ~resid(., type = "p"), abline = c(0,1))

#somewhat better on the residual distribution front, but the qqplot does not look ideal. Will log transform the response variable and see if that helps

infloDat$logReproPropn <- log1p(infloDat$reproPropn)

investlme_log <- lme(logReproPropn~PoolType , random = ~1 | PoolID/Trans, data = infloDat)
summary(investlme_log)

plot.lme(investlme_log)

qqnorm(investlme_log, ~resid(., type = "p"), abline = c(0,1))

#this is somewhat better, though the qq plot is still bothersome. Will try and unequal variances model to see if we can improve it further

investlme_log_uv <- lme(logReproPropn~PoolType , random = ~1 | PoolID/Trans, data = infloDat, weights = varIdent(form = ~1|PoolType))

summary(investlme_log_uv)

plot.lme(investlme_log_uv) 

qqnorm(investlme_log_uv, ~resid(., type = "p"), abline = c(0,1))

#' Performing anova on equal and unequal variance models of log transformed data

anova(investlme_log, investlme_log_uv) #suggests that equal variances model is worth using

#' __Results of Model: Final Answer__
#' The above model suggests that there is no difference in mean veg biomass between created and natural pools. This suggests that, when using vegetative biomass as a predictor of stress, the two habitats do not affect the plants deferentially.
summary_lme_repro_model <- summary(reprolme_log) 
#' 
#' __Plotting the Results__

#fitting same model from final answer with no intercept for plotting:

investlme_log_noint <- linvestlme_log <- lme(logReproPropn~ 0 + PoolType , random = ~1 | PoolID/Trans, data = infloDat)

investlme_noint_summary <- summary(investlme_log_noint)

#creating data frame for plotting:
reproOutput_df <- data.frame(poolmean = investlme_noint_summary$tTable[,1],
                             poolse = investlme_noint_summary$tTable[,2],
                             trtCat = levels(as.factor(infloDat$PoolType)))
#Final Plot

investPlot <- ggplot() +
  geom_jitter(data = infloDat, aes(PoolType, logReproPropn, col = factor(PoolType)), width = 0.07, alpha = 0.4, show.legend = F) +
  geom_errorbar(data = reproOutput_df, aes(x = trtCat, ymin=poolmean-poolse, ymax=poolmean+poolse, col = factor(trtCat)), width = 0.4, show.legend = F) +
  geom_point(data = reproOutput_df, aes(x = trtCat, y = poolmean)) +
  scale_color_manual(values = wes_palette("GrandBudapest1")) +
  xlab("Pool Type") +
  ylab("% Total Biomass Invested in Reproductive Body (/g) - log transformed") +
  ggtitle("Energy Investment Response to Habitat Type") +
  theme_light() +
  annotate("text", x= 1, y = 1.6, label = "a") +
  annotate("text", x= 2, y = 2.43, label = "b")
investPlot 

#### Final Analyis Plot ####

#' __Final Analysis Multiplot__

finalPlot <- grid.arrange(vegPlot, reproPlot, investPlot, nrow = 1, top = "Biomass Response to Habitat Type")

#### Spinning to html ####
spin("/Users/jacksonanderson/WorkRepo/ThesisAnalysis.R")
