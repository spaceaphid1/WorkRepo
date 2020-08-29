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

ggplot(vegOutput_df, aes(trtCat, poolmean)) +
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
  
