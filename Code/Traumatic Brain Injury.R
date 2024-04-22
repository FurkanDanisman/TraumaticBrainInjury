                              ### Math4939 - Group Project - Fan ###

# Meaning of the variables: 

## Hippocampus-L side ##

# Plays a crucial role in linguistic skills. In other words, episodic verbal memory.

## Hippocampus-R side ##

# Spatial detection. Helps you understand where you are spatially or where everything is. Basically, your reality. 

## VBR ## 

# The ventricle to brain ratio. Brain shrinks. 

## Capsules ## 

# The internal capsule describes a region deep in the brain that functions as a communication pathway. 
# The internal capsule allows communication between areas of the cerebral cortex and areas of the brainstem.

## Capsule_anterior ## 

# Carries thalamic and brainstem fibers from prefrontal cortical regions that are associated with different aspects of emotion
# motivation, cognition processing, and decision-making.

# Differences between left and right Capsule Anterior # 

# The anterior limb of the internal capsule passes between the caudate nucleus (medially) and lentiform nucleus (laterally).
# The posterior limb of the internal capsule passes between the thalamus (medially) and the lentiform nucleus (laterally).

## Fornix Crux ## 

# The fornix is a white matter tract that connects the hippocampus to several subcortical brain regions and is pivotal for episodic memory functioning.
# Functionally, the fornix transmits essential neurotransmitters, as well as theta rhythms, to the hippocampus.

# Differences between left and right Fornix Crux # 
# The left fornix primarily carries verbal memory information, while the right carries visuospatial memory information 

## Gray Matter ##

# The place where the processing of sensation, perception, voluntary movement, learning, speech and cognition takes place.


## White Matter ##

# To provide communication between different grey matter areas and between grey matter and the rest of your body


# Required Libraries

library(devtools)
library(car)
library(effects)
library(snow)
library(rbenchmark)
library(spida2)
library(readxl)
library(dplyr)
library(p3d)
library(lattice)
library(latticeExtra)
library(ggplot2)
library(gridExtra)
library(nlme)
library(rgl)
library(Hmisc)

# Wide-to-Long form Data Transformation
TBI <- read_excel("C:/Users/zenci/Downloads/TBI.xlsx")
TBI2 <- as.data.frame(TBI)

a <- names(TBI2)
names(TBI2) <- gsub("_(.)$","__\\1",a)

New_TBI <- tolong(TBI2,timevar = "visit",sep = "__",idvar="SubID")



# Plot for each Y-variable: VBR, CC_TOT, HPC_L_TOT, HPC_R_TOT

p1 <- ggplot(New_TBI, aes(x = date, y = VBR, group = SubID,col=SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ group, scales = "free")+
  theme(legend.position = "none")

p2 <- ggplot(New_TBI, aes(x = date, y = CC_TOT, group = SubID,col=SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ group, scales = "free")+
  theme(legend.position = "none")

p3 <- ggplot(New_TBI, aes(x = date, y = HPC_L_TOT, group = SubID,col=SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ group, scales = "free")+
  theme(legend.position = "none")

p4 <- ggplot(New_TBI, aes(x = date, y = HPC_R_TOT, group = SubID,col=SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ group, scales = "free")+
  theme(legend.position = "none")

# grid.arrange(p1,p2,p3,p4)

# Patterns are similar between Patients and Control group for HPC_L_TOT and HPC_R_TOT.
# However, for VBR and CC_TOT patterns between Patiens and Control groups are visiually different. 
# For VBR and CC_TOT control group have a straight pattern, whereas patients have an increasing path for VBR and decreasing path for CC_TOT.

# Counting Available Observations 

# Format 1 

Available_obs <- New_TBI %>%
  group_by(SubID) %>%
  summarise(Available_obs = sum(!is.na(VBR)))

some(Available_obs)

# Format 2 

New_TBI$SEX <- ifelse(New_TBI$sex==1,"Male","Female")
a <- New_TBI[,c("SubID","VBR")]
a <- na.omit(a)
table(a$SubID)

New_TBI_filtered <- New_TBI[complete.cases(New_TBI$SEX),]


### Plots for several X-VARIABLES ### 

# Date with SEX 
HC_S_p <- ggplot(New_TBI_filtered, aes(x = date, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~SEX) +
  theme(legend.position = "none") +
  scale_y_continuous()

# Date with groups
library(ggplot2)
HC_G_p <- ggplot(New_TBI_filtered, aes(x = date, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~group) +
  ggtitle("Left-Hippocampus Total Volume")+
  theme(legend.position = "none") +
  scale_y_continuous()

# Date with both SEX and groups
HC_G_p <- ggplot(New_TBI_filtered, aes(x = date, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~group+SEX) +
  ggtitle("Left-Hippocampus Total Volume")+
  theme(legend.position = "none") +
  scale_y_continuous()

HC_G_p

# Fornix-Crux - Around the same for both sides
HC_F_L <- ggplot(New_TBI_filtered, aes(x = fornix_crux_L, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~group+SEX) +
  theme(legend.position = "none") +
  ggtitle("Fornix Crux - Left Side") +
  scale_y_continuous()

HC_F_L

HC_F_R <- ggplot(New_TBI_filtered, aes(x = fornix_crux_R, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~SEX) +
  theme(legend.position = "none") +
  scale_y_continuous()

# grid.arrange(HC_F_L,HC_F_R)

# Capsule Anterior - For Right-side of capsule anterior there seems to be more negative correlation compare to Left-side

HC_CA_L <- ggplot(New_TBI_filtered, aes(x = capsule_anterior_L, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~group+SEX) +
  theme(legend.position = "none") +
  scale_y_continuous()

HC_CA_R <- ggplot(New_TBI_filtered, aes(x = capsule_anterior_R, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~group+SEX) +
  theme(legend.position = "none") +
  ggtitle("Capsule Anterior - Right Side") +
  scale_y_continuous()

HC_CA_R

# grid.arrange(HC_C_L,HC_C_R)

# Capsule Posterior 
HC_CP_L <- ggplot(New_TBI_filtered, aes(x = capsule_posterior_L, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~SEX) +
  theme(legend.position = "none") +
  scale_y_continuous()

HC_CP_R <- ggplot(New_TBI_filtered, aes(x = capsule_posterior_R, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~SEX) +
  theme(legend.position = "none") +
  scale_y_continuous()

# grid.arrange(HC_C_L,HC_C_R)

# VBR - Natural - Negative Relationship
HC_VBR <- ggplot(New_TBI_filtered, aes(x = VBR, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~group+SEX) +
  theme(legend.position = "none") +
  scale_y_continuous()

# Age - Similar to date - just the scale is a bit different

HC_Age <- ggplot(New_TBI_filtered, aes(x = age, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~group+SEX) +
  theme(legend.position = "none") +
  scale_y_continuous()

HC_Age

# HC_R - Expected positive relationships

HC_R <- ggplot(New_TBI_filtered, aes(x = HPC_R_TOT, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~group+SEX) +
  theme(legend.position = "none") +
  scale_y_continuous()

# Gray Matter - For SEX=2 the positive relationship is relatively low compare to SEX=1

HC_GM <- ggplot(New_TBI_filtered, aes(x = GM, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~group+SEX) +
  theme(legend.position = "none") +
  scale_y_continuous()

HC_GM

# White Matter

HC_WM <- ggplot(New_TBI_filtered, aes(x = WM, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~SEX) +
  theme(legend.position = "none") +
  scale_y_continuous()

# grid.arrange(HC_GM,HC_WM)

# Corpus Collesum

HC_CC <- ggplot(New_TBI_filtered, aes(x = CC_TOT, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~group+SEX) +
  theme(legend.position = "none") +
  scale_y_continuous()


# Data Manipulations 
table(New_TBI_filtered$SEX)
table(New_TBI_filtered$sex)
New_TBI_filtered$SEX <- as.factor(New_TBI_filtered$SEX)
New_TBI_filtered$group <- as.factor(New_TBI_filtered$group)
clean_TBI_WO_Ls <- subset(New_TBI_filtered,!is.na(HPC_L_TOT) & !is.na(SEX) & !is.na(age_30))
clean_TBI_W_Ls <- subset(clean_TBI_WO_Ls, !is.na(fornix_crux_L) & !is.na(capsule_anterior_R) &
                         !is.na(GM))
only_clean_HPC <- subset(New_TBI_filtered,!is.na(HPC_L_TOT))

clean_TBI_W_Ls$visit<- as.factor(clean_TBI_W_Ls$visit)
clean_TBI_W_Ls$SubID <- as.factor(clean_TBI_W_Ls$SubID)
clean_TBI_W_Ls$id <- as.factor(clean_TBI_W_Ls$id)
a <- select(clean_TBI_W_Ls,c(HPC_L_TOT,HPC_R_TOT,age_30,fornix_crux_L,capsule_anterior_R,GM,visit,SEX,group))
b <- subset(clean_TBI_W_Ls,visit==2)
c <- subset(b,age>25 & age< 50)
d <- subset(b,age<=25)
middle_age_prop <- nrow(c)/nrow(b)
young_age_prop <- nrow(d)/nrow(b)
old_age_prop <- (nrow(b) - nrow(c) - nrow(d)) / nrow(b)
Prop_age <- data.frame("Younger -25" = young_age_prop, "Middle Age 25-50"=middle_age_prop,
                       "Older 50-"=old_age_prop)
rownames(Prop_age) <- "Proportion"
colnames(Prop_age) <- c("Younger-25","Middle Age 25-50", "Older 50-")
Prop_age <- Prop_age *100
Prop_age
range_of_age <- range(clean_TBI_W_Ls$age_30)

# qq-plot For each X Variables

xqplot(a)

# NA Values
nrow(New_TBI_filtered) - nrow(clean_TBI_W_Ls)

# Mixed Models 


# Model with the random part

fit.wLs <-lme( HPC_L_TOT ~ SEX + age_30 +group*age_30 + capsule_anterior_R +
                 GM  + fornix_crux_L, clean_TBI_W_Ls, 
               random = ~ 1 + age_30 | SubID, correlation = corCAR1(form = ~ age_30 | SubID) )

# Model without the random part

fit.wLs2 <- gls( HPC_L_TOT ~ SEX + age_30 + group*age_30 + capsule_anterior_R +GM  + fornix_crux_L, clean_TBI_W_Ls )

# Testing G-matrix 

anova(fit.wLs,fit.wLs2)

# G-matrix is significantly greater than 0 

# Model Updates to compare models - Section 1 

fit2 <- update( fit.wLs, . ~ . + SEX*age_30)
fit4 <- update( fit2, . ~ . + cvar(GM,SubID))
fit2l.1 <- fit4
possible_rmv <- c(346)
fit2l.d <- update( fit2l.1, data = subset(clean_TBI_W_Ls, !(id %in% possible_rmv)) )

w.fit2 <- wald(fit2)[[1]]
w.fit2$anova
w.fit4 <- wald(fit4)[[1]]
w.fit4$anova
w.fitd <- wald(fit2l.d)[[1]]
w.fitd$anova


# The-p-value for all of them is relatively low. Through Trials of the significance, these models are the final models. 

# Y variable: Left-Hippocampus-Total (Volume) 
# X variables: 
    # Fixed Part: Sex + Age (Centered at 30) + Sex*Age (Centered at 30) Capsule Anterior-R 
    #                 + Gray Matter + Fornix Crux - L + cvar(Gray Matter)
    # Random Part: Intercept + Age_30 conditioned on subject
    # Correlation: ~ Age_30 | Subject

# Kelvin

# Q1 - Does the volume of the hippocampus undergo significant changes after an injury, and do these changes vary
# over time post-injury?

# Answer1: Since we do not have a pre-injury data, we can not conclude if the changes are significant or normal. 
# Answer2: However, we can observe the pattern after the injury factoring other variables. For example, we can observe
# how different the patients compare the control group. It appears to be a change in the slope for two groups. 

# Plot vs time wrapped by group and sex

HC_G_p

# Wald-Test

wald(fit4,"group")
wald(fit4,"age")

# Also, there seems to be a significant difference of 249 Hippocampus (TOT?) between patients and control group given the model estimate.

# Q2 - At different time points post-injury, does the level of cognitive recovery (especially language comprehension ability)  improve over age or time?

# Answer: 

HC_Age

# Based on the age-graph, there could be slight downward pattern for the intercept as age increase for Males. 

# Starting model 

wald(fit.wLs,"age_30")

# Age has a negative relationship and slightly significant

# Model with interaction term

wald(fit2,"age_30")

# The relationship of age differs by sex with the interaction term. 
# Although, age_30 is not significant, due to principle of marginality we can not eliminate it. And also,
# together with interaction term, it is significant at d=2 level. 

# Model with contextual variables

wald(fit4,"age_30")

# Similar story with slightly less significance

# Model without outlier 

wald(fit2l.d,"age_30")
wald(fitglh,"age_30")
wald(fit4,"age_30")
wald(fit2l.d)

# Separately, they became less significant, but together at d=2 level they are highly significant. 
# We can observe the affect of outlier on the age factor. 

# Auto-correlation 

intervals(fit4)

# As phi is significantly positive (do not include 0), we can conclude that there is a positive autocorrelation. 
# In other words, if the hippocampus level is going down, it is likely that, it will hold that trajectory for the following days, vice a versa. 
# And we know the trend of time might differs among sex. 

# For females, hippocampus total increases by 1.83 for each increase in age, holding others constant. 

# For males below the age of 30, hippocampus total decreases by 10.12 for each increase in age, holding others constant. 

# The <- fore, combining with the positive autocorrelation, for females trend appears to be upward as positive trajectory follows positive trajectory.
# Whereas, for males trend is downward, as negative track follows negative track. 

# Q3 - During the process of changes in hippocampal volume post-injury, does sex have an impact on this process?

# Answer: 

# Pooled Data

wald(fit.lm, 'SEX') 

# Pooled data suggest sex has a effect of -11.04 on Hippocampus for Male compare to Female at age 30. 

# First mixed-model without upgrades 

wald(fit.wLs,"SEX")

# Suggests sex is insignificant. 

# # Second Mixed-Model - Interaction Term is added (Sex*Age)

wald(fit2,"SEX") 

# Suggests sex and interaction together is insignificant but the interaction term may be significant based on our confidence level.
# Due to principle of marginality we do not reduce the variables.

# With the interaction term, relationship of age has changed for each sex. For females, age has a slightly 
# negative relationship prior to age 30 and positive after 30. On the other hand, for males, both sign of the slope 
# is different, and effect of age when age increases by one, holding others constant is stronger. 


# Mixed-Model without the outlier

wald(fit2l.d,"SEX") 

# Similar story to above model with more significance.


####### Q4- Some sort of way, Q2 is answered by the age phenomenon  #######


# Q5 - Is the deep communication pathway region and episodic memory of the brain, capsule anterior and fornix crux, respectively,
# related to decrease in hippocampal volume and a decline in cognitive function? 

HC_CA_R
HC_F_L

# First mixed-model without upgrades 

wald(fit.wLs,"fornix_crux_L")
wald(fit.wLs,"capsule_anterior_R")


# Mixed-model with contextual variables

wald(fit4,"fornix_crux_L")

# Contextual variable increased the over-all significancy of fornix_crux to a p-value of 0.04 from 0.22
# We can observe that the as we increased the mean by one unit, hippocampus level increase by 5178, holding others constant.
# Another interesting observation is Simpson paradox. Fornix crux by itself has a negative relationship where, between effect is positive. 

wald(fit4,"capsule_anterior_R")

# Contextual variable increased the over-all significancy of fornix_crux to a p-value of 0.06 from 0.15
# Both the one unit increase of capsule anterior and the mean of capsule anterior decrease the hippocampus level holding others constant.


# Mixed-Model without the outlier  

wald(fit2l.d,"fornix_crux_L")
wald(fit2l.d,"capsule_anterior_R")

# Without the outlier, it appears that our model produce less significant estimates, once again. 
# Maybe, outliers were carrying an important value. 

# Q6 - Considering that the right side of the hippocampus is related to spatial perception abilities, is there a
# connection between spatial perception abilities and language  comprehension and production?

HC_R 

# As expected, there is a strong relationship, we suspect a confounding variable. But we can not measure it. 
# Hence, it is better to avoid adding as a variable at this point. 


# Q7 - What is connection between the Gray Matter, where the processing of sensation, perception, voluntary movement, learning, speech and cognition happens,
# and the hippocampus left-side, where the brain construct the languages?

# First Mixed-Model 

wald( fit.wLs, 'GM')

# It appears to play a significant role for the prediction of hippocampus level. It is highly significant with a low value of estimate compare to other units.

# Mixed-Model without outliers

wald(fit2l.d,"GM")

# Outlier model produced slightly better results.  

# Q8 - Predictions 

wald(fit2l.d)

# What would be the predicted Hippocampus level for a male at the age of 20 with the traumatic brain injury, 
# who has aaverage fornix crux, capsule anterior, and Gray Matter Level?

a <- select(clean_TBI_W_Ls,id,fornix_crux_L,capsule_anterior_R,GM)
b <- a %>% group_by(id)
b <- a %>% group_by(id) %>% slice(1)
L <- c(1,1,-10,1,mean(clean_TBI_W_Ls$capsule_anterior_R),mean(clean_TBI_W_Ls$GM),mean(clean_TBI_W_Ls$fornix_crux_L),
  c(mean(b$fornix_crux_L),mean(b$capsule_anterior_R)),-10)
L <- as.matrix(L)
L <- t(L)
L
wald(fit2l.d,L)
wald(fit4,L)

# How about for a female with same features?

L2 <- c(1,0,-10,1,mean(clean_TBI_W_Ls$capsule_anterior_R),mean(clean_TBI_W_Ls$GM),mean(clean_TBI_W_Ls$fornix_crux_L),
       c(mean(b$fornix_crux_L),mean(b$capsule_anterior_R)),0)
L2 <- as.matrix(L2)
L2 <- t(L2)
L3 <- rbind(L,L2)
rownames(L3) <- c("Male","Female")
Young_result <- wald(fit2l.d,L3)
Young_result2 <- wald(fit4,L3)

# How about for middle-aged (50) people

L <- c(1,1,20,1,mean(clean_TBI_W_Ls$capsule_anterior_R),mean(clean_TBI_W_Ls$GM),mean(clean_TBI_W_Ls$fornix_crux_L),
       c(mean(b$fornix_crux_L),mean(b$capsule_anterior_R)),20)
L <- as.matrix(L)
L <- t(L)
wald(fit2l.d,L)
wald(fit4,L)

# Female 

L2 <- c(1,0,20,1,mean(clean_TBI_W_Ls$capsule_anterior_R),mean(clean_TBI_W_Ls$GM),mean(clean_TBI_W_Ls$fornix_crux_L),
        c(mean(b$fornix_crux_L),mean(b$capsule_anterior_R)),0)
L2 <- as.matrix(L2)
L2 <- t(L2)
L3 <- rbind(L,L2)
rownames(L3) <- c("Male","Female")
Middle_aged_result <- wald(fit2l.d,L3)
Middle_aged_result2 <- wald(fit4,L3)

# How about for someone around 70's?

L <- c(1,1,44,1,mean(clean_TBI_W_Ls$capsule_anterior_R),mean(clean_TBI_W_Ls$GM),mean(clean_TBI_W_Ls$fornix_crux_L),
       c(mean(b$fornix_crux_L),mean(b$capsule_anterior_R)),44)
L <- as.matrix(L)
L <- t(L)
wald(fit4,L)


# How about for a female with same features?

L2 <- c(1,0,40,1,mean(clean_TBI_W_Ls$capsule_anterior_R),mean(clean_TBI_W_Ls$GM),mean(clean_TBI_W_Ls$fornix_crux_L),
        c(mean(b$fornix_crux_L),mean(b$capsule_anterior_R)),0)
L2 <- as.matrix(L2)
L2 <- t(L2)
L3 <- rbind(L,L2)
rownames(L3) <- c("Male","Female")
Old_result <-wald(fit2l.d,L3)
Old_result2 <-wald(fit4,L3)

# Age Comparison 

a <- list()
a[[1]] <- Young_result
a[[2]] <- Middle_aged_result
a[[3]] <- Old_result
names(a) <- c("Young-20's","Middle-50's","Elder-70's")
a

b <- list()
b[[1]] <- Young_result2
b[[2]] <- Middle_aged_result2
b[[3]] <- Old_result2
names(b) <- c("Young-20's","Middle-50's","Elder-70's")
b

# Visit and improvement # 
range(clean_TBI_W_Ls$date)

table(only_clean_HPC$visit)
v4 <- subset(only_clean_HPC,visit==4)
v3 <- subset(only_clean_HPC,visit==3)
v2 <- subset(only_clean_HPC,visit==2)

v41 <- select(v4,id,HPC_L_TOT)
v31 <- select(v3,id,HPC_L_TOT)
v21 <- select(v2,id,HPC_L_TOT)
id_list <- 0

diffHC <- 0
for (i in 1:nrow(v3)) {
  if(v31[i,"id"]%in%v21$id){
    id_list[i] <- as.character(v31[i,"id"]) 
    diffHC[i] <- v21[which(v21$id==v31[i,"id"]),"HPC_L_TOT"] - v31[i,"HPC_L_TOT"]
  }else{id_list[i] <- 0}
}

id_list2 <- 0
for (i in 1:nrow(v4)) {
  if(!(v41[i,"id"]%in%v31$id)){
    id_list2[i] <- as.character(v31[i,"id"]) 
  }else{id_list2[i] <- 0}
}

id_list[which(diffHC<0)] %in% id_list2[id_list2!=0]
id_list[which(diffHC<0)] 

# All of the subjects in patients group, 6 out of 6, who had a positive improvement stopped coming after the visit-3. Whereas only small percentage of the non-improved patients stopped coming. 

# Level 1 - Diagnostics # 

fit2l.1 <- fit4


plot( fit2l.1 )

qqnorm(fit2l.1)

plot( fit2l.1, id = .02)

plot( fit2l.1, resid(.) ~ fitted(.) | group, id = .02,sub = "scale-location plot",
      ylab = expression(" resid "),
      panel = function(x, y, ...){
        panel.xyplot( x, y, ...)
        panel.loess(x ,y, ...)
      })

plot( fit2l.1, resid(.) ~ fitted(.) | SEX, id = .02,sub = "scale-location plot",
      ylab = expression(" resid "),
      panel = function(x, y, ...){
        panel.xyplot( x, y, ...)
        panel.loess(x ,y, ...)
      })

plot( fit2l.1, resid(.) ~ fitted(.) | group * SEX, id = .02,sub = "scale-location plot",
      ylab = expression(" resid "),
      panel = function(x, y, ...){
        panel.xyplot( x, y, ...)
        panel.loess(x ,y, ...)
      })


#
### Diagnostic for heteroskedasticity ----
#

plot( fit2l.1, sqrt( abs( resid(.))) ~ fitted(.), id = .02) # exploratory version

# fancy version
plot( fit2l.1, sqrt( abs( resid(.))) ~ fitted(.), id = .02)

plot( fit2l.1, sqrt( abs( resid(., type = 'p'))) ~ fitted(.) | SEX, id = .03,
      panel = function(x, y, ...) {
        panel.xyplot( x, y, ...)
        panel.loess( x, y,...)
      })

# id didn't work so we do it manually

#trellis.focus()            
#panel.identify(labels = clean_TBI_W_Ls$SubID)
#trellis.unfocus()

# Identified Points: SubID = { 368 - 380 - 325 - 462 - 346 - 453 - 358 } 
sus <- c(368,380,325,462,346,453,358)
subset_data <- subset(clean_TBI_W_Ls,SubID %in% sus)

# Plotting the highlighted "suspicious patients".

HC_Sample2.1 <- ggplot(New_TBI_filtered, aes(x = date, y = HPC_L_TOT, group = SubID, col = SubID)) +
  geom_point() +
  geom_line() +
  facet_wrap(~SEX) +
  theme(legend.position = "none") +
  geom_text(data = subset(subset_data, SubID %in% sus),
            aes(label = SubID), vjust = -0.5) + 
  scale_y_continuous()

HC_sample2 <- ggplot(data = subset_data,aes(x=date,y=HPC_L_TOT,col=SubID,group=SubID))+
  geom_point()+
  geom_line()+
  theme(legend.position="none")+
  geom_text(data = subset(subset_data, SubID %in% sus),
            aes(label = SubID), vjust = -0.5)+
  scale_y_continuous()

grid.arrange(HC_Sample2.1,HC_sample2)

possible_rmv <- c(346)

# QQnorms 

qqnorm( fit2l.1, id = .05 )
qqnorm( fit2l.1, ~ resid(.) | SEX, id = .05 )

#
#  Variogram: diagnostics for autocorrelation
#  New level 1 diagnostic for longitudinal data
#

vv <- Variogram( fit2l.d , form = ~ age_30 | SubID,resType = "normalized")
vv         # Variance of differences between pairs as a function of distance in time
str(vv)
plot(vv)
xyplot(variog ~ dist, vv)
wald(fit2l.d,"SE")

# Differences between pairs of residuals that are close have smaller variance than between those that are far apart.


ranef(fit2l.1)   # Level 2 residuals BLUPS for u_is
class(ranef(fit2l.1))
methods(class = 'ranef.lme')
# plot( ranef( fit2l.1, aug = T ), form =  ~ SEX) # ERROR
# note ranef( fit2l, aug = T) has a 'bug' and doesn't work if there's a matrix in data frame

# To plot BLUPS

RE <- cbind( ranef( fit2l.1),up(clean_TBI_W_Ls,~SubID) )
head( RE )

# Do NOT use CIs for SDs to test whether they are 0. Use anova + simulate. Cf Lab 1.

getVarCov(fit.wLs)
getVarCov(fit2l.d)

# Variation of the true parameter of intercept is relatively high as expected.
# or 

VarCorr(fit.wLs)
VarCorr(fit2l.d)

wald(fit.wLs)
fit.wLs
fit2l.d
intervals(fit2l.d)
intervals(fit.wLs,which = "fixed")

# Random Part 
data = subset(clean_TBI_W_Ls, !(id %in% possible_rmv))
RE <- cbind( ranef( fit2l.d),up(data,~SubID) )
colnames(RE)[2] <- "Slope"
RE = subset(RE, !(id %in% 453))
ggplot(RE, aes(x = Slope, y = `(Intercept)`,group = SEX,col=SEX)) +
  facet_wrap(~SEX)+
  geom_point() +  # Plot the data points
  geom_smooth(method = "lm", se = FALSE)

wald(fit2l.d,"SEX")

# As age increases there seems to be a pattern for downward intercept for females, whereas, for males it is relatively straight.

qqnorm(RE[[1]])
qqnorm(RE[[2]])

#
# Acting on diagnostics
# Should we drop some observations
#

possible_rmv <- c(346)

fit2l.d <- update( fit2l.1, data = subset(clean_TBI_W_Ls, !(id %in% possible_rmv)))

# or

fit2l.d <- update( fit2l.1, subset = !(id %in% possible_rmv) )

summary (fit2l.d)


# do we need the autocorrelation?

intervals( fit2l.1 )   # look at CI for Phi (correlation)

# plot( simulate( fit4, nsim = 1000, m2 = fit2))  # out of luck with fancier model


#
# Heteroscedasticiy revisited ----
#


plot( fit2l.d, sqrt( abs( resid(., type = 'p'))) ~ fitted(.), id = .03)

plot( fit2l.d, sqrt( abs( resid(., type = 'p'))) ~ fitted(.), id = .03,
      panel = function(x, y, ...) {
        panel.xyplot( x, y, ...)
        panel.loess( x, y,...)
      })

plot( fit2l.d, sqrt( abs( resid(., type = 'p'))) ~ fitted(.) | SEX, id = .03,
      panel = function(x, y, ...) {
        panel.xyplot( x, y, ...)
        panel.loess( x, y,...)
      })

plot( fit2l.d, sqrt( abs( resid(., type = 'p'))) ~ fitted(.) | group, id = .03,
      panel = function(x, y, ...) {
        panel.xyplot( x, y, ...)
        panel.loess( x, y,...)
      })
