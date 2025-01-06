rm(list=ls())

##### Load relevant packages #####
library(scales)
library(rio)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(foreign)
library(devtools)
library(tidyr)
library(rlang)
library(digest)
library(DescTools)
library(splines)
library(tidyverse)
library(nlme)
library(splines)
library(grid)
library(ggthemes)
library(xlsx)
library(png)
library(gridExtra)
library(kader)
library(car)
library(lme4)
library(lmerTest)
library(readxl)
library(stargazer)
library(broom)
library(dplyr)
library(emmeans)
library(magrittr)
library(MuMIn)
library(knitr)
library(ggeffects)
library(naniar)
library(gdata)
library(pbkrtest)
library(patchwork)
library(ggsci)
library(corrplot)
library(rstatix)
library(plotrix)

##### Import dataset #####
home <- "G:/diva/Promovendi/Max research group/2. Researchers/Madelief Wijdeveld/PhD/2019_211 AISCHA/Data analyse/"
results <- "G:/diva/Promovendi/Max research group/2. Researchers/Madelief Wijdeveld/PhD/2019_211 AISCHA/Data Management/"
setwd(results)
data <- rio :: import("AISCHA_masterfile.xlsx")
d_long <- rio :: import("AISCHA_masterfile_long.xlsx")
setwd(home)
d_longGlucose <- rio :: import("d_longGlucose.xlsx")

d_longGLP <- rio :: import("d_longGLP1.xlsx")

d_longVAS <- rio :: import("d_longVAS.xlsx")

d_longAcetate <- rio :: import("d_longAcetate.xlsx")

d_longAI <- rio :: import("d_longAI.xlsx")
d_longAI <- rio :: import("d_longAI_NoOutlier.xlsx")

d_longPH <- rio :: import("d_longPH.xlsx")
d_longLipids <- rio :: import("d_longLipids.xlsx")
d_longUrine <- rio  :: import("d_longUrine.xlsx")

##### Create Table one #####
setwd(results)
setwd('Castor/')
Castor <- rio :: import('AISCHA_excel_export_20230424080522.xlsx')

Castor$Age <- as.numeric(Castor$Age)
Castor$Height <- as.numeric(Castor$Height)
Castor$Weight <- as.numeric(Castor$Weight)
Castor$BMI <- as.numeric(Castor$BMI)
Castor$Waist <- as.numeric(Castor$Waist)
Castor$Hip <- as.numeric(Castor$Hip)
Castor$SBP <- as.numeric(Castor$SBP)
Castor$DBP <- as.numeric(Castor$DBP)
Castor$BPM <- as.numeric(Castor$BPM)
Castor$BIA_FatPerc <- as.numeric(Castor$BIA_FatPerc)
Castor$Screening_glucose <- as.numeric(Castor$Screening_glucose)
Castor$Screening_insulin <- as.numeric(Castor$Screening_insulin)
Castor$HOMA_IR <- as.numeric(Castor$HOMA_IR)
Castor$Cholesterol <- as.numeric(Castor$Cholesterol)
Castor$HDL <- as.numeric(Castor$HDL)
Castor$LDL <- as.numeric(Castor$LDL)
Castor$Triglyceride <- as.numeric(Castor$Triglyceride)

CreateTableOne(vars = c("Age", "Height", "Weight", "BMI", "Waist", "Hip", "SBP", "DBP", "BPM", "BIA_FatPerc", "screening_glucose", "screening_insulin", "HOMA_IR", "Cholesterol", "HDL", "LDL", "Triglyceride"), strata = "BMI_class", data = Castor, includeNA = FALSE)

Castor$Screening_glucose <- as.numeric(Castor$Screening_glucose)
Castor$Screening_insulin <- as.numeric(Castor$Screening_insulin)

##### Remove empty columns #####
data <- data[,colSums(is.na(data))<nrow(data)]

##### Remove outliers #####
for(i in 1:c(107:118)){
  positie <- d_long[, i]
  outlier.LThr.STR <- mean(positie, na.rm=TRUE) - 3*sd(positie, na.rm=TRUE) # lower threshold
  outlier.UThr.STR <- mean(positie, na.rm=TRUE) + 3*sd(positie, na.rm=TRUE) # upper threshold
  
  STR_removed <- sum(positie>outlier.UThr.STR, na.rm=TRUE) + sum(positie<outlier.LThr.STR, na.rm=TRUE)
  d_long[i][d_long[i]>outlier.UThr.STR] <- NA
  d_long[i][d_long[i]<outlier.LThr.STR] <- NA
}

d_long$FvsNF_putamen

##### Create separate dataframe for lean and MetSyn group #####
table(data$BMI_class, data$Sexe)

##### Set correct layout for plots #####
theme_Publication <- function(base_size=12, base_family="sans") {
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold",
                                     size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(colour = NA),
           axis.title = element_text(face='bold',size = rel(1)),
           axis.title.y = element_text(angle=90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(), 
           axis.line = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_line(colour="#f0f0f0"),
           panel.grid.minor = element_blank(),
           legend.key = element_rect(colour = NA),
           legend.position = "right",
           legend.key.size= unit(0.2, "cm"),
           legend.spacing  = unit(0, "cm"),
           legend.title = element_text(face='bold'),
           plot.margin=unit(c(10,5,5,5),"mm"),
           strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
           strip.text = element_text(face="bold")
   ))
}

##### Plot missing data #####
dat2 <- select(data, c('Subject',starts_with('Glucose')))
missing <- apply(dat2,1, function(x) sum(is.na(x)))
missing
sum(missing)
plot(missing)
hist(missing)
nrow(dat2)

##### Plot missing data #####
dat2 <- select(data, c('Subject',starts_with('VAS')))
missing <- apply(dat2,1, function(x) sum(is.na(x)))
missing
sum(missing)
plot(missing)
hist(missing)
nrow(dat2)

##### Convert data to long format #####
d_longGlucose <- data %>%
  pivot_longer(
    cols = starts_with("Glucose"),
    names_to = "time",
    names_prefix = "Glucose_T_",
    values_to = "Glucose",
    values_drop_na = F
  )

rio :: export(d_longGlucose, "d_longGlucose.xlsx")

# For lean group #
d_longGlucose_lean <- lean %>%
  pivot_longer(
    cols = starts_with("Glucose"),
    names_to = "time",
    names_prefix = "Glucose_T_",
    values_to = "Glucose",
    values_drop_na = F
  )

# For metsyn group #
d_longGlucose_metsyn <- MetSyn %>%
  pivot_longer(
    cols = starts_with("Glucose"),
    names_to = "time",
    names_prefix = "Glucose_T_",
    values_to = "Glucose",
    values_drop_na = F
  )

##### Convert data to long format for GLP1 #####
d_longGLP <- data %>%
  pivot_longer(
    cols = starts_with("GLP1"),
    names_to = "time",
    names_prefix = "GLP1_T_",
    values_to = "GLP1",
    values_drop_na = F
  )

rio :: export(d_longGLP, "d_longGLP1.xlsx")

##### Convert VAS data to long format #####
d_longVAS <- data %>%
  pivot_longer(
    cols = starts_with("VAS"),
    names_to = "time",
    names_prefix = "VAS_hunger_",
    values_to = "VAS_hunger",
    values_drop_na = F
  )

rio :: export(d_longVAS, "d_longVAS.xlsx")

##### Convert data to long format Insulin #####
d_longInsulin <- data %>%
  pivot_longer(
    cols = starts_with("Insulin"),
    names_to = "time",
    names_prefix = "Insulin_T_",
    values_to = "Insulin",
    values_drop_na = F
  )

getwd()
rio :: export(d_longInsulin, "d_longInsulin.xlsx")

# For lean group #
d_longInsulin_lean <- data_lean %>%
  pivot_longer(
    cols = starts_with("Insulin"),
    names_to = "time",
    names_prefix = "Insulin_T_",
    values_to = "Insulin",
    values_drop_na = F
  )

rio :: export(d_longInsulin_lean, "d_longInsulin_lean.xlsx")

# For metsyn group #
d_longInsulin_metsyn <- data_metsyn %>%
  pivot_longer(
    cols = starts_with("Insulin"),
    names_to = "time",
    names_prefix = "Insulin_T_",
    values_to = "Insulin",
    values_drop_na = F
  )

rio :: export(d_longInsulin_metsyn, "d_longInsulin_metsyn.xlsx")

##### Convert data to long format Acetate #####
d_longAcetate <- data %>%
  pivot_longer(
    cols = starts_with("Acetate"),
    names_to = "time",
    names_prefix = "Acetate_T_",
    values_to = "Acetate",
    values_drop_na = F
  )

getwd()
rio :: export(d_longInsulin, "d_longAcetate.xlsx")


# Data adjusted and saved > relevant data added to d_longGlucose file #
# Use this for further insulin analyses #

##### Create -0.25h as timepoint for Glucose baseline sample #####
d_longGlucose$time[d_longGlucose$time =='min15'] <- -15
d_longGlucose$time <- as.numeric(d_longGlucose$time)
d_longGlucose$time

d_longLipids$time[d_longLipids$time =='min15'] <- -90
d_longLipids$time <- as.numeric(d_longLipids$time)
d_longLipids$time
d_longLipids$Group <- as.factor(d_longLipids$Group)

d_longAcetate$time[d_longAcetate$time =='baseline'] <- -90
d_longAcetate$time[d_longAcetate$time =='min30'] <- -30
d_longAcetate$time <- as.numeric(d_longAcetate$time)
d_longAcetate$time

d_longAI$time <- as.numeric(d_longAI$time)
d_longPH$time <- as.numeric(d_longPH$time)

##### Convert group to factor #####
d_longGlucose$Group <- factor(d_longGlucose$Group)
d_longGlucose$Condition <- factor(d_longGlucose$Condition)
d_longGlucose$time <- as.numeric(d_longGlucose$time)

d_longAcetate$Group <- factor(d_longAcetate$Group)
d_longAI$Group <- factor(d_longAI$Group)

d_longPH$Group <- factor(d_longPH$Group)
d_longUrine$Group <- factor(d_longUrine$Group)

##### Calculate AUCs from Baseline to 120 minutes and save as csv file #####
# Condition A #
d_longGlucose$Subject <- factor(d_longGlucose$Subject)
cat('There are', length(unique(d_longGlucose$Subject)), 'subjects in the initial dataset.\n')

res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter (Glucose_a != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=Glucose_a, 
                    method = 'spline'),
            mean=mean(Glucose_a))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_B_120_min_AISCHA_Condition_A.xlsx")

##### Repeat for Insulin #####
# Condition A #
res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter (Insulin_a != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=Insulin_a, 
                    method = 'spline'),
            mean=mean(Insulin_a))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_B_120_min_AISCHA_Insulin_Condition_A.xlsx")

##### Repeat for Insulin 0-120 instead of B-120 #####
# Condition A #
res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter (Insulin_a != 'NA') %>% # solution is to subset to only keep the ones without NA
  filter (time >= 0) %>% 
  summarize(AUC=AUC(x=time,
                    y=Insulin_a, 
                    method = 'spline'),
            mean=mean(Insulin_a))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_0_120_min_AISCHA_Insulin_Condition_A.xlsx")

# Condition B #
res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter (Glucose_b != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=Glucose_b, 
                    method = 'spline'), # default is trapezoidal, spline is better!
            mean=mean(Glucose_b))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_B_120_min_AISCHA_Condition_B.xlsx")

##### Repeat for Insulin #####
# Condition B #
res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter (Insulin_b != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=Insulin_b, 
                    method = 'spline'),
            mean=mean(Insulin_b))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_B_120_min_AISCHA_Insulin_Condition_B.xlsx")

##### Repeat for Insulin 0-120 instead of B-120 #####
# Condition B #
res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter (Insulin_b != 'NA') %>% # solution is to subset to only keep the ones without NA
  filter (time >= 0) %>% 
  summarize(AUC=AUC(x=time,
                    y=Insulin_b, 
                    method = 'spline'), # default is trapezoidal, spline is better!
            mean=mean(Insulin_b))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_0_120_min_AISCHA_Insulin_Condition_B.xlsx")

##### Calculate bottom to substract from AUC for iAUCs from Baseline to 120 minutes and save as csv file #####
# Condition A
d_longGlucoseA$Subject <- factor(d_longGlucoseA$Subject)

res <- d_longGlucoseA %>% 
  group_by(Subject) %>%
  filter (for_AUC != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC, 
                    method = 'spline'),
            mean=mean(for_AUC))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "To_substract_AUC_B_120_min_AISCHA_Condition_A.xlsx")

# Condition A 0-120 instead of B-120
d_longGlucoseA$Subject <- factor(d_longGlucoseA$Subject)

res <- d_longGlucoseA %>% 
  group_by(Subject) %>%
  filter (for_AUC != 'NA') %>% # solution is to subset to only keep the ones without NA
  filter (time >= 0) %>%
  summarize(AUC=AUC(x=time,
                    y=for_AUC, 
                    method = 'spline'),
            mean=mean(for_AUC))
res

res$Group <- d_longGlucoseA$Group[match(res$Subject, d_longGlucoseA$Subject)]
rio :: export(res, "To_substract_AUC_0_120_min_AISCHA_Condition_A.xlsx")

##### Repeat for insulin #####
# Condition A
d_longGlucoseA$Subject <- factor(d_longGlucoseA$Subject)

res <- d_longGlucoseA %>% 
  group_by(Subject) %>%
  filter (for_AUC_Insulin != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC_Insulin, 
                    method = 'spline'),
            mean=mean(for_AUC_Insulin))
res

res$Group <- d_longGlucoseA$Group[match(res$Subject, d_longGlucoseA$Subject)]
rio :: export(res, "To_substract_AUC_B_120_min_AISCHA_Insulin_Condition_A.xlsx")

##### Repeat for insulin 0-120 instead of B-120 #####
# Condition A
d_longGlucoseA$Subject <- factor(d_longGlucoseA$Subject)

res <- d_longGlucoseA %>% 
  group_by(Subject) %>%
  filter (for_AUC_Insulin != 'NA') %>% # solution is to subset to only keep the ones without NA
  filter (time >= 0) %>%
  summarize(AUC=AUC(x=time,
                    y=for_AUC_Insulin, 
                    method = 'spline'),
            mean=mean(for_AUC_Insulin))
res

res$Group <- d_longGlucoseA$Group[match(res$Subject, d_longGlucoseA$Subject)]
rio :: export(res, "To_substract_AUC_0_120_min_AISCHA_Insulin_Condition_A.xlsx")

# Condition B #
res <- d_longGlucoseB %>% 
  group_by(Subject) %>%
  filter (for_AUC != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC, 
                    method = 'spline'),
            mean=mean(for_AUC))
res

res$Group <- d_longGlucoseB$Group[match(res$Subject, d_longGlucoseB$Subject)]
rio :: export(res, "To_substract_AUC_B_120_min_AISCHA_Condition_B.xlsx")

# Condition B 0-120 instead of B-120 #
res <- d_longGlucoseB %>% 
  group_by(Subject) %>%
  filter (for_AUC != 'NA') %>% # solution is to subset to only keep the ones without NA
  filter (time >= 0) %>%
  summarize(AUC=AUC(x=time,
                    y=for_AUC, 
                    method = 'spline'),
            mean=mean(for_AUC))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "To_substract_AUC_0_120_min_AISCHA_Condition_B.xlsx")

##### Repeat for insulin #####
# Condition B #
d_longGlucoseB$Subject <- factor(d_longGlucoseB$Subject)

res <- d_longGlucoseB %>% 
  group_by(Subject) %>%
  filter (for_AUC_Insulin != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC_Insulin, 
                    method = 'spline'),
            mean=mean(for_AUC_Insulin))
res

res$Group <- d_longGlucoseB$Group[match(res$Subject, d_longGlucoseB$Subject)]
rio :: export(res, "To_substract_AUC_B_120_min_AISCHA_Insulin_Condition_B.xlsx")

# Condition B 0-120 instead of B-120 #
d_longGlucoseB$Subject <- factor(d_longGlucoseB$Subject)

res <- d_longGlucoseB %>% 
  group_by(Subject) %>%
  filter (for_AUC_Insulin != 'NA') %>% # solution is to subset to only keep the ones without NA
  filter (time >= 0) %>%
  summarize(AUC=AUC(x=time,
                    y=for_AUC_Insulin, 
                    method = 'spline'),
            mean=mean(for_AUC_Insulin))
res

res$Group <- d_longGlucoseB$Group[match(res$Subject, d_longGlucoseB$Subject)]
rio :: export(res, "To_substract_AUC_0_120_min_AISCHA_Insulin_Condition_B.xlsx")

##### Calculate AUCs from 0 to 60 minutes and save as csv file #####
# Condition A #
res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter (Glucose_a != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=Glucose_a, 
                    method = 'spline'), # default is trapezoidal, spline is better!
            mean=mean(Glucose_a))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_B_60_min_AISCHA_Condition_A.xlsx")

##### Repeat for insulin #####
# Condition A #
res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter (Insulin_a != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=Insulin_a, 
                    method = 'spline'), # default is trapezoidal, spline is better!
            mean=mean(Insulin_a))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_B_60_min_AISCHA_Insulin_Condition_A.xlsx")

##### Repeat for insulin 0-60 instead of B-60 #####
# Condition A #
res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter(time >= 0) %>%
  filter (Insulin_a != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=Insulin_a, 
                    method = 'spline'), # default is trapezoidal, spline is better!
            mean=mean(Insulin_a))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_0_60_min_AISCHA_Insulin_Condition_A.xlsx")

# Condition B #
res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter (Glucose_b != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=Glucose_b, 
                    method = 'spline'), # default is trapezoidal, spline is better!
            mean=mean(Glucose_b))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_B_60_min_AISCHA_Condition_B.xlsx")

##### Repeat for insulin #####
# Condition B #
res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter (Insulin_b != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=Insulin_b, 
                    method = 'spline'), # default is trapezoidal, spline is better!
            mean=mean(Insulin_b))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_B_60_min_AISCHA_Insulin_Condition_B.xlsx")

##### Repeat for insulin 0-60 instead of B-60 #####
# Condition B #
res <- d_longGlucose %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter(time >= 0) %>%
  filter (Insulin_b != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=Insulin_b, 
                    method = 'spline'), # default is trapezoidal, spline is better!
            mean=mean(Insulin_b))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "AUC_0_60_min_AISCHA_Insulin_Condition_B.xlsx")

##### Calculate bottom to substract from AUC for iAUCs from Baseline to 60 minutes and save as csv file #####
# Condition A
d_longGlucoseA$Subject <- factor(d_longGlucoseA$Subject)

res <- d_longGlucoseA %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter (for_AUC != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC, 
                    method = 'spline'),
            mean=mean(for_AUC))
res

res$Group <- d_longGlucoseA$Group[match(res$Subject, d_longGlucoseA$Subject)]
rio :: export(res, "To_substract_AUC_B_60_min_AISCHA_Condition_A.xlsx")

# Condition A 0-60 instead of B-60
d_longGlucoseA$Subject <- factor(d_longGlucoseA$Subject)

res <- d_longGlucoseA %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter (time >= 0) %>%
  filter (for_AUC != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC, 
                    method = 'spline'),
            mean=mean(for_AUC))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "To_substract_AUC_0_60_min_AISCHA_Condition_A.xlsx")

##### Repeat for insulin #####
# Condition A
d_longGlucoseA$Subject <- factor(d_longGlucoseA$Subject)

res <- d_longGlucoseA %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter (for_AUC_Insulin != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC_Insulin, 
                    method = 'spline'),
            mean=mean(for_AUC_Insulin))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "To_substract_AUC_B_60_min_AISCHA_Insulin_Condition_A.xlsx")

##### Repeat for insulin 0-60 instead of B-60 #####
# Condition A
d_longGlucoseA$Subject <- factor(d_longGlucoseA$Subject)

res <- d_longGlucoseA %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter(time >= 0) %>%
  filter (for_AUC_Insulin != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC_Insulin, 
                    method = 'spline'),
            mean=mean(for_AUC_Insulin))
res

res$Group <- d_longGlucose$Group[match(res$Subject, d_longGlucose$Subject)]
rio :: export(res, "To_substract_AUC_0_60_min_AISCHA_Insulin_Condition_A.xlsx")

# Condition B #
res <- d_longGlucoseB %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter (for_AUC != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC, 
                    method = 'spline'),
            mean=mean(for_AUC))
res

res$Group <- d_longGlucoseB$Group[match(res$Subject, d_longGlucoseB$Subject)]
rio :: export(res, "To_substract_AUC_B_60_min_AISCHA_Condition_B.xlsx")

# Condition B 0-60 instead of B-60 #
res <- d_longGlucoseB %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter (time >= 0) %>%
  filter (for_AUC != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC, 
                    method = 'spline'),
            mean=mean(for_AUC))
res

res$Group <- d_longGlucoseB$Group[match(res$Subject, d_longGlucoseB$Subject)]
rio :: export(res, "To_substract_AUC_0_60_min_AISCHA_Condition_B.xlsx")

##### Repeat for insulin #####
# Condition B
d_longGlucoseB$Subject <- factor(d_longGlucoseB$Subject)

res <- d_longGlucoseB %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter (for_AUC_Insulin != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC_Insulin, 
                    method = 'spline'),
            mean=mean(for_AUC_Insulin))
res

res$Group <- d_longGlucoseB$Group[match(res$Subject, d_longGlucoseB$Subject)]
rio :: export(res, "To_substract_AUC_B_60_min_AISCHA_Insulin_Condition_B.xlsx")

##### Repeat for insulin 0-60 instead of B-60 #####
# Condition B
d_longGlucoseB$Subject <- factor(d_longGlucoseB$Subject)

res <- d_longGlucoseB %>% 
  group_by(Subject) %>%
  filter(time <= 61) %>%
  filter(time >= 0) %>%
  filter (for_AUC_Insulin != 'NA') %>% # solution is to subset to only keep the ones without NA
  summarize(AUC=AUC(x=time,
                    y=for_AUC_Insulin, 
                    method = 'spline'),
            mean=mean(for_AUC_Insulin))
res

res$Group <- d_longGlucoseB$Group[match(res$Subject, d_longGlucoseB$Subject)]
rio :: export(res, "To_substract_AUC_0_60_min_AISCHA_Insulin_Condition_B.xlsx")

##### Compare AUCs per group #####
##### 120 min AUC per condition in the lean group #####
data_lean <- data %>% filter(BMI_class == 'Lean')
data_metsyn <- data %>% filter(BMI_class == 'MetSyn')

d_long_lean <- d_long %>% filter(BMI_class == 'Lean')
d_long_metsyn <- d_long %>% filter(BMI_class == 'MetSyn')

comps <- list(c('A', 'B'))

pl <- ggplot(d_long_lean, aes(x=Condition, y=AUC_Glucose_0_120))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose AUC 0 - 120 minutes (mmol)')+
  ggtitle('Glucose AUC 0 - 120 minutes Lean Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_AUC_120_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_AUC_120_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_AUC_120_lean.jpg', width = 7, height = 5, device = 'jpeg')

##### 120 min iAUC per condition in the lean group from masterfile #####
data_lean <- dplyr::filter(data, BMI_class %in% "Lean")
data_metsyn <- dplyr::filter(data, BMI_class %in% "MetSyn")

data_lean_long <- melt(data_lean, id.vars = 'Subject', measure.vars = c('iAUC_120_Glucose_A', 'iAUC_120_Glucose_B'))
colnames(data_lean_long) <- c('Subject', 'Condition', 'iAUC')
data_lean_long$Condition <- as.character(data_lean_long$Condition)
data_lean_long$Condition[data_lean_long$Condition=="iAUC_120_Glucose_A"] <- "A"
data_lean_long$Condition[data_lean_long$Condition=="iAUC_120_Glucose_B"] <- "B"
data_lean_long$Condition <- as.factor(data_lean_long$Condition)

data_metsyn_long <- melt(data_metsyn, id.vars = 'Subject', measure.vars = c('iAUC_120_Glucose_A', 'iAUC_120_Glucose_B'))
colnames(data_metsyn_long) <- c('Subject', 'Condition', 'iAUC Glucose')
data_metsyn_long$Condition <- as.character(data_metsyn_long$Condition)
data_metsyn_long$Condition[data_metsyn_long$Condition=="iAUC_120_Glucose_A"] <- "A"
data_metsyn_long$Condition[data_metsyn_long$Condition=="iAUC_120_Glucose_B"] <- "B"
data_metsyn_long$Condition <- as.factor(data_metsyn_long$Condition)

data_long <- melt(data, id.vars = 'Subject', measure.vars = c('iAUC_120_Glucose_A', 'iAUC_120_Glucose_B'))
colnames(data_long) <- c('Subject', 'Condition', 'iAUC Glucose')
data_long$Condition <- as.character(data_long$Condition)
data_long$Condition[data_long$Condition=="iAUC_120_Glucose_A"] <- "A"
data_long$Condition[data_long$Condition=="iAUC_120_Glucose_B"] <- "B"
data_long$Condition <- as.factor(data_long$Condition)

comps <- list(c('A', 'B'))

# Lean group
pl <- ggplot(d_long_lean, aes(x=Condition, y=iAUC_Glucose_0_120))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose iAUC 0 - 120 minutes (mmol)')+
  ggtitle('Glucose iAUC 0 - 120 minutes Lean Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_iAUC_120_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_iAUC_120_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_iAUC_120_lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=iAUC_Glucose_0_120))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose iAUC 0 - 120 minutes (mmol)')+
  ggtitle('Glucose iAUC 0 - 120 minutes MetSyn Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_iAUC_120_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_iAUC_120_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_iAUC_120_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

# Pooled
pl <- ggplot(d_long, aes(x=Condition, y=iAUC_Glucose_0_120))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose iAUC 0 - 120 minutes (mmol)')+
  ggtitle('Glucose iAUC 0 - 120 minutes all subs')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_iAUC_120_pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_iAUC_120_pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_iAUC_120_pooled.jpg', width = 7, height = 5, device = 'jpeg')

##### 60 min iAUC per condition in the lean group from masterfile #####
data_lean_long <- melt(data_lean, id.vars = 'Subject', measure.vars = c('iAUC_60_Glucose_A', 'iAUC_60_Glucose_B'))
colnames(data_lean_long) <- c('Subject', 'Condition', 'iAUC Glucose')
data_lean_long$Condition <- as.character(data_lean_long$Condition)
data_lean_long$Condition[data_lean_long$Condition=="iAUC_60_Glucose_A"] <- "A"
data_lean_long$Condition[data_lean_long$Condition=="iAUC_60_Glucose_B"] <- "B"
data_lean_long$Condition <- as.factor(data_lean_long$Condition)

data_metsyn_long <- melt(data_metsyn, id.vars = 'Subject', measure.vars = c('iAUC_60_Glucose_A', 'iAUC_60_Glucose_B'))
colnames(data_metsyn_long) <- c('Subject', 'Condition', 'iAUC Glucose')
data_metsyn_long$Condition <- as.character(data_metsyn_long$Condition)
data_metsyn_long$Condition[data_metsyn_long$Condition=="iAUC_60_Glucose_A"] <- "A"
data_metsyn_long$Condition[data_metsyn_long$Condition=="iAUC_60_Glucose_B"] <- "B"
data_metsyn_long$Condition <- as.factor(data_metsyn_long$Condition)

data_long <- melt(data, id.vars = 'Subject', measure.vars = c('iAUC_60_Glucose_A', 'iAUC_60_Glucose_B'))
colnames(data_long) <- c('Subject', 'Condition', 'iAUC Glucose')
data_long$Condition <- as.character(data_long$Condition)
data_long$Condition[data_long$Condition=="iAUC_60_Glucose_A"] <- "A"
data_long$Condition[data_long$Condition=="iAUC_60_Glucose_B"] <- "B"
data_long$Condition <- as.factor(data_long$Condition)

# Lean group
pl <- ggplot(d_long_lean, aes(x=Condition, y=iAUC_Glucose_0_60))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose iAUC 0 - 60 minutes (mmol)')+
  ggtitle('Glucose iAUC 0 - 60 minutes Lean Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_iAUC_60_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_iAUC_60_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_iAUC_60_lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=iAUC_Glucose_0_60))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose iAUC 0 - 60 minutes (mmol)')+
  ggtitle('Glucose iAUC 0 - 60 minutes MetSyn Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_iAUC_60_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_iAUC_60_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_iAUC_60_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

# Pooled
pl <- ggplot(d_long, aes(x=Condition, y=iAUC_Glucose_0_60))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose iAUC 0 - 60 minutes (mmol)')+
  ggtitle('Glucose iAUC 0 - 60 minutes all subs')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_iAUC_60_pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_iAUC_60_pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_iAUC_60_pooled.jpg', width = 7, height = 5, device = 'jpeg')


##### Remove outliers #####
outlier.LThr <- mean(AUC_120_Condition_A_lean$AUC, na.rm=TRUE) - 3*sd(AUC_120_Condition_A_lean$AUC, na.rm=TRUE) # lower threshold
outlier.UThr <- mean(AUC_120_Condition_A_lean$AUC, na.rm=TRUE) + 3*sd(AUC_120_Condition_A_lean$AUC, na.rm=TRUE) # upper threshold
  
STR_removed <- sum(AUC_120_Condition_A_lean$AUC>outlier.UThr, na.rm=TRUE) + sum(AUC_120_Condition_A_lean$AUC<outlier.LThr, na.rm=TRUE)
AUC_120_Condition_A_lean$AUC[AUC_120_Condition_A_lean$AUC>outlier.UThr] <- NA
AUC_120_Condition_A_lean$AUC[AUC_120_Condition_A_lean$AUC<outlier.LThr] <- NA
# Appearantly no outliers in data, thus no repeat necessary #

##### Compare AUCs per group #####
##### 60 min AUC per condition in the lean group #####
AUC_60_lean <- AUC_0_60_min_AISCHA_Glucose %>% filter(BMI_class == 'Lean')
comps <- list(c('A', 'B'))

pl <- ggplot(d_long_lean, aes(x=Condition, y=AUC_Glucose_0_60))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose AUC 0 - 60 minutes (mmol)')+
  ggtitle('Glucose AUC 0 - 60 minutes Lean Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_AUC_60_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_AUC_60_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_AUC_60_lean.jpg', width = 7, height = 5, device = 'jpeg')

##### Remove outliers #####
outlier.LThr <- mean(d_long_lean$AUC_Glucose_0_60, na.rm=TRUE) - 3*sd(d_long_lean$AUC_Glucose_0_60, na.rm=TRUE) # lower threshold
outlier.UThr <- mean(d_long_lean$AUC_Glucose_0_60, na.rm=TRUE) + 3*sd(d_long_lean$AUC_Glucose_0_60, na.rm=TRUE) # upper threshold

STR_removed <- sum(d_long_lean$AUC_Glucose_0_60>outlier.UThr, na.rm=TRUE) + sum(d_long_lean$AUC_Glucose_0_60<outlier.LThr, na.rm=TRUE)
d_long_lean$AUC_Glucose_0_60[d_long_lean$AUC_Glucose_0_60>outlier.UThr] <- NA
d_long_lean$AUC_Glucose_0_60[d_long_lean$AUC_Glucose_0_60<outlier.LThr] <- NA

# Repeat without outliers #
pl <- ggplot(d_long_lean, aes(x=Condition, y=AUC_Glucose_0_60))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose AUC 0 - 60 minutes (mmol)')+
  ggtitle('Glucose AUC 0 - 60 minutes Lean Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_AUC_60_lean_NoOutliers.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_AUC_60_lean_NoOutliers.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_AUC_60_lean_NoOutliers.jpg', width = 7, height = 5, device = 'jpeg')

##### Compare AUCs per group #####
##### 120 min AUC per condition in the metsyn group #####
AUC_120_metsyn <- AUC_0_120_min_AISCHA_Glucose %>% filter(BMI_class == 'MetSyn')
comps <- list(c('A', 'B'))

pl <- ggplot(d_long_metsyn, aes(x=Condition, y=AUC_Glucose_0_120))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose AUC 0 - 120 minutes (mmol)')+
  ggtitle('Glucose AUC 0 - 120 minutes MetSyn Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_AUC_120_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_AUC_120_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_AUC_120_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

##### Remove outliers #####
outlier.LThr <- mean(d_long_metsyn$AUC_Glucose_0_120, na.rm=TRUE) - 3*sd(d_long_metsyn$AUC_Glucose_0_120, na.rm=TRUE) # lower threshold
outlier.UThr <- mean(d_long_metsyn$AUC_Glucose_0_120, na.rm=TRUE) + 3*sd(d_long_metsyn$AUC_Glucose_0_120, na.rm=TRUE) # upper threshold

STR_removed <- sum(d_long_metsyn$AUC_Glucose_0_120>outlier.UThr, na.rm=TRUE) + sum(d_long_metsyn$AUC_Glucose_0_120<outlier.LThr, na.rm=TRUE)
d_long_metsyn$AUC_Glucose_0_120[d_long_metsyn$AUC_Glucose_0_120>outlier.UThr] <- NA
d_long_metsyn$AUC_Glucose_0_120[d_long_metsyn$AUC_Glucose_0_120<outlier.LThr] <- NA

# Repeated without outliers #
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=AUC_Glucose_0_120))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose AUC 0 - 120 minutes (mmol)')+
  ggtitle('Glucose AUC 0 - 120 minutes MetSyn Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_AUC_120_metsyn_NoOutliers.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_AUC_120_metsyn_NoOutliers.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_AUC_120_metsyn_NoOutliers.jpg', width = 7, height = 5, device = 'jpeg')

##### Compare AUCs per group #####
##### 60 min AUC per condition in the metsyn group #####
AUC_60_metsyn <- AUC_0_60_min_AISCHA_Glucose %>% filter(BMI_class == 'MetSyn')
comps <- list(c('A', 'B'))

pl <- ggplot(d_long_metsyn, aes(x=Condition, y=AUC_Glucose_0_60))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Glucose AUC 0 - 60 minutes (mmol)')+
  ggtitle('Glucose AUC 0 - 60 minutes MetSyn Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Glucose_AUC_60_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_AUC_60_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_AUC_60_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

##### Remove outliers #####
outlier.LThr <- mean(AUC_60_metsyn$AUC, na.rm=TRUE) - 3*sd(AUC_60_metsyn$AUC, na.rm=TRUE) # lower threshold
outlier.UThr <- mean(AUC_60_metsyn$AUC, na.rm=TRUE) + 3*sd(AUC_60_metsyn$AUC, na.rm=TRUE) # upper threshold

STR_removed <- sum(AUC_60_metsyn$AUC>outlier.UThr, na.rm=TRUE) + sum(AUC_60_metsyn$AUC<outlier.LThr, na.rm=TRUE)
AUC_60_metsyn$AUC[AUC_60_metsyn$AUC>outlier.UThr] <- NA
AUC_60_metsyn$AUC[AUC_60_metsyn$AUC<outlier.LThr] <- NA
# Appearantly no outliers in data, thus no repeat necessary #

################## Repeat everything for INSULIN ##############################
##### Compare AUCs per group #####
##### 120 min Insulin AUC per condition in the lean group #####
comps <- list(c('A', 'B'))

pl <- ggplot(d_long_lean, aes(x=Condition, y=AUC_Insulin_0_120))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Insulin AUC 0 - 120 minutes (mmol)')+
  ggtitle('Insulin AUC 0 - 120 minutes Lean Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Insulin_AUC_120_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_AUC_120_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_AUC_120_lean.jpg', width = 7, height = 5, device = 'jpeg')

# Lean group
pl <- ggplot(d_long_lean, aes(x=Condition, y=iAUC_Insulin_0_120))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Insulin iAUC 0 - 120 minutes (mmol)')+
  ggtitle('Insulin iAUC 0 - 120 minutes Lean Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Insulin_iAUC_120_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_iAUC_120_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_iAUC_120_lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=iAUC_Insulin_0_120))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Insulin iAUC 0 - 120 minutes (mmol)')+
  ggtitle('Insulin iAUC 0 - 120 minutes MetSyn Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Insulin_iAUC_120_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_iAUC_120_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_iAUC_120_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

# Pooled
pl <- ggplot(d_long, aes(x=Condition, y=iAUC_Insulin_0_120))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Insulin iAUC 0 - 120 minutes (mmol)')+
  ggtitle('Insulin iAUC 0 - 120 minutes all subs')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Insulin_iAUC_120_pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_iAUC_120_pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_iAUC_120_pooled.jpg', width = 7, height = 5, device = 'jpeg')

##### 60 min iAUC per condition in the lean group from masterfile #####
# Lean group
pl <- ggplot(d_long_lean, aes(x=Condition, y=iAUC_Insulin_0_60))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Insulin iAUC 0 - 60 minutes (mmol)')+
  ggtitle('Insulin iAUC 0 - 60 minutes Lean Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Insulin_iAUC_60_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_iAUC_60_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_iAUC_60_lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=iAUC_Insulin_0_60))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Insulin iAUC 0 - 60 minutes (mmol)')+
  ggtitle('Insulin iAUC 0 - 60 minutes MetSyn Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Insulin_iAUC_60_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_iAUC_60_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_iAUC_60_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

# Pooled
pl <- ggplot(d_long, aes(x=Condition, y=iAUC_Insulin_0_60))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Insulin iAUC 0 - 60 minutes (mmol)')+
  ggtitle('Insulin iAUC 0 - 60 minutes all subs')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Insulin_iAUC_60_pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_iAUC_60_pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_iAUC_60_pooled.jpg', width = 7, height = 5, device = 'jpeg')

##### Compare AUCs per group #####
##### 60 min AUC per condition in the lean group #####
comps <- list(c('A', 'B'))

pl <- ggplot(d_long_lean, aes(x=Condition, y=AUC_Insulin_0_60))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Insulin AUC 0 - 60 minutes (mmol)')+
  ggtitle('Insulin AUC 0 - 60 minutes Lean Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Insulin_AUC_60_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_AUC_60_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_AUC_60_lean.jpg', width = 7, height = 5, device = 'jpeg')

##### Compare AUCs per group #####
##### 120 min AUC per condition in the metsyn group #####
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=AUC_Insulin_0_120))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Insulin AUC 0 - 120 minutes (mmol)')+
  ggtitle('Insulin AUC 0 - 120 minutes MetSyn Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Insulin_AUC_120_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_AUC_120_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_AUC_120_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

##### Compare AUCs per group #####
##### 60 min AUC per condition in the metsyn group #####
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=AUC_Insulin_0_60))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Insulin AUC 0 - 60 minutes (mmol)')+
  ggtitle('Insulin AUC 0 - 60 minutes MetSyn Group')+
  scale_color_manual(breaks=c("A", "B"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'Insulin_AUC_60_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_AUC_60_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_AUC_60_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

##### Compare Urine Acetate per timepoint #####
d_long_A <- d_longUrine %>% filter(Condition == 'Acetate')
d_long_B <- d_longUrine %>% filter(Condition == 'Placebo')

comps <- list(c('Baseline', 'Infusion'))

# Condition A #
pl1 <- ggplot(d_long_A, aes(x=time, y=UrineAcetate))+
  geom_line(aes(col = "grey", group=Subject), size=1)+
  geom_point(aes(col = time), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Timepoint')+
  ylab('Urine acetate concentration (umol/L)')+
  ggtitle('Acetate condition')+
  scale_color_manual(breaks=c("Baseline", "Infusion"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl1

pl1 + labs(color = "Time of collection")
pl1 <- pl1 + labs(color = "Time of collection")

ggsave(pl, filename = 'Urine_Acetate_conditionA.pdf', width = 7, height = 6, device = 'pdf')
ggsave(pl, filename = 'Urine_Acetate_conditionA.png', width = 7, height = 6, device = 'png')
ggsave(pl, filename = 'Urine_Acetate_conditionA.jpg', width = 7, height = 6, device = 'jpeg')

# Condition B #
pl2 <- ggplot(d_long_B, aes(x=time, y=UrineAcetate))+
  geom_line(aes(col = "grey", group=Subject), size=1)+
  geom_point(aes(col = time), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Timepoint')+
  ylab('Urine acetate concentration (umol/L)')+
  ggtitle('Placebo condition')+
  scale_color_manual(breaks=c("Baseline", "Infusion"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl2

pl2 + labs(color = "Time of collection")
pl2 <- pl2 + labs(color = "Time of collection")

ggsave(pl, filename = 'Urine_Acetate_conditionB.pdf', width = 7, height = 6, device = 'pdf')
ggsave(pl, filename = 'Urine_Acetate_conditionB.png', width = 7, height = 6, device = 'png')
ggsave(pl, filename = 'Urine_Acetate_conditionB.jpg', width = 7, height = 6, device = 'jpeg')

plots <- ggarrange(pl1, pl2,
                   labels = c("A", "B"), 
                   ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
plots

ggsave(plots, filename = 'Urine_acetate.pdf', width = 9, height = 6, device = 'pdf')
ggsave(plots, filename = 'Urine_acetate.png', width = 9, height = 6, device = 'png')
ggsave(plots, filename = 'Urine_acetate.jpg', width = 9, height = 6, device = 'jpeg')

##### Plot VAS hunger per groep #####
# Lean group #
d_longUrine <- d_longVAS %>% filter(BMI_class=='Lean')

str(d_longUrine$Mean)

ggplot(data = d_longUrine, x = time, y = UrineAcetate)

pl1 <- ggplot(data=d_longUrine, aes(x=time, y=Mean, group=Condition, colour=Condition))+
  geom_line(size = 1.4)+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = Mean_min_SE, ymax = Mean_plus_SE, col=Condition), size = 1.4, width = 0.12)+
  scale_x_discrete(breaks=c("Baseline", "Infusion"))+
  scale_color_manual(labels = c("Acetate", "Placebo"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  labs(x='Timepoint', title='Urinary acetate excretion', y='Urine acetate (umol/L)')+
  theme_Publication()+
  annotate("text", x=2.33, y=180, label = "p = 0.78", fontface = 2, size = 5)+
  annotate("text", x=2.33, y=1267, label = "*p = 2.9e-11", fontface = 2, size = 5)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = 'right')+
  theme(plot.title = element_text(size = 19, face = "bold"),
        axis.text=element_text(size=18),
        axis.title.x=element_text(size=16,face="bold"),
        axis.title.y=element_text(size=16,face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
pl1

ggsave(pl1, filename = 'Urinary_Acetate_Combined_line.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl1, filename = 'Urinary_Acetate_Combined_line.png', width = 7, height = 5, device = 'png')
ggsave(pl1, filename = 'Urinary_Acetate_Combined_line.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group #
d_longVAS_lean <- d_longVAS %>% filter(BMI_class=='Lean')

pl1 <- ggplot(data=d_longVAS_lean, aes(x = time, y = VAS_hunger))+
  geom_point(aes(x=time, y=Mean,
                 col=Condition), size = 3)+
  geom_line(aes(x=time, y=Mean, col=Condition), size = 1.2)+
  geom_errorbar(aes(ymin = Mean_min_SE, ymax = Mean_plus_SE, col=Condition), size = 0.8, width = 0.05)+
  scale_x_continuous(breaks=c(1, 2, 3))+
  scale_color_manual(labels = c("Acetate", "Placebo"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  labs(x='Timepoint', title='Lean group', y = "")+
  theme_Publication()+
  annotate("text", x=2.4, y=5.5, label = "p = 0.32", fontface = 2)+
  ylim(2, 6)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = 'right')+
  theme(axis.text=element_text(size=20),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=20,face="bold"))
pl1

d_longVAS_metsyn <- d_longVAS %>% filter(BMI_class=='MetSyn')

pl2 <- ggplot(data=d_longVAS_metsyn, aes(x = time, y = VAS_hunger))+
  geom_point(aes(x=time, y=Mean,
                 col=Condition), size = 3)+
  geom_line(aes(x=time, y=Mean, col=Condition), size = 1.2)+
  geom_errorbar(aes(ymin = Mean_min_SE, ymax = Mean_plus_SE, col=Condition), size = 0.8, width = 0.05)+
  scale_x_continuous(breaks=c(1, 2, 3))+
  scale_color_manual(labels = c("Acetate", "Placebo"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  labs(x='Timepoint', title='MetSyn group', y = "")+
  theme_Publication()+
  annotate("text", x=2.4, y=5.5, label = "p = 0.12", fontface = 2)+
  ylim(2, 6)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = 'right')+
  theme(axis.text=element_text(size=20),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=20,face="bold"))
pl2

ggsave(pl2, filename = 'VAS_hunger_metsyn_SE.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl2, filename = 'VAS_hunger_metsyn_SE.png', width = 7, height = 5, device = 'png')
ggsave(pl2, filename = 'VAS_hunger_metsyn_SE.jpg', width = 7, height = 5, device = 'jpeg')

plots <- ggarrange(pl1, pl2,
                   labels = c("A", "B"), 
                   ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
plots

plots <- annotate_figure(plots, top = text_grob("VAS hunger score upon SMMT per BMI group",
                                                color = "black", face = "bold", size = 20))

ggsave(plots, filename = 'VAS_hunger_combined_SE.pdf', width = 8, height = 4, device = 'pdf')
ggsave(plots, filename = 'VAS_hunger_combined_SE.png', width = 8, height = 4, device = 'png')
ggsave(plots, filename = 'VAS_hunger_combined_SE.jpg', width = 8, height = 4, device = 'jpeg')
getwd()

###################################### DONE INSULIN ANALYSES #################

##### Plots smoothed voor glucose #####
##### Plot met juiste mooie lay out voor artikel Glucose lean groep #####
d_longGlucose_lean <- d_longGlucose %>% filter(BMI_class %in% 'Lean')
pl1 <- ggplot(data=d_longGlucose_lean, aes(x = time, y = Glucose))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Glucose response upon SMMT lean group', y='Glucose (mmol/L)')+
  theme_Publication()+
  annotate("text", x=70, y=7.1, label = "p = 0.1486", fontface = 2)+
theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl1

##### Lineplot with errorbars voor glucose ##### 
d_longGlucose_lean <- d_longGlucose %>% filter(BMI_class %in% 'Lean')
pl1 <- ggplot(data=d_longGlucose_lean, aes(x = time, y = Glucose))+
  #geom_smooth(aes(group = Group, col = Group), method = "loess", level=0.68)+
  geom_point(aes(x=time, y=Mean,
                 col=Condition), size = 3)+
  #geom_boxplot(aes(x = time, y = Glucose, color = Group), outlier.colour = NA, show.legend = F, fatten = 1, lwd = 1.2)+
  geom_line(data = subset(d_longGlucose_lean, Condition == 'Acetate'), aes(x=time, y=Mean, col=Condition), size = 1)+
  geom_line(data = subset(d_longGlucose_lean, Condition == 'Placebo'), aes(x=time, y=Mean, col=Condition), size = 1)+
  geom_errorbar(data = subset(d_longGlucose_lean, Condition == 'Acetate'), aes(ymin = Mean_min_SD, ymax = Mean, col=Condition), width = 2, size = 1)+
  geom_errorbar(data = subset(d_longGlucose_lean, Condition == 'Placebo'), aes(ymin = Mean, ymax = Mean_plus_SD, col=Condition), width = 2, size = 1)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(labels = c("Acetate", "Placebo"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Glucose response upon SMMT Lean group', y='Glucose (mmol/L)')+
  theme_Publication()+
  annotate("text", x=75, y=7.1, label = "p = 0.1486", fontface = 2)+
  ylim(3.2, 8.1)+
  #theme(panel.grid.major.x = element_line(size = 0.5, color='grey'))+
  theme(legend.position = 'right')
pl1

ggsave(pl1, filename = 'Glucose_errorbars_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl1, filename = 'Glucose_errorbars_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl1, filename = 'Glucose_errorbars_lean.jpg', width = 7, height = 5, device = 'jpeg')

##### Lineplot with errorbars voor acetaat pooled ##### 
pl1 <- ggplot(data=d_longAcetate, aes(x = time, y = Acetate))+
  #geom_smooth(aes(group = Group, col = Group), method = "loess", level=0.68)+
  geom_point(aes(x=time, y=Mean,
                 col=Condition), size = 2)+
  #geom_boxplot(aes(x = time, y = Glucose, color = Group), outlier.colour = NA, show.legend = F, fatten = 1, lwd = 1.2)+
  geom_line(data = d_longAcetate, aes(x=time, y=Mean, col=Condition), size = 1)+
  geom_errorbar(data = d_longAcetate, aes(ymin = Mean_min_SE, ymax = Mean_plus_SE, col=Condition), width = 4, size = 1)+
  scale_x_continuous(breaks=c(-90, -30, 30, 60, 90, 120))+
  scale_color_manual(labels = c("Acetate", "Placebo"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Acetate response upon SMMT', y='Acetate (umol/L)')+
  theme_Publication()+
  annotate("text", x=75, y=7.1, label = "p < 0.0001", fontface = 2)+
  ylim(0, 600)+
  #theme(panel.grid.major.x = element_line(size = 0.5, color='grey'))+
  theme(legend.position = 'right')
pl1

ggsave(pl1, filename = 'Acetate_errorbars.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl1, filename = 'Acetate_errorbars.png', width = 7, height = 5, device = 'png')
ggsave(pl1, filename = 'Acetate_errorbars.jpg', width = 7, height = 5, device = 'jpeg')
getwd()

##### Plot met juiste mooie lay out voor artikel Glucose metsyn groep #####
d_longGlucose_metsyn <- d_longGlucose %>% filter(BMI_class %in% 'MetSyn')
pl2 <- ggplot(data=d_longGlucose_metsyn, aes(x = time, y = Glucose))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Glucose response upon SMMT MetSyn group', y='Glucose (mmol/L)')+
  theme_Publication()+
  annotate("text", x=70, y=7.1, label = "p = 0.9143", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl2

ggsave(pl, filename = 'Glucose_spline_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_spline_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_spline_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

##### Lineplot with errorbars voor glucose ##### 
d_longGlucose_metsyn <- d_longGlucose %>% filter(BMI_class %in% 'MetSyn')
pl2 <- ggplot(data=d_longGlucose_metsyn, aes(x = time, y = Glucose))+
  #geom_smooth(aes(group = Group, col = Group), method = "loess", level=0.68)+
  geom_point(aes(x=time, y=Mean,
                 col=Condition), size = 3)+
  #geom_boxplot(aes(x = time, y = Glucose, color = Group), outlier.colour = NA, show.legend = F, fatten = 1, lwd = 1.2)+
  geom_line(data = subset(d_longGlucose_metsyn, Condition == 'Acetate'), aes(x=time, y=Mean, col=Condition), size = 1)+
  geom_line(data = subset(d_longGlucose_metsyn, Condition == 'Placebo'), aes(x=time, y=Mean, col=Condition), size = 1)+
  geom_errorbar(data = subset(d_longGlucose_metsyn, Condition == 'Acetate'), aes(ymin = Mean_min_SD, ymax = Mean, col=Condition), width = 2, size = 1)+
  geom_errorbar(data = subset(d_longGlucose_metsyn, Condition == 'Placebo'), aes(ymin = Mean, ymax = Mean_plus_SD, col=Condition), width = 2, size = 1)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(labels = c("Acetate", "Placebo"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Glucose response upon SMMT MetSyn group', y='Glucose (mmol/L)')+
  theme_Publication()+
  annotate("text", x=75, y=7.1, label = "p = 0.9143", fontface = 2)+
  ylim(3.2, 8.1)+
  #theme(panel.grid.major.x = element_line(size = 0.5, color='grey'))+
  theme(legend.position = 'right')
pl2

ggsave(pl2, filename = 'Glucose_errorbars_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl2, filename = 'Glucose_errorbars_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl2, filename = 'Glucose_errorbars_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

plots <- ggarrange(pl1, pl2,
                   labels = c("A", "B"),
                   ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")
plots

plots <- annotate_figure(plots, top = text_grob("Postprandial glucose response per BMI group",
                                                color = "black", face = "bold", size = 20))

ggsave(plots, filename = 'Glucose_spline_perBMI_2rows.png', width = 8, height = 9, device = 'png')
ggsave(plots, filename = 'Glucose_spline_perBMI_2rows.jpg', width = 8, height = 9, device = 'jpg')
ggsave(plots, filename = 'Glucose_spline_perBMI_2rows.pdf', width = 8, height = 9, device = 'pdf')

ggsave(plots, filename = 'Glucose_errorbars_perBMI_2rows.png', width = 8, height = 9, device = 'png')
ggsave(plots, filename = 'Glucose_errorbars_perBMI_2rows.jpg', width = 8, height = 9, device = 'jpg')
ggsave(plots, filename = 'Glucose_errorbars_perBMI_2rows.pdf', width = 8, height = 9, device = 'pdf')

##### Plot met juiste mooie lay out voor artikel Glucose pooled #####
pl <- ggplot(data=d_longGlucose, aes(x = time, y = Glucose))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Glucose response upon SMMT MetSyn group', y='Glucose (mmol/L)')+
  theme_Publication()+
  annotate("text", x=70, y=7.1, label = "p = 0.3525", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

ggsave(pl, filename = 'Glucose_spline_Pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Glucose_spline_Pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Glucose_spline_Pooled.jpg', width = 7, height = 5, device = 'jpeg')

##### Plots smoothed repeated voor insuline #####
##### Plot met juiste mooie lay out voor artikel Insuline lean groep #####
pl <- ggplot(data=d_longGlucose_lean, aes(x = time, y = Insulin))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Insulin response upon SMMT lean group', y='Insulin (pmol/L)')+
  theme_Publication()+
  annotate("text", x=70, y=7.1, label = "p = 0.0623", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

ggsave(pl, filename = 'Insulin_spline_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_spline_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_spline_lean.jpg', width = 7, height = 5, device = 'jpeg')

##### Lineplot with errorbars voor insuline lean group ##### 
d_longGlucose_lean <- d_longGlucose %>% filter(BMI_class %in% 'Lean')
pl3 <- ggplot(data=d_longGlucose_lean, aes(x = time, y = Insulin))+
  #geom_smooth(aes(group = Group, col = Group), method = "loess", level=0.68)+
  geom_point(aes(x=time, y=Mean_Insulin,
                 col=Condition), size = 3)+
  #geom_boxplot(aes(x = time, y = Glucose, color = Group), outlier.colour = NA, show.legend = F, fatten = 1, lwd = 1.2)+
  geom_line(data = subset(d_longGlucose_lean, Condition == 'Acetate'), aes(x=time, y=Mean_Insulin, col=Condition), size = 1)+
  geom_line(data = subset(d_longGlucose_lean, Condition == 'Placebo'), aes(x=time, y=Mean_Insulin, col=Condition), size = 1)+
  geom_errorbar(data = subset(d_longGlucose_lean, Condition == 'Acetate'), aes(ymin = Mean_Insulin, ymax = Mean_Insulin_plus_SD, col=Condition), width = 2, size = 1)+
  geom_errorbar(data = subset(d_longGlucose_lean, Condition == 'Placebo'), aes(ymin = Mean_Insulin_min_SD, ymax = Mean_Insulin, col=Condition), width = 2, size = 1)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(labels = c("Acetate", "Placebo"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Insulin response upon SMMT Lean group', y='Insulin (pmol/L)')+
  theme_Publication()+
  annotate("text", x=75, y=400, label = "p = 0.0623", fontface = 2)+
  ylim(0, 600)+
  #theme(panel.grid.major.x = element_line(size = 0.5, color='grey'))+
  theme(legend.position = 'right')
pl3

ggsave(pl1, filename = 'Insulin_errorbars_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl1, filename = 'Insulin_errorbars_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl1, filename = 'Insulin_errorbars_lean.jpg', width = 7, height = 5, device = 'jpeg')

##### Plot met juiste mooie lay out voor artikel Insulin metsyn groep #####
pl <- ggplot(data=d_longGlucose_metsyn, aes(x = time, y = Insulin))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Insulin response upon SMMT MetSyn group', y='Insulin (pmol/L)')+
  theme_Publication()+
  annotate("text", x=70, y=7.1, label = "p = 0.4723", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

ggsave(pl, filename = 'Insulin_spline_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_spline_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_spline_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

##### Lineplot with errorbars voor insuline metsyn group ##### 
d_longGlucose_metsyn <- d_longGlucose %>% filter(BMI_class %in% 'MetSyn')
pl4 <- ggplot(data=d_longGlucose_metsyn, aes(x = time, y = Insulin))+
  #geom_smooth(aes(group = Group, col = Group), method = "loess", level=0.68)+
  geom_point(aes(x=time, y=Mean_Insulin,
                 col=Condition), size = 3)+
  #geom_boxplot(aes(x = time, y = Glucose, color = Group), outlier.colour = NA, show.legend = F, fatten = 1, lwd = 1.2)+
  geom_line(data = subset(d_longGlucose_metsyn, Condition == 'Acetate'), aes(x=time, y=Mean_Insulin, col=Condition), size = 1)+
  geom_line(data = subset(d_longGlucose_metsyn, Condition == 'Placebo'), aes(x=time, y=Mean_Insulin, col=Condition), size = 1)+
  geom_errorbar(data = subset(d_longGlucose_metsyn, Condition == 'Acetate'), aes(ymin = Mean_Insulin, ymax = Mean_Insulin_plus_SD, col=Condition), width = 2, size = 1)+
  geom_errorbar(data = subset(d_longGlucose_metsyn, Condition == 'Placebo'), aes(ymin = Mean_Insulin_min_SD, ymax = Mean_Insulin, col=Condition), width = 2, size = 1)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(labels = c("Acetate", "Placebo"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Insulin response upon SMMT MetSyn group', y='Insulin (pmol/L)')+
  theme_Publication()+
  annotate("text", x=75, y=550, label = "p = 0.4723", fontface = 2)+
  ylim(0, 900)+
  #theme(panel.grid.major.x = element_line(size = 0.5, color='grey'))+
  theme(legend.position = 'right')
pl4

ggsave(pl2, filename = 'Insulin_errorbars_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl2, filename = 'Insulin_errorbars_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl2, filename = 'Insulin_errorbars_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

plots <- ggarrange(pl1, pl2, pl3, pl4,
                   labels = c("A", "B", "C", "D"),
                   ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
plots

plots <- annotate_figure(plots, top = text_grob("Postprandial insulin response per BMI group",
                                                color = "black", face = "bold", size = 20))

ggsave(plots, filename = 'GI_errorbars_perBMI_2rows.png', width = 11, height = 9, device = 'png')
ggsave(plots, filename = 'GI_errorbars_perBMI_2rows.jpg', width = 11, height = 9, device = 'jpg')
ggsave(plots, filename = 'GI_errorbars_perBMI_2rows.pdf', width = 11, height = 9, device = 'pdf')

##### Plot met juiste mooie lay out voor artikel Insulin pooled #####
pl <- ggplot(data=d_longGlucose, aes(x = time, y = Insulin))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Insulin response upon SMMT MetSyn group', y='Insulin (pmol/L)')+
  theme_Publication()+
  annotate("text", x=70, y=7.1, label = "p = 0.1772", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

ggsave(pl, filename = 'Insulin_spline_Pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Insulin_spline_Pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Insulin_spline_Pooled.jpg', width = 7, height = 5, device = 'jpeg')

##### Plots repeated for acetate #####
##### Plot met juiste mooie lay out voor artikel Acetate pooled #####
pl <- ggplot(data=d_longAcetate, aes(x = time, y = Acetate))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-90, -30, 30, 60, 90, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Acetate response upon SMMT', y='Acetate (uM)')+
  theme_Publication()+
  annotate("text", x=5, y=300, label = "p < 0.0001", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

ggsave(pl, filename = 'Acetate_spline_Pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Acetate_spline_Pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Acetate_spline_Pooled.jpg', width = 7, height = 5, device = 'jpeg')

##### Alleen acetaat in acetaat conditie en dan vergeleken tussen lean en metsyn #####
d_longAcetate2 <- d_longAcetate %>% filter(Condition == 'Acetate')
comps <- list(c('Lean', 'MetSyn'))

pl <- ggplot(data=d_longAcetate2, aes(x = time, y = Acetate))+
  geom_smooth(aes(group = BMI_class, col = BMI_class), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-90, -30, 30, 60, 90, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Acetate response upon SMMT', y='Acetate (uM)')+
  theme_Publication()+
  annotate("text", x=5, y=300, label = "p = 0.8856", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

getwd()
ggsave(pl, filename = 'Acetate_spline_PerBMI.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Acetate_spline_PerBMI.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Acetate_spline_PerBMI.jpg', width = 7, height = 5, device = 'jpeg')

##### Plots repeated for pH #####
##### Plot met juiste mooie lay out voor artikel pH pooled #####
pl <- ggplot(data=d_longPH, aes(x = time, y = pH))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-90, -60, -30, 0, 30, 60))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='pH response upon infusion', y='pH')+
  theme_Publication()+
  annotate("text", x=5, y=7.42, label = "p = 0.5159", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

getwd()
ggsave(pl, filename = 'PH_spline_Pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'PH_spline_Pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'PH_spline_Pooled.jpg', width = 7, height = 5, device = 'jpeg')

# Lean group #
d_longPH_lean <- d_longPH %>% filter(BMI_class == 'Lean')
d_longPH_metsyn <- d_longPH %>% filter(BMI_class == 'MetSyn')

pl <- ggplot(data=d_longPH_lean, aes(x = time, y = pH))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-90, -60, -30, 0, 30, 60))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='pH response upon infusion Lean group', y='pH')+
  theme_Publication()+
  annotate("text", x=5, y=7.42, label = "p = 0.3602", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

getwd()
ggsave(pl, filename = 'PH_spline_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'PH_spline_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'PH_spline_Lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group #
pl <- ggplot(data=d_longPH_metsyn, aes(x = time, y = pH))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-90, -60, -30, 0, 30, 60))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='pH response upon infusion MetSyn group', y='pH')+
  theme_Publication()+
  annotate("text", x=5, y=7.42, label = "p = 0.2683", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

getwd()
ggsave(pl, filename = 'PH_spline_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'PH_spline_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'PH_spline_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

##### Plots repeated for triglycerides #####
##### Plot met juiste mooie lay out voor artikel Acetate pooled #####
d_longLipids$Group <- factor(d_longLipids$Group)
d_longLipids$time <- as.numeric(d_longLipids$time)

pl <- ggplot(data=d_longLipids, aes(x = time, y = Triglyceride))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 30, 60, 90, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Triglyceride response upon SMMT', y='Triglyeride (mmol/L)')+
  theme_Publication()+
  #annotate("text", x=5, y=300, label = "p < 0.0001", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

getwd()
ggsave(pl, filename = 'Triglyceride_spline_Pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Triglyceride_spline_Pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Triglyceride_spline_Pooled.jpg', width = 7, height = 5, device = 'jpeg')

# Lean #
d_longLipids_lean <- d_longLipids %>% filter(BMI_class=='Lean')
pl <- ggplot(data=d_longLipids_lean, aes(x = time, y = Triglyceride))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 30, 60, 90, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Triglyceride response upon SMMT Lean Group', y='Triglyeride (mmol/L)')+
  theme_Publication()+
  #annotate("text", x=5, y=300, label = "p < 0.0001", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

ggsave(pl, filename = 'Triglyceride_spline_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Triglyceride_spline_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Triglyceride_spline_Lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn #
d_longLipids_metsyn <- d_longLipids %>% filter(BMI_class=='MetSyn')

pl <- ggplot(data=d_longLipids_metsyn, aes(x = time, y = Triglyceride))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 30, 60, 90, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Triglyceride response upon SMMT MetSyn Group', y='Triglyeride (mmol/L)')+
  theme_Publication()+
  #annotate("text", x=5, y=300, label = "p < 0.0001", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

ggsave(pl, filename = 'Triglyceride_spline_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Triglyceride_spline_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Triglyceride_spline_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')


##### Plots repeated for acetate COMBINED WITH INSULIN #####
##### Plot met juiste mooie lay out voor artikel Acetate pooled #####
d_longAI1 <- d_longAI %>% filter(Condition == 'B') %>% filter(!time %in% c('-30', '-90'))
d_longAI1 <- d_longAI %>% filter(Condition == 'A') %>% filter(!time %in% c('-30', '-90'))

d_longAI2 <- d_longAI %>% filter(Condition == 'B')
d_longAI2 <- d_longAI %>% filter(Condition == 'A')

scl = 1

# Pooled #
pl <- ggplot()+
  geom_smooth(data = subset(d_longAI2, !is.na(Acetate)), aes(x = time, y = Acetate, colour = "Acetate"), method = "loess", level=0.68)+
  geom_smooth(data = subset(d_longAI1, !is.na(Insulin)), aes(x = time, y = Insulin, colour = "Insulin"), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-90, -30, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Insulin (pmol/L)", labels = waiver()))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Acetate and Insulin upon SMMT Acetate', y='Acetate (uM)')+
  theme_Publication()+
  annotate("text", x=5, y=300, label = "p < 0.0001", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),
        axis.text.y.right=element_text(colour=pal_nejm()(2)[1]),
        axis.ticks.y.right=element_line(colour=pal_nejm()(2)[1]),
        axis.title.y.right=element_text(colour="black"),
        axis.text.y=element_text(colour=pal_nejm()(2)[2]),
        axis.ticks.y=element_line(colour=pal_nejm()(2)[2]),
        axis.title.y=element_text(colour="black"))
pl

getwd()
ggsave(pl, filename = 'AI_spline_ConditionB.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'AI_spline_ConditionB.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'AI_spline_ConditionB.jpg', width = 7, height = 5, device = 'jpeg')

ggsave(pl, filename = 'AI_spline_ConditionA.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'AI_spline_ConditionA.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'AI_spline_ConditionA.jpg', width = 7, height = 5, device = 'jpeg')

# Lean #
d_longAI1 <- d_longAI1 %>% filter(BMI_class == 'Lean')
d_longAI2 <- d_longAI2 %>% filter(BMI_class == 'Lean')

pl <- ggplot()+
  geom_smooth(data = subset(d_longAI2, !is.na(Acetate)), aes(x = time, y = Acetate, colour = "Acetate"), method = "loess", level=0.68)+
  geom_smooth(data = subset(d_longAI1, !is.na(Insulin)), aes(x = time, y = Insulin, colour = "Insulin"), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-90, -30, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Insulin (pmol/L)", labels = waiver()))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Acetate and Insulin upon SMMT Acetate Lean group', y='Acetate (uM)')+
  theme_Publication()+
  annotate("text", x=5, y=300, label = "p < 0.0001", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),
        axis.text.y.right=element_text(colour=pal_nejm()(2)[1]),
        axis.ticks.y.right=element_line(colour=pal_nejm()(2)[1]),
        axis.title.y.right=element_text(colour="black"),
        axis.text.y=element_text(colour=pal_nejm()(2)[2]),
        axis.ticks.y=element_line(colour=pal_nejm()(2)[2]),
        axis.title.y=element_text(colour="black"))
pl

getwd()
ggsave(pl, filename = 'AI_spline_ConditionA_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'AI_spline_ConditionA_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'AI_spline_ConditionA_Lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn #
# Data opnieuw ingeladen #
d_longAI <- rio :: import('d_longAI_NoOutlier.xlsx')

d_longAI1 <- d_longAI %>% filter(Condition == 'A') %>% filter(!time %in% c('-30', '-90'))
d_longAI2 <- d_longAI %>% filter(Condition == 'A')

d_longAI1 <- d_longAI1 %>% filter(BMI_class == 'MetSyn')
d_longAI2 <- d_longAI2 %>% filter(BMI_class == 'MetSyn')

pl <- ggplot()+
  geom_smooth(data = subset(d_longAI2, !is.na(Acetate)), aes(x = time, y = Acetate, colour = "Acetate"), method = "loess", level=0.68)+
  geom_smooth(data = subset(d_longAI1, !is.na(Insulin)), aes(x = time, y = Insulin, colour = "Insulin"), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-90, -30, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Insulin (pmol/L)", labels = waiver()))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='Acetate and Insulin upon SMMT Acetate MetSyn group', y='Acetate (uM)')+
  theme_Publication()+
  annotate("text", x=5, y=300, label = "p < 0.0001", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),
        axis.text.y.right=element_text(colour=pal_nejm()(2)[1]),
        axis.ticks.y.right=element_line(colour=pal_nejm()(2)[1]),
        axis.title.y.right=element_text(colour="black"),
        axis.text.y=element_text(colour=pal_nejm()(2)[2]),
        axis.ticks.y=element_line(colour=pal_nejm()(2)[2]),
        axis.title.y=element_text(colour="black"))
pl

getwd()
ggsave(pl, filename = 'AI_spline_ConditionA_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'AI_spline_ConditionA_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'AI_spline_ConditionA_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

##### Compare REE between conditions #####
# REE/day #
# Lean group #
comps <- list(c('Acetate', 'Placebo'))

d_long$Mean[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$REE_day[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$REE_day[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$REE_day[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$REE_day[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long$Mean_min_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$REE_day[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T) - std.error(d_long$REE_day[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$REE_day[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T) - std.error(d_long$REE_day[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$REE_day[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T) - std.error(d_long$REE_day[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$REE_day[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T) - std.error(d_long$REE_day[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long$Mean_plus_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$REE_day[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T) + std.error(d_long$REE_day[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$REE_day[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T) + std.error(d_long$REE_day[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$REE_day[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T) + std.error(d_long$REE_day[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$REE_day[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T) + std.error(d_long$REE_day[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long_lean <- d_long %>% filter(BMI_class=='Lean')
t.test(d_long_lean$REE_day ~ d_long_lean$Condition)
d_long_metsyn <- d_long %>% filter(BMI_class=='MetSyn')
t.test(d_long_metsyn$REE_day ~ d_long_metsyn$Condition)

pl1 <- ggplot(data=d_long, aes(x=Condition, y=Mean, group=BMI_class, colour=BMI_class))+
  geom_line(size = 1.4)+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = Mean_min_SE, ymax = Mean_plus_SE, col=BMI_class), size = 1.4, width = 0.12)+
  scale_x_discrete(breaks=c("Acetate", "Placebo"))+
  scale_color_manual(labels = c("Lean", "MetSyn"), values=c("#0072B5FF", "#20854EFF"))+
  labs(x='Condition', title='REE per condition', y='REE (kcal/day)', color = "Group")+
  theme_Publication()+
  annotate("text", x=2.33, y=1785, label = "p = 0.63", fontface = 2, size = 5)+
  annotate("text", x=2.33, y=2007, label = "p = 0.77", fontface = 2, size = 5)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = 'right')+
  theme(plot.title = element_text(size = 19, face = "bold"),
        axis.text=element_text(size=18),
        axis.title.x=element_text(size=16,face="bold"),
        axis.title.y=element_text(size=16,face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
pl1

d_long$Mean[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$VO2[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$VO2[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VO2[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VO2[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long$Mean_min_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$VO2[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T) - std.error(d_long$VO2[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$VO2[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T) - std.error(d_long$VO2[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VO2[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T) - std.error(d_long$VO2[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VO2[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T) - std.error(d_long$VO2[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long$Mean_plus_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$VO2[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T) + std.error(d_long$VO2[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$VO2[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T) + std.error(d_long$VO2[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VO2[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T) + std.error(d_long$VO2[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VO2[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T) + std.error(d_long$VO2[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long_lean <- d_long %>% filter(BMI_class=='Lean')
t.test(d_long_lean$VO2 ~ d_long_lean$Condition)
d_long_metsyn <- d_long %>% filter(BMI_class=='MetSyn')
t.test(d_long_metsyn$VO2 ~ d_long_metsyn$Condition)

pl2 <- ggplot(data=d_long, aes(x=Condition, y=Mean, group=BMI_class, colour=BMI_class))+
  geom_line(size = 1.4)+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = Mean_min_SE, ymax = Mean_plus_SE, col=BMI_class), size = 1.4, width = 0.12)+
  scale_x_discrete(breaks=c("Acetate", "Placebo"))+
  scale_color_manual(labels = c("Lean", "MetSyn"), values=c("#0072B5FF", "#20854EFF"))+
  labs(x='Condition', title='VO2 per condition', y='VO2 (L/min)', color = "Group")+
  theme_Publication()+
  annotate("text", x=2.33, y=0.2545, label = "p = 0.96", fontface = 2, size = 5)+
  annotate("text", x=2.33, y=0.2865, label = "p = 0.87", fontface = 2, size = 5)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = 'right')+
  theme(plot.title = element_text(size = 19, face = "bold"),
        axis.text=element_text(size=18),
        axis.title.x=element_text(size=16,face="bold"),
        axis.title.y=element_text(size=16,face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
pl2

d_long$Mean[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$VCO2[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$VCO2[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VCO2[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VCO2[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long$Mean_min_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$VCO2[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T) - std.error(d_long$VCO2[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$VCO2[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T) - std.error(d_long$VCO2[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VCO2[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T) - std.error(d_long$VCO2[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VCO2[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T) - std.error(d_long$VCO2[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long$Mean_plus_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$VCO2[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T) + std.error(d_long$VCO2[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$VCO2[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T) + std.error(d_long$VCO2[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VCO2[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T) + std.error(d_long$VCO2[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$VCO2[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T) + std.error(d_long$VCO2[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long_lean <- d_long %>% filter(BMI_class=='Lean')
t.test(d_long_lean$VCO2 ~ d_long_lean$Condition)
d_long_metsyn <- d_long %>% filter(BMI_class=='MetSyn')
t.test(d_long_metsyn$VCO2 ~ d_long_metsyn$Condition)
t.test(d_long$VCO2 ~ d_long$Condition, paired = T)

pl3 <- ggplot(data=d_long, aes(x=Condition, y=Mean, group=BMI_class, colour=BMI_class))+
  geom_line(size = 1.4)+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = Mean_min_SE, ymax = Mean_plus_SE, col=BMI_class), size = 1.4, width = 0.12)+
  scale_x_discrete(breaks=c("Acetate", "Placebo"))+
  scale_color_manual(labels = c("Lean", "MetSyn"), values=c("#0072B5FF", "#20854EFF"))+
  labs(x='Condition', title='VCO2 per condition', y='VCO2 (L/min)', color = "Group")+
  theme_Publication()+
  annotate("text", x=2.33, y=0.217, label = "p = 0.078", fontface = 2, size = 5)+
  annotate("text", x=2.33, y=0.244, label = "* p = 0.0056", fontface = 2, size = 5)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = 'right')+
  theme(plot.title = element_text(size = 19, face = "bold"),
        axis.text=element_text(size=18),
        axis.title.x=element_text(size=16,face="bold"),
        axis.title.y=element_text(size=16,face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
pl3

d_long$Mean[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$RQ[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$RQ[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$RQ[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$RQ[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long$Mean_min_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$RQ[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T) - std.error(d_long$RQ[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$RQ[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T) - std.error(d_long$RQ[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$RQ[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T) - std.error(d_long$RQ[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean_min_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$RQ[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T) - std.error(d_long$RQ[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long$Mean_plus_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'] <- mean(d_long$RQ[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T) + std.error(d_long$RQ[d_long$Condition=='Acetate' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'] <- mean(d_long$RQ[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T) + std.error(d_long$RQ[d_long$Condition=='Placebo' & d_long$BMI_class=='Lean'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'] <- mean(d_long$RQ[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T) + std.error(d_long$RQ[d_long$Condition=='Acetate' & d_long$BMI_class=='MetSyn'], na.rm = T)
d_long$Mean_plus_SE[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'] <- mean(d_long$RQ[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T) + std.error(d_long$RQ[d_long$Condition=='Placebo' & d_long$BMI_class=='MetSyn'], na.rm = T)

d_long_lean <- d_long %>% filter(BMI_class=='Lean')
t.test(d_long_lean$RQ ~ d_long_lean$Condition)
d_long_metsyn <- d_long %>% filter(BMI_class=='MetSyn')
t.test(d_long_metsyn$RQ ~ d_long_metsyn$Condition)

pl4 <- ggplot(data=d_long, aes(x=Condition, y=Mean, group=BMI_class, colour=BMI_class))+
  geom_line(size = 1.4)+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = Mean_min_SE, ymax = Mean_plus_SE, col=BMI_class), size = 1.4, width = 0.12)+
  scale_x_discrete(breaks=c("Acetate", "Placebo"))+
  scale_color_manual(labels = c("Lean", "MetSyn"), values=c("#0072B5FF", "#20854EFF"))+
  labs(x='Condition', title='RQ per condition', y='Respiratory Quotient', color = "Group")+
  theme_Publication()+
  annotate("text", x=2.33, y=0.853, label = "* p = 4.9e-05", fontface = 2, size = 5)+
  #annotate("text", x=2.33, y=0.84, label = "* p = 0.0028", fontface = 2, size = 5)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = 'right')+
  theme(plot.title = element_text(size = 19, face = "bold"),
        axis.text=element_text(size=18),
        axis.title.x=element_text(size=16,face="bold"),
        axis.title.y=element_text(size=16,face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
pl4

plots <- ggarrange(pl1, pl2, pl3, pl4, nrow = 2, ncol = 2, 
                   labels = c("A", "B", "C", "D"),
                   common.legend = T, legend = "bottom")
plots

plots <- annotate_figure(plots, top = text_grob("Resting Energy Expenditure measures",
                                                color = "black", face = "bold", size = 20))

ggsave(plots, filename = 'Metabolic_measures_line.pdf', width = 12, height = 9, device = 'pdf')
ggsave(plots, filename = 'Metabolic_measures_line.png', width = 12, height = 9, device = 'png')
ggsave(plots, filename = 'Metabolic_measures_line.jpg', width = 12, height = 9, device = 'jpeg')

pl <- ggplot(d_long_lean, aes(x=Condition, y=REE_day))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('REE (kcal/day)')+
  ggtitle('Resting Energy Expenditure per condition Lean Group')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'REE_day_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'REE_day_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'REE_day_lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group #
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=REE_day))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('REE (kcal/day)')+
  ggtitle('Resting Energy Expenditure per condition MetSyn Group')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'REE_day_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'REE_day_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'REE_day_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

# Pooled #
d_long1 <- d_long %>% filter(!Subject %in% c('Sub-10', 'Sub-11', 'Sub-32'))
pl1 <- ggplot(d_long1, aes(x=Condition, y=REE_day))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('REE (kcal/day)')+
  ggtitle('Resting Energy Expenditure per condition')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl1

pl1 + labs(color = "Condition")
pl1 <- pl1 + labs(color = "Condition")

ggsave(pl, filename = 'REE_day_pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'REE_day_pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'REE_day_pooled.jpg', width = 7, height = 5, device = 'jpeg')

# REE/kg #
# Lean group #
comps <- list(c('Acetate', 'Placebo'))

pl <- ggplot(d_long_lean, aes(x=Condition, y=REE_kg))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('REE (kcal/day/kg)')+
  ggtitle('Resting Energy Expenditure per condition Lean Group')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'REE_kg_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'REE_kg_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'REE_kg_lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group #
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=REE_kg))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('REE (kcal/day/kg)')+
  ggtitle('Resting Energy Expenditure per condition MetSyn Group')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'REE_kg_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'REE_kg_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'REE_kg_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

# Pooled #
pl <- ggplot(d_long, aes(x=Condition, y=REE_kg))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('REE (kcal/day/kg)')+
  ggtitle('Resting Energy Expenditure per condition pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'REE_kg_pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'REE_kg_pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'REE_kg_pooled.jpg', width = 7, height = 5, device = 'jpeg')

# VO2 #
# Lean group #
comps <- list(c('Acetate', 'Placebo'))

pl2 <- ggplot(d_long_lean, aes(x=Condition, y=VO2))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('VO2 (L/min)')+
  ggtitle('VO2 per condition Lean Group')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl2

pl2 + labs(color = "Condition")
pl2 <- pl2 + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'VO2_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'VO2_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'VO2_lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group #
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=VO2))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('VO2 (L/min)')+
  ggtitle('VO2 per condition MetSyn Group')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'VO2_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'VO2_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'VO2_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

# Pooled #
pl2 <- ggplot(d_long1, aes(x=Condition, y=VO2))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('VO2 (L/min)')+
  ggtitle('VO2 per condition')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl2

pl2 + labs(color = "Condition")
pl2 <- pl2 + labs(color = "Condition")

ggsave(pl, filename = 'VO2_pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'VO2_pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'VO2_pooled.jpg', width = 7, height = 5, device = 'jpeg')

# VCO2 #
# Lean group #
comps <- list(c('Acetate', 'Placebo'))

pl <- ggplot(d_long_lean, aes(x=Condition, y=VCO2))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('VCO2 (L/min)')+
  ggtitle('VCO2 per condition Lean Group')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'VCO2_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'VCO2_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'VCO2_lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group #
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=VCO2))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('VCO2 (L/min)')+
  ggtitle('VCO2 per condition MetSyn Group')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'VCO2_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'VCO2_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'VCO2_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

# Pooled #
pl3 <- ggplot(d_long, aes(x=Condition, y=VCO2))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('VCO2 (L/min)')+
  ggtitle('VCO2 per condition')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl3

pl3 + labs(color = "Condition")
pl3 <- pl3 + labs(color = "Condition")

ggsave(pl, filename = 'VCO2_pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'VCO2_pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'VCO2_pooled.jpg', width = 7, height = 5, device = 'jpeg')

# RQ #
# Lean group #
comps <- list(c('Acetate', 'Placebo'))

pl <- ggplot(d_long_lean, aes(x=Condition, y=RQ))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Respiratory Quoefficient')+
  ggtitle('RQ condition Lean Group')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'RQ_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'RQ_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'RQ_lean.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group #
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=RQ))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  ylab('Respiratory Quoefficient')+
  ggtitle('RQ per condition MetSyn Group')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'RQ_metsyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'RQ_metsyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'RQ_metsyn.jpg', width = 7, height = 5, device = 'jpeg')

# Pooled #
pl4 <- ggplot(d_long1, aes(x=Condition, y=RQ))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('Respiratory Quoefficient')+
  ggtitle('RQ per condition')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl4

pl4 + labs(color = "Condition")
pl4 <- pl4 + labs(color = "Condition")

ggsave(pl, filename = 'RQ_pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'RQ_pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'RQ_pooled.jpg', width = 7, height = 5, device = 'jpeg')

plots <- ggarrange(pl1, pl2, pl3, pl4,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
plots

ggsave(plots, filename = 'Metabolic_measures.pdf', width = 9, height = 9, device = 'pdf')
ggsave(plots, filename = 'Metabolic_measures.png', width = 9, height = 9, device = 'png')
ggsave(plots, filename = 'Metabolic_measures.jpg', width = 9, height = 9, device = 'jpeg')

##### LMM lean group #####
# doesn't work with missing values, remove them
wo_na <- d_longGlucose_lean %>% filter(Glucose != 'NA')
wo_na <- d_longGlucose %>% filter(Glucose != 'NA') %>% filter(Condition == 'Placebo')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

d_longGlucose1 <- d_longGlucose %>% filter(Condition=='Placebo')
wo_na <- d_longGlucose1 %>% filter(Glucose != 'NA')

# model 0 -> only Time (spline)
m0 = lme(Glucose ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Acetate group
m1 = lme(Glucose ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is not significant

# model 2 -> Time (spline), Acetate group, and interaction between Time (spline) & Acetate group
m2 = lme(Glucose ~ ns(time, df = 3) * Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time group interaction is not significant, sadly

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(Glucose ~ ns(time, df = 3) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # still, only time is significant

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(Glucose ~ ns(time, df = 3) * Condition * Group + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4)

##### LMM metsyn group #####
# doesn't work with missing values, remove them
wo_na <- d_longGlucose_metsyn %>% filter(Glucose != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(Glucose ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Condition
m1 = lme(Glucose ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is trending: 0.0639

# model 2 -> Time (spline), Condition, and interaction between Time (spline) & Acetate group
m2 = lme(Glucose ~ ns(time, df = 3) * Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time condition interaction is ns: 0.9143

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(Glucose ~ ns(time, df = 3) * Condition * Group + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # time condition interaction is ns: 0.9154

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(Glucose ~ ns(time, df = 3) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # time condition interaction is ns: 0.9159

##### LMM pooled #####
##### do normal glucose analyses like above #####
# doesn't work with missing values, remove them
wo_na <- d_longGlucose %>% filter(Glucose != 'NA')
wo_na <- d_longGlucose_lean %>% filter(Glucose != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(Glucose ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Condition
m1 = lme(Glucose ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is ns: 0.0953

# model 2 -> Time (spline), Condition, and interaction between Time (spline) & Acetate group
m2 = lme(Glucose ~ ns(time, df = 3) * Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time condition interaction is ns: 0.3525

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(Glucose ~ ns(time, df = 3) * Condition * BMI_class + Group, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # time condition interaction is ns: 0.3532

intervals(m3, level = 0.95, which = "fixed")

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(Glucose ~ ns(time, df = 3) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # time condition interaction is ns: 0.3546

##### Model with iAUC instead of glucose #####
data_long$Group <- data$Group[match(data_long$Subject, data$Subject)]
data_long$BMI_class <- data$BMI_class[match(data_long$Subject, data$Subject)]

# doesn't work with missing values, remove them
# model 0 -> only Time (spline)
wo_na <- data_long %>% filter(`iAUC Glucose` != 'NA')
wo_na <- as.data.frame(wo_na)
colnames(wo_na) <- c('Subject', 'Condition', 'iAUC', 'Group', 'BMI_class')
wo_na$iAUC
library(lmerTest)
m0 = lme(iAUC ~ Condition * Group * BMI_class, random = ~ 1 | Subject, data = wo_na, method = 'ML')

wo_na$Subject <- as.factor(wo_na$Subject)
# m0 = lmer(iAUC ~ Condition * Group * BMI_class || Subject, data = wo_na) Does not work

##### Plots smoothed voor GLP1 #####
##### Plot met juiste mooie lay out voor artikel Glucose lean groep #####
pl <- ggplot(data=d_longGLP, aes(x = time, y = GLP1))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 30, 60, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='GLP1 response upon SMMT lean group', y='GLP1 (pg/ml)')+
  theme_Publication()+
  annotate("text", x=75, y=7.1, label = "p = 0.9108", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

ggsave(pl, filename = 'GLP1_spline_lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'GLP1_spline_lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'GLP1_spline_lean.jpg', width = 7, height = 5, device = 'jpeg')

##### Plot met juiste mooie lay out voor artikel Glucose metsyn groep #####
pl <- ggplot(data=d_longGLP_metsyn, aes(x = time, y = GLP1))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 30, 60, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='GLP1 response upon SMMT MetSyn group', y='GLP1 (pg/ml)')+
  theme_Publication()+
  annotate("text", x=75, y=15, label = "p = 0.5390", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

ggsave(pl, filename = 'GLP1_spline_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'GLP1_spline_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'GLP1_spline_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

##### Plot met juiste mooie lay out voor artikel Glucose pooled #####
pl <- ggplot(data=d_longGLP, aes(x = time, y = GLP1))+
  geom_smooth(aes(group = Condition, col = Condition), method = "loess", level=0.68)+
  scale_x_continuous(breaks=c(-15, 30, 60, 120))+
  scale_color_manual(values = c(pal_nejm()(2)[2],pal_nejm()(2)[1]))+
  labs(x='Time (min)', title='GLP1 response upon SMMT MetSyn group', y='GLP1 (pg/ml)')+
  theme_Publication()+
  annotate("text", x=75, y=15, label = "p = 0.706", fontface = 2)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))
pl

ggsave(pl, filename = 'GLP1_spline_Pooled.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'GLP1_spline_Pooled.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'GLP1_spline_Pooled.jpg', width = 7, height = 5, device = 'jpeg')


##### LMM lean group #####
# doesn't work with missing values, remove them
wo_na <- d_longGLP_lean %>% filter(GLP1 != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(GLP1 ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Acetate group
m1 = lme(GLP1 ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is not significant

# model 2 -> Time (spline), Acetate group, and interaction between Time (spline) & Acetate group
m2 = lme(GLP1 ~ ns(time, df = 3) * Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time group interaction is not significant, sadly

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(GLP1 ~ ns(time, df = 3) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # still, only time is significant

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(GLP1 ~ ns(time, df = 3) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # ns

##### LMM metsyn group #####
# doesn't work with missing values, remove them
wo_na <- d_longGLP_metsyn %>% filter(GLP1 != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(GLP1 ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Condition
m1 = lme(GLP1 ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is ns: 0.2089

# model 2 -> Time (spline), Condition, and interaction between Time (spline) & Acetate group
m2 = lme(GLP1 ~ ns(time, df = 3) * Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time condition interaction is ns: 0.5390

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(GLP1 ~ ns(time, df = 3) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # time condition interaction is ns: 0.5474

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(GLP1 ~ ns(time, df = 3) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # time condition interaction is ns: 0.5410

##### LMM pooled #####
# doesn't work with missing values, remove them
wo_na <- d_longGLP %>% filter(GLP1 != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(GLP1 ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Acetate group
m1 = lme(GLP1 ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is not significant: 0.4696

# model 2 -> Time (spline), Acetate group, and interaction between Time (spline) & Acetate group
m2 = lme(GLP1 ~ ns(time, df = 3) * Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time group interaction is not significant, sadly

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(GLP1 ~ ns(time, df = 3) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # still, only time is significant

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(GLP1 ~ ns(time, df = 3) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # ns

##### Plot VAS hunger per groep #####
# Lean group #
d_longVAS_lean <- d_longVAS %>% filter(BMI_class=='Lean')

pl1 <- ggplot(data=d_longVAS_lean, aes(x = time, y = VAS_hunger))+
  geom_point(aes(x=time, y=Mean,
                 col=Condition), size = 3)+
  geom_line(aes(x=time, y=Mean, col=Condition), size = 1)+
  geom_errorbar(aes(ymin = Mean_min_SE, ymax = Mean_plus_SE, col=Condition), size = 0.2, width = 0.05)+
  scale_x_continuous(breaks=c(1, 2, 3))+
  scale_color_manual(labels = c("A", "B"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  labs(x='Timepoint', title='Lean group', y='VAS hunger score')+
  theme_Publication()+
  annotate("text", x=2.3, y=7.1, label = "p = 0.3207", fontface = 2)+
  ylim(0, 10)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = 'right')+
  theme(axis.text=element_text(size=20),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=20,face="bold"))
pl1

ggsave(pl1, filename = 'VAS_hunger_lean_SE.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl1, filename = 'VAS_hunger_lean_SE.png', width = 7, height = 5, device = 'png')
ggsave(pl1, filename = 'VAS_hunger_lean_SE.jpg', width = 7, height = 5, device = 'jpeg')

# MetSyn group #
d_longVAS_metsyn <- d_longVAS %>% filter(BMI_class=='MetSyn')

pl2 <- ggplot(data=d_longVAS_metsyn, aes(x = time, y = VAS_hunger))+
  geom_point(aes(x=time, y=Mean,
                 col=Condition), size = 3)+
  geom_line(aes(x=time, y=Mean, col=Condition), size = 1)+
  geom_errorbar(aes(ymin = Mean_min_SE, ymax = Mean_plus_SE, col=Condition), size = 0.2, width = 0.05)+
  scale_x_continuous(breaks=c(1, 2, 3))+
  scale_color_manual(labels = c("A", "B"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  labs(x='Timepoint', title='MetSyn group', y = "")+
  theme_Publication()+
  annotate("text", x=2.3, y=7.1, label = "p = 0.1153", fontface = 2)+
  ylim(0, 10)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = 'right')+
  theme(axis.text=element_text(size=20),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=20,face="bold"))
pl2

ggsave(pl2, filename = 'VAS_hunger_metsyn_SE.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl2, filename = 'VAS_hunger_metsyn_SE.png', width = 7, height = 5, device = 'png')
ggsave(pl2, filename = 'VAS_hunger_metsyn_SE.jpg', width = 7, height = 5, device = 'jpeg')

plots <- ggarrange(pl1, pl2,
                   labels = c("A", "B"), 
                   ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
plots

plots <- annotate_figure(plots, top = text_grob("VAS hunger score upon SMMT per BMI group",
                                                color = "black", face = "bold", size = 20))

ggsave(plots, filename = 'VAS_hunger_combined_SE.pdf', width = 8, height = 4, device = 'pdf')
ggsave(plots, filename = 'VAS_hunger_combined_SE.png', width = 8, height = 4, device = 'png')
ggsave(plots, filename = 'VAS_hunger_combined_SE.jpg', width = 8, height = 4, device = 'jpeg')
getwd()

# Pooled #
pl <- ggplot(data=d_longVAS, aes(x = time, y = VAS_hunger))+
  geom_point(aes(x=time, y=Mean,
                 col=Condition), size = 3)+
  geom_line(aes(x=time, y=Mean, col=Condition), size = 1)+
  geom_errorbar(aes(ymin = Mean_min_SE, ymax = Mean_plus_SE, col=Condition), size = 0.2, width = 0.05)+
  scale_x_continuous(breaks=c(1, 2, 3))+
  scale_color_manual(labels = c("A", "B"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  labs(x='Timepoint', title='VAS hunger score upon SMMT and MRI pooled', y='VAS hunger score')+
  theme_Publication()+
  annotate("text", x=2.3, y=7.1, label = "p = 0.8263", fontface = 2)+
  ylim(0, 10)+
  theme(panel.grid.major.x = element_line(size = 0.75, color='grey'))+
  theme(legend.position = 'right')
pl

ggsave(pl, filename = 'VAS_hunger_pooled_SE.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'VAS_hunger_pooled_SE.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'VAS_hunger_pooled_SE.jpg', width = 7, height = 5, device = 'jpeg')

# doesn't work with missing values, remove them
d_longVAS_lean <- d_longVAS %>% filter(BMI_class=='Lean')

wo_na <- d_longVAS_lean %>% filter(VAS_hunger != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(VAS_hunger ~ ns(time, df = 2), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is slightly significant: 0.0366

# model 1 -> Time (spline) and Condition
m1 = lme(VAS_hunger ~ ns(time, df = 2) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is sign: 0.0217

# model 2 -> Time (spline), Condition, and interaction between Time (spline) & Acetate group
m2 = lme(VAS_hunger ~ ns(time, df = 2) * Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time condition interaction is ns: 0.3207

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(VAS_hunger ~ ns(time, df = 2) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # time condition interaction is ns: 0.3298

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(VAS_hunger ~ ns(time, df = 2) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # time condition interaction is ns: 0.2856

##### LMM metsyn group #####
# doesn't work with missing values, remove them
d_longVAS_metsyn <- d_longVAS %>% filter(BMI_class=='MetSyn')
wo_na <- d_longVAS_metsyn %>% filter(VAS_hunger != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(VAS_hunger ~ ns(time, df = 2), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant: 0.0091

# model 1 -> Time (spline) and Condition
m1 = lme(VAS_hunger ~ ns(time, df = 2) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is trending: 0.054

# model 2 -> Time (spline), Condition, and interaction between Time (spline) & Acetate group
m2 = lme(VAS_hunger ~ ns(time, df = 2) * Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time condition interaction is ns: 0.1153

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(VAS_hunger ~ ns(time, df = 2) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # time condition interaction is ns: 0.1199

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(VAS_hunger ~ ns(time, df = 2) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # time condition interaction is ns: 0.1428

# Op verzoek van Richard: VAS per tijdpunt vergeleken #
# GROUPG plot #
comps <- list(c('A', 'B'))

pl <- ggplot(try, aes(x=Condition, y=VAS_hunger))+
  geom_point(aes(col=Condition), size=1.5, show.legend = FALSE)+
  geom_line(aes(col='grey', group=Subject), size=1, show.legend = FALSE)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, method = "t.test", paired = T)+
  xlab('Condition')+
  ylab('VAS hunger score')+
  ggtitle('VAS hunger score per condition after MRI - Lean Group')+
  scale_color_manual(values=c("#0072B599", "#20854E99", "grey"))+
  scale_fill_manual(values=c("#0072B599", "#20854E99", "grey"))
pl

##### LMM pooled #####
# doesn't work with missing values, remove them
wo_na <- d_longVAS %>% filter(VAS_hunger != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(VAS_hunger ~ ns(time, df = 2), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant.

# model 1 -> Time (spline) and Condition
m1 = lme(VAS_hunger ~ ns(time, df = 2) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is sign: 0.0026

# model 2 -> Time (spline), Condition, and interaction between Time (spline) & Acetate group
m2 = lme(VAS_hunger ~ ns(time, df = 2) * Condition * BMI_class, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time condition interaction is ns: 0.8263

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(VAS_hunger ~ ns(time, df = 2) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # time condition interaction is ns: 0.8268

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(VAS_hunger ~ ns(time, df = 2) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # time condition interaction is ns: 0.8169

##### LMM pooled repeated for ACETATE #####
# doesn't work with missing values, remove them
wo_na <- d_longAcetate %>% filter(Acetate != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(Acetate ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant <0.0001

# model 1 -> Time (spline) and Condition
m1 = lme(Acetate ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is ns: 0.0953

# model 2 -> Time (spline), Condition, and interaction between Time (spline) & Acetate group
m2 = lme(Acetate ~ ns(time, df = 3) * Condition * BMI_class, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time condition interaction is ns: 0.3525

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(Acetate ~ ns(time, df = 3) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # time condition interaction is ns: 0.3532

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(Acetate ~ ns(time, df = 3) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # time condition interaction is ns: 0.3546

##### LMM to check difference acetate over time between groups
##### Only in acetate condition 
wo_na <- d_longAcetate %>% filter(Acetate != 'NA') %>% filter(Condition == 'Acetate')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(Acetate ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant <0.0001

# model 1 -> Time (spline) and Condition
m1 = lme(Acetate ~ ns(time, df = 3) + BMI_class, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # BMI_class is ns: 0.8856

##### LMM pooled repeated for PH #####
# doesn't work with missing values, remove them
wo_na <- d_longPH %>% filter(pH != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(pH ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Condition
m1 = lme(pH ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is ns: 0.2061

# model 2 -> Time (spline), Condition, and interaction between Time (spline) & Acetate group
m2 = lme(pH ~ ns(time, df = 3) * Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time condition interaction is ns: 0.6250

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(pH ~ ns(time, df = 3) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # time condition interaction is ns: 0.6426

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(pH ~ ns(time, df = 3) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # time condition interaction is ns: 0.6430

# Lean group #
wo_na <- d_longPH_lean %>% filter(pH != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(pH ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant 0.0012

# model 1 -> Time (spline) and Condition
m1 = lme(pH ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is ns: 0.5243

# model 2 -> Time (spline), Condition, and interaction between Time (spline) & Acetate group
m2 = lme(pH ~ ns(time, df = 3) * Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time condition interaction is ns: 0.2873

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(pH ~ ns(time, df = 3) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # time condition interaction is ns: 0.2798

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(pH ~ ns(time, df = 3) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # time condition interaction is ns: 0.2770

# MetSyn group #
wo_na <- d_longPH_metsyn %>% filter(pH != 'NA')
wo_na <- as.data.frame(wo_na) # doesn't work with tibble, make into DF

# model 0 -> only Time (spline)
m0 = lme(pH ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Condition
m1 = lme(pH ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is ns: 0.2683

# model 2 -> Time (spline), Condition, and interaction between Time (spline) & Acetate group
m2 = lme(pH ~ ns(time, df = 3) * Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time condition interaction is ns: 0.0991

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(pH ~ ns(time, df = 3) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # time condition interaction is ns: 0.1049

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(pH ~ ns(time, df = 3) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4) # time condition interaction is ns: 0.1074

##### Repeat everything for INCREMENTAL area under the curve #####
# PLOT glucose per time for each individual SEPARATELY lean
d_longGlucose_lean$time <- as.numeric(d_longGlucose_lean$time)

pl <- ggplot(data=d_longGlucose_lean, aes(x=time,y=Glucose, color=Condition)) + 
  geom_point(size=2) +
  xlab("time (min)") + 
  ylab("Glucose (mmol/L)")+
  geom_line(aes(group=Condition, color=Condition))+
  theme_Publication()+ 
  geom_line() + 
  facet_wrap(~Subject, nrow = 6)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(labels = c("A", "B"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  ggtitle("Glucose per subject lean group")
pl 

ggsave(pl, filename = 'Glucose_per_sub_lean.pdf', width = 12, height = 10, device = 'pdf')
ggsave(pl, filename = 'Glucose_per_sub_lean.png', width = 12, height = 10, device = 'png')
ggsave(pl, filename = 'Glucose_per_sub_lean.jpg', width = 12, height = 10, device = 'jpeg')

# PLOT insulin per time for each indiviual SEPARATELY lean
d_longGlucose_lean$time <- as.numeric(d_longGlucose_lean$time)

pl <- ggplot(data=d_longGlucose_lean, aes(x=time,y=Insulin, color=Condition)) + 
  geom_point(size=2) +
  xlab("time (min)") + 
  ylab("Insulin (pmol/L)")+
  geom_line(aes(group=Condition, color=Condition))+
  theme_Publication()+ 
  geom_line() + 
  facet_wrap(~Subject, nrow = 6)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(labels = c("A", "B"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  ggtitle("Insulin per subject lean group")
pl 

ggsave(pl, filename = 'Insulin_per_sub_lean.pdf', width = 12, height = 10, device = 'pdf')
ggsave(pl, filename = 'Insulin_per_sub_lean.png', width = 12, height = 10, device = 'png')
ggsave(pl, filename = 'Insulin_per_sub_lean.jpg', width = 12, height = 10, device = 'jpeg')

# PLOT insulin per time for each indiviual SEPARATELY metsyn
d_longGlucose_metsyn$time <- as.numeric(d_longGlucose_metsyn$time)

pl <- ggplot(data=d_longGlucose_metsyn, aes(x=time,y=Insulin, color=Condition)) + 
  geom_point(size=2) +
  xlab("time (min)") + 
  ylab("Insulin (pmol/L)")+
  geom_line(aes(group=Condition, color=Condition))+
  theme_Publication()+ 
  geom_line() + 
  facet_wrap(~Subject, nrow = 6)+
  scale_x_continuous(breaks=c(-15, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(labels = c("A", "B"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  ggtitle("Insulin per subject MetSyn group")
pl 

ggsave(pl, filename = 'Insulin_per_sub_metsyn.pdf', width = 12, height = 10, device = 'pdf')
ggsave(pl, filename = 'Insulin_per_sub_metsyn.png', width = 12, height = 10, device = 'png')
ggsave(pl, filename = 'Insulin_per_sub_metsyn.jpg', width = 12, height = 10, device = 'jpeg')

# PLOT insulin per time for each indiviual SEPARATELY ONLY ONES WITH KNOWN ACETATE
d_longGlucose$time <- as.numeric(d_longGlucose$time)
d_longGlucose2 <- d_longGlucose %>% filter(Subject %in% c('Sub-02','Sub-03', 'Sub-04','Sub-05','Sub-06','Sub-07','Sub-08', 'Sub-43', 'Sub-47'))
d_longGlucose3 <- d_longGlucose %>% filter(Subject %in% c('Sub-02'))


##### Create -0.25h as timepoint for Glucose baseline sample #####
d_longGlucose$time[d_longGlucose$time =='-15'] <- -90
d_longGlucose$time
d_longGlucose$time <- as.numeric(d_longGlucose$time)

pl <- ggplot(data=d_longGlucose3, aes(x=time,y=Insulin, color=Condition)) + 
  geom_point(size=2) +
  xlab("time (min)") + 
  ylab("Insulin (pmol/L)")+
  geom_line(aes(group=Condition, color=Condition))+
  theme_Publication()+ 
  geom_line(size = 1) + 
  facet_wrap(~Subject, nrow = 1)+
  scale_x_continuous(breaks=c(-90, 0, 10, 15, 20, 30, 60, 90, 120))+
  scale_color_manual(labels = c("A", "B"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  ggtitle("Insulin per subject")
pl 

ggsave(pl, filename = 'Insulin_per_sub.pdf', width = 12, height = 10, device = 'pdf')
ggsave(pl, filename = 'Insulin_per_sub.png', width = 12, height = 10, device = 'png')
ggsave(pl, filename = 'Insulin_per_sub.jpg', width = 12, height = 10, device = 'jpeg')

# PLOT GLP-1 per time for each indiviual SEPARATELY metsyn
d_longGLP_metsyn$time <- as.numeric(d_longGLP_metsyn$time)

pl <- ggplot(data=d_longGLP_metsyn, aes(x=time, y=GLP1, color=Condition)) + 
  geom_point(size=2) +
  xlab("time (min)") + 
  ylab("GLP-1 (pg/ml)")+
  geom_line(aes(group=Condition, color=Condition))+
  theme_Publication()+ 
  geom_line() + 
  facet_wrap(~Subject, nrow = 6)+
  scale_x_continuous(breaks=c(-15, 30, 60, 120))+
  scale_color_manual(labels = c("A", "B"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  ggtitle("GLP-1 per subject MetSyn group")
pl 

ggsave(pl, filename = 'GLP1_per_sub_metsyn.pdf', width = 12, height = 10, device = 'pdf')
ggsave(pl, filename = 'GLP1_per_sub_metsyn.png', width = 12, height = 10, device = 'png')
ggsave(pl, filename = 'GLP1_per_sub_metsyn.jpg', width = 12, height = 10, device = 'jpeg')

# PLOT GLP-1 per time for each indiviual SEPARATELY lean
d_longGLP_lean$time <- as.numeric(d_longGLP_lean$time)

pl <- ggplot(data=d_longGLP_lean, aes(x=time, y=GLP1, color=Condition)) + 
  geom_point(size=2) +
  xlab("time (min)") + 
  ylab("GLP-1 (pg/ml)")+
  geom_line(aes(group=Condition, color=Condition))+
  theme_Publication()+ 
  geom_line()+ 
  facet_wrap(~Subject, nrow = 6)+
  scale_x_continuous(breaks=c(-15, 30, 60, 120))+
  scale_color_manual(labels = c("A", "B"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  ggtitle("GLP-1 per subject lean group")
pl 

ggsave(pl, filename = 'GLP1_per_sub_lean.pdf', width = 12, height = 10, device = 'pdf')
ggsave(pl, filename = 'GLP1_per_sub_lean.png', width = 12, height = 10, device = 'png')
ggsave(pl, filename = 'GLP1_per_sub_lean.jpg', width = 12, height = 10, device = 'jpeg')

# PLOT Acetate per time for each indiviual SEPARATELY lean
d_longAI1 <- d_longAI %>% filter(!Subject %in% c('Sub-09','Sub-10', 'Sub-14','Sub-11','Sub-41','Sub-40','Sub-42', 'Sub-36', 'Sub-45', 'Sub-44', 'Sub-48', 'Sub-46')) %>% filter(Condition %in% c('A'))
d_longAcetate1 <- d_longAcetate %>% filter(!Subject %in% c('Sub-09','Sub-10', 'Sub-14','Sub-11','Sub-41','Sub-40','Sub-42', 'Sub-36', 'Sub-45', 'Sub-44', 'Sub-48', 'Sub-46'))

pl <- ggplot(data=d_longAcetate, aes(x=time, y=Acetate, color=Condition)) + 
  geom_point(size=2) +
  xlab("time (min)") + 
  ylab("Acetate (uM)")+
  geom_line(aes(group=Condition, color=Condition))+
  theme_Publication()+ 
  geom_line(size = 1)+ 
  facet_wrap(~Subject, nrow = 5)+
  scale_x_continuous(breaks=c(-90, -30, 30, 60, 90, 120))+
  scale_color_manual(labels = c("Acetate", "Bicarbonate"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  ggtitle("Acetate per subject")
pl 

getwd()
ggsave(pl, filename = 'Acetate_per_sub_A_B.pdf', width = 12, height = 10, device = 'pdf')
ggsave(pl, filename = 'Acetate_per_sub_A_B.png', width = 12, height = 10, device = 'png')
ggsave(pl, filename = 'Acetate_per_sub_A_B.jpg', width = 12, height = 10, device = 'jpeg')

# PLOT pH per time for each indiviual SEPARATELY
pl <- ggplot(data=d_longPH, aes(x=time, y=pH, color=Condition)) + 
  geom_point(size=2) +
  xlab("time (min)") + 
  ylab("plasma pH")+
  geom_line(aes(group=Condition, color=Condition))+
  theme_Publication()+ 
  geom_line(size = 1)+ 
  facet_wrap(~Subject, nrow = 5)+
  scale_x_continuous(breaks=c(-90, -60, -30, 0, 30, 60))+
  scale_color_manual(labels = c("Acetate", "Placebo"), values = c(pal_nejm()(2)[2], pal_nejm()(2)[1]))+
  ggtitle("pH over time per subject")
pl 

getwd()
ggsave(pl, filename = 'PH_per_sub.pdf', width = 12, height = 10, device = 'pdf')
ggsave(pl, filename = 'PH_per_sub.png', width = 12, height = 10, device = 'png')
ggsave(pl, filename = 'PH_per_sub.jpg', width = 12, height = 10, device = 'jpeg')

##################### Aantekeningen Ids Madelief, gelukt #####
scl <- 1

pl <- ggplot() + 
  geom_line(data = subset(d_longAI1, !is.na(Insulin)), size = 1, aes(x = time, y = Insulin, colour = "Insulin")) + 
  geom_line(data = subset(d_longAI1, !is.na(Acetate)), size = 1, aes(x = time, y = Acetate, colour = "Acetate")) +
  geom_point(data = subset(d_longAI1, !is.na(Insulin)), size = 2, aes(x = time, y = Insulin, colour = "Insulin")) +
  geom_point(data = subset(d_longAI1, !is.na(Acetate)), size = 2, aes(x = time, y = Acetate, colour = "Acetate")) +
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Acetate (umol/L)"))+ 
  ggtitle("Insulin and acetate plotted per subject") +
  facet_wrap(~Subject, nrow = 5) +
  theme_Publication()+
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),
        axis.text.y.right=element_text(colour=pal_nejm()(2)[2]),
        axis.ticks.y.right=element_line(colour=pal_nejm()(2)[2]),
        axis.title.y.right=element_text(colour="black"),
        axis.text.y=element_text(colour=pal_nejm()(2)[1]),
        axis.ticks.y=element_line(colour=pal_nejm()(2)[1]),
        axis.title.y=element_text(colour="black")) +
  scale_colour_manual(values=c(pal_nejm()(2)[2], pal_nejm()(2)[1], pal_nejm()(2)[2])) +
  labs(colour="")
pl

getwd()
ggsave(pl, filename = 'AI_per_sub_NoOutliers.pdf', width = 12, height = 10, device = 'pdf')
ggsave(pl, filename = 'AI_per_sub_NoOutliers.png', width = 12, height = 10, device = 'png')
ggsave(pl, filename = 'AI_per_sub_NoOutliers.jpg', width = 12, height = 10, device = 'jpeg')
##################### Aantekeningen Ids Madelief, gelukt #####

# doesn't work with missing values, remove them
wo_na <- d_longGlucose %>% filter(Glucose != 'NA')
wo_na <- as.data.frame(wo_na)
wo_na$time <- as.numeric(wo_na$time)
wo_na$time <- as.factor(wo_na$time)

# model 0 -> only Time (spline)
m0 = lme(Glucose ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Acetate group
m1 = lme(Glucose ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is not significant

# model 2 -> Time (spline), Acetate group, and interaction between Time (spline) & Acetate group
m2 = lme(Glucose ~ ns(time, df = 3) * Condition * Group * BMI_class, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time group interaction is not significant, sadly

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(Glucose ~ ns(time, df = 3) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # still, only time is significant

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(Glucose ~ ns(time, df = 3) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4)

##### Repeat for insulin #####
#wo_na <- d_longGlucose %>% filter(Glucose != 'NA')
wo_na <- d_longGlucose_lean %>% filter (Insulin != 'NA')
wo_na <- as.data.frame(wo_na)
wo_na$time <- as.numeric(wo_na$time)

# model 0 -> only Time (spline)
m0 = lme(Insulin ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Acetate group
m1 = lme(Insulin ~ ns(time, df = 3) + BMI_class, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is significant for the lean group

# model 2 -> Time (spline), Acetate group, and interaction between Time (spline) & Acetate group
m2 = lme(Insulin ~ ns(time, df = 3) * Condition * Group, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time group interaction is significant, but time:condition is not

# Repeat for metsyn group #
wo_na <- d_longGlucose_metsyn %>% filter (Insulin != 'NA')
wo_na <- as.data.frame(wo_na)
wo_na$time <- as.numeric(wo_na$time)

# model 0 -> only Time (spline)
m0 = lme(Insulin ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Acetate group
m1 = lme(Insulin ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is not significant for the metsyn group

# model 2 -> Time (spline), Acetate group, and interaction between Time (spline) & Acetate group
m2 = lme(Insulin ~ ns(time, df = 3) * Condition * Group, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time group interaction is significant, but time:condition is not

# Repeat for pooled #
wo_na <- d_longGlucose %>% filter (Insulin != 'NA')
wo_na <- as.data.frame(wo_na)
wo_na$time <- as.numeric(wo_na$time)

# model 0 -> only Time (spline)
m0 = lme(Insulin ~ ns(time, df = 3), random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m0) # as expected, time is super significant

# model 1 -> Time (spline) and Acetate group
m1 = lme(Insulin ~ ns(time, df = 3) + Condition, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m1) # condition is not significant for the metsyn group

# model 2 -> Time (spline), Acetate group, and interaction between Time (spline) & Acetate group
m2 = lme(Insulin ~ ns(time, df = 3) * Condition * Group, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m2) # time group interaction is significant, but time:condition is not

# model 3 -> model 2 + Gender & Age (as covariates)
m3 = lme(Insulin ~ ns(time, df = 3) * Condition + Sexe + Age, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3) # still, only time is significant

m3 = lme(Insulin ~ ns(time, df = 3) * Condition * BMI_class + Group, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m3)

# model 4 -> model 3 + interactions between Age and Time; and between Gender and Time
m4 = lme(Insulin ~ ns(time, df = 3) * Condition + Sexe + Age + Age:time + Sexe:time, random = ~ 1 | Subject, data = wo_na, method = 'ML')
anova(m4)

##### Opnieuw poging plaatje glucose Mark #####
pl <- ggplot(d_long, aes(color=Condition, y=iAUC_Glucose_0_120,x=interaction(BMI_class,Condition))) + 
  geom_boxplot(fatten = 1, lwd = 1) + 
  geom_jitter() + 
  geom_line(aes(group=Subject, color=NULL), size = 0.7) +
  ylab('iAUC Glucose 0 - 120 minutes') +
  facet_wrap(~Group) +
  scale_color_manual(values=c("#20854E99", "#0072B599")) +
  theme_Publication() +
  NULL
pl

ggsave(pl, filename = 'iAUC_Glucose_0_120_CombinedModel.pdf', width = 12, height = 10, device = 'pdf')
ggsave(pl, filename = 'iAUC_Glucose_0_120_CombinedModel.png', width = 12, height = 10, device = 'png')
ggsave(pl, filename = 'iAUC_Glucose_0_120_CombinedModel.jpg', width = 12, height = 10, device = 'jpg')

########## ROI ANALYSES ##########
# Contrast 1 FvsNF
t.test(d_long$FvsNF_amygdala ~ d_long$BMI_class) # 0.09
t.test(d_long$FvsNF_caudate ~ d_long$BMI_class) # 0.8
t.test(d_long$FvsNF_insula ~ d_long$BMI_class) # 0.03
t.test(d_long$FvsNF_putamen ~ d_long$BMI_class) # 0.2

t.test(d_long$HCvsNF_amygdala ~ d_long$BMI_class) # 0.08
t.test(d_long$HCvsNF_caudate ~ d_long$BMI_class) # 0.8
t.test(d_long$HCvsNF_insula ~ d_long$BMI_class) # 0.05
t.test(d_long$HCvsNF_putamen ~ d_long$BMI_class) # 0.25

t.test(d_long$HCvsLC_amygdala ~ d_long$BMI_class) # 0.75
t.test(d_long$HCvsLC_caudate ~ d_long$BMI_class) # 0.87
t.test(d_long$HCvsLC_insula ~ d_long$BMI_class) # 0.71
t.test(d_long$HCvsLC_putamen ~ d_long$BMI_class) # 0.96


t.test(d_long$FvsNF_amygdala[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) # 
t.test(d_long$FvsNF_caudate[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) # 
t.test(d_long$FvsNF_insula[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) # 
t.test(d_long$FvsNF_putamen[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) # 

t.test(d_long$HCvsNF_amygdala[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) # 
t.test(d_long$HCvsNF_caudate[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) # 
t.test(d_long$HCvsNF_insula[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) # 
t.test(d_long$HCvsNF_putamen[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) # 

t.test(d_long$HCvsLC_amygdala[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) # 
t.test(d_long$HCvsLC_caudate[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) # 
t.test(d_long$HCvsLC_insula[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) # 
t.test(d_long$HCvsLC_putamen[d_long$Condition=='Acetate'] ~ d_long$BMI_class[d_long$Condition=='Acetate']) #

t.test(d_long$FvsNF_amygdala[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) # 
t.test(d_long$FvsNF_caudate[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) # 
t.test(d_long$FvsNF_insula[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) # 
t.test(d_long$FvsNF_putamen[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) # 

t.test(d_long$HCvsNF_amygdala[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) # 
t.test(d_long$HCvsNF_caudate[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) # 
t.test(d_long$HCvsNF_insula[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) # 
t.test(d_long$HCvsNF_putamen[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) # 

t.test(d_long$HCvsLC_amygdala[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) # 
t.test(d_long$HCvsLC_caudate[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) # 
t.test(d_long$HCvsLC_insula[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) # 
t.test(d_long$HCvsLC_putamen[d_long$Condition=='Placebo'] ~ d_long$BMI_class[d_long$Condition=='Placebo']) #


comps <- list(c('Acetate', 'Placebo'))
d_long$Condition <- factor(d_long$Condition)
# Exclude sub-11 ivm uitschieters #
d_long <- d_long %>% filter(!Subject %in% c('Sub-11'))

pl <- ggplot(d_long, aes(x=Condition, y=FvsNF_amygdala))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Amygdala
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'FvsNF_amygdala.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_amygdala.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_amygdala.jpg', width = 7, height = 5, device = 'jpeg')

d_long_lean <- d_long %>% filter(BMI_class %in% c('Lean'))
pl <- ggplot(d_long_lean, aes(x=Condition, y=FvsNF_amygdala))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Amygdala
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'FvsNF_amygdala_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_amygdala_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_amygdala_Lean.jpg', width = 7, height = 5, device = 'jpeg')

d_long_metsyn <- d_long %>% filter(BMI_class %in% c('MetSyn'))
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=FvsNF_amygdala))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Amygdala
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'FvsNF_amygdala_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_amygdala_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_amygdala_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

#Tijdelijk sub-47 eruit#
d_long1 <- d_long %>% filter(!Subject %in% c('Sub-47'))
pl <- ggplot(d_long1, aes(x=Condition, y=FvsNF_caudate))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Caudate nucleus
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'FvsNF_caudate.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_caudate.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_caudate.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_lean, aes(x=Condition, y=FvsNF_caudate))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Caudate nucleus
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'FvsNF_caudate_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_caudate_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_caudate_Lean.jpg', width = 7, height = 5, device = 'jpeg')

# tijdelijk sub-47 eruit #
d_long_metsyn1 <- d_long_metsyn %>% filter(!Subject %in% c('Sub-47'))
pl <- ggplot(d_long_metsyn1, aes(x=Condition, y=FvsNF_caudate))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Caudate nucleus
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'FvsNF_caudate_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_caudate_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_caudate_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long, aes(x=Condition, y=FvsNF_insula))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Insula
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'FvsNF_insula.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_insula.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_insula.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_lean, aes(x=Condition, y=FvsNF_insula))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Insula
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'FvsNF_insula_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_insula_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_insula_Lean.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_metsyn, aes(x=Condition, y=FvsNF_insula))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Insula
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'FvsNF_insula_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_insula_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_insula_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long, aes(x=Condition, y=FvsNF_putamen))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Putamen
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'FvsNF_putamen.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_putamen.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_putamen.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_lean, aes(x=Condition, y=FvsNF_putamen))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Putamen
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'FvsNF_putamen_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_putamen_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_putamen_Lean.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_metsyn, aes(x=Condition, y=FvsNF_putamen))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('Food versus non-food contrast - Putamen
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'FvsNF_putamen_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'FvsNF_putamen_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'FvsNF_putamen_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

# Contrast 2 HCvsNF
comps <- list(c('Acetate', 'Placebo'))
d_long$Condition <- factor(d_long$Condition)

pl <- ggplot(d_long, aes(x=Condition, y=HCvsNF_amygdala))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Amygdala
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'HCvsNF_amygdala.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_amygdala.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_amygdala.jpg', width = 7, height = 5, device = 'jpeg')

d_long_lean <- d_long %>% filter(BMI_class %in% c('Lean'))
pl <- ggplot(d_long_lean, aes(x=Condition, y=HCvsNF_amygdala))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Amygdala
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsNF_amygdala_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_amygdala_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_amygdala_Lean.jpg', width = 7, height = 5, device = 'jpeg')

d_long_metsyn <- d_long %>% filter(BMI_class %in% c('MetSyn'))
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=HCvsNF_amygdala))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Amygdala
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsNF_amygdala_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_amygdala_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_amygdala_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

# Tijdelijk verwijderen sub-47
d_long <- d_long %>% filter(!Subject %in% 'Sub-47')
pl <- ggplot(d_long, aes(x=Condition, y=HCvsNF_caudate))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Caudate nucleus
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsNF_caudate.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_caudate.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_caudate.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_lean, aes(x=Condition, y=HCvsNF_caudate))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Caudate nucleus
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsNF_caudate_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_caudate_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_caudate_Lean.jpg', width = 7, height = 5, device = 'jpeg')

# Tijdelijk sub-47 verwijderen #
d_long_metsyn <- d_long_metsyn %>% filter(!Subject %in% 'Sub-47')
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=HCvsNF_caudate))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Caudate nucleus
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsNF_caudate_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_caudate_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_caudate_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long, aes(x=Condition, y=HCvsNF_insula))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Insula
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'HCvsNF_insula.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_insula.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_insula.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_lean, aes(x=Condition, y=HCvsNF_insula))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Insula
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsNF_insula_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_insula_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_insula_Lean.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_metsyn, aes(x=Condition, y=HCvsNF_insula))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Insula
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsNF_insula_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_insula_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_insula_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long, aes(x=Condition, y=HCvsNF_putamen))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Putamen
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'HCvsNF_putamen.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_putamen.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_putamen.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_lean, aes(x=Condition, y=HCvsNF_putamen))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Putamen
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsNF_putamen_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_putamen_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_putamen_Lean.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_metsyn, aes(x=Condition, y=HCvsNF_putamen))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus non-food contrast - Putamen
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsNF_putamen_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsNF_putamen_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsNF_putamen_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

# Contrast 3 HCvsLC
comps <- list(c('Acetate', 'Placebo'))
d_long$Condition <- factor(d_long$Condition)

# Tijdelijk sub-05 verwijderen #
d_long <- d_long %>% filter(!Subject %in% 'Sub-05')
pl <- ggplot(d_long, aes(x=Condition, y=HCvsLC_amygdala))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Amygdala
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

getwd()
ggsave(pl, filename = 'HCvsLC_amygdala.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_amygdala.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_amygdala.jpg', width = 7, height = 5, device = 'jpeg')

d_long_lean <- d_long %>% filter(BMI_class %in% c('Lean'))
pl <- ggplot(d_long_lean, aes(x=Condition, y=HCvsLC_amygdala))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Amygdala
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsLC_amygdala_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_amygdala_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_amygdala_Lean.jpg', width = 7, height = 5, device = 'jpeg')

d_long_metsyn1 <- d_long_metsyn %>% filter(!Subject %in% c('Sub-05'))
pl <- ggplot(d_long_metsyn1, aes(x=Condition, y=HCvsLC_amygdala))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Amygdala
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsLC_amygdala_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_amygdala_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_amygdala_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

# Tijdelijk verwijderen sub-45 en 19
d_long1 <- d_long %>% filter(!Subject %in% c('Sub-45', 'Sub-19'))
pl <- ggplot(d_long1, aes(x=Condition, y=HCvsLC_caudate))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Caudate nucleus
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsLC_caudate.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_caudate.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_caudate.jpg', width = 7, height = 5, device = 'jpeg')

# Tijdelijk sub-45 verwijderen #
d_long_lean1 <- d_long_lean %>% filter(!Subject %in% 'Sub-45')
pl <- ggplot(d_long_lean1, aes(x=Condition, y=HCvsLC_caudate))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Caudate nucleus
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsLC_caudate_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_caudate_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_caudate_Lean.jpg', width = 7, height = 5, device = 'jpeg')

# Tijdelijk sub-19 verwijderen #
d_long_metsyn <- d_long_metsyn %>% filter(!Subject %in% 'Sub-19')
pl <- ggplot(d_long_metsyn, aes(x=Condition, y=HCvsLC_caudate))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Caudate nucleus
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsLC_caudate_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_caudate_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_caudate_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long, aes(x=Condition, y=HCvsLC_insula))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Insula
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsLC_insula.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_insula.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_insula.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_lean, aes(x=Condition, y=HCvsLC_insula))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Insula
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsLC_insula_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_insula_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_insula_Lean.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_metsyn, aes(x=Condition, y=HCvsLC_insula))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Insula
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsLC_insula_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_insula_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_insula_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')

d_long1 <- d_long %>% filter(!Subject %in% 'Sub-45')
pl <- ggplot(d_long1, aes(x=Condition, y=HCvsLC_putamen))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Putamen
          Pooled')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsLC_putamen.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_putamen.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_putamen.jpg', width = 7, height = 5, device = 'jpeg')

d_long_lean1 <- d_long_lean %>% filter(!Subject %in% 'Sub-45')
pl <- ggplot(d_long_lean1, aes(x=Condition, y=HCvsLC_putamen))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Putamen
          Lean')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsLC_putamen_Lean.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_putamen_Lean.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_putamen_Lean.jpg', width = 7, height = 5, device = 'jpeg')

pl <- ggplot(d_long_metsyn, aes(x=Condition, y=HCvsLC_putamen))+
  geom_line(aes(col='grey', group=Subject), size=1)+
  geom_point(aes(col=Condition), size=2.1)+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = T)+
  xlab('Condition')+
  ylab('MRI BOLD signal stats/cope')+
  ggtitle('High-caloric versus low-caloric contrast - Putamen
          MetSyn')+
  scale_color_manual(breaks=c("Acetate", "Placebo"), values=c("#20854E99", "#0072B599", "grey"))+
  scale_fill_manual(values=c("#20854E99", "#0072B599"))
pl

pl + labs(color = "Condition")
pl <- pl + labs(color = "Condition")

ggsave(pl, filename = 'HCvsLC_putamen_MetSyn.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'HCvsLC_putamen_MetSyn.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'HCvsLC_putamen_MetSyn.jpg', width = 7, height = 5, device = 'jpeg')
##### Tot hier gekomen, herhaal voor andere contrasten en indien klaar ook voor de slokjestaak

d_long$SD[d_long$BMI_class=='Lean'] <- sd(d_long$Uptake_fraction[d_long$BMI_class=='Lean'])
d_long$SD[d_long$BMI_class=='MetSyn'] <- sd(d_long$Uptake_fraction[d_long$BMI_class=='MetSyn'])
d_long$SE <- (d_long$SD)/sqrt(37)
d_long$SE
d_long$Mean[d_long$BMI_class=='Lean'] <- mean(d_long$Uptake_fraction[d_long$BMI_class=='Lean'])
d_long$Mean[d_long$BMI_class=='MetSyn'] <- mean(d_long$Uptake_fraction[d_long$BMI_class=='MetSyn'])
d_long$Mean_min_SD <- d_long$Mean - d_long$SD
d_long$Mean_plus_SD <- d_long$Mean + d_long$SD

d_long1 <- d_long %>% filter(Condition=='Acetate')
comps <- list(c('Lean', 'MetSyn'))
pl <- ggplot(data=d_long1, aes(x = BMI_class, y = Uptake_fraction))+
  geom_boxplot(aes(x = BMI_class, y = Uptake_fraction, color = BMI_class), outlier.colour = NA, fatten = 1, lwd = 1.2)+
  geom_jitter(aes(fill = BMI_class), shape=21, color='black', position = position_jitter(width = 0.2, height = 0), show.legend = F)+
  scale_color_manual(labels = c("Lean", "MetSyn"), values=c("#0072B5FF", "#20854EFF"))+
  scale_fill_manual(labels = c("Lean", "MetSyn"), values=c("#0072B5FF", "#20854EFF"))+
  labs(x='Group', title='Acetate fraction taken up upon infusion', y='Acetate uptake fraction')+
  theme_Publication()+
  stat_compare_means(comparisons = comps, paired = F, method = 't.test', size = 6, label.y = 1.005)+
  #theme(panel.grid.major.x = element_line(size = 0.5, color='grey'))+
  theme(legend.position = 'right')+
  theme(plot.title = element_text(size = 19, face = "bold"),
        axis.text=element_text(size=18),
        axis.title.x=element_text(size=16,face="bold"),
        axis.title.y=element_text(size=16,face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
pl

pl <- pl + guides(color=guide_legend(title="Group"))

ggsave(pl, filename = 'Acetate_Uptake.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl, filename = 'Acetate_Uptake.png', width = 7, height = 5, device = 'png')
ggsave(pl, filename = 'Acetate_Uptake.jpg', width = 7, height = 5, device = 'jpeg')

plots <- ggarrange(pl1, pl,
                   labels = c("A", "B"), 
                   ncol = 2, nrow = 1, legend = "bottom")
plots

plots <- annotate_figure(plots, top = text_grob("Acetate excretion and absorption",
                                                color = "black", face = "bold", size = 20))

ggsave(plots, filename = 'Acetate_excretion_absorption.pdf', width = 12, height = 7, device = 'pdf')
ggsave(plots, filename = 'Acetate_excretion_absorption.png', width = 12, height = 7, device = 'png')
ggsave(plots, filename = 'Acetate_excretion_absorption.jpg', width = 12, height = 7, device = 'jpeg')

d_long_lean <- d_long %>% filter(BMI_class=='Lean')
d_long_metsyn <- d_long %>% filter(BMI_class=='MetSyn')

##### Make corplots requested by Max #####
##### Plots z-score correlations for article #####
##### 3. 
LM <- lm(d_long_lean$Uptake_fraction ~ d_long_lean$VCO2, data=d_long_lean)
LM
pl1 <- ggplot(LM$model, aes_string(x = LM$model$`d_long_lean$Uptake_fraction`, y = LM$model$`d_long_lean$VCO2`))+
  geom_point()+
  stat_smooth(method = "lm", col = "blue")+
  theme_Publication()+
  theme(title = element_text(size = 14))
pl1

t <- cor.test(d_long_lean$Uptake_fraction, d_long_lean$VCO2)
pl1 <- pl1+labs(title = paste("Rho =", signif(t$estimate, digits = 3),
                              #"S-statistic =", signif(t$statistic, digits = 3),
                              "P = ", signif(t$p.value, digits = 2)))+
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 16))
pl1
pl1 <- pl1+xlab("Acetate fraction absorbed")+
  ylab("VCO2")+
pl1

pl1 <- pl1+theme(title = element_text(size = 20))
pl1

ggsave(pl1, filename = 'Acetate_Absorption_VCO2_Corr.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl1, filename = 'Acetate_Absorption_VCO2_Corr.png', width = 7, height = 5, device = 'png')
ggsave(pl1, filename = 'Acetate_Absorption_VCO2_Corr.jpg', width = 7, height = 5, device = 'jpeg')

LM <- lm(d_long_lean$Uptake_fraction ~ d_long_lean$RQ, data=d_long_lean)
LM
pl2 <- ggplot(LM$model, aes_string(x = LM$model$`d_long_lean$Uptake_fraction`, y = LM$model$`d_long_lean$RQ`))+
  geom_point()+
  stat_smooth(method = "lm", col = "blue")+
  theme_Publication()+
  theme(title = element_text(size = 14))
pl2

t <- cor.test(d_long_lean$Uptake_fraction, d_long_lean$RQ)
pl2 <- pl2+labs(title = paste("Rho =", signif(t$estimate, digits = 3),
                              #"S-statistic =", signif(t$statistic, digits = 3),
                              "P = ", signif(t$p.value, digits = 2)))+
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 16))
pl2
pl2 <- pl2+xlab("Acetate fraction absorbed")+
  ylab("RQ")
pl2

pl2 <- pl2+theme(title = element_text(size = 20))
pl2

ggsave(pl1, filename = 'Acetate_Absorption_RQ_Corr.pdf', width = 7, height = 5, device = 'pdf')
ggsave(pl1, filename = 'Acetate_Absorption_RQ_Corr.png', width = 7, height = 5, device = 'png')
ggsave(pl1, filename = 'Acetate_Absorption_RQ_Corr.jpg', width = 7, height = 5, device = 'jpeg')

plots <- ggarrange(pl1, pl2,
                   labels = c("A", "B"),
                   font.label = list(size = 16),
                   ncol = 2, nrow = 1, legend = "bottom")
plots

plots <- annotate_figure(plots, top = text_grob("Acetate absorption and metabolic measures",
                                                color = "black", face = "bold", size = 20))

ggsave(plots, filename = 'Acetate_VCO2_RQ.pdf', width = 12, height = 6, device = 'pdf')
ggsave(plots, filename = 'Acetate_VCO2_RQ.png', width = 12, height = 6, device = 'png')
ggsave(plots, filename = 'Acetate_VCO2_RQ.jpg', width = 12, height = 6, device = 'jpeg')




mean(d_long_lean$FvsNF_amygdala[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$FvsNF_amygdala[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$FvsNF_amygdala[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$FvsNF_amygdala[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$FvsNF_amygdala ~ d_long_lean$Condition)

mean(d_long_lean$FvsNF_caudate[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$FvsNF_caudate[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$FvsNF_caudate[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$FvsNF_caudate[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$FvsNF_caudate ~ d_long_lean$Condition)

mean(d_long_lean$FvsNF_putamen[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$FvsNF_putamen[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$FvsNF_putamen[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$FvsNF_putamen[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$FvsNF_putamen ~ d_long_lean$Condition)

mean(d_long_lean$FvsNF_insula[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$FvsNF_insula[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$FvsNF_insula[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$FvsNF_insula[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$FvsNF_insula ~ d_long_lean$Condition)

mean(d_long_lean$HCvsNF_amygdala[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$HCvsNF_amygdala[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$HCvsNF_amygdala[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$HCvsNF_amygdala[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$HCvsNF_amygdala ~ d_long_lean$Condition)

mean(d_long_lean$HCvsNF_caudate[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$HCvsNF_caudate[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$HCvsNF_caudate[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$HCvsNF_caudate[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$HCvsNF_caudate ~ d_long_lean$Condition)

mean(d_long_lean$HCvsNF_putamen[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$HCvsNF_putamen[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$HCvsNF_putamen[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$HCvsNF_putamen[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$HCvsNF_putamen ~ d_long_lean$Condition)

mean(d_long_lean$HCvsNF_insula[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$HCvsNF_insula[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$HCvsNF_insula[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$HCvsNF_insula[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$HCvsNF_insula ~ d_long_lean$Condition)

mean(d_long_lean$HCvsLC_amygdala[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$HCvsLC_amygdala[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$HCvsLC_amygdala[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$HCvsLC_amygdala[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$HCvsLC_amygdala ~ d_long_lean$Condition)

mean(d_long_lean$HCvsLC_caudate[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$HCvsLC_caudate[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$HCvsLC_caudate[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$HCvsLC_caudate[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$HCvsLC_caudate ~ d_long_lean$Condition)

mean(d_long_lean$HCvsLC_putamen[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$HCvsLC_putamen[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$HCvsLC_putamen[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$HCvsLC_putamen[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$HCvsLC_putamen ~ d_long_lean$Condition)

mean(d_long_lean$HCvsLC_insula[d_long_lean$Condition=='Acetate'])
sd(d_long_lean$HCvsLC_insula[d_long_lean$Condition=='Acetate'])
mean(d_long_lean$HCvsLC_insula[d_long_lean$Condition=='Placebo'])
sd(d_long_lean$HCvsLC_insula[d_long_lean$Condition=='Placebo'])
t.test(d_long_lean$HCvsLC_insula ~ d_long_lean$Condition)

mean(d_long_metsyn$FvsNF_amygdala[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$FvsNF_amygdala[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$FvsNF_amygdala[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$FvsNF_amygdala[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$FvsNF_amygdala ~ d_long_metsyn$Condition)

mean(d_long_metsyn$FvsNF_caudate[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$FvsNF_caudate[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$FvsNF_caudate[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$FvsNF_caudate[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$FvsNF_caudate ~ d_long_metsyn$Condition)

mean(d_long_metsyn$FvsNF_putamen[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$FvsNF_putamen[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$FvsNF_putamen[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$FvsNF_putamen[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$FvsNF_putamen ~ d_long_metsyn$Condition)

mean(d_long_metsyn$FvsNF_insula[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$FvsNF_insula[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$FvsNF_insula[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$FvsNF_insula[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$FvsNF_insula ~ d_long_metsyn$Condition)

mean(d_long_metsyn$HCvsNF_amygdala[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$HCvsNF_amygdala[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$HCvsNF_amygdala[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$HCvsNF_amygdala[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$HCvsNF_amygdala ~ d_long_metsyn$Condition)

mean(d_long_metsyn$HCvsNF_caudate[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$HCvsNF_caudate[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$HCvsNF_caudate[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$HCvsNF_caudate[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$HCvsNF_caudate ~ d_long_metsyn$Condition)

mean(d_long_metsyn$HCvsNF_putamen[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$HCvsNF_putamen[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$HCvsNF_putamen[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$HCvsNF_putamen[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$HCvsNF_putamen ~ d_long_metsyn$Condition)

mean(d_long_metsyn$HCvsNF_insula[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$HCvsNF_insula[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$HCvsNF_insula[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$HCvsNF_insula[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$HCvsNF_insula ~ d_long_metsyn$Condition)

mean(d_long_metsyn$HCvsLC_amygdala[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$HCvsLC_amygdala[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$HCvsLC_amygdala[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$HCvsLC_amygdala[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$HCvsLC_amygdala ~ d_long_metsyn$Condition)

mean(d_long_metsyn$HCvsLC_caudate[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$HCvsLC_caudate[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$HCvsLC_caudate[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$HCvsLC_caudate[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$HCvsLC_caudate ~ d_long_metsyn$Condition)

mean(d_long_metsyn$HCvsLC_putamen[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$HCvsLC_putamen[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$HCvsLC_putamen[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$HCvsLC_putamen[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$HCvsLC_putamen ~ d_long_metsyn$Condition)

mean(d_long_metsyn$HCvsLC_insula[d_long_metsyn$Condition=='Acetate'])
sd(d_long_metsyn$HCvsLC_insula[d_long_metsyn$Condition=='Acetate'])
mean(d_long_metsyn$HCvsLC_insula[d_long_metsyn$Condition=='Placebo'])
sd(d_long_metsyn$HCvsLC_insula[d_long_metsyn$Condition=='Placebo'])
t.test(d_long_metsyn$HCvsLC_insula ~ d_long_metsyn$Condition)

t.test(d_long$Acetate_T_baseline ~ d_long$BMI_class)
t.test(d_long$Urine_acetate_baseline[d_long$Condition=='Placebo'], d_long$Urine_acetate_baseline[d_long$Condition=='Acetate'])

d_long_lean <- d_long %>% filter(BMI_class == 'Lean')
d_long_metsyn <- d_long %>% filter(BMI_class == 'MetSyn')

d_longGlucose_lean <- d_longGlucose %>% filter(BMI_class=='Lean')
d_longGlucose_metsyn <- d_longGlucose %>% filter(BMI_class=='MetSyn')
t.test(d_longGlucose_lean$Glucose[d_longGlucose_lean$time=='-15'] ~ d_longGlucose_lean$Condition[d_longGlucose_lean$time=='-15'])
t.test(d_longGlucose_lean$Insulin[d_longGlucose_lean$time=='-15'] ~ d_longGlucose_lean$Condition[d_longGlucose_lean$time=='-15'])
t.test(d_long_lean$`HOMA-IR` ~ d_long_lean$Condition)
t.test(d_long_lean$`HOMA-B` ~ d_long_lean$Condition)
t.test(d_long_lean$IGI ~ d_long_lean$Condition)
t.test(d_long_lean$DI ~ d_long_lean$Condition)
t.test(d_long_lean$iAUC_Glucose_0_120 ~ d_long_lean$Condition)
t.test(d_long_lean$iAUC_Insulin_0_120 ~ d_long_lean$Condition)

t.test(d_long_lean$iAUC_Glucose_0_60 ~ d_long_lean$Condition)
t.test(d_long_lean$iAUC_Insulin_0_60 ~ d_long_lean$Condition)

t.test(d_long_lean$iAUC_ratio_0_120 ~ d_long_lean$Condition)
t.test(d_long_lean$iAUC_ratio_0_60 ~ d_long_lean$Condition)

t.test(d_longGlucose_metsyn$Glucose[d_longGlucose_metsyn$time=='-15'] ~ d_longGlucose_metsyn$Condition[d_longGlucose_metsyn$time=='-15'])
t.test(d_longGlucose_metsyn$Insulin[d_longGlucose_metsyn$time=='-15'] ~ d_longGlucose_metsyn$Condition[d_longGlucose_metsyn$time=='-15'])
t.test(d_long_metsyn$`HOMA-IR` ~ d_long_metsyn$Condition)
t.test(d_long_metsyn$`HOMA-B` ~ d_long_metsyn$Condition)
t.test(d_long_metsyn$IGI ~ d_long_metsyn$Condition)
t.test(d_long_metsyn$DI ~ d_long_metsyn$Condition)
t.test(d_long_metsyn$iAUC_Glucose_0_120 ~ d_long_metsyn$Condition)
t.test(d_long_metsyn$iAUC_Insulin_0_120 ~ d_long_metsyn$Condition)

t.test(d_long_metsyn$iAUC_Glucose_0_60 ~ d_long_metsyn$Condition)
t.test(d_long_metsyn$iAUC_Insulin_0_60 ~ d_long_metsyn$Condition)

t.test(d_long_metsyn$iAUC_ratio_0_120 ~ d_long_metsyn$Condition)
t.test(d_long_metsyn$iAUC_ratio_0_60 ~ d_long_metsyn$Condition)

t.test(d_long$REE_day ~ d_long$Condition)
t.test(d_long$RQ ~ d_long$Condition)
t.test(d_long_metsyn$RQ[d_long_metsyn$Condition=='Acetate'], d_long_metsyn$VCO2[d_long_metsyn$Condition=='Placebo'])

d_long <- d_long %>% filter(VCO2 != 'NA')
d_long_metsyn <- d_long %>% filter(BMI_class=='MetSyn')

d_long$Urine_acetate_baseline
t.test(d_long$Urine_acetate_baseline ~ d_long$BMI_class)
