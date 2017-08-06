---
title: "MultiLevelRepeatedMeasures"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Extra resources
#http://rpsychologist.com/r-guide-longitudinal-lme-lmer#unconditional-growth-model
#https://datascienceplus.com/analysing-longitudinal-data-multilevel-growth-models-i/
#https://idaejin.github.io/bcam-courses/neiker-2016/material/mixed-models/#repeated-measurements-and-longitudinal-data


GORT rate reading age
```{r}
setwd("~/Desktop")
readData = read.csv("TheCrossingDataSet.csv")
head(readData)
id = 1:nrow(readData)
readData = cbind(id,readData)
head(readData)
library(reshape)
readDataSub = readData[c("id","Group", "PREGORTRateAge", "POSTGORTRateAge")]

readDataGORTAc = readData[c("id","Group", "PREGORTAccuracyAge", "POSTGORTAccuracyAge")]

readDataSubLong = reshape(readDataSub, varying = list(c("PREGORTRateAge", "POSTGORTRateAge")), times = c(1,2), direction = "long")

library(nlme)
readDataSubLong$Group = factor(readDataSubLong$Group)
readMuliModel = groupedData(PREGORTRateAge ~ Group*time | id, data = readDataSubLong)

unconResults = lme(PREGORTRateAge ~ 1, random = ~ 1 | id, data = readMuliModel, method = "ML")

randomInResults = lme(PREGORTRateAge ~ time*Group, random = ~ 1 | id, data = readMuliModel, method = "ML")

summary(randomInResults)

anova(unconResults, randomInResults)

readMuliModelResults = lme(PREGORTRateAge ~ time*Group, random =~ time | id, data = readMuliModel, method = "ML")

anova(randomInResults, readMuliModelResults)

summary(readMuliModelResults)


```

GORT accuracy reading age
```{r}
readDataGORTAc = readData[c("id","Group", "PREGORTAccuracyAge", "POSTGORTAccuracyAge")]

# First get into the format of the first wide transformation.
readDataSubLong = reshape(readDataGORTAc, varying = list(c("PREGORTAccuracyAge", "POSTGORTAccuracyAge")), times = c(1,2), direction = "long")

library(nlme)
readDataSubLong$Group = factor(readDataSubLong$Group)
readDataSubLong$time = factor(readDataSubLong$time)
readDataSubLong$id = factor(readDataSubLong$id)
head(readDataSubLong)

readMuliModel = groupedData(PREGORTAccuracyAge ~ Group*time | id, data = readDataSubLong)

unconResults = lme(PREGORTAccuracyAge ~ 1, random = ~ 1 | id, data = readMuliModel, method = "ML")

randomInResults = lme(PREGORTAccuracyAge ~ time*Group, random = ~ 1 | id, data = readMuliModel, method = "ML")

summary(randomInResults)

anova(unconResults, randomInResults)

readMuliModelResults = lme(PREGORTAccuracyAge ~ time*Group, random =~ time | id, data = readMuliModel, method = "ML")
summary(readMuliModelResults)

anova(randomInResults, readMuliModelResults)

sd1 = 3.5
sd2 = 2.9
n1 = 17
n2 = 14
m1 = (17.1-14.4)
m2 = (15.1-14.6)
poolSD = sqrt((sd1^2*(n1-1) + sd2^2*(n2-1))/((n1-1)+(n2-1))) 
efSize = (m1-m2)/poolSD
efSize

```
GORT fluency reading age
```{r}
readDataGORTFl = readData[c("id","Group", "PREGORTFluencyAge", "POSTGORTFluencyAge")]

readData$PREGORTFluencyAge
# First get into the format of the first wide transformation.
readDataSubLong = reshape(readDataGORTFl, varying = list(c("PREGORTFluencyAge", "POSTGORTFluencyAge")), times = c(1,2), direction = "long")

library(nlme)
readDataSubLong$Group = factor(readDataSubLong$Group)
readDataSubLong$time = factor(readDataSubLong$time)
readDataSubLong$id = factor(readDataSubLong$id)
head(readDataSubLong)

readMuliModel = groupedData(PREGORTFluencyAge ~ Group*time | id, data = readDataSubLong)


unconResults = lme(PREGORTFluencyAge ~ 1, random = ~ 1 | id, data = readMuliModel, method = "ML")

randomInResults = lme(PREGORTFluencyAge ~ time*Group, random = ~ 1 | id, data = readMuliModel, method = "ML")

summary(randomInResults)

anova(unconResults, randomInResults)


readMuliModelResults = lme(PREGORTFluencyAge ~ time*Group, random=~ time | id, data = readMuliModel, method = "ML")
summary(readMuliModelResults)

anova(readMuliModelResults, randomInResults)



sd1 = 3.5
sd2 = 2.6
n1 = 17
n2 = 14
m1 = (15.9-13.7)
m2 = (14.4-13.6)

poolSD = sqrt((sd1^2*(n1-1) +sd2^2*(n2-1))/((n1-1)+(n2-1))) 
efSize = (m1-m2)/poolSD
efSize
```
GORT comprehension reading age
```{r}
readDataGORTComp = readData[c("id","Group", "PREGORTComprehensionAge", "POSTGORTComprehensionAge")]


readDataSubLong = reshape(readDataGORTComp, varying = list(c("PREGORTComprehensionAge", "POSTGORTComprehensionAge")), times = c(1,2), direction = "long")

library(nlme)
readDataSubLong$Group = factor(readDataSubLong$Group)
readDataSubLong$time = factor(readDataSubLong$time)
readDataSubLong$id = factor(readDataSubLong$id)

readMuliModel = groupedData(PREGORTComprehensionAge ~ Group*time | id, data = readDataSubLong)

unconResults = lme(PREGORTComprehensionAge ~ 1, random = ~ 1 | id, data = readMuliModel, method = "ML")

randomInResults = lme(PREGORTComprehensionAge ~ time*Group, random = ~ 1 | id, data = readMuliModel, method = "ML")

summary(randomInResults)

anova(unconResults, randomInResults)

readMuliModelResults = lme(PREGORTComprehensionAge ~ time*Group, random =~ time | id, data = readMuliModel, method = "ML")
summary(readMuliModelResults)
```

BURT reading age
```{r}
readDataBURT = readData[c("id","Group", "PREBURTReadingAge", "POSTBURTReadingAge")]

readData$PREBURTReadingAge
# First get into the format of the first wide transformation.
readDataSubLong = reshape(readDataBURT, varying = list(c("PREBURTReadingAge", "POSTBURTReadingAge")), times = c(1,2), direction = "long")

library(nlme)
readDataSubLong$Group = factor(readDataSubLong$Group)
readDataSubLong$time = factor(readDataSubLong$time)
readDataSubLong$id = factor(readDataSubLong$id)

readMuliModel = groupedData(PREBURTReadingAge ~ Group*time | time/id, data = readDataSubLong)

unconResults = lme(PREBURTReadingAge ~ 1, random = ~ 1 | id, data = readMuliModel, method = "ML")

randomInResults = lme(PREBURTReadingAge ~ time*Group, random = ~ 1 | id, data = readMuliModel, method = "ML")

summary(randomInResults)

anova(unconResults, randomInResults)



readMuliModelResults = lme(PREBURTReadingAge ~ time*Group, random =~ time | id, data = readMuliModel, method = "ML")
summary(readMuliModelResults)

sd1 = 2.9
sd2 = 3.3
n1 = 17
n2 = 14
m1 = (15.9-13.7)
m2 = (14.4-13.6)

poolSD = sqrt(sd1^2*(n1-1) +sd2^2*(n2-1)/((n1-1)+(n2-1))) 
efSize = (m1-m2)/poolSD
efSize
```
