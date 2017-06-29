---
title: "ExperimentalMulti"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Multilevel approach for GORTRateAge
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

# First get into the format of the first wide transformation.
readDataSubLong = reshape(readDataSub, varying = list(c("PREGORTRateAge", "POSTGORTRateAge")), times = c(1,2), direction = "long")

library(nlme)
readDataSubLong$Group = factor(readDataSubLong$Group)
readMuliModel = groupedData(PREGORTRateAge ~ Group*time | id, data = readDataSubLong)


readMuliModelResults = lme(PREGORTRateAge ~ time*Group, random =~ time*Group | id, data = readMuliModel, method = "ML")
summary(readMuliModelResults)
ranef(readMuliModelResults)
```

GORT Reading accuarcy age
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


readMuliModelResults = lme(PREGORTAccuracyAge ~ time*Group, random =~ Group*time | id, data = readMuliModel, method = "ML")
summary(readMuliModelResults)
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


readMuliModelResults = lme(PREGORTFluencyAge ~ time*Group, random=~ 1 | id, data = readMuliModel, method = "ML")
summary(readMuliModelResults)
```
GORT comprehension reading age
```{r}
readDataGORTComp = readData[c("id","Group", "PREGORTComprehensionAge", "POSTGORTComprehensionAge")]

readData$PREGORTComprehensionAge
# First get into the format of the first wide transformation.
readDataSubLong = reshape(readDataGORTComp, varying = list(c("PREGORTComprehensionAge", "POSTGORTComprehensionAge")), times = c(1,2), direction = "long")

library(nlme)
readDataSubLong$Group = factor(readDataSubLong$Group)
readDataSubLong$time = factor(readDataSubLong$time)
readDataSubLong$id = factor(readDataSubLong$id)

readMuliModel = groupedData(PREGORTComprehensionAge ~ Group*time | id, data = readDataSubLong)
dim(readMuliModel)

readMuliModelResults = lme(PREGORTComprehensionAge ~ time*Group, random =~ time*Group | id, data = readMuliModel, method = "ML")
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

readMuliModel = groupedData(PREBURTReadingAge ~ Group*time | id, data = readDataSubLong)
dim(readMuliModel)

readMuliModelResults = lme(PREBURTReadingAge ~ time*Group, random =~ time*Group | id, data = readMuliModel, method = "ML")
summary(readMuliModelResults)

```
I believe I am using an unstructred covariance matrix so there are no assumptions about shperiticy, we can unbalanced designs.  With a random slopes model, we do not make any assumptions about the individual across time.  

RCBM Scores
```{r}
readDataRCBM = readData[c("id","Group", "PREWRCPMAverage", "POSTWRCPMAverage")]

readData$PREWRCPMAverage
# First get into the format of the first wide transformation.
readDataSubLong = reshape(readDataRCBM, varying = list(c("PREWRCPMAverage", "POSTWRCPMAverage")), times = c(1,2), direction = "long")

library(nlme)
readDataSubLong$Group = factor(readDataSubLong$Group)
readDataSubLong$time = factor(readDataSubLong$time)
readDataSubLong$id = factor(readDataSubLong$id)

readMuliModel = groupedData(PREWRCPMAverage ~ Group*time | id, data = readDataSubLong)

readMuliModelResults = lme(PREWRCPMAverage ~ time*Group, random =~ time*Group | id, data = readMuliModel, method = "ML")
summary(readMuliModelResults)
```

