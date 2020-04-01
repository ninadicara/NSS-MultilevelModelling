load("/Users/Nina/Documents/University/Year 4/Final Yr Project/Data/All_Files/fullnssdata.Rdata")

#subset data so that we only have the records for % agreement
PerctAgree <- subset(dataset, Measure=='% Agree')
PerctAgree <- PerctAgree[ which(PerctAgree$Mode != 'All' & PerctAgree$Level != 'All'), ]


library(car)
library(nlme)
library(ggplot2)
library(reshape)


#This subsets 10 rows with NA in the OS column (only those rows with ALL NAs)
PerctAgreeNA <- PerctAgree[is.na(PerctAgree$OS), ]
#Originally has 40463 values, want to remove any with NA in OS column - 40453 afterwards
PerctAgree <- PerctAgree[ which(!is.na(PerctAgree$OS)), ]


#scale the data to standardise and then bind back together with key info
  myvars <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "OS")
  scaledDF <- PerctAgree[myvars]
  scaledDF <- scale(scaledDF, TRUE, TRUE)
  scaledDF <- as.data.frame(scaledDF)
  myvars2 <- c("Year", "Mode", "Level", "Institution", "Subject")
  df <- PerctAgree[myvars2]
  #bind the scaled data back to the associated information 
  scDF <- cbind(df, scaledDF)

#investigate missing values
scDF_NAvals <-scDF[!complete.cases(scDF),]
scDF <- na.omit(scDF)
scDF_vals <- na.omit(scaledDF)

#Take out institution wide variables from dataset
scDF <- scDF[ which (scDF$Institution != "Sector-wide"), ]
scDF_vals <- scDF[c(-1, -2, -3, -4, -5)]
