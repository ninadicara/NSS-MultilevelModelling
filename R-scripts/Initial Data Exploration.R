load("/Users/Nina/Documents/University/Year 4/Final Yr Project/Data/All_Files/fullnssdata.Rdata")

##Initial & basic data exploration
# Create a subset of the data with % agree for overall modes and levels, excluding the sector-wide results as we don't need these
All_Agree <- subset(dataset, Mode=='All' & Level=='All' & Institution!='Sector-wide' & Measure=='% Agree')

# Need to remove the empty rows. First rename the rows and then remove them from the dataset
rownames(All_Agree) <- NULL
All_Agree           <- All_Agree[-c(32947, 33001, 33018, 33085, 34798, 35773, 36536, 36740), ]

# Remove label columns to use data for analysis - also remove thematic area columns
All_Agree$Year                        <- NULL
All_Agree$Mode                        <- NULL
All_Agree$Level                       <- NULL
All_Agree$Institution                 <- NULL
All_Agree$Subject                     <- NULL
All_Agree$Measure                     <- NULL
All_Agree$The.teaching.on.my.course   <- NULL
All_Agree$Assessment.and.feedback     <- NULL
All_Agree$Academic.support            <- NULL
All_Agree$Organisation.and.management <- NULL
All_Agree$Learning.resources          <- NULL
All_Agree$Personal.development        <- NULL

# Omit rows with NA values - removes 8 rows
All_Agree <- na.omit(All_Agree)

#Get ready to use the LaTex table package
library(xtable)
options(xtable.floating = FALSE)

######Inital Data Exploration

#Inital Data Diagnostics
#Each varibale IQR, mean, range, min/max, 
options(digits=3)
xtable(summary(All_Agree))

##Boxplots
layout(matrix(1,1,1)) #Layout boxplot for one graph per page
boxplot(All_Agree$Q1, All_Agree$Q2, All_Agree$Q3, All_Agree$Q4, All_Agree$Q5, All_Agree$Q6, All_Agree$Q7, All_Agree$Q8, 
All_Agree$Q9, All_Agree$Q10, All_Agree$Q11, All_Agree$Q12, All_Agree$Q13, All_Agree$Q14, All_Agree$Q15, All_Agree$Q16, 
All_Agree$Q17, All_Agree$Q18, All_Agree$Q19, All_Agree$Q20, All_Agree$Q21, All_Agree$OS)

#Counting key info like how many unis there are, how many JACS codes, how many individual observations
library(plyr)
universitiescount <- count(dataset, "Institution")
JACS3count <- count(dataset, "Subject")
obvscount <- count(dataset, "Measure")

library(xlsx)
write.xlsx(universitiescount, "/Users/Nina/Documents/University/Year 4/Final Yr Project/Data/universitiescount.xlsx")
write.xlsx(JACS3count, "/Users/Nina/Documents/University/Year 4/Final Yr Project/Data/JACS3count.xlsx")
write.xlsx(obvscount, "/Users/Nina/Documents/University/Year 4/Final Yr Project/Data/obvscount.xlsx")

#Correlation Matrix
cormatrix2 <- cor(All_Agree)
cormatrix2 <- as.matrix(cormatrix2)
heatmap(cormatrix2, Colv=F, scale='none')

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
upper_tri <- get_lower_tri(cormatrix2)
library(ggplot2)
ggplot(upper_tri, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
  theme(axis.text.x = element_text(angle=90, hjust=TRUE)) +
  xlab("") + ylab("") + 
  geom_text(label=cordata$label, size=txtsize) + 
  geom_text(label=cordata$strike, size=txtsize * 4, color="red", alpha=0.4)

library(corrgram)
corrgram1<- corrgram(All_Agree, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt)

#Scatterplotmatrix of the data
pairs(All_Agree)
par()$mar
par(mar=c(0.1, 0.1, 0.1, 0.1))
