load("/Users/Nina/Documents/University/Year 4/Final Yr Project/Data/All_Files/fullnssdata.Rdata")

# Create a subset of the data with % agree for overall modes and levels, excluding the sector-wide results as we don't need these
yeardata <- subset(dataset, Mode=='All' & Level=='All' & Institution!='Sector-wide' & Measure=='% Agree')

# Need to remove the empty rows. First rename the rows and then remove them from the dataset
rownames(yeardata) <- NULL
yeardata           <- yeardata[-c(32947, 33001, 33018, 33085, 34798, 35773, 36536, 36740), ]

# Remove label columns to use data for analysis - also remove thematic area columns
yeardata$Mode                        <- NULL
yeardata$Level                       <- NULL
yeardata$Measure                     <- NULL
yeardata$The.teaching.on.my.course   <- NULL
yeardata$Assessment.and.feedback     <- NULL
yeardata$Academic.support            <- NULL
yeardata$Organisation.and.management <- NULL
yeardata$Learning.resources          <- NULL
yeardata$Personal.development        <- NULL

# Omit rows with NA values - removes 8 rows
yeardata <- na.omit(yeardata)

#create ID column
yeardata$ID<-seq.int(nrow(yeardata))

##Swapping Institution names for numbers
university_index <- read.csv("/Users/Nina/Documents/University/Year 4/Final Yr Project/Data/universities_replace_index.csv")

#Need to match Institution Column to the index in university_index
yeardata <- merge(yeardata, university_index, "Institution")
yeardata <- yeardata[c(26, 27, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)]
#rename Institutions.1 column to be called Instutition No. 
names(yeardata)[2] <- "Inst_No"

##Subset data based on year
data2006 <- subset(yeardata, Year == '2006')
data2007 <- subset(yeardata, Year == '2007')
data2008 <- subset(yeardata, Year == '2008')
data2009 <- subset(yeardata, Year == '2009')
data2010 <- subset(yeardata, Year == '2010')
data2011 <- subset(yeardata, Year == '2011')
data2012 <- subset(yeardata, Year == '2012')
data2013 <- subset(yeardata, Year == '2013')
data2014 <- subset(yeardata, Year == '2014')
data2015 <- subset(yeardata, Year == '2015')


#Standardise Data By Year
data2006 <- cbind(data2006[c(1:5)], scale(data2006[c(6:27)], center = TRUE, scale = TRUE))
data2007 <- cbind(data2007[c(1:5)], scale(data2007[c(6:27)], center = TRUE, scale = TRUE))
data2008 <- cbind(data2008[c(1:5)], scale(data2008[c(6:27)], center = TRUE, scale = TRUE))
data2009 <- cbind(data2009[c(1:5)], scale(data2009[c(6:27)], center = TRUE, scale = TRUE))
data2010 <- cbind(data2010[c(1:5)], scale(data2010[c(6:27)], center = TRUE, scale = TRUE))
data2011 <- cbind(data2011[c(1:5)], scale(data2011[c(6:27)], center = TRUE, scale = TRUE))
data2012 <- cbind(data2012[c(1:5)], scale(data2012[c(6:27)], center = TRUE, scale = TRUE))
data2013 <- cbind(data2013[c(1:5)], scale(data2013[c(6:27)], center = TRUE, scale = TRUE))
data2014 <- cbind(data2014[c(1:5)], scale(data2014[c(6:27)], center = TRUE, scale = TRUE))
data2015 <- cbind(data2015[c(1:5)], scale(data2015[c(6:27)], center = TRUE, scale = TRUE))


M2006 <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q21, data2006 )
M2007 <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q21, data2007 )
M2008 <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q21, data2008 )
M2009 <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q21, data2009 )
M2010 <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q21, data2010 )
M2011 <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q21, data2011 )
M2012 <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q21, data2012 )
M2013 <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q21, data2013 )
M2014 <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q21, data2014 )
M2015 <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q21, data2015 )

summary(M2006)
summary(M2007)
summary(M2008)
summary(M2009)
summary(M2010)
summary(M2011)
summary(M2012)
summary(M2013)
summary(M2014)
summary(M2015)

stargazer(M2006, M2007, M2008, M2009, M2010) 
M2011, M2012, M2013, M2014, M2015)
