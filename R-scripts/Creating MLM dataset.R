load("/Users/Nina/Documents/University/Year 4/Final Yr Project/Data/All_Files/fullnssdata.Rdata")

# Create a subset of the data with % agree for overall modes and levels, excluding the sector-wide results as we don't need these
df_agree <- subset(dataset, Mode=='All' & Level=='All' & Institution!='Sector-wide' & Measure=='% Agree')

# Need to remove the empty rows. First rename the rows and then remove them from the dataset
rownames(df_agree) <- NULL
df_agree           <- df_agree[-c(32947, 33001, 33018, 33085, 34798, 35773, 36536, 36740), ]

# Remove label columns to use data for analysis - also remove thematic area columns
df_agree$Year                        <- NULL
df_agree$Mode                        <- NULL
df_agree$Level                       <- NULL
df_agree$Measure                     <- NULL
df_agree$The.teaching.on.my.course   <- NULL
df_agree$Assessment.and.feedback     <- NULL
df_agree$Academic.support            <- NULL
df_agree$Organisation.and.management <- NULL
df_agree$Learning.resources          <- NULL
df_agree$Personal.development        <- NULL

# Omit rows with NA values - removes 8 rows
df_agree <- na.omit(df_agree)

#create ID column
df_agree$ID<-seq.int(nrow(df_agree))

##Swapping Institution names for numbers
university_index <- read.csv("/Users/Nina/Documents/University/Year 4/Final Yr Project/Data/universities_replace_index.csv")

#Need to match Institution Column to the index in university_index
df_agree <- merge(df_agree, university_index, "Institution")
df_agree <- df_agree[c(25, 26, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)]
#rename Institutions.1 column to be called Instutition No. 
names(df_agree)[2] <- "Inst_No"

#Now standardise all numerical columns
df_agree_val <- scale(df_agree[c(5:26)], center = TRUE, scale = TRUE)

df_agree <- cbind(df_agree[c(1:4)], df_agree_val)

