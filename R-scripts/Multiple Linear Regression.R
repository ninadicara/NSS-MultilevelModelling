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

# Multiple Linear Regression - Inital Model

Full_lm <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21, data = All_Agree)
Base_lm <- lm(OS ~ 1, data = All_Agree)

# Backward regression

Step_Forward  <- step(Base_lm, scope = list( upper = Full_lm, lower = ~ 1), direction = "forward", trace = FALSE)
Step_Backward <- step(Full_lm, direction = "backward" , trace = FALSE)

summary(Full_lm)
summary(Step_Forward)
summary(Step_Backward)

#Get ready to use the LaTex table package
library(xtable)
options(xtable.floating = FALSE)

##Residual plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 



plot(Step_Backward)
plot(ScStep_Forward)

##Correlation Matrix
#Function to do correlation matrix with significance stars
#Function credit: http://myowelt.blogspot.co.uk/2008/04/beautiful-correlation-tables-in-r.html
corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}
cormatrix <- corstarsl(All_Agree)
write.xlsx(cormatrix, "/Users/Nina/Documents/University/Year 4/Final Yr Project/cormatrix.xlsx")

require(lattice)
levelplot(cor(na.omit(dataset2)), xlab = NULL, ylab = NULL, col.regions = gray(0:100/100))

#VIF analysis
require(car)
vif(model1)
1/vif(ScStep_Forward) #tolerance
mean(vif(model1)) 


#Standardised Data
scaled_dat <- scale(All_Agree, TRUE, TRUE)

scaled_dat<- as.data.frame(scaled_dat)

#Multiple Linear Regression - Inital Model
ScFull_lm <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21, data = scaled_dat)
ScBase_lm <- lm(OS ~ 1, data = scaled_dat)

#backward regression
ScStep_Forward <- step(ScBase_lm, scope = list( upper = ScFull_lm, lower = ~ 1), direction = "forward", trace = FALSE)
ScStep_Backward <- step(ScFull_lm, direction = "backward" , trace = FALSE)

summary(Full_lm)
summary(Step_Forward)
xtable(summary(Step_Forward))

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(model1)


#Studentized residuals
sresid <- studres(fit)

#Leverageplots 
#http://www.stat.columbia.edu/~martin/W2024/R7.pdf
Leverage <- hat(model.matrix(Step_Forward))
plot(Leverage)
All_Agree[lev>0.0045,]

cook = cooks.distance(Step_Forward)
plot(cook)
points(35611,cook[35611],col='red') #why is this colouring the wrong point??
All_Agree[cook>0.008,]

#Alternative way of calculating Cook's Distance - giving correct outlier... 
cutoff <- 4/((nrow(All_Agree)-length(Step_Forward$coefficients)-2)) 
plot(Step_Forward, which=4, cook.levels=cutoff)
All_Agree[cutoff>0.09,]

durbinWatsonTest(model1)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(Step_Forward)

lm_validation <- repCV(model1, cost = rmspe, K = 10, R = 3, foldType = c("random"))
lm_validation
summary(lm_validation)
plot(lm_validation, method = "xyplot")

cv.lm(data=scaled_dat, ScStep_Forward, m=3)
summary(scaled_dat$OS)

##March 2016 - Rerunning Enter model for write up
##Model 1
df_agree_MLR <- df_agree[-c(1, 2, 3, 4)]
df_agree_MLR <- as.data.frame(scale(df_agree_MLR[-c(23)], TRUE, TRUE))
df_agree_MLR$ID<-seq.int(nrow(df_agree_MLR))

Full_lm <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21, data = df_agree_val)
Base_lm <- lm(OS ~ 1, data = df_agree_val)

summary(Full_lm)

model1 <- step(Base_lm, scope = list( upper = Full_lm, lower = ~ 1), direction = "forward", trace = FALSE)
summary(model1)
xtable(summary(model1))

#plotting
library("arm") 
coefplot(model1 , ylab = "Independent Variables",  xlim=c(-0.2, 0.4), intercept = TRUE)


install.packages("stargazer")
library(stargazer)
stargazer(dataset2)

dataset2 <- subset(dataset, Mode=='All' & Level=='All' & Institution!='Sector-wide' & Measure=='% Agree')
# Remove label columns to use data for analysis - also remove thematic area columns
dataset2$Year                        <- NULL
dataset2$Mode                        <- NULL
dataset2$Level                       <- NULL
dataset2$Institution                 <- NULL
dataset2$Subject                     <- NULL
dataset2$Measure                     <- NULL
dataset2$The.teaching.on.my.course   <- NULL
dataset2$Assessment.and.feedback     <- NULL
dataset2$Academic.support            <- NULL
dataset2$Organisation.and.management <- NULL
dataset2$Learning.resources          <- NULL
dataset2$Personal.development        <- NULL

require(stargazer)
stargazer(Full_lm, model1, BCStep_Forward2, title = "Regression Model Results", omit.stat=c("f"), no.space = TRUE, single.row = TRUE)

#Outliers
outliers_dfagree <- df_agree["ID" == 35611]

