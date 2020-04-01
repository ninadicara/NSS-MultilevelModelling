df_agree_MLR$Q1 <- as.numeric(df_agree_MLR$Q1)
df_agree_MLR$Q2 <- as.numeric(df_agree_MLR$Q2)
df_agree_MLR$Q3 <- as.numeric(df_agree_MLR$Q3)
df_agree_MLR$Q4 <- as.numeric(df_agree_MLR$Q4)
df_agree_MLR$Q5 <- as.numeric(df_agree_MLR$Q5)
df_agree_MLR$Q6 <- as.numeric(df_agree_MLR$Q6)
df_agree_MLR$Q7 <- as.numeric(df_agree_MLR$Q7)
df_agree_MLR$Q8 <- as.numeric(df_agree_MLR$Q8)
df_agree_MLR$Q9 <- as.numeric(df_agree_MLR$Q9)
df_agree_MLR$Q10 <- as.numeric(df_agree_MLR$Q10)
df_agree_MLR$Q11 <- as.numeric(df_agree_MLR$Q11)
df_agree_MLR$Q12 <- as.numeric(df_agree_MLR$Q12)
df_agree_MLR$Q13 <- as.numeric(df_agree_MLR$Q13)
df_agree_MLR$Q14 <- as.numeric(df_agree_MLR$Q14)
df_agree_MLR$Q15 <- as.numeric(df_agree_MLR$Q15)
df_agree_MLR$Q16 <- as.numeric(df_agree_MLR$Q16)
df_agree_MLR$Q17 <- as.numeric(df_agree_MLR$Q17)
df_agree_MLR$Q18 <- as.numeric(df_agree_MLR$Q18)
df_agree_MLR$Q19 <- as.numeric(df_agree_MLR$Q19)
df_agree_MLR$Q20 <- as.numeric(df_agree_MLR$Q20)
df_agree_MLR$Q21 <- as.numeric(df_agree_MLR$Q21)
df_agree_MLR$OS <- as.numeric(df_agree_MLR$OS)
df_agree_MLR$OS2 <- as.numeric(df_agree_MLR$OS2)

min(df_agree_val) #minimum was -8.328709 in Q17

#Add constant = 10 to all values in the dataframe to make it strictly +ve
df_agree_MLR_up10 <- df_agree_val + 10

#Multiple Linear Regression - Inital Model
BCFull_lm <- lm(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21, data = df_agree_MLR_up10)
BCBase_lm <- lm(OS ~ 1, data = df_agree_MLR_up10)

#regression
BCStep_Forward <- step(BCBase_lm, scope = list( upper = BCFull_lm, lower = ~ 1), direction = "forward", trace = FALSE)

summary(BCStep_Forward)


require(MASS)
BC <- boxcox(BCStep_Forward, lambda = seq(-5, 5, 1/10))
#Reduce range to area of interest
BC_2 <- boxcox(BCStep_Forward, lambda = seq(2.7, 2.85, 0.01))

#create new column with transformed OS
df_agree_MLR_up10$OS2 <- (((df_agree_MLR_up10$OS^2.8)-1)/2.8)

#Rerun regression analysis with OS transformed as per Box Cox 
BCFull_lm2 <- lm(OS2 ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21, data = df_agree_MLR_up10)
BCBase_lm2 <- lm(OS2 ~ 1, data = df_agree_MLR_up10)
BCStep_Forward2 <- step(BCBase_lm2, scope = list( upper = BCFull_lm2, lower = ~ 1), direction = "forward", trace = FALSE)
summary(BCStep_Forward2)

##Residual plots
title(main="MLR of Scaled Data", col.main="black", font.main=4)
summary(BCStep_Forward2, )

plot(BCStep_Forward, which = c(2))
plot(BCStep_Forward2, which = c(2))
