##Model Building
library(lme4)
library(lattice)


##STEP 1 - fit "unconditional" model and decide whether to omit random subject effects
model1 <- lmer(OS ~ 1 + (1|Inst_No/Subject), df_agree, REML = TRUE)
model2 <- lmer(OS ~ 1 + (1|Inst_No), df_agree, REML = TRUE)
anova(model1, model2)
#highly significant difference - keep nested random effects associated with Subject, i.e. choose model1

##STEP 2 - Adding Level 1 covariates and testing the significance of their fixed effects
model3 <- lmer(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21 + (1|Inst_No/Subject), df_agree, REML = TRUE)
#Test hypothesis that fixed effects associated with Level 1 covariates in model3 are 0. 
#When testing these hypothesis with anova must use ML estimation (necessary by Pinheiro & Bates, 2000; Bolker et al., 2009)
anova(model1, model3)
#highly significant difference. Fixed effects from level covariates are significant, so keep them. Choose Model3

##STEP 3 - Introduce automatic model selection. 
library(LMERConvenienceFunctions)

#Backward fitting random effects
bfFixefLMER_F.fnc(model3, item = FALSE, method = "BIC")

#backfit fixed effects
model4 <- lmer(OS ~ Q1 + Q2 + Q3 + Q4 + Q6 + Q7 + Q8 + Q10 + Q11 + Q14 + Q15 + Q17 + Q19 + Q21 + (1|Inst_No/Subject), df_agree, REML = FALSE)

model5 <- lmer(OS ~ Q1 + Q2 + Q3 + Q4 + Q6 + Q7 + Q8 + Q10 + Q11 + Q14 + Q15 + Q17 + Q19 + Q21 + (1|Inst_No/Subject) + (Q1|Inst_No/Subject) +
                 (Q2|Inst_No/Subject) + (Q3|Inst_No/Subject) + (Q4|Inst_No/Subject) + (Q6|Inst_No/Subject) + (Q7|Inst_No/Subject) + 
                 (Q8|Inst_No/Subject) + (Q10|Inst_No/Subject) + (Q11|Inst_No/Subject) + (Q14|Inst_No/Subject) + (Q15|Inst_No/Subject) +
                 (Q17|Inst_No/Subject) + (Q19|Inst_No/Subject) + (Q21|Inst_No/Subject), df_agree, REML = FALSE)


#Investigating crossed effects
model6a <- lmer(OS ~ Q1 + Q2*Q3 + Q4 + Q6 + Q7 + Q8 + Q10 + Q11 + Q14 + Q15 + Q17 + Q19 + Q21 + (1|Inst_No/Subject), df_agree, REML = FALSE)

model6 <- lmer(OS ~ Q1 + Q2*Q3 + Q4 + Q6 + Q7 + Q8 + Q10 + Q11 + Q14*Q15 + Q17 + Q19 + Q21 + (1|Inst_No/Subject), df_agree, REML = FALSE)

model7 <- lmer(OS ~ Q1 + Q2*Q3 + Q4 + Q6 + Q7 + Q8 + Q10 + Q11 + Q14*Q15 + Q17 + Q19 + Q21 + (1|Inst_No/Subject) + (Q1|Inst_No/Subject) +
                 (Q2|Inst_No/Subject) + (Q3|Inst_No/Subject) + (Q4|Inst_No/Subject) + (Q6|Inst_No/Subject) + (Q7|Inst_No/Subject) + 
                 (Q8|Inst_No/Subject) + (Q10|Inst_No/Subject) + (Q11|Inst_No/Subject) + (Q14|Inst_No/Subject) + (Q15|Inst_No/Subject) +
                 (Q17|Inst_No/Subject) + (Q19|Inst_No/Subject) + (Q21|Inst_No/Subject), df_agree, REML = FALSE)

##Diagnostic Checks
plot(model7)
ranef(model7, condVar = TRUE)
coef(model7)
fixef(model7)

#Plot of each random intercept with error around each intercept - prediction intervals for random effects
qqmath(ranef(model7, condVar = TRUE), strip = FALSE, main=T)$Inst_No
qqmath(ranef(model7, condVar = TRUE), strip = FALSE, main=T)$Subject
qqmath(ranef(model7), strip = FALSE, main = T)

qqmath(ranef(model7$"Subject:Inst_No"[[1]], postVar=TRUE))$Inst_No

#EBLUPs plots for QQ norms of random plots
qqnorm( ranef(model7)$"Subject:Inst_No"[[1]], main= "Subject within Institution", ylab = "EBLUP", xlab = "Normal Score")
qqline(ranef(model7)$"Subject:Inst_No"[[1]], col = 'red')
qqnorm( ranef(model7)$"Inst_No"[[1]], main= "Institution", ylab = "EBLUP", xlab = "Normal Score")
qqline(ranef(model7)$"Inst_No"[[1]], col = 'red')

library(lattice)
#Level 1 QQ Norm Plot
plot(resid(model7, type="pearson") ~ fitted(model7), main = "Standardized Residuals against Fitted Values", ylab = "Standardized Residuals", xlab = "Fitted Values")
abline(h = 0, lty = 2, col = 'red')
lines(lowess(resid(model7, type="pearson") ~ fitted(model7)), col = 'red')

#Create data frame for random effects to make it easier to look up outliers
M7.ranef_Inst <- ranef(model7)$Inst_No
M7.ranef_SubjInst <- ranef(model7)$Subject
M7.ranef_Inst$ID<-seq.int(nrow(M7.ranef_Inst))
M7.ranef_SubjInst$ID<-seq.int(nrow(M7.ranef_SubjInst))


#These all give the same plot of the fixed effects
identify(qqnorm(fixef(model7, level = 1)))
qqnorm(fixef(model7, level = "Inst_No"), main= "Level 2 QQ Plot")
qqnorm(fixef(model7, level = "Subj:Inst_No"), main= "Level 3 QQ Plot")


##LEVERAGE
library(influence.ME)
infl <- influence(model7, group= "Inst_No")

#Calculate Cook's distance:
cooks.distance(infl)

#Plot Cook's distance:
plot(infl, which = "cook")

