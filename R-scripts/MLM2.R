##MODEL BUILDING
##STEP 1 - fit "unconditional" model and decide whether to omit random subject effects
model1.1 <- lme(OS ~ 1, data = df_agree, random = ~ 1|Inst_No/Subject, method = "REML")
model1.2 <- lme(OS ~ 1, data = df_agree, random = ~ 1| Inst_No, method = "REML")
anova(model1.1, model1.2)
#highly significant difference - keep nested random effects associated with Subject, i.e. choose model1.1
random.effects(model1.2)

##STEP 2 - Adding Level 1 covariates and testing the significance of their fixed effects
model2.1 <- lme(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21, data = df_agree, random = ~1 | Inst_No/Subject, method = "REML")
#Test hypothesis that fixed effects associated with Level 1 covariates in model2.1 are 0. 
#To do, refit this and model1.1 using ML estimation (necessary by Pinheiro & Bates, 2000; Bolker et al., 2009)
#Optimisation algorithm used on 1.2 to solve non-convergence issues
model1.1.ml <- lme(OS ~ 1, data = df_agree, random = ~ 1| Inst_No/Subject, method = "ML", control=lmeControl(opt = "optim"))
model2.1.ml <- lme(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21, data = df_agree, random = ~1 | Inst_No/Subject, method = "ML")
anova(model1.1.ml, model2.1.ml)
#p value highly significant, therefore at least one of the fixed effects associated with level 1 covariates
  #is signigifant. Therefore we keep model2.1 as preferred model. 

#Now there are no more covariates to add at any level so we have added everything we can. 
#All we could possibly do is backward selection of our level 1 fixed effects since there are so many of them. 
#Possible varbs to remove: Q5 (p = 1.587), Q7 (p = 0.410), Q12 (p = 0.4556)
summary(model2.1)


##Now remodel using lmer so that I can use another package to do backward selection
lmer.model.1 <- lmer(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21 + (1 + Inst_No|Subject), data = df_agree)
summary(lmer.model.1)




