library(car)
library(nlme)
library(ggplot2)
library(reshape)

#use GLS because MLM & GLS use maximum likelihood methods, where as lm() uses ordinary least squares so can't be
#compared
interceptOnly <- gls(OS ~ 1, data = scDF, method = "ML")
summary(interceptOnly)
randomInterceptOnly <- lme(OS ~ 1, data = scDF, random = ~1|Institution, method = "ML")
summary(randomInterceptOnly)
anova(interceptOnly, randomInterceptOnly)

#Now consider if we need the 3 level structure
#Model 1 - the ML fit wouldn't work (false convergence error) so used optimisation algorthim to get it to work in 'control' option
model1.MLfit <- lme(OS ~ 1, data = scDF, random = ~1|Institution/Subject, method = "ML", control=lmeControl(opt = "optim"))

summary(model1.MLfit)

anova(randomInterceptOnly, model1.MLfit)
#highly significant which means that the nesting structure is justified. 


model.2.MLfit <- lme(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21, data = scDF, random = ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21|Institution/Subject, method = "ML")
anova(model.2.MLfit, model1.MLfit)
#highly significant
summary(model.2.MLfit)

model.2.MLfit <- lme(OS ~ 1, data = scDF, random = ~1|Institution/Subject, method = "ML")


##outliers 
mahal = mahalanobis(scDF_vals, colMeans(scDF_vals), cov(scDF_vals))
summary(mahal)
cutoff =qchisq(1-.001, ncol(scDF_vals)) 

summary(mahal < cutoff)
model1 <- lme(OS ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21, data = scDF, random = ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12 + Q13 + Q14 + Q15 + Q16 + Q17 + Q18 + Q19 + Q20 + Q21|Institution/Subject, method = "ML")


