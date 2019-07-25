# MIMP session N, Hanne Oberman

library(mice)
library(pan)
library(micemd)
library(miceadds)
library(lme4)
set.seed(123)

con <- url("https://gerkovink.github.io/AMA/P3-Multilevel%20imputation/Practical/popular.RData")
load(con) #or: load("popular.RData"), or: load("C:/Users/youraccount/Documents/popular.RData")
ls() #show workspace objects

# 2. data
summary(popNCR)
str(popNCR)

# 3. missingness
md.pattern(popNCR) #half of the missingness is in texp, so we exclude this
md.pattern(popNCR[, -5]) #bettah

# 4. explore popular vs popteach
missing <- is.na(popNCR$popular) #missingness indicator
histogram(~ popteach | missing, data=popNCR) #plot the distributions of teach given missing popular y/n

# 5. explore other vars
histogram(~ popteach | is.na(sex), data = popNCR)  #There seems to be a left-tailed relation between popteach and the missingness in sex.
histogram(~ popteach | is.na(extrav), data = popNCR) #There also seems to be a left-tailed relation between popteach and the missingness in extrav.
histogram(~ popteach | is.na(texp), data = popNCR) #There seems to be no observable relation between popteach and the missingness in texp. It might be MCAR or even MNAR.

# 6. other way around
histogram(~ popular | is.na(popteach), data = popNCR) #Yes: there is a dependency. The relation seems to be right-tailed.

# 7. icc
icc(aov(popular ~ as.factor(class), data = popNCR)) #using anova, what is the correlation between two pupils in the same class?
icc(aov(popteach ~ class, data = popNCR))
icc(aov(texp ~ class, data = popNCR)) #icc = 1 means that the claas explains every variation between students, they're all equal. lvl2 var?
# note: Please note that the function icc() comes from the package multilevel (function ICC1()), but is included in the workspace popular.RData. Write down the ICC's, you'll need them later.

# 9. impute
ini <- mice(popNCR, maxit = 0)
meth <- ini$meth #get methods mat
meth
meth[c(3, 5, 6, 7)] <- "norm" #replace by bayesian stoch reg
meth
pred <- ini$pred #get predictor mat
pred
pred[, "class"] <- 0 #do not use class (ml)
pred[, "pupil"] <- 0 #do not use id var
pred
imp1 <- mice(popNCR, meth = meth, pred = pred, print = FALSE) #impute with mats

# 10. compare
summary(complete(imp1)) #texp of -6 is impossible!
summary(popNCR)
# note: teacher experience increases slightly after imputation. However, texp is the same for all pupils in a class. But not all pupils have this information recorded (as if some pupils did not remember, or were not present during data collection). This is not a problem, because as long as at least one pupil in each class has teacher experience recorded, we can deductively impute the correct (i.e. true) value for every pupil in the class.

# 12. iccs
data.frame(vars = names(popNCR[c(6, 7, 5)]), #print the iccs of the incomplete vs the imputed data
           observed = c(icc(aov(popular ~ class, popNCR)), 
                        icc(aov(popteach ~ class, popNCR)), 
                        icc(aov(texp ~ class, popNCR))), 
           norm     = c(icc(aov(popular ~ class, complete(imp1))), 
                        icc(aov(popteach ~ class, complete(imp1))), 
                        icc(aov(texp ~ class, complete(imp1)))))


# 13. include lvl2 var
pred <- ini$pred
pred[, "pupil"] <- 0 #We exclude pupil here to avoid overfitting our data with the pupil identifier. Including pupil would result in a model with zero residual variance.
imp2 <- mice(popNCR, meth = meth, pred = pred, print = FALSE)

# 14. compare
data.frame(vars = names(popNCR[c(6, 7, 5)]), 
           observed  = c(icc(aov(popular ~ class, popNCR)), 
                         icc(aov(popteach ~ class, popNCR)), 
                         icc(aov(texp ~ class, popNCR))), 
           norm      = c(icc(aov(popular ~ class, complete(imp1))), 
                         icc(aov(popteach ~ class, complete(imp1))), 
                         icc(aov(texp ~ class, complete(imp1)))), 
           normclass = c(icc(aov(popular ~ class, complete(imp2))), 
                         icc(aov(popteach ~ class, complete(imp2))), 
                         icc(aov(texp ~ class, complete(imp2)))))
# note: By simply forcing the algorithm to use the class variable during estimation we adopt a fixed effects approach. This conforms to formulating seperate regression models for each class and imputing within classes from these models.

# 15. convergence
plot(imp2, c("popular", "texp", "popteach")) #the lines intermingle, but popular seems to be trending, check by adding 20 iterations
imp2b <- mice.mids(imp2, maxit = 20, print = FALSE)
plot(imp2b, c("popular", "texp", "popteach")) #it disappeared!
# note: always use mice.mids() instead of rerunning mice()

# 16. eval the imps
densityplot(imp2) ####### interpret?? #####

# 17. pmm
imp4 <- mice(popNCR[, -1], print = F) #rerum with defaults

# 18 eval
densityplot(imp4) # imputations follow the shape of the observed data (peaks).

# 20. iccs again
data.frame(vars      = names(popNCR[c(6, 7, 5)]), 
           observed  = c(icc(aov(popular ~ class, popNCR)), 
                         icc(aov(popteach ~ class, popNCR)), 
                         icc(aov(texp ~ class, popNCR))), 
           norm      = c(icc(aov(popular ~ class, complete(imp1))), 
                         icc(aov(popteach ~ class, complete(imp1))), 
                         icc(aov(texp ~ class, complete(imp1)))), 
           normclass = c(icc(aov(popular ~ class, complete(imp2))), 
                         icc(aov(popteach ~ class, complete(imp2))), 
                         icc(aov(texp ~ class, complete(imp2)))), 
           pmm       = c(icc(aov(popular ~ class, complete(imp4))), 
                         icc(aov(popteach ~ class, complete(imp4))), 
                         icc(aov(texp ~ class, complete(imp4)))), 
           orig      = c(icc(aov(popular ~ as.factor(class), popular)), 
                         icc(aov(popteach ~ as.factor(class), popular)), 
                         icc(aov(texp ~ as.factor(class), popular))))

#####

# 21. two-level normal model with heterogeneous within group variances
ini <- mice(popNCR2, maxit = 0) #get mats
pred <- ini$pred #get predictor mat
pred
pred["popular", ] <- c(0, -2, 2, 2, 2, 0, 2) #for popular var, do not use pupil, set class as class var (-2), and all others as random effect
# note: In the predictor matrix, -2 denotes the class variable, a value 1 indicates a fixed effect and a value 2 indicates a random effect. However, the currently implemented algorithm does not handle predictors that are specified as fixed effects (type = 1). When using mice.impute.2l.norm(), the current advice is to specify all predictors as random effects (type = 2).
meth <- ini$meth
meth
meth <- c("", "", "", "", "", "2l.norm", "") #replace ppm by 2l.norm
meth
imp5 <- mice(popNCR2, pred = pred, meth=meth, print = FALSE) #impute

# 22. eval
densityplot(imp5, ~popular, ylim = c(0, 0.35), xlim = c(-1.5, 10))
densityplot(imp4, ~popular, ylim = c(0, 0.35), xlim = c(-1.5, 10))# noticable difference in height of peak
plot(density(popular$popular))  #true data 
lines(density(complete(imp5)$popular), col = "red", lwd = 2)  #2l.norm
lines(density(complete(imp4)$popular), col = "green", lwd = 2)  #PMM
# note: almost overlapping :)

# 23. same but homogeneous
ini <- mice(popNCR2, maxit = 0)
pred <- ini$pred
pred["popular", ] <- c(0, -2, 2, 2, 1, 0, 2)
meth <- ini$meth
meth <- c("", "", "", "", "", "2l.pan", "")
imp6 <- mice(popNCR2, pred = pred, meth = meth, print = FALSE)
densityplot(imp6, ~popular, ylim = c(0, 0.35), xlim = c(-1.5, 10)) #looks similar too!
plot(density(popular$popular), main = "black = truth | green = PMM | red = 2l.pan")  # 
lines(density(complete(imp6)$popular), col = "red", lwd = 2)  #2l.pan
lines(density(complete(imp4)$popular), col = "green", lwd = 2)  #PMM
# same!

# 24. all different methods
ini <- mice(popNCR3, maxit = 0) #get mats
pred <- ini$pred
pred["extrav", ] <- c(0, -2, 0, 2, 2, 2, 2)  #2l.norm
pred["sex", ] <- c(0, -2, 1, 0, 1, 1, 1)  #2l.bin
pred["texp", ] <- c(0, -2, 1, 1, 0, 1, 1)  #2lonly.mean
pred["popular", ] <- c(0, -2, 2, 2, 1, 0, 2)  #2l.pan
pred["popteach", ] <- c(0, -2, 2, 2, 1, 2, 0)  #2l.lmer
meth <- ini$meth
meth <- c("", "", "2l.norm", "2l.bin", "2lonly.mean", "2l.pan", "2l.lmer")
imp7 <- mice(popNCR3, pred = pred, meth = meth, print = FALSE)
# note: error message (boundary (singular) fit see ?issingular) means the parameters are on the boundary of the feasible parameter space: variances of one or more linear combinations of effects are (close to) zero.
# 25. eval
plot(imp7) #no convergence yet?
imp7.b <- mice.mids(imp7, maxit = 20, print = FALSE) #add some it
plot(imp7.b) #see they did converge
densityplot(imp7) #see biasses due to MNAR
stripplot(imp7) #looks very reasonably

# 26. now all pmm
pmmdata <- popNCR3
pmmdata$class <- as.factor(popNCR3$class)
imp8 <- mice(pmmdata, m = 5, print = FALSE)
head(imp8$loggedEvents)
tail(imp8$loggedEvents) #texp has been excluded as a predictor in 90 instances.
# note: this is to be expected, because when class is added as a factor (categorical variable) to the model, a seperate model will be fitted for each class. In each of these models, observed texp is a constant and, hence, will automatically be removed by mice to avoid estimation problems because of a redundant parameter.
