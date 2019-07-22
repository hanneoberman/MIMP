# MIMP session D: Practical 2, day 1

library(mice)
set.seed(123)

# 2. Pred matrix
imp <- mice(nhanes, m = 3, print=F)
imp$pred # a 1 in the column indicates that this var was used to impute the row var
# note that the row of age is all 1s, but not in fact used (because all ages are obs)
ini <- mice(nhanes, maxit=0, print=F) # get initial pred matrix to change it
pred <- ini$pred #extract the prediction matrix
pred # still equal to above
pred <- make.predictorMatrix(nhanes) #alternative but equal result
pred
pred[, "hyp"] <- 0 #replace all 1s in the hyp column to zero: this var is not used to predict anything
pred
imp <- mice(nhanes, predictorMatrix =  pred, print = F) #use this pred mat instead
ini <- mice(nhanes, pred=quickpred(nhanes, mincor=.3), print=F) #let R determine which vars to use
ini$pred #this is different! minimal bivariate correlations are .30

# 3. Eval
imp <- mice(nhanes, print=F) #run default again
plot(imp) #check convergence across iterations
imp <- mice(nhanes, seed=123, print=F) #same with seed

# 4. Change imp method
imp$meth #default method for continuous data is pmm: predective mean matching
# note "" for age: empty string because not imputed
summary(nhanes2) #see that not all vars are continuous
str(nhanes2) #see in structure that there are 2 factors and 2 numerical vars
imp <- mice(nhanes2, print=F) #default in mice is to use check the type of var
imp$meth #it uses logreg for the categorical var hyp: logistic regression
methods(mice) #show all possible imp methods
ini <- mice(nhanes2, maxit = 0) #like pred mat, we can retrieve the initial methods
meth <- ini$meth #store in object meth
meth #same as above
meth["bmi"] <- "norm" #change bmi method to bayesian instead of pmm
meth #see norm in place of pmm
imp <- mice(nhanes2, meth = meth, print=F) #perform imp with new meth object
plot(imp) #eval the new imps, see smaller SD for bmi

# 5. Extend niter
imp40 <- mice.mids(imp, maxit=35, print=F) #add extra 35 it to get 40 in total
plot(imp40) #we want no trend in the trace lines

#6. Diagnostics
stripplot(imp, chl~.imp, pch=20, cex=2) #see distrib of the obs versus imp data
# note the gap, which is plausible under MCAR, but not MAR per se
stripplot(imp) #get all continuous var imps
# note that the bayesian norm method does not yield these gaps

# 7. Repeated analysis
fit <- with(imp, lm(bmi ~ chl)) #perform anaysis on all imps (to get mira)
fit #yields m different inferences
ls(fit) #use ls() to check what's inside this object
summary(fit$analyses[[2]]) #to get just the second imp, first subset the list and get summary after

# 8. Pool
pool.fit <- pool(fit)
summary(pool.fit) #get just 1 regression table
pool.fit #get complete analysis with reg table and inference stats like U and lambda (proportion of variance attributable to the missing data)





