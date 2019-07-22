# MIMP Session B: Practical 1
set.seed(123)
library(mice)

# 4. Inspect the missing data pattern
md.pattern(nhanes) #get red and blue squares with variables on x axis and nr of times the pattern occurs on y axis

# 5. Regression
fit <- with(nhanes, lm(age ~ bmi)) #use with to apply function to multiple objects
summary(fit)

# 6. Mean imp
imp <- mice(nhanes, method = "mean", m = 1, maxit = 1) #impute mean once

# 7. Evaluate
complete(imp) #see first imputed data set
colMeans(nhanes, na.rm = TRUE) #check variable means again
fit <- with(imp, lm(age ~ bmi))
summary(fit) #nothing much changed
densityplot(nhanes$bmi) #there is more weight at the mean than in the plot now

# 8. Regression imp = impute the conditional mean 
imp <- mice(nhanes, method = "norm.predict", m = 1, maxit = 1) #single regression imp 

# 9. Eval
complete(imp)
fit <- with(imp, lm(age ~ bmi))
summary(fit) #there is stat sig now!

# 10. stoch regr = regr with added error term
imp <- mice(nhanes, method = "norm.nob", m = 1, maxit = 1)

# 11. Eval
complete(imp)
fit <- with(imp, lm(age ~ bmi))
summary(fit) #again not sig!

# 12. Check using set.seed argument
imp <- mice(nhanes, method = "norm.nob", m = 1, maxit = 1, seed = 123)
fit <- with(imp, lm(age ~ bmi))
summary(fit) #same!

# 13. MI
imp <- mice(nhanes) # perform default multiple imp: m=5, print = T
imp #look at imp summary
attributes(imp) # class = mids = multiply imputed data set 
imp$data # original data
imp$imp # imputations

# 14. Look at completed data
c3 <- complete(imp, 3) 
md.pattern(c3) # it is indeed complete
c.long <- complete(imp, "long") # export imputed data under each other 
c.broad <- complete(imp, "broad") # export next to each other
