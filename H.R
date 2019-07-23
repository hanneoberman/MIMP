# MIMP session H by Hanne Oberman

# 1. start
library(mice) # Data imputation
library(dplyr) # Data manipulation
set.seed(123)

# 2. imputation method
meth <- make.method(mammalsleep)
meth #check current imp methods
pred <- make.predictorMatrix(mammalsleep)
pred #check current prediction matrix 
# note: in last practical we saw that ts is the sum of sws and ps and should not be imputed by others
meth["ts"]<- "~ I(sws + ps)" #passive imp of total sleep from other two (stochastic relation, not deterministical)
meth #see it's included now
pred[c("sws", "ps"), "ts"] <- 0 #now, remove these from pred matrix to avert cyclical relations
# note: This avoids circularity problems where ts would feed back into sws and ps, from which it is calculated:
pred[, "species"] <- 0 #like before (session F) do not include species
pred #see they're removed
pas.imp <- mice(mammalsleep, #impute with these matrices
                meth = meth, 
                pred = pred, 
                maxit = 10, 
                seed = 123, 
                print = F)

# 3. eval
plot(pas.imp) #better!

#######################

# 4. post-processing
meth <- make.method(boys)
meth["tv"] <- "norm"
post <- make.post(boys)
post["tv"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(1, 25))"
imp <- mice(boys, 
            meth = meth, 
            post = post, 
            print = FALSE)

# 5. compare
imp.pmm <- mice(boys, print=FALSE)
# table(complete(imp)$tv)
table(complete(imp.pmm)$tv)
densityplot(imp, ~tv)
tv <- c(complete(imp.pmm)$tv, complete(imp)$tv)
used.method <- rep(c("pmm", "norm"), each = nrow(boys))
tvm <- data.frame(tv = tv, method = used.method)
histogram( ~tv | method, data = tvm, nint = 25)

# 6.
miss <- is.na(imp$data$bmi)
xyplot(imp, bmi ~ I (wgt / (hgt / 100)^2),
       na.groups = miss, cex = c(0.8, 1.2), pch = c(1, 20),
       ylab = "BMI (kg/m2) Imputed", xlab = "BMI (kg/m2) Calculated")


# 7.
meth <- make.method(boys)
meth["bmi"] <- "~ I(wgt / (hgt / 100)^2)"
imp <- mice(boys, 
            meth = meth, 
            print=FALSE)
# 8. 
xyplot(imp, bmi ~ I(wgt / (hgt / 100)^2), na.groups = miss,
       cex = c(1, 1), pch = c(1, 20),
       ylab = "BMI (kg/m2) Imputed", xlab = "BMI (kg/m2) Calculated")
plot(imp, c("bmi"))


# 9.
pred <- make.predictorMatrix(boys)
pred[c("hgt", "wgt"), "bmi"] <- 0
pred
imp <-mice(boys, 
           meth = meth, 
           pred = pred, 
           print = FALSE)
xyplot(imp, bmi ~ I(wgt / (hgt / 100)^2), na.groups = miss,
       cex=c(1, 1), pch=c(1, 20),
       ylab="BMI (kg/m2) Imputed", xlab="BMI (kg/m2) Calculated")
plot(imp, c("bmi"))

