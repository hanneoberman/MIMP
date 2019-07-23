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
meth <- make.method(boys) #get the meth mat
meth["tv"] <- "norm" #change to bayesian stochastic regression
post <- make.post(boys) #creat post proc matrix
post["tv"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(1, 25))" #insert rules: between 1 and 25
# note: the inserted text means that for each iteration (imp{[[j]]), the value in column i should be between 1 and 25
# correction: j is the variable that is currently imputed, i is the iteration (because column in imp$imp contents)
# nb: squeeze does not censor/truncate/replace by min/max, it actually pushes the entire distribution more to the center
imp <- mice(boys, #impute
            meth = meth, 
            post = post, 
            print = FALSE)

# 5. compare
imp.pmm <- mice(boys, print=FALSE) #do regular imputation to compare
#table(complete(imp)$tv) #too large to look at
table(complete(imp.pmm)$tv) #all observed values
densityplot(imp, ~tv) #look at distributions of observed and imputed data
tv <- c(complete(imp.pmm)$tv, complete(imp)$tv) #create new object with data and method specified
used.method <- rep(c("pmm", "norm"), each = nrow(boys)) #attach method
tvm <- data.frame(tv = tv, method = used.method) #combine
histogram( ~tv | method, data = tvm, nint = 25) #see different distributions
# note: the post-processed norm is way more plausible! And has more realistic spread/distribution

# 6. ratios
miss <- is.na(imp$data$bmi) #missingness indicator
xyplot(imp, bmi ~ I (wgt / (hgt / 100)^2), #plot relation between observed and imputed bmi
       na.groups = miss, cex = c(0.8, 1.2), pch = c(1, 20),
       ylab = "BMI (kg/m2) Imputed", xlab = "BMI (kg/m2) Calculated")
# note: the relation between hgt, wgt and bmi is not preserved in the imputed values

# 7. passive imp again
meth <- make.method(boys) #get method matrix
meth["bmi"] <- "~ I(wgt / (hgt / 100)^2)" #impose relation
imp <- mice(boys, #impute, but do not fix pred matrix for circularity
            meth = meth, 
            print=FALSE)
# 8. eval
xyplot(imp, bmi ~ I(wgt / (hgt / 100)^2), na.groups = miss, #plot again
       cex = c(1, 1), pch = c(1, 20),
       ylab = "BMI (kg/m2) Imputed", xlab = "BMI (kg/m2) Calculated")
# note: now they're neatly on the line, BUT very unplausible. why?
plot(imp, c("bmi")) #terrible convergence!


# 9. fix circularity
pred <- make.predictorMatrix(boys) #now fix the pred mat
pred[c("hgt", "wgt"), "bmi"] <- 0 #remove dependencies
pred #check that it worked
imp <-mice(boys, 
           meth = meth, 
           pred = pred, 
           print = FALSE) #impute again with correct pred mat
xyplot(imp, bmi ~ I(wgt / (hgt / 100)^2), na.groups = miss, #plot again and be happy
# note: I is used to compute this separately from the rest of the model! here, the () is sufficient, but coding habits should be I() instead of just ()
      cex=c(1, 1), pch=c(1, 20),
       ylab="BMI (kg/m2) Imputed", xlab="BMI (kg/m2) Calculated")
plot(imp, c("bmi")) #yay better convergence, although trending? Increase iteration nr?
# note: according to the practical, all is well now

