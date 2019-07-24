# MIMP session L, Hanne Oberman
load(url("https://www.gerkovink.com/mimp/leiden.rda"))

# 1. set-up
set.seed(123)
library("mice")
library("survival")

# 2. the data
summary(leiden)
str(leiden) #all numerical, but should they be?

# 3. get mice objects
ini <- mice(leiden, maxit = 0) #initial data (incomplete)
ini$nmis #nr of missings per var

# 4. missingness
md.pattern(leiden) #non-monotone
fx <- fluxplot(leiden) #The lower (bottom) variables have an outflux with 0.5 or lower, so their predictive power is limited. Also, this group has a higher influx, and, thus, depend more highly on the imputation model.
# note: Variables with higher outflux are (potentially) the more powerful predictors. Variables with higher influx depend stronger on the imputation model. When points are relatively close to the diagonal, it indicates that influx and outflux are balanced.
fx #same but values in table

# 5. inspect kaplan meier curves
km <- survfit(Surv(survda/365, 1-dwa) ~ is.na(rrsyst), data = leiden) 
plot(km, 
     lty  = 1, 
     lwd  = 1.5, 
     xlab = "Years since intake",
     ylab = "K-M Survival probability", las=1, 
     col  = c(mdc(4), mdc(5)), 
     mark.time = FALSE)
text(4, 0.7, "BP measured")
text(2, 0.3, "BP missing")

# 6. adjust for MAR and MNAR
delta <- c(0, -5, -10, -15, -20) #MAR=0, MNAR=others

# 7. impute
imp.all <- list() #empty list to store stuff in
post <- ini$post #get post proc matrix
for (i in 1:length(delta)){ #for each of the adjustments
  d <- delta[i] #get the adj value
  cmd <- paste("imp[[j]][,i] <- imp[[j]][,i] +", d) #post-proc the cmd var
  post["rrsyst"] <- cmd #update post mat
  imp <- mice(leiden, post = post, maxit = 5, seed = i, print = FALSE) #impute with new mat
  imp.all[[i]] <- imp #put in the list
} #should yield a list with 5 adj options, each with 5 imps

# 8. eval
bwplot(imp.all[[1]]) #box+whisker plots of adj 1 (MAR) for all var
bwplot(imp.all[[5]]) #same with extreme MNAR
# note: We can clearly see that the adjustment has an effect on the imputations for rrsyst and, thus, on those for rrdiast

# 9. eval agian
densityplot(imp.all[[1]], lwd = 2) #MAR
densityplot(imp.all[[5]], lwd = 2) #extreme MNAR
# note: We can once more clearly see that the adjustment has an effect on the imputations for rrsyst and, thus, on those for rrdiast.

# 10. eval per imp
xyplot(imp.all[[1]], rrsyst ~ rrdiast | .imp) #MAR
xyplot(imp.all[[5]], rrsyst ~ rrdiast | .imp) #extr MNAR
# note: The scatter plot comparison between rrsyst and rrdiast shows us that the adjustment has an effect on the imputations and that the imputations are lower for the situation where delta=-20.
cda <- expression( #create expression to automate steps:
  sbpgp <- cut(rrsyst, breaks = c(50, 124, 144, 164, 184, 200, 500)), #Create two categorical variables sbpgp and agegp that divide the observations into groups based on, respectively, systolic blood pressure and age.
  agegp <- cut(lftanam, breaks = c(85, 90, 95, 110)),
  dead  <- 1 - dwa, #Calculate whether person died or not.
  coxph(Surv(survda, dead) #Fit a Cox proportional hazards model to estimate the relative mortality risk corrected for sex and age group.
        ~ C(sbpgp, contr.treatment(6, base = 3)) 
        + strata(sexe, agegp)))

# 11. apply cda shortcut on every adj
fit1 <- with(imp.all[[1]], cda)
fit2 <- with(imp.all[[2]], cda)
fit3 <- with(imp.all[[3]], cda)
fit4 <- with(imp.all[[4]], cda)
fit5 <- with(imp.all[[5]], cda)
summary(fit1) #there are 5x5 rows, for all vars and imps

# 12. pool
summary(pool(fit1)) #see that the imps are combined now, var are not
# note: ignore errors
r1 <- as.vector(t(exp(summary(pool(fit1))[, c(1)]))) #store estimates
r2 <- as.vector(t(exp(summary(pool(fit2))[, c(1)])))
r3 <- as.vector(t(exp(summary(pool(fit3))[, c(1)])))
r4 <- as.vector(t(exp(summary(pool(fit4))[, c(1)])))
r5 <- as.vector(t(exp(summary(pool(fit5))[, c(1)])))
pars <- round(t(matrix(c(r1,r2,r3,r4,r5), nrow = 5)),2) #show in table
pars <- pars[, c(1, 2, 5)]
dimnames(pars) <- list(delta, c("<125", "125-140", ">200"))
pars
# note: All in all, it seems that even big changes to the imputations (e.g. deducting 20 mmHg) has little influence on the results. This suggests that the results are stable relatively to this type of MNAR-mechanism.

# 13. mammalsleep sensitivity
delta <- c(8, 6, 4, 2, 0, -2, -4, -6, -8) #adjustments
meth<- make.method(mammalsleep) #get methods mat
meth["ts"]<- "~ I(sws + ps)" #again, total sleep is a function of the two others
pred <- make.predictorMatrix(mammalsleep) #avoid circularity
pred[c("sws", "ps"), "ts"] <- 0
pred[, "species"] <- 0 #remove the 62 species vars
post <- make.post(mammalsleep) #get post-proc mat
imputations <- list() #create obj
for (i in 1:length(delta)) {
  d <- delta[i]
  post["sws"] <- cmd
  cmd <- paste("imp[[j]][, i] <- imp[[j]][, i] +", d)
  imputations[[i]]  <- mice(mammalsleep, 
                            meth=meth, 
                            pred=pred, 
                            post = post, 
                            maxit = 10, 
                            seed = i * 22, 
                            print = FALSE)
} #get list with 9 different adjustments, 5 imps each
output <- sapply(imputations, function(x) pool(with(x, lm(sws ~ log10(bw) + odi)))$pooled$estimate) #pool
result <- cbind(delta, as.data.frame(t(output)))#get pooled est per adj value
colnames(result) <- c("delta", "Intercept", "log10(bw)", "odi") #create readible table
result #see what MNAR does to the estimate
# note: A clear trend for the estimates for the intercept and for  bw emerges. Thus, the results are not essentially the same under all specified mechanisms and the outcomes can be deemed sensitive to the assumed mechanism
summary(mammalsleep$sws) #see why: the adj were unrealistic compared to the distr of sws

