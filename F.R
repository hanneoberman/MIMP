# MIMP session F by Hanne Oberman

library(mice) # Data imputation
library(dplyr) # Data manipulation
set.seed(123)

# 2. boys data
!is.unsorted(boys$age) #the data is not sorted; altenative: is.unsorted()
dim(boys) #dimensions are 748 rows, 9 columns

# 4. missingness
md.pattern(boys) #non-monotone

# 5. pattern
mpat <- md.pattern(boys, plot = FALSE) #store
sum(mpat[, "gen"] == 0) #count the nr of patterns with gen missing
# note: 8 patterns, but 503 cases in total (bottom row)

# 6. explore missing gen
R <- is.na(boys$gen) #missingness, not response indicator
histogram(boys$gen) #or: histogram(~ gen, data = boys)
histogram(~ age | R, data=boys) #split hist of age by missingness on gen
# note: missingness in gen is not equally distributed across age/age seems to be differently distributed for observed and missing gen

# 7. impute
imp <- mice(boys, print=FALSE)

# 8. eval
summary(complete(imp)) #get summary of first imp iteration
summary(boys) #compare with incomplete data
# note: the mean of tv is much lower in the first imputed data set, when compared to the incomplete data. makes sense because most genital measures are unobserved for the lower ages
imp %>% #with the imputed data
  with(summary(tv)) %>% #summarize the tv var
  summary() #get summary of all 5 imps
imp %>% #same with means only extracted
  with(mean(tv)) %>%
  summary()

# 9. mammalsleep data
str(mammalsleep) #see the data types vary across vars
md.pattern(mammalsleep) #only 8 types of patterns

# 10. impute
imp1 <- mice(mammalsleep, maxit = 10, print=F)
plot(imp1) #inspect trace lines for convergence

# 11. complete-data analysis
fit1 <- with(imp1, lm(sws ~ log10(bw) + odi)) 

# 12. pool
est1 <- pool(fit1)
est1 #evaluate the imputed outcomes
# note: fmi and lambda are too high, there is too little info in the data. it's because all 62 species are imputed as separate var

# 13. fix it
imp2 <- mice(mammalsleep[ , -1], maxit = 10, print = F)

# 14. analysis and pool
fit2 <- with(imp2, lm(sws ~ log10(bw) + odi))
est2 <- pool(fit2)
est2 #fmi and lambda are better now, big improvement!

# 15. eval 
plot(imp2) #eval trace lines: not intermigling
# note: convergence turns out to be a real problem because total sleep (ts) is the sum of paradoxical sleep (ps) and short wave sleep (sws).

