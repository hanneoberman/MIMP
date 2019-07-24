# MIMP session J, Hanne Oberman

# 1. set-up
library(mice) # Data imputation
library(dplyr) # Data manipulation
library(magrittr) # Flexible piping in R
library(purrr) # Flexible functional programming
set.seed(123)

# 2. impute
meth <- make.method(boys) #adapt bmi again
meth["bmi"] <- "~ I(wgt / (hgt / 100)^2)" #bmi as a function of height and weight
pred <- make.predictorMatrix(boys)
pred[c("hgt", "wgt"), "bmi"] <- 0
pred #check if it worked
imp <-mice(boys, #impute 10x
           meth = meth, 
           pred = pred, 
           print = FALSE, 
           m = 10)

# 3. correl
ave <- imp %>% #take the imputed data
  mice::complete("long") %>% #extract the imps under each other
  group_by(.id) %>% #for each participant
  summarise_all(.funs = mean) %>% #get the means
  select(-.id, -.imp, -phb, -gen, -reg) #extract only numerical vars
head(ave) #see that there is only 1 value per var per person now
cor.wrong <- ave %>% #compute correlations
  cor() %>%
  round(digits = 2)
cor.wrong #quite high!
# note: we need to transform and back-transform the estimates!
fisher.trans <- function(x) 1/2 * log((1 + x) / (1 - x)) #create formulas
fisher.backtrans <- function(x) (exp(2 * x) - 1) / (exp(2 * x) + 1)
cor <- imp %>% #apply formulas
  mice::complete("all") %>%
  map(select, -phb, -gen, -reg) %>%  
  # note: map = which function to apply to the dataset
  map(cor) %>%
  map(fisher.trans)
cor
cor.rect <- Reduce("+", cor) / length(cor) # m is equal to the length of the list
cor.rect <- fisher.backtrans(cor.rect)
cor.rect <- round(cor.rect, digits = 2)
diag(cor.rect) <- 1
cor.rect
all.equal(cor.rect, cor.wrong) #in conclusion, the averaged estimates have an upward bias

# 4. lm
fit1.lm <- imp %>% #fit lm model on mids object to get mira
  with(lm(age ~ wgt + hgt))
est1.lm <- pool(fit1.lm) #pool mira to get mipo
est1.lm
summary(est1.lm) #sooooo significant

# expand model
fit2.lm <- imp %>% #add hgt squared, get mira
  with(lm(age ~ wgt + hgt + I(hgt^2)))
est2.lm <- pool(fit2.lm) #get mipo
est2.lm
summary(est2.lm) #still all sig
# note: we do not know whether this is sig better

# 6. compare
D1(fit2.lm, fit1.lm) # multivariate Wald test, see https://stefvanbuuren.name/fimd/sec-multiparameter.html
D2(fit2.lm, fit1.lm) # combining test statistics
D3(fit2.lm, fit1.lm) # likelihood ratio test
############# look at this still! #################

# 7. step
scope <- list(upper = ~ age + wgt + hc + gen + phb + tv + reg,
              lower = ~ 1) #argument for step-function: which vars to consider
# note: lower argument specifies the lower bound of the mode, where a 1 indicates an intercept only model.
expr <- expression(f1 <- lm(hgt ~ 1), 
# note: f1 is the linear model to be evaluated
                  f2 <- step(f1, #f2 the step() function that evaluates the f1 function
                              scope = scope, 
                              direction = "forward",
                              trace = 0
                   ))
fit <- with(imp, expr) #get mira with function scope
# note: we can evaluate how often each variable was selected by running:
formulas <- lapply(fit$analyses, formula)
terms <- lapply(formulas, terms)
votes <- unlist(lapply(terms, labels))
table(votes) #some were always selected, some not often. Run multivar Wald to be sure
fit.gen <- with(imp, lm(hgt ~ age + hc + phb + wgt + gen))
fit.nogen <- with(imp, lm(hgt ~ age + hc + phb + wgt))
D1(fit.gen, fit.nogen) #not sig, we can omit gen
BIC.gen <- fit.gen$analyses %>% #verify with IC
  sapply(BIC) 
BIC.nogen <- fit.nogen$analyses %>%
  sapply(BIC) 
sum(BIC.gen > BIC.nogen) #out of 10 imputations, the IC for the model with gen was greater 8x. Ergo, rm gen

# 8. condition
imp %>%
  mice::complete("long") %>% #get imps
  select(reg, bmi) %>% #extract the 2 vars
  group_by(reg) %>% #sort by reg
  summarise_all(.funs = mean) #get bmi per reg

# 9. anova
aov.empty <- lapply(with(imp, lm(age ~ 1))$analyses, aov) #perform anova on intercept-only
aov.reg <- lapply(with(imp, lm(age ~ 1 + reg))$analyses, aov) #include reg
lapply(aov.empty, summary) #summarize results
lapply(aov.reg, summary)
fit.empty <- imp %>% #to get overall est of the anova, fit intercept only model
  mice::complete("all") %>%
  map(lm, formula = bmi ~ 1) #get mira
fit.reg <- imp %>%
  mice::complete("all") %>% #fit model with reg
  map(lm, formula = bmi ~ 1 + reg) #get mira
D1(fit.reg, fit.empty) #test (also sig)
