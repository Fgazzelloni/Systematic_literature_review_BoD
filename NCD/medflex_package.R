# medflex
# https://cran.r-project.org/web/packages/medflex/vignettes/medflex.pdf

library(medflex)

ls("package:medflex")

# natural effect models

# 385 individuals related to romantic relationship characteristics
#(such as adult attachment style) and breakup characteristics
#(such as breakup initiator status,
# experiencing negative affectivity and engaging in unwanted pursuit behaviors; UPB)

data(UPBdata)

head(UPBdata)

names(UPBdata)
# anxious attachment level (attbin)
# negative affectivity (negaff)
# unwanted pursuit behavior (UPB)


mean(UPBdata$age) # 43.28312


# To control for confounding

# age (in years),
# gender and education level
# (educ; with H or ‘high’,M or ‘intermediate’ secondary school and L or ‘low’ otherwise)



# functions for 2 models:
# two steps are implemented in the functions:

# mediator model "neWeight()"
# outcome model "neImpute()"

# natural effect model "neModel()"
# linear hypoteses "neLht()"




# expanding the dataset and calculating weights can be done in a single run, using the neWeight function

medFit <- glm(negaff ~ factor(attbin) + gender + educ + age,
              family = gaussian, data = UPBdata)
# https://cran.r-project.org/web/packages/medflex/vignettes/medflex.pdf ---->glm vgm...(*)
summary(medFit)


# mediator model
expData <- neWeight(medFit)
# codes the first predictor variable in the formula argument as the exposure
# and then expands the data along hypothetical values of this variable.

head(expData, 4)
head(UPBdata)


# expanding the data and calculating regression weights for each of the replicates
expData <- neWeight(negaff ~ factor(attbin) + gender + educ + age,
                     data = UPBdata)
w <- weights(expData)
head(w, 10)


# natural effect model
neModel()
# automatically extracts the regression weights from this expanded dataset and applies them
# for model fitting
# neModel returns bootstrapped standard errors

neMod1 <- neModel(UPB ~ attbin0 + attbin1 + gender + educ + age,
                  family = binomial("logit"), expData = expData)
summary(neMod1)


# with robust standard errors based on the sandwich estimator
neMod1 <- neModel(UPB ~ attbin0 + attbin1 + gender + educ + age,
                  family = binomial("logit"), expData = expData, se = "robust")
# https://www.stat.berkeley.edu/~census/mlesan.pdf ---> se = "robust"
summary(neMod1)

exp(confint(neMod1)[c("attbin01", "attbin11"), ])

# simple logistic regression model
impFit <- glm(UPB ~ factor(attbin) + negaff + gender + educ + age,
                family = binomial("logit"), data = UPBdata)

# "neImpute" to create valid pointers to different types of predictor variables
# first expands the data along hypothetical exposure values
# then imputes the nested counter-factual outcomes by fitted values
# based on the imputation model
expData <- neImpute(impFit)

expData <- neImpute(UPB ~ factor(attbin) + negaff + gender + educ + age,
                      family = binomial("logit"), data = UPBdata)

# The outcomes are no longer binary, but are substituted by conditional mean imputations.
head(expData, 4)


neMod1 <- neModel(UPB ~ attbin0 + attbin1 + gender + educ + age,
                  family = binomial("logit"), expData = expData, se = "robust")
# Natural direct and indirect effect odds ratio estimates
summary(neMod1)


# How to fit natural effect models with multicategorical

# e weighting-based approach, models for binary, count and
# continuous mediators can be fitted using the glm function or
# the vglm function from the VGAM package (*)


head(UPBdata)

# Models for nominal mediators, on the other hand, can only be fitted using
# the vglm function (setting family = multinomial).8 Although models for ordinal mediators
# are not compatible with the neWeight function, ordered factors can easily be treated as
# nominal variables

# categorical
expData <- neImpute(UPB ~ attcat + negaff + gender + educ + age,
                      family = binomial("logit"), data = UPBdata)
head(expData)


neMod <- neModel(UPB ~ attcat0 + attcat1 + gender + educ + age,
                 family = binomial("logit"), expData = expData, se = "robust")
summary(neMod)
# summary returns estimates for the natural direct and indirect effect log odds ratios
# comparing intermediate and high anxious attachment levels
# to low levels of anxious attachment

##########################################################################

# assessment of natural effects
# requires an Anova table for the natural effect model

library("car")
# ls("package:car")


Anova(neMod)



#################################################

# defining causal effects on their most natural scale
# library(mediation) package not being used but compared
# ls("package:mediation")

# continuous variable
expData <- neImpute(UPB ~ att + negaff + gender + educ + age,
                    family = binomial("logit"), data = UPBdata, nRep = 3)
head(expData)

# hypothetical exposure levels are drawn from a linear model for the exposure

neMod1 <- neModel(UPB ~ att0 + att1 + gender + educ + age,
                  family = binomial("logit"), expData = expData, se = "robust")
summary(neMod1)




# Exposure-mediator interactions

#  Rather than reflecting the difference between total and pure direct or indirect effects,
# the mediated interaction odds ratio corresponds to the ratio of total and pure direct or indirect
# effect odds ratios.

expData <- neImpute(UPB ~ att * negaff + gender + educ + age,
                       family = binomial("logit"), data = UPBdata)
neMod2 <- neModel(UPB ~ att0 * att1 + gender + educ + age,
                      family = binomial("logit"), expData = expData, se = "robust")
summary(neMod2)
# there is no evidence for mediated interaction at the 5% significance level (p = .0541)


# Effect modification by baseline covariates

impData <- neImpute(UPB ~ (att + negaff) * gender + educ + age,
                     family = binomial("logit"), data = UPBdata)
neMod3 <- neModel(UPB ~ att0 + att1 * gender + educ + age,
                      family = binomial("logit"), expData = impData, se = "robust")
summary(neMod3)
# natural indirect effect does not differ significantly between men
# and women (p = 0.1156).


impData <- neImpute(UPB ~ (att + negaff) * educ + gender + age,
                       family = binomial("logit"), data = UPBdata)
neMod4 <- neModel(UPB ~ (att0 + att1) * educ + gender + age,
                      family = binomial("logit"), expData = impData, se = "robust")


lht <- neLht(neMod2, linfct = c("att0 + att0:att1 = 0",
                                   "att1 + att0:att1 = 0", "att0 + att1 + att0:att1 = 0"))

exp(cbind(coef(lht), confint(lht)))
summary(lht)


effdecomp <- neEffdecomp(neMod2)
summary(effdecomp)

neEffdecomp(neMod3)
neEffdecomp(neMod3, covLev = c(gender = "M"))


# Global hypothesis tests
modmed <- neLht(neMod4, linfct = c("att1:educM = 0", "att1:educH = 0"))
summary(modmed, test = Chisqtest())


# plotting---------------------------------------------------------
par(mfrow = c(1, 2))
plot(neMod2, xlab = "log odds ratio")
plot(neMod2, xlab = "odds ratio", transf = exp)


# Population-average natural effects
expFit <- glm(att ~ gender + educ + age, data = UPBdata)

impData <- neImpute(UPB ~ att + negaff + gender + educ + age,
                       family = binomial("logit"), data = UPBdata)


neMod5 <- neModel(UPB ~ att0 + att1, family = binomial("logit"),
                   expData = impData, xFit = expFit, se = "robust")
summary(neMod5)


# Intermediate confounding: A joint mediation approach

impData <- neImpute(UPB ~ att + initiator * negaff + gender + educ + age,
                       family = binomial("logit"), nMed = 2, data = UPBdata)

neMod6 <- neModel(UPB ~ att0 + att1 + gender + educ + age,
                     family = binomial("logit"), expData = impData, se = "robust")
summary(neMod6)

# Weighting or imputing?--------------------------------


library("mice")
library("mitools")

missdat <- UPBdata
for (i in 1:ncol(missdat)) {
 missdat[sample(nrow(missdat))[1:10], i] <- NA
 }

multImp <- mice(missdat, m = 10)

with(multImp,neWeight(negaff ~ factor(attbin) + gender + educ + age))

expData <- with(multImp, neWeight(negaff ~ factor(attbin) + gender + educ + age))


expData <- imputationList(expData$analyses)
neMod1 <- with(expData, neModel(UPB ~ attbin0 + attbin1 + gender
                                   + + educ + age, family = binomial("logit"), se = "robust"))

# Finally, the results can be pooled by using the MIcombine function.
MIcombine(neMod1)


























