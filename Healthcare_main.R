rm(list = ls()) 
# remove working space

library(readxl)
library(leaps)
library(car)
library(MASS)

setwd("~/Desktop/Final_Project")

# PART1
##################################################################################
# All-subsets regression with exhaustive algorithm before testing correlation
# monthly
mon_data <- data.frame(read_excel("Group_Project_Data.xlsx"), sheet = 1)

mon_sec_re      <- mon_data[, 2]
mon_rf          <- mon_data[, 4]
mon_sec_exc_re  <- mon_sec_re - mon_rf

cpi <- mon_data[, 5]
dpi <- mon_data[, 6]
id  <- mon_data[, 7]
une <- mon_data[, 8]
afs <- mon_data[, 9]
per <- mon_data[, 10]
pbv <- mon_data[, 11]
pm  <- mon_data[, 12]
roe <- mon_data[, 13]
emp <- mon_data[, 14]

attach(mon_data)
mon_leaps <- regsubsets(mon_sec_exc_re ~ cpi + dpi + id + une + afs 
                     + per + pbv + pm + roe + emp, data = mon_data, 
                     nbest = 1)
summary(mon_leaps)

# plot statistic by subset size
subsets(mon_leaps,  abbrev = 2, legend = FALSE, statistic="cp", las = 1,
        main = 
          "Monthly all-subsets regression with exhaustive algorithm (bf corr)")
subsets(mon_leaps,  abbrev = 2, legend = FALSE, statistic="adjr2", las = 1,
        main = 
          "Monthly all-subsets regression with exhaustive algorithm (bf corr)")

detach(mon_data)
# selection_result: mon: dpi, per, pbv, pm.

##################################################################################
# All-subsets regression with exhaustive algorithm before testing correlation

# annually
ann_data <- data.frame(read_excel("Group_Project_Data.xlsx"), sheet = 2)

ann_sec_return <- ann_data[, 2]
ann_rf         <- ann_data[, 3]
ann_sec_exc_re <- ann_sec_return - ann_rf

cspi <- ann_data[, 4]
vc   <- ann_data[, 5]
dmee <- ann_data[, 6]
pde  <- ann_data[, 7]
pce  <- ann_data[, 8]
he   <- ann_data[, 9]
ap   <- ann_data[, 10]
ir   <- ann_data[, 11]
ce   <- ann_data[, 12]
dpi  <- ann_data[, 13]

attach(ann_data)
ann_leaps <- regsubsets(ann_sec_exc_re ~ cspi + vc + dmee + pde + pce 
                     + he + ap + ir + ce + dpi, data = ann_data, nbest = 1)
summary(ann_leaps)

subsets(ann_leaps,  abbrev = 2, legend = TRUE, statistic="cp", las = 1,
        main = 
          "Annual all-subsets regression with exhaustive algorithm (bf corr)")
subsets(ann_leaps,  abbrev = 2, min.size = 3, legend = TRUE, 
        statistic="cp", las = 1,
        main = 
          "Annual all-subsets regression with exhaustive algorithm (bf corr)")
detach(ann_data)
# selection_result: ann: cspi, dmee, ap, ir, ce.

##################################################################################
# Correlation among Variables

# monthly
mon_cormatrix <- matrix(NA, 10, 10)

rownames(mon_cormatrix) <- c("cpi", "dpi", "id", "une", "afs", 
                             "per", "pbv", "pm", "roe", "emp")
colnames(mon_cormatrix) <- c("cpi", "dpi", "id", "une", "afs", 
                               "per", "pbv", "pm", "roe", "emp")

for (i in 5:14)
{
  for (j in 5:14)
  {
    x <- mon_data[, i]
    y <- mon_data[, j]
    mon_cormatrix[i - 4, j - 4]<- cor(x, y)
  }
}
print(mon_cormatrix, digits = 3)

# mon: pm & roe are highly correlated. 
# mon: pbv & per are highly correlated.
# exclude pbv
attach(mon_data)
mon_leaps <- regsubsets(mon_sec_exc_re ~ cpi + dpi + id + une + afs 
                        + per + pm + roe + emp, data = mon_data, 
                        nbest = 1)
summary(mon_leaps)

subsets(mon_leaps,  abbrev = 2, legend = FALSE, statistic="cp", las = 1, 
        main = "Monthly all-subsets regression with exhaustive algorithm")
detach(mon_data)

# selection_result: mon: dpi, per, pbv, pm.
# selection_result_after_testing_correlation
# mon: dpi, per, roe.

# compare the result of excluding roe and pbv at the begining
attach(mon_data)
com_mon_leaps <- regsubsets(mon_sec_exc_re ~ cpi + dpi + id + une + afs 
                            + per + roe + emp, data = mon_data, 
                            nbest = 1)
summary(com_mon_leaps)

subsets(com_mon_leaps,  abbrev = 2, legend = FALSE, statistic="cp", las = 1,
        main =
        "Monthly all-subsets regression with exhaustive algorithm (exclude corr)")
detach(mon_data)
# selection_result: mon: dpi, per, roe.
# conclusion: same.

##################################################################################
# Correlation among Variables

# annually
ann_cormatrix <- matrix(NA, 10, 10)

rownames(ann_cormatrix) <- c("cspi", "vc", "dmee", "pde", "pce", "he", 
                             "ap", "ir", "ce", "dpi")
colnames(ann_cormatrix) <- c("cspi", "vc", "dmee", "pde", "pce", "he", 
                             "ap", "ir", "ce", "dpi")

for (i in 4:13)
{
  for (j in 4:13)
  {
    x <- ann_data[, i]
    y <- ann_data[, j]
    ann_cormatrix[i - 3, j - 3]<- cor(x, y)
  }
}
print(ann_cormatrix, digits = 3)

# ann: ap & ir are highly correlated.
# ann: ce & dpi are highly correlated.
# exclude ir

attach(ann_data)
ann_leaps <- regsubsets(ann_sec_exc_re ~ cspi + vc + dmee + pde + pce 
                        + he + ap + ce + dpi, data = ann_data, nbest = 1)
summary(ann_leaps)

subsets(ann_leaps,  abbrev = 2, legend = TRUE, statistic="cp", las = 1,
        main = "Annual all-subsets regression with exhaustive algorithm")
subsets(ann_leaps,  abbrev = 2, min.size = 3, legend = TRUE, 
        statistic="cp", las = 1, 
        main = "Annual all-subsets regression with exhaustive algorithm")
detach(ann_data)
# selection_result_after_testing_correlation
# ann: cspi, ap, dpi, ce
# and we exclude ce, which is identical to excluding ce and ir at the beginning

# compare the result of excluding ce and ir at the begining
attach(ann_data)
com_ann_leaps <- regsubsets(ann_sec_exc_re ~ cspi + vc + dmee + pde + pce 
                        + he + ap + dpi, data = ann_data, nbest = 1)
summary(com_ann_leaps)

subsets(com_ann_leaps,  abbrev = 2, min.size = 3, legend = TRUE, 
        statistic = "cp", las = 1,
        main =
        "Annual all-subsets regression with exhaustive algorithm (exclude corr)")
detach(ann_data)
# selection_result: ann: cspi, ap, dpi.
# conclusion: Same.

##################################################################################
# Regression: Final model

mon_rslt <- lm(mon_sec_exc_re ~ dpi + per + roe, mon_data)
ann_rslt <- lm(ann_sec_exc_re ~ cspi + dpi + ap, ann_data)

summary(mon_rslt)
summary(ann_rslt)

##################################################################################
# Regression Diagnostics

# monthly
# Test for Autocorrelated Errors
durbinWatsonTest(mon_rslt)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(mon_rslt)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(mon_rslt)

# Test Normality of Residuals
# qq plot for studentized residuals
qqPlot(mon_rslt, las = 1, main = "Mon QQ Plot")
# distribution of studentized residuals
mon_sresid <- studres(mon_rslt)

hist(mon_sresid, freq=FALSE, las = 1,
     main = "Monthly distribution of studentized residuals")

mon_xfit <- seq(min(mon_sresid), max(mon_sresid),length = 40) 
mon_yfit <- dnorm(mon_xfit) 

lines(mon_xfit, mon_yfit)

# annually
durbinWatsonTest(ann_rslt)

ncvTest(ann_rslt)
spreadLevelPlot(ann_rslt)

qqPlot(ann_rslt, las = 1, main = "Ann QQ Plot")
ann_sresid <- studres(ann_rslt)

hist(sresid, freq=FALSE, las = 1,
     main = "Annual distribution of studentized residuals")

ann_xfit <- seq(min(ann_sresid), max(ann_sresid), length = 40) 
ann_yfit <- dnorm(ann_xfit) 

lines(ann_xfit, ann_yfit)

# PART2
##################################################################################
# Prediction

# monthly
n <- dim(mon_data)[1]

t_mon_sec_re     <- mon_data[2:n, 2]
t_mon_rf         <- mon_data[2:n, 4]
t_mon_sec_exc_re <- t_mon_sec_re - t_mon_rf

t_cpi <- mon_data[1:n-1, 5]
t_dpi <- mon_data[1:n-1, 6]
t_id  <- mon_data[1:n-1, 7]
t_une <- mon_data[1:n-1, 8]
t_afs <- mon_data[1:n-1, 9]
t_per <- mon_data[1:n-1, 10]
t_pbv <- mon_data[1:n-1, 11]
t_pm  <- mon_data[1:n-1, 12]
t_roe <- mon_data[1:n-1, 13]
t_emp <- mon_data[1:n-1, 14]

attach(mon_data)
t_mon_leaps <- regsubsets(t_mon_sec_exc_re ~ t_cpi + t_dpi + t_id + t_une 
                          + t_afs + t_per + t_pbv + t_pm + t_roe + t_emp, 
                          data = mon_data, nbest = 1)
summary(t_mon_leaps)

# plot statistic by subset size
subsets(t_mon_leaps,  abbrev = 2, legend = FALSE, statistic="cp", las = 1,
        main = "Monthly all-subsets regression (prediction)")
detach(mon_data)
# mon: pm & roe are highly correlated. 
# mon: pbv & per are highly correlated.
# selection_result: mon: pbv, roe

#############################################################################
# annually
ann_data <- data.frame(read_excel("Group_Project_Data.xlsx"), sheet = 2)

m <- dim(ann_data)[1]

t_ann_sec_return <- ann_data[2:m, 2]
t_ann_rf         <- ann_data[2:m, 3]
t_ann_sec_exc_re <- t_ann_sec_return - t_ann_rf

t_cspi <- ann_data[1:m-1, 4]
t_vc   <- ann_data[1:m-1, 5]
t_dmee <- ann_data[1:m-1, 6]
t_pde  <- ann_data[1:m-1, 7]
t_pce  <- ann_data[1:m-1, 8]
t_he   <- ann_data[1:m-1, 9]
t_ap   <- ann_data[1:m-1, 10]
t_ir   <- ann_data[1:m-1, 11]
t_ce   <- ann_data[1:m-1, 12]
t_dpi  <- ann_data[1:m-1, 13]

attach(ann_data)
t_ann_leaps <- regsubsets(t_ann_sec_exc_re ~ t_cspi + t_vc + t_dmee + t_pde 
                          + t_pce + t_he + t_ap + t_ir + t_ce + t_dpi, 
                          data = ann_data, nbest = 1)
summary(t_ann_leaps)

subsets(t_ann_leaps,  abbrev = 2, min.size = 3, legend = TRUE, 
        statistic="cp", las = 1,
        main = "Annual all-subsets regression (prediction)")
detach(mon_data)
# ann: ap & ir are highly correlated.
# ann: ce & dpi are highly correlated.
# selection_result_after_corrleaiton_test: ann: cspi,ir, dpi.

##################################################################################
# Regression: Final model
t_mon_rslt <- lm(t_mon_sec_exc_re ~ t_pbv + t_roe, mon_data)
t_ann_rslt <- lm(t_ann_sec_exc_re ~ t_cspi + t_ir + t_dpi, ann_data)

summary(t_mon_rslt)
summary(t_ann_rslt)

##################################################################################





