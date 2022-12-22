library(haven)
library(car)

data_raw = haven::read_sav("ESS9e03_1.sav")
data = data_raw[data_raw$cntry == "FR", c("vteurmmb", "agea", "imwbcnt", "frprtpl", "hincfel", "prtclffr")]

# drop missing values for EU stance, recode to 0-1
cases_before = length(data$vteurmmb)
data = data[(data$vteurmmb == 1 | data$vteurmmb == 2),]
data = data[!is.na(data$vteurmmb),]
diff = cases_before - length(data$vteurmmb)
data[data$vteurmmb == 1, "vteurmmb"] = 0
data[data$vteurmmb == 2, "vteurmmb"] = 1

# re-code missing values for all other variables, recode missing cases in party identification to "other" for the base case
data$agea[is.na(data$agea)] = mean(data$agea, na.rm = TRUE)
data$imwbcnt[is.na(data$imwbcnt)] = mean(data$imwbcnt, na.rm = TRUE)
data$frprtpl[is.na(data$frprtpl)] = mean(data$frprtpl, na.rm = TRUE)
data$hincfel[is.na(data$hincfel)] = mean(data$hincfel, na.rm = TRUE)
data$prtclffr[is.na(data$prtclffr)] = 12

# rename columns
names(data)[names(data) == "vteurmmb"] = "frexit"
names(data)[names(data) == "agea"] = "age"
names(data)[names(data) == "imwbcnt"] = "immigrants"
names(data)[names(data) == "frprtpl"] = "pol_trust"
names(data)[names(data) == "hincfel"] = "econ_difficulty"
names(data)[names(data) == "prtclffr"] = "party"

# create dummies for party identification
data$LO = ifelse(data$party == 1, 1, 0)
data$NPA = ifelse(data$party == 2, 1, 0)
data$PCF = ifelse(data$party == 3, 1, 0)
data$LFI = ifelse(data$party == 4, 1, 0)
data$PS = ifelse(data$party == 5, 1, 0)
data$EELV = ifelse(data$party == 6, 1, 0)
data$LREM = ifelse(data$party == 7, 1, 0)
data$MODEM = ifelse(data$party == 8, 1, 0)
data$LR = ifelse(data$party == 9, 1, 0)
data$DLF = ifelse(data$party == 10, 1, 0)
data$FN = ifelse(data$party == 11, 1, 0)

summary(data)

# Getting info on the standard deviation (removed because they cluttered my script):
# sd(data$var) for each of the variables

# game on
attach(data)

model = glm(frexit ~ age + immigrants + pol_trust + econ_difficulty +
              LO + NPA + PCF + LFI + PS + EELV + LREM + MODEM + LR + DLF + FN,
            family = binomial, data = data)
summary(model)

model2 = glm(frexit ~ age + immigrants + pol_trust + econ_difficulty +
              LO + NPA + PCF + LFI + PS + EELV + LREM + MODEM + LR + DLF + FN + FN:pol_trust,
            family = binomial, data = data)
summary(model2)

# calculate error reduction (code shamelessly adapted from the lecture example script)

  # estimated selection probability
model.prob=predict(model,type="response")
  # dichotomize using 0.5 as cut-off
model.pred=ifelse(model.prob>0.5,1,0)
  # table of predicted and observed selections
table(model.pred,frexit)
  # average of correct predictions
mean(model.pred==frexit)

  # Reduction of Prediction Error
error.base <- 100 * (min(mean(frexit),(1-mean(frexit)))); error.base
error.model <- 100 * (1-mean(model.pred==frexit)); error.model
error.reduction <- 100 * ((error.base - error.model) / error.base); error.reduction

# Averaged value prediction model (only one left to declutter script)
b = coef(model)

# Lower expected probability:
LEP <- plogis(b[1] + b[2]*mean(age) + b[3]*mean(immigrants) + b[4]*mean(pol_trust) +
                b[5]*mean(econ_difficulty))

# Upper expected probability:
UEP <- plogis(b[1] + b[2]*mean(age) + b[3]*mean(immigrants) + b[4]*mean(pol_trust) +
                b[5]*mean(econ_difficulty) + b[16])

# First difference (percentage points):
cat("LEP:", round(LEP*100, 2), "% | UEP:", round(UEP*100, 2), "% | First difference:", 
    round((UEP-LEP)*100,2), "percentage points\n")

# Plots for model 2
b = coef(model2)

# Plot effect of trust in political system for base case
curve(plogis(b[1] + b[2]*mean(age) + b[3]*mean(immigrants) + b[4]*pol_trust + 
        b[5]*mean(econ_difficulty) + b[16]*min(FN) +
        b[17]*pol_trust*min(FN)),
      xlim=c(1,5), ylim=c(0,1), xname="pol_trust", xlab="Trust in the political system", 
      ylab="Predicted probability of voting to leave EU", col="red", lwd=2)

# Plot effect of trust in political system for FN supporter
curve(plogis(b[1] + b[2]*mean(age) + b[3]*mean(immigrants) + b[4]*pol_trust + 
        b[5]*mean(econ_difficulty) + b[16]*max(FN) +
        b[17]*pol_trust*max(FN)),
      xlim=c(1,5), ylim=c(-3,0), xname="pol_trust", xlab="Trust in the political system", 
      ylab="Predicted probability of voting to leave EU", col="blue", lwd=2, add=TRUE)
text(2, 0.1, "Base case", col="red")
text(2, 0.5, "FN supporter", col="blue")


# Testing for assumption violations

# Does not work, as log(0) is undefined
# model3 = glm(frexit ~ age + log(age) + immigrants + log(immigrants) + pol_trust + log(pol_trust) + econ_difficulty + log(econ_difficulty) +
#               LO + NPA + PCF + LFI + PS + EELV + LREM + MODEM + LR + DLF + FN,
#             family = binomial, data = data)

car::vif(model)
plot(model)

# Stolen from the example script
logit.checks <- function(model) {
  cat("\n--------- Residual Diagnostics for Multivariate Logit Models ---------- \n \n")
  # proportion of large absolute standardized residuals
  large.residual.2 <- rstandard(model) > 2 | rstandard(model) < -2
  large.residual.25 <- rstandard(model) > 2.5 | rstandard(model) < -2.5
  large.residual.3 <- rstandard(model) > 3 | rstandard(model) < -3
  ASR1 <- round((sum(large.residual.2) / nrow(model$model))*100, 2)
  ASR2 <- round((sum(large.residual.25) / nrow(model$model))*100, 2)
  ASR3 <- round((sum(large.residual.3) / nrow(model$model))*100, 2)
  if(ASR1 <= 5) { test1 <- "passed"
  } else test1 <- "failed"
  if(ASR2 <= 1) { test2 <- "passed"
  } else test2 <- "failed"
  if(ASR3 <= 0) { test3 <- "passed"
  } else test3 <- "failed"
  cat("Proportion of large absolute standardized residuals (ASR): \n")
  cat("    Criterion ASR>2.0 (<5%):", round(sum(large.residual.2),4),"case(s),", ASR1, "% | Check", test1, "\n")
  cat("    Criterion ASR>2.5 (<1%):", round(sum(large.residual.25),4),"case(s),", ASR2, "% | Check", test2, "\n")
  cat("    Criterion ASR>3.0 (=0%):", round(sum(large.residual.3),4),"case(s),", ASR3, "% | Check", test3, "\n")
  # leverage/hat value
  AL1.crit <- model$rank / nrow(model$model)
  AL2.crit <- 2 * model$rank / nrow(model$model)
  AL3.crit <- 3 * model$rank / nrow(model$model)
  AL1 <- hatvalues(model) > AL1.crit
  AL2 <- hatvalues(model) > AL2.crit
  AL3 <- hatvalues(model) > AL3.crit
  if(max(hatvalues(model)) < AL1.crit) { test1 <- "passed"
  } else test1 <- sprintf("failed for %d case(s)", sum(AL1))
  if(max(hatvalues(model)) < AL2.crit) { test2 <- "passed"
  } else test2 <- sprintf("failed for %d case(s)", sum(AL2))
  if(max(hatvalues(model)) < AL3.crit) { test3 <- "passed"
  } else test3 <- sprintf("failed for %d case(s)", sum(AL3))
  cat("Leverage / Hat Values: \n")
  cat("    Largest observed hat value =", round(max(hatvalues(model)),4), "\n")
  cat("    Criterion  k/n:", round(AL1.crit,4), "| Check", test1, "\n")
  cat("    Criterion 2k/n:", round(AL2.crit,4), "| Check", test2, "\n")
  cat("    Criterion 3k/n:", round(AL3.crit,4), "| Check", test3, "\n")
  # difference statistics (DFBeta)
  dfbeta.lim <- 2/sqrt(nrow(model$model))
  dfbeta <- abs(dfbeta(model)) > dfbeta.lim
  if(max(abs(dfbeta(model))) < dfbeta.lim) { test1 <- "passed"
  } else test1 <- sprintf("failed for %d case(s)", sum(dfbeta))
  cat("Difference Statistics (for excluded cases): \n")
  cat("    DFBeta (across all coefficients): \n")
  cat("      Largest observed absolute DFBeta =", round(max(abs(dfbeta(model))),4), "\n")
  cat("      Criterion 2/sqrt(n):", round(dfbeta.lim, 4), "| Check", test1, "\n")
  dffit.lim <- 2*sqrt(model$rank/nrow(model$model))
  cat("\n------------------------------------------------------------------------ \n")
}
logit.checks(model)
logit.checks(model2)

