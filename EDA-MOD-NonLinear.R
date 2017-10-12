# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-MOD-NonLinearRegression- unit.
#
# Version:  0.1
#
# Date:     2017  10  11
# Author:   Denitsa Vasileva (denitsa.vasileva@mail.utoronto.ca)
#           Parts of code adapted from Dr. Steipe's BCH2024/30 example
#
# Versions:
#           0.1    Has the basic underpinnings of the NonLinear Regression
#                  Unit
#
#
# TODO:   Improving Fit, Model for Data Mining...
#
#
###
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================















load("~/Downloads/BCB410-DataScience/data/myGOExSet.RData")

#Pick a gene with ID YAL021C from the myGOExSet database
#to examine its expression profile
yal021c <-t (myGOExSet["YAL021C", 5:17])

#Time is measured in minutes in 10 minute intervals
t <- seq (0,120,by = 10)

par(mar = rep(2, 4))
# plot the expression profile data for gene with ID YAL021C
plot (t, yal021c,
      xlab = "t (min.)",
      ylab = "expression log-ratio",
      col = "#DD99CC",
      type = "b"
      )

# To model the yeast gene cycle we use a cyclic function
# with parameters time, amplitude, phase and frequency
cosFun <- function(t, Ampl, Phase, Freq) {
    Ampl * (cos((((t - Phase) * 2 * pi) / 60) / Freq) )
}

#allows us to draw plots on top of existing plots
par(new=TRUE)

#a sample of the  cyclic model wih values
#of amplitude, phase and frequency which have been set
# after trial-and-error
plot (t, cosFun(t, Ampl=0.8, Phase=30, Freq=1),
      xlab = "t (min.)",
      ylab = "expression log-ratio",
      col = "#3311FF",
      type = "b"
)

#Finding the correlation between the
#plot of gene YAL021C's expression profile
#and our cyclicModel
cor (yal021c, cosFun(t, Ampl=0.8, Phase=30, Freq=1)) #cor=0.7359

# We note that a cyclic model may be more suitable for our data.
# We use the nls() function to find the best parameters.
myFit <- nls(yal021c ~ cosFun(t, Ampl, Phase, Freq),
             start = list(Ampl = 0.8,
                          Phase = 30,
                          Freq = 1.0), control = list(maxiter = 500) )

# We plot the new model obtained by myFit.
plotModel <- function(t, Ampl,Phase, Freq, thisCol = "#000000", plt = TRUE) {

    ex <- cosFun(t, Ampl, Phase, Freq)
    if (plt) {
        plot(t, ex, col = thisCol, type = "l",
             xlab = "t (min.)", ylab = "expression log-ratio",
             main = "Model",
             sub = sprintf("A: %5.3f, f: %5.3f, phi: %5.3f", Ampl, Freq, Phase)
        )
    } else {
        points(t, ex, col = thisCol, type = "l")
    }
}

plotModel(t, Ampl = coef(myFit)["Ampl"],
          Phase = coef(myFit)["Phase"],
          Freq = coef(myFit)["Freq"],
          thisCol = "#CC0000", plt = FALSE)

# Determine the correlation between the gene expression  plot
#of YAL021C and the model developed by the nls() function.

cor (yal021c, predict (myFit))

checkFit <- function(ID, fit) {
    t <- seq(0, 120, by = 10)
    y <-t (myGOExSet[ID, 5:17])
    plot(t, y, col = "red", type = "b",
         xlab = "t (min.)", ylab = "expression log-ratio",
         main = sprintf("%s: %s (%s)",
                        ID,
                        "",
                        "" ))
    mtext(sprintf("Parameters: cor: %5.3f, %s",
                  cor(y, predict(fit)),
                  paste(names(coef(fit)),
                        sprintf("%5.3f", coef(fit))
                        , sep = ": ", collapse = ", ")),
          col = "#DD99CC", side = 1, line = 4)
    t2 <- seq(0, 120, by = 1)
    y2 <- data.frame(t = t2)
    points(t2, predict(fit, newdata = y2), col = "#DD99CC", type = "l")
}
#Trying out hte function  with a different gene
# to check it works

yar007c <- t (myGOExSet["YAR007C", 5:17])
checkFit("YAR007C", myFit)

cor (yar007c, predict (myFit))

#Using nls() to calculate how well our model fits the rest of the
#expression profiles and then trying to find parameters in the
#data that may be of interest and may be useful features

NumRow <- nrow(myGOExSet)
nlsResults <- data.frame(Ampl = numeric(NumRow),
                         Phase = numeric(NumRow),
                         Freq = numeric(NumRow),
                         cor = numeric(NumRow))
for (i in 1:NumRow) {


    y <- t (myGOExSet[1,5:17])

    try(myFit <- nls(y ~ cosFun(t, Ampl, Phase,Freq),
                     start = list(Ampl = 0.8,
                                  Phase = 30,
                                  Freq = 1.0) ), silent = TRUE)

    if (length(myFit) > 0) {
        nlsResults$Ampl[i] <- coef(myFit)["Ampl"]
        nlsResults$Phase[i] <- coef(myFit)["Phase"]
        nlsResults$Freq[i] <- coef(myFit)["Freq"]
        nlsResults$cor[i] <- cor(y, predict(myFit))
    }
}
#Plots points with high amplitude and moderate to high correlation
plot(nlsResults$Ampl, nlsResults$cor)
( sel <- which(nlsResults$Ampl > 0.05 & nlsResults$cor > 0.3) )

#Still To do:
# Improving Fit
# Model for Data Mining
#END
