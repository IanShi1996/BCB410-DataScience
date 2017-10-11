# EDA-MOD-Assessment.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-MOD unit.
#
# Version:  0.1
#
# Date:     2017  10  10
# Author:   Adriel Martinez (adriel.martinez@mail.utoronto.ca)
#
# Versions:
#           0.1    (Initial)

#
# TODO: Refactor into one file
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

# Setup

if (!require(ggplot2, quietly=TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(devtools, quietly=TRUE)) {
  install.packages("devtools")
  library(devtools)
}

if (!require(here, quietly=TRUE)) {
  devtools::install_github("krlmlr/here")
  library(here)
}

# We are
here()

# Source some files
source(here("EDA-MOD-ASSESSMENT", "cost_gradients.R"))
source(here("EDA-MOD-ASSESSMENT", "gradient_descent.R"))

# = 1 Linear Regression

file.edit(here("EDA-MOD-ASSESSMENT", "linear_regression.R"))
file.edit(here("EDA-MOD-ASSESSMENT", "nonlinear_regression.R"))
file.edit(here("EDA-MOD-ASSESSMENT", "visualization_boundaries.R"))



# = 1.1 Subsection



# = 99  Task solutions


# = 99.1  Task 1: ...

# = 99.2  Task 2: ...


# [END]
