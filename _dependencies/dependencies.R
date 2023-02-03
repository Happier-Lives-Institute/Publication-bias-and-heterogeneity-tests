#---------
# Prep
#---------

# Clean
rm(list=ls())
if (!is.null(dev.list())) dev.off()

# prepare a seed for simulations
set.seed(2022)

# Set number of runs for our Monte Carlo simulations
MCruns <- 10000

#---------
# Libraries
#---------

# If you don't have a package, this will install it...
if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  cowplot, # for plots
  devtools,# in case you need to install the next package...
  dmetar,  # may need to use devtools::install_github("MathiasHarrer/dmetar")
  googlesheets4, # for loading from googlesheets. Should become obsolete in future version.
  ggpattern, # for patterns within bars of ggplots
  lazyeval, # for own functions
  magrittr, # For pipe operator %<>%
  meta,     # for metagen
  metafor,  # for meta-analytic averages, metaregressions and forest plots
  metasens, # for limit meta-analysis
  tidyverse# For many helpful packages. Like a language within R.
)
# If a package doesn't load, you may need to update your version of R.

# get rid of summarise messages
options(dplyr.summarise.inform = FALSE)

#---------
# Commands
#---------

# Not in
'%ni%' <- Negate('%in%')

#---------
# Functions
#---------

# Get number of unique instances
uniqueN <- function(x) {
  return (length(unique(x)))
}

# Get standard error
se <- function(x) sd(x)/sqrt(length(x))

# Get weighted SD
wtd.sd <- function(x, wt, na.rm = F) {
  sqrt(Hmisc::wtd.var(x, wt, na.rm))
}

# Get pooled SD
getPooledSD <- function(n1, sd1, n2, sd2){
  return(
    abs(
      sqrt(
        (((n1-1)*(sd1^2))+((n2-1)*(sd2^2))) / (n1 + n2 - 2)
      )
    )
  )
}

# Get Cohen's D
getCohenD <- function(m1, m2, pooledSD){
  return(
    (m1 - m2) / pooledSD
  )
}

# Get standard error of Cohen's D
# Done this way in metaregression R textbook
getDSE <- function(d, n1, n2){
  return(
    sqrt(((n1 + n2)/ (n1*n2)) + ((d^2) / (2*(n1 + n2))))
  )
}

# # Get error of the ratio
getRatioSE <- function(d.x, d_se.x, d.y, d_se.y){
  return(
    sqrt(
      (((d.x^2)*(d_se.x^2)) +
         ((d.y^2)*(d_se.y^2))) / (d.x^2)
    )
  )
}

# Round number to string, fills in missing digits (decimals) with 0s
round.c <- function(x, digits) {
  roundedElement <- round(x, digits)
  roundedElement.char <- as.character(roundedElement)

  # If digits are more than 0
  if(digits > 0) {
    # If no decimals
    if(roundedElement %% 1 == 0) {
      roundedElement.char <- paste0(roundedElement.char, ".")
      for (i in 1:digits) {
        roundedElement.char <- paste0(roundedElement.char, "0")
      }
    } else {
      ndecimals <- nchar(strsplit(roundedElement.char, ".", fixed = T)[[1]][2])
      diffToDigits <- digits - ndecimals

      if(diffToDigits > 0) {
        for (i in 1:diffToDigits) {
          roundedElement.char <- paste0(roundedElement.char, "0")
        }
      }
    }
  }

  return(roundedElement.char)
}