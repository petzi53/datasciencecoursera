# 3rd part of programming assignment 3
# Ranking hospitals by outome and state
library(readr)
library(dplyr)


rankhospital <- function(state, outcome, num = "best") {
    # Rank hospitals by outcome and state
    #
    # Args:
    #   state: a two letter abbreviation of one of the US states
    #   outcome: specified mortality condition
    #       allowed are heart attack, heart failure, pneumonia
    #   num: rank of the hospital (lower is better) 
    #       allowed are additionally "best" and "worst"
    f <- read_csv("outcome-of-care-measures.csv", 
                  # n_max = 10, # for test purposes
                  na = "Not Available",
                  col_types = cols_only(
                      "Provider Number" = col_character(),
                      "Hospital Name" = col_character(),
                      "State" = col_character(),
                      "Hospital 30-Day Death (Mortality) Rates from Heart Attack"
                      = col_double(),
                      "Hospital 30-Day Death (Mortality) Rates from Heart Failure"
                      = col_double(),
                      "Hospital 30-Day Death (Mortality) Rates from Pneumonia"
                      = col_double()))
    names(f) <- c("nr", "hospital", "state", 
                  "heart attack", "heart failure", "pneumonia")
    f <- select (f, hospital, state, matches(outcome))
    f$state <- as.factor(f$state) # convert column state to factors
    # f <- na.omit(f)
    # f <- f[ order(f[[2]], f[[3]], f[[1]]), ]
    # f <- split(f, f$state)
    # mutate(f, rank = 0L)
    return(f)
}

result1 <- rankhospital("TX", "heart attack")