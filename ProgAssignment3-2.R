## TEXT


best <- function(state, outcome) {
    ## EXPLAIN ARGS
    my_msg <- NULL
    my_msg <- checkArgs(state, outcome)
        if (!is.null(my_msg)) {
        return(message(my_msg))
        }
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
    f <- na.omit(f)
    f <- f[ order(f[[2]], f[[3]], f[[1]]), ]
    f <- split(f, f$state == state)
    f <- f[[2]][[c(1,1)]]
    print(f)
    return(f)
}

checkArgs <- function(s="XX", o="Herz") {
    myState <- s
    myOutcome <- o
    msg <- NULL
    outcome <- c("heart attack", "heart failure", "pneumonia")
    if (!(myState %in% state.abb)) {
        msg <- paste0("\"Error in best(\"", myState, "\", \"", myOutcome, "\") : invalide state\"")
    }   else if (!(myOutcome %in% outcome)) {
        msg <- paste0("\"Error in best(\"", myState, "\", \"", myOutcome, "\") : invalide outcome\"")
    }
    return(msg)
} 


# m <- checkArgs("BB", "heart failure")

result1 <- best("TX", "heart attack")
result2 <- best("TX", "heart failure")
result3 <- best("MD", "heart attack")
result4 <- best("MD", "pneumonia")
result5 <- best("BB", "heart attack")
result6 <- best("NY", "hert attack")




