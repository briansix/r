best <- function(state, outcome) {

    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv")

    ## Check that state and outcome are valid

    if(outcome == "heart attack") {
        selectedoutcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        outcol <- 11
    } else {
        if(outcome == "heart failure"){
            selectedoutcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
            outcol <- 17
        } else {
            if(outcome == "pneumonia") {
                selectedoutcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
                outcol <- 23
                
            } else {stop("invalid outcome")
            }
        }
    }
    
    if(! state %in% outcomedata$State){
        stop("invalid state")
    }
    
    
    ## Return hospital name in that state with lowest 30-day death
    outcomedata <- subset(outcomedata, State == state, select=c("Hospital.Name", "State", selectedoutcome))
    # remove NAs
    outcomedata <- na.omit(outcomedata)  
    # make results numbers
    outcomedata[,3] <- as.numeric(as.character(outcomedata[,3]))
    # return one result
    outcomedata <- outcomedata[which.min(outcomedata[, 3]), "Hospital.Name"]
    outcomedata
    
}
