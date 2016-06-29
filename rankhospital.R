rankhospital <- function(state, outcome, num = "best") {

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
    
    if(class(num) == "character"){
        if (! (num == "best" || num == "worst")){
            stop("invalid number")
        }
    }


        
    ## Return specified state and measure

    outcomedata <- subset(outcomedata, State == state, select=c("Hospital.Name", "State", selectedoutcome))

    ## Make results numbers
    outcomedata[,3] <- as.numeric(as.character(outcomedata[,3]))

    ## Remove NAs
    outcomedata <- na.omit(outcomedata) 

    ## Order by mortality    
    outcomedata = outcomedata[order(outcomedata[3], outcomedata$Hospital.Name),]
        
    ## Check num input
    if(num == "best"){
        num <- 1
    } else {
        if(num == "worst"){
            num <- nrow(outcomedata)
        } else {num <- as.numeric(num)}
    }
    
    ## Return specified result
    outcomedata <- outcomedata[num,]
    
    ## Return result
    outcomedata
    
}