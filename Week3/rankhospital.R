rankhospital <- function(state, outcome, num = "best") {
	##
	## NOTE: This is a protype version that does not use apply() functions. 
	##
	
	## Read outcome data
	f <- "./Wk3Data/outcome-of-care-measures.csv"
	outcomeData <- read.csv(f, colClasses="character")

	## Check that state and outcome are valid
	bStateValid <- FALSE
	bOutcomeValid <- FALSE
	
	# Get a list of states, column 7 in data
	states <- outcomeData[,7]
	for (aState in states) {
		if (state == aState) {
			bStateValid <- TRUE
			break
		}
	}
	
	# Get a list of valid outcomes, column 7 in data 
	#  Requirements do not specify limit to three, but doing so like previous part.
	validOutcomes <- c("heart attack", "heart failure", "pneumonia")
	outcomeColumns <- c(11,17,23)
	idx<- 1
	for (aOut in validOutcomes) {
		if (outcome == aOut) {
			bOutcomeValid <- TRUE
			targetColumn <- outcomeColumns[idx]
			break
		}
		idx <- idx + 1
	}

	# if state or outcome not valid, leave with specified error mesage
	if (!bStateValid) stop("invalid state")
	if (!bOutcomeValid) stop("invalid outcome")
	
	## Return hospital name in that state with the given rank
	## 30-day death rate
	
	# build a data frame to hold hospital and targetValue
	targetHospitals <- data.frame(Hospital.Name=1, TargetVal=1)
	idx <- 1
	for (x in 1:nrow(outcomeData)) {

		# skip this entry if no value was provided
		if (is.na(outcomeData[x,targetColumn])) next
		if (is.nan(outcomeData[x,targetColumn])) next
		if (outcomeData[x,targetColumn] == "Not Available") next

		testVal <- as.numeric(outcomeData[x,targetColumn])
		if((outcomeData[x,7] == state)) { 
				targetHospitals[idx,1] <- outcomeData[x,2]
				targetHospitals[idx,2] <- testVal
				idx <- idx + 1
				
		}
	}
	
	# calculate the order of the indicies in the data frame
	idxOrder <- order(targetHospitals[,2], targetHospitals[,1])
	targetHospitals <- targetHospitals[idxOrder,]

	# if num param used "best" or "worst", adjust it accordingly
	if (num == "best") num <- 1
	if (num == "worst") num <- nrow(targetHospitals)
	
	# fetch the entry that contains the requested 'rank'
	rval <- targetHospitals[num,1]
	
	return(rval)

}

