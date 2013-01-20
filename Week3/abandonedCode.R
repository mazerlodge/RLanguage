
best_old <- function(state, outcome) {
	##
	## NOTE: This is a protype version that does not use apply() functions. 
	##
	
	## Read outcome data
	f <- "outcome-of-care-measures.csv"
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
	
	# Get a list of outcomes, column 7 in data but requirements
	#  say to only consider three specific values.
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
	
	## Return hospital name in that state with lowest 30-day death
	## rate
	
	# get hospitals in the state specified
	targetHospitals <- c()
	targetValues <- c()
	idx <- 1
	for (x in 1:nrow(outcomeData)) {
		if(outcomeData[x,7] == state) {
			# collect provider IDs
			targetHospitals[idx] <- outcomeData[x,1]
			idx <- idx + 1
		}
	}
	
	# get the lowest rate from the targetHospitals
	lowRate <- 999999
	lowRateHospitals <- c()
	lrIdx <- 1
	for (pid in targetHospitals) {
		currentRate <- getRateForProvider(outcomeData, pid, targetColumn)
		
		if (currentRate == lowRate) {
			# this hospital matched the rate, add it to the list
			lrIdx <- lrIdx + 1
			lowRateHospitals[lrIdx] <- pid
			
		}
		
		if (currentRate < lowRate) {
			# baseline at this rate
			lowRate <- currentRate
			
			# reset list of hospitals matching the rate to just this hospital	
			lrIdx <- 1
			lowRateHospitals <- c(pid)
		}
	}
	
	# get all hospitals that have a rate matching the low rate
	#idx <- 1
	#for (x in 1:length(targetHospitals)) {
	#	currentRate <- getRateForProvider(outcomeData, targetHospitals[x], targetColumn)
	#	if (currentRate == lowRate) {
	#		lowRateHospitals[idx] <- targetHospitals[x]
	#		idx <- idx + 1
	#	}
	#
	#}	
	
	return(lowRateHospitals)

}

getRateForProvider <- function(ds, pid, colIdx) {
	
	rval <- -1
	
	for (x in 1:nrow(ds)) {
		if(ds[x,1] == pid) {
			# get val from target column
			rval <- ds[x,colIdx]
			break

		}
	}
	
	return(rval)
	
}
