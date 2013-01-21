rankall <- function(outcome, num = "best") {
	## Read outcome data
	f <- "./Wk3Data/outcome-of-care-measures.csv"
	outcomeData <- read.csv(f, colClasses="character")

	## Check that outcome is valid (no state parameter)	
	# Get a list of valid outcomes, column 7 in data 
	#  Requirements do not specify limit to three, but doing so like previous part.
	validOutcomes <- c("heart attack", "heart failure", "pneumonia")
	outcomeColumns <- c(11,17,23)
	idx<- 1
	bOutcomeValid <- FALSE
	for (aOut in validOutcomes) {
		if (outcome == aOut) {
			bOutcomeValid <- TRUE
			targetColumn <- outcomeColumns[idx]
			break
		}
		idx <- idx + 1
	}

	if (!bOutcomeValid) stop("invalid outcome")

	# Get a state list to work with.
	distinctStates <- distinctStateList(outcomeData)

	# build a data frame to hold hospital name, state, and start and stop row indexes 
	targetHospitals <- data.frame(hospital=1, state=1, startIdx=1, stopIdx=1)

	# stub in the frame with states
	targetHospitals[1:length(distinctStates),2] <- distinctStates

	# sort the outcome data by state first, then by the specified value, then by hospital name
	rawOrder <- order(outcomeData[,7], outcomeData[,targetColumn], outcomeData[,2])
	workingData <- outcomeData[rawOrder,]

	# find start and stop indexes for each state in the sorted data
	currentState <- workingData[1,7]
	stateIdx <- getIndexForState(distinctStates, currentState)
	targetHospitals[stateIdx,"startIdx"] <- 1
	for (x in 2:nrow(workingData)) {
		# take advantage of "Not Available" values sorting to the end
		if (workingData[x,targetColumn] == "Not Available") {
			if (is.na(targetHospitals[stateIdx,"stopIdx"])) {
				# end the current hospital's range, excluding Not Available values
				targetHospitals[stateIdx,"stopIdx"] <- x-1
			}
			next
		}
		
		if (workingData[x,7] != currentState) {
			# walked off current state, if stop hasn't been set, do it now
			if (is.na(targetHospitals[stateIdx,"stopIdx"])) {
				targetHospitals[stateIdx,"stopIdx"] <- x-1
			}
			
			# ... and note index of new state starting here.
			currentState <- workingData[x,7]
			stateIdx <- getIndexForState(distinctStates, currentState)
			targetHospitals[stateIdx,"startIdx"] <- x

		}
	}
	
	# if required, tag the last state worked as ending on the last line of the file
	if (is.na(targetHospitals[stateIdx,"stopIdx"])) {
		targetHospitals[stateIdx,"stopIdx"] <- nrow(workingData)
	}

	# get the hospital at the rank requested for each state	
	originalNum <- num
	for(x in 1:nrow(targetHospitals)) {

		# create some friendly short named variables
		start <- targetHospitals[x,"startIdx"]
		stop <- targetHospitals[x,"stopIdx"]
		#print(paste("ln67: state=", targetHospitals[x,"state"], " using num=", num, 
		#			" start/stop=", start, "/", stop))

		# For each state determine the hospital found at the num specified
		# create a mini-dataset with just this state's rows
		subset <- workingData[start:stop,]  
		
		# if num param used "best" or "worst", adjust it accordingly
		if (originalNum == "best") num <- 1
		if (originalNum == "worst") num <- nrow(subset)
	
		# order the subset
		subsetOrder <- order(as.numeric(subset[,targetColumn]), subset[,2])
		subset <- subset[subsetOrder,]

		# put the hospital name found at the requested position in the target hospital data frame
		targetHospitals[x,"hospital"] <- subset[num, 2]

	}
	
	# make a 'clean' data frame that only has the information requested
	rval <- data.frame(hospital=targetHospitals[,"hospital"], state=targetHospitals[,"state"])
			
	return(rval)

	## Return a data frame with the hospital names and the
	## (abbreviated) state name

}

getIndexForState <- function(stateList, state) {
	
	rval <- -1
	for(x in 1:length(stateList)) {
		if (stateList[x] == state) {
			rval <- x
			break
		}
		
	}
	
	return(rval)
}

distinctStateList <- function(rawData) {
	
	# get a list of distinct states from the outcome data file in alpha order
	
	rawList <- rawData[,7]
	stateList <- c()
	
	for(aState in rawList) {
	
		# is this state all ready known
		bStateFound <- FALSE
		for (existingState in stateList) {
			if (aState == existingState) {
				bStateFound <- TRUE
				break
			}
		}
		
		# if the state wasn't known, add it.
		if (!bStateFound) {
			stateList[length(stateList)+1] <- aState
		}

	}
	
	stateOrder <- order(stateList)
	stateList <- stateList[stateOrder]

	return(stateList)
	
}
