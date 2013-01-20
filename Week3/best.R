best <- function(state, outcome) {
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
	
	# Get a list of valid outcomes, column 7 in data but requirements
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
	
	# get best hospital in the state specified
	targetHospitals <- c()
	lowRate <- 999999
	for (x in 1:nrow(outcomeData)) {

		# skip this entry if no value was provided
		if (is.na(outcomeData[x,targetColumn])) next
		if (is.nan(outcomeData[x,targetColumn])) next
		if (outcomeData[x,targetColumn] == "Not Available") next

		testVal <- as.numeric(outcomeData[x,targetColumn])
		if((outcomeData[x,7] == state) && (testVal <= lowRate)) {	
			# have a candidate hospital, determine if it bumps previous candidates
			
			if(testVal < lowRate) {				
				# new low leader, drop the existing one and set low rate
				# print(paste("New low set ", outcomeData[x,2], " Val/OldLow:", outcomeData[x,targetColumn], ":", lowRate))
				targetHospitals <- outcomeData[x,2]
				lowRate <- testVal
				
			}
			else {				
				# this was a tie, add it to the targetHospitals temporarily
				targetHospitals[2] <- outcomeData[x,2]
				
				# sort alphabetically and keep only the low alpha order item
				sortOrder <- order(targetHospitals)
				if (sortOrder[1] == 1) {
					# keep only first item
					targetHospitals <- targetHospitals[1]
					
				}
				else {
					# keep only the second item
					targetHospitals <- targetHospitals[2]
					
				}
				
			}

		}
	}
	
	
	return(targetHospitals)

}


