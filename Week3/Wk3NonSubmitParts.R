part1 <- function(inFile) {
	## Note: This code is not required as part of the assignment submission
	
	outcome <- read.csv(inFile, colClasses="character")
	##head(outcome)
	
	outcome[,11] <- as.numeric(outcome[,11])
	## Ignore warnings about NAs via coercion
	
	hist(outcome[,11], main="Hear Attack 30-day Death Rate", xlab="30-day Death Rate")
	
	return(outcome)
}

part2 <- function(inFile) {
	## Note: This code is not required as part of the assignment submission

	outcome <- read.csv(inFile, colClasses="character")	
	
	# columns for Heart Attack, Heart Failure, and Pneumonia
	colNums <- c(11, 17, 23)
	colTitles <- c("Heart Attack", "Heart Failure", "Pneumonia")
	
	# calc overall range for the three columns
	#  while doing this, convert those columns to numeric
	minMaxRange <- c(999999999,-999999999)
	for (x in 1:3) {
		col <- colNums[x]
		outcome[,col] <- as.numeric(outcome[,col])
		currentRange <- range(outcome[,col], na.rm=TRUE)
		if (currentRange[1] < minMaxRange[1]) minMaxRange[1] <- currentRange[1]
		if (currentRange[2] > minMaxRange[2]) minMaxRange[2] <- currentRange[2]
		
	}
	
	# prep plot to output 3 rows 1 column
	par(mfrow = c(3,1))

	# create the three plots as specified in the instructions
	for (idx in 1:3) {
		col <- colNums[idx]
		titleText <- colTitles[idx]
		hist(outcome[,col], main=titleText, xlab="30-day Death Rate", xlim=minMaxRange)
			
	}
	
	return(minMaxRange)
	
}

