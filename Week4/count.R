count <- function(cause = NULL) {

	## Check that "cause" is non-NULL; else throw error
	if (is.null(cause)) {
		stop("No cause provided.")
	}

	## Check that specific "cause" is allowed; else throw error
	causeIdx <- 0
	bValidCause <- FALSE
	causes <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
	for(idx in 1:length(causes)) {
		if (cause == causes[idx]) {
			bValidCause <- TRUE
			causeIdx <- idx
			break
		}
	}
	
	if (!bValidCause) {
		stop("Unexpected cause.")
	}

	## Read "homicides.txt" data file
	f <- "homicides.txt"
	ds <- readLines(f)
	
	## Extract causes of death
	causePatterns <- c("[Aa]sphyxiation", "[Bb]lunt [Ff]orce", "[Oo]ther", 
						"[Ss]hooting", "[Ss]tabbing", "[Uu]nknown")
	pattern <- paste("Cause:",causePatterns[causeIdx])

	## Return integer containing count of homicides for that cause
	rval <- length(grep(pattern,ds))
	return(rval)
}

tfn <- function(basePattern) {

	pattern <- paste("Cause:", basePattern)
	length(grep(pattern,ds))	
	
}



