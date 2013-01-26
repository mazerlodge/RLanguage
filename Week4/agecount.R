agecount <- function(age = NULL) {
	## Check that "age" is non-NULL; else throw error
	if (is.null(age)) {
		stop("No age provided.")
	}

	## Read "homicides.txt" data file
	f <- "homicides.txt"
	ds <- readLines(f)

	## Extract ages of victims; ignore records where no age is
	## given
	pattern <- "(..) years old"
	posIdx <- regexec(pattern,ds)
	m <- regmatches(ds,posIdx)
	
	matchCount <- 0
	for (x in 1:length(m)) {
		if (is.na(m[[x]][1])) next
		sl <- m[[x]]
		if (sl[2] == age) {
			matchCount <- matchCount + 1
		}
	}

	## Return integer containing count of homicides for that age
	return(matchCount)
	
}