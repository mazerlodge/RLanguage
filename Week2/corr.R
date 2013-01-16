corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        
        # prepare the return value vector
 		rval <- c()
        
        rowIdx <- 1
        for (id in 1:332) {

				# retrieve data for the specified monitor
				#   and capture complete cases (list of t/f flags).
        		ds <- getmonitor(id, directory)
        		bCandidate <- complete.cases(ds)
        		
        		# get the observations that were complete
        		good <- ds[bCandidate,]
				if (nrow(good) > threshold) {
					# compute the correlation 
					temp <- cor(good[2], y=good[3], use="complete.obs")
					rval[rowIdx] <- temp[1,1]
					rowIdx <- rowIdx + 1
				}
        	
        		
        		# Output the count of complete cases to the rval data frame.
        		# Note: This works because sum of a vector of t/f values returns
        		#         the count of TRUE values.
        		#rval[rowIdx,"id"] <- x
        		#rval[rowIdx,"nobs"] <- sum(candidates)
        		#rowIdx <- rowIdx + 1
        
        } # for id
        
        rval
	
}

