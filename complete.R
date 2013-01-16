
complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases 
        
        # This will be the return value, an n x 2 data frame
        # Cell values for nobs will get set in the following loop.
        rval <- data.frame(id = id, nobs=0)              
        
        rowIdx <- 1
        for (x in id) {
		# retrieve data for the specified monitor
		#   and count the complete cases (no nulls).
        	ds <- getmonitor(x, directory)
        	good <- complete.cases(ds)
        	
        	# Output the count of complete cases to the rval data frame.
        	# Note: This works because sum of a vector of t/f values returns
        	#         the count of TRUE values.
        	rval[rowIdx,"id"] <- x
        	rval[rowIdx,"nobs"] <- sum(good)
        	rowIdx <- rowIdx + 1
        }
        
        rval
}

