## 
# NOTE: To load this into R:
# codeFile <-/Users/mazerlodge/Documents/XProjects/RProjects/cs1workspace/week2functions.R
# source(codeFile)
##
getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        ##
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
	   ##
	   ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        # determine how much zero padding is required (0 or 00)
        idn <- as.numeric(id)
        zeroPad <- ""
        if (idn <10) {
        	zeroPad <- "00"
        }
        else if(idn >=10 && idn <= 99) {
        	zeroPad <- "0"
        }
        
        #build the filename with directory as a path 
        fnParts <- c("./", directory, "/", zeroPad, id, ".csv")
        fn <- paste(fnParts, collapse="")
        
        # load the file specified
        ds <- read.csv(fn)
        
 	   # Generate Summary if requested
 	   if (summarize)
 	   	print(summary(ds))
        
        return <- ds
}
