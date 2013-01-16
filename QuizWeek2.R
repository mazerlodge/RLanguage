cube <- function(x,n) {
	
	x^3
	
}

pow <- function(x=4, n=3) {
	
	x^n
	
}

sepalLength <- function(irisData) {
		
	pl <- c()
	idx <- 1
	for(x in 1:nrow(irisData)) {
		if(irisData[x,"Species"] == "virginica") {
			pl[idx] <- irisData[x,"Petal.Length"]
			idx <- idx + 1
		}
		
	}
	
	mean(pl)
	
}

f <- function(x) {
	g <- function(y) {
		y + z
		
	}
	z <- 4
	x + g(x)
}
