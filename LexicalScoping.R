#
# Fun example of Lexical vs. Dynamic Scoping
#

y <- 10

f <- function(x) {
	y <- 2
	y^2 + g(x)
	
}

g <- function(x) {
	x * y
	
}

# what is the value of f(3)?
