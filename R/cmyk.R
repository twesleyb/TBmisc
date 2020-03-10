cmyk <- function(C,M,Y,K) {
	n.c <- (C * (1-K) + K)
	n.m <- (M * (1-K) + K)  
        n.y <- (Y * (1-K) + K)
        R <- ceiling(255 * (1-n.c))
	G <- ceiling(255 * (1-n.m))
	B <- ceiling(255 * (1-n.y))
	hex <- rgb(R,G,B,maxColorValue=255)
	return(hex)
}
