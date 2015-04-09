# X is mxn matrix
# Y is n vector
# output is m vector having correlation values of n vectors n X matrix

mat_corr <- function(X,Y)
{
	rc 			<-dim(X)
	rows 		<- rc[1]
	cols 		<- rc[2]
	# check length of Y with cols if not equal exit
	Ymn 		<- t(replicate(rows, Y))
	
	Xrow_means 	<- rowMeans(X)
	Yrow_means	<- rowMeans(Ymn)

	XY 			<- X * Ymn

	XYrow_means <- rowMeans(XY)

	# sweep applies FUN to MARGIN dimension of matrix X
	# here 1 is for row i.e. subtract Xrow_means[i] from
	# each column of ith row of X  
	stdX 		<- rowMeans((sweep(X, MARGIN = 1, Xrow_means, FUN = "-"))^2)
	stdY		<- rowMeans((sweep(Ymn, MARGIN = 1, Yrow_means, FUN = "-"))^2)

	stdXY 		<- stdX * stdY

	corrXY 		<- (XYrow_means - (Xrow_means * Yrow_means)) / (sqrt(stdXY))

	corrXY
}