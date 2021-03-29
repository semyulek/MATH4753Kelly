#' @title Uniform Distribution approximation
#'
#' @param n The size of each sample space
#' @param iter The amount of sample spaces
#' @param a Lower bounds of the sample space
#' @param b Upper bounds of the sample space
#'
#' @return Outputs a histogram of the sample mean along with a red theoretical uniform curve, a blue density uniform curve, and a black mean curve.
#' @export
#'
#' @examples
#' \dontrun{mycltu(n=10,iter=10000,a=0,b=10)}
mycltu=function(n,iter,a=0,b=10){
  ## r-random sample from the uniform
  y=runif(n*iter,a,b)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean) #### the 2 refers that we are applying the function to the columns
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax
  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  ## add a density curve made from the sample distribution
  lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  ## Add the density from which the samples were taken
  curve(dunif(x,a,b),add=TRUE,lwd=4)

}
