#' @title Normal Distribution with probability
#'
#' @param mu The mean
#' @param sigma The standard deviation
#' @param a P(Y<=a)
#'
#' @return A graph with the normal distribution and the area under the curve up to a.
#' @export
#'
#' @examples
#' \dontrun{myncurve(10,5,10)}
myncurve = function(mu, sigma, a) {
  curve(
    dnorm(x, mean = mu, sd = sigma),
    xlim = c(mu - 3 * sigma, mu + 3 * sigma),
    col = "cornflowerblue",
    lwd = 2,
    xlab = "Y",
    ylab = "Beta Density",
    main = paste("Norm, mean = ", mu, ",sd = ", sigma)
  )
  xnorm = seq(mu - 3 * sigma, a, length = 1000)
  ynorm = dnorm(xnorm, mean = mu, sd = sigma)
  polygon(c(mu - 3 * sigma, xnorm, a), c(0, ynorm, 0), col = "darkseagreen")
  nprob = pnorm(a, mean = mu, sd = sigma)
  nprob = round(nprob, 4)
  text(mu, dnorm(a, mu, sigma) / 2, paste("P(Y<=", a, ")=", nprob))
}
