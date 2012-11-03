#' Statistical comparisons of n alpha coefficients
#'
#' Performs a test of significance for the difference between \eqn{n} alpha coefficients (Cronbach, 1951).
#'
#' To compare \eqn{n} dependent or independent alpha coefficients (Cronbach, 1951), the methods by Feldt, Woodruff, and Salih (1987) are implemented.
#'
#' @param alpha A numeric vector containing the alpha coefficients.
#' @param n A numeric vector containing the number of participants who provided the data for the test for which alpha coefficients were determined.
#' @param items A numeric vector containing the number of items the alpha coefficients are based on.
#' @param indep A logical indicating whether the alpha coefficients are based on independent groups of participants.
#' @param r A matrix that contains in the upper triangle all correlations between the scores the alpha coefficients are based on (see examples). Only required if the alpha coefficients are computed for independent groups of participants (\code{indep = TRUE}).
#' @param los A number indicating the level of significance (default is \code{.05}).
#'
#' @return Returns an object of the class "\code{cocron.n.coefficients}" with the following slots:
#' \item{alpha}{Input parameter}
#' \item{n}{Input parameter}
#' \item{items}{Input parameter}
#' \item{indep}{Input parameter}
#' \item{r}{Input parameter}
#' \item{los}{Input parameter}
#' \item{statistic}{The value of the test statistic}
#' \item{distribution}{The distribution of the test statistic}
#' \item{df}{The degrees of freedom of the distribution of the test statistic}
#' \item{p.value}{The p-value of the test}
#'
#' @references
#' Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests. \emph{Psychometrika}, \emph{16}, 297–334.
#'
#' Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical inference for coefficient alpha. \emph{Applied Psychological Measurement}, \emph{11}, 93–103.
#'
#' @examples
#'
#' # independent alpha coefficients
#' cocron.n.coefficients(alpha=c(.784,.875,.936), items=c(5,5,5), n=c(51,101,151),
#' indep=TRUE)
#'
#' # dependent alpha coefficients
#' r <- rbind(
#'  c(1,.8,.6,.75),
#'  c(NA,1,.65,.7),
#'  c(NA,NA,1,.55),
#'  c(NA,NA,NA,1)
#' )
#' cocron.n.coefficients(alpha=c(.857,.875,.800,.833), items=c(50,40,35,25), n=100,
#' indep=FALSE, r=r)
#'
#' @export
cocron.n.coefficients <- function(alpha, n, items=NULL, indep=TRUE, r=NULL, los=.05) {
  if(length(alpha) < 2 || any(is.na(alpha)) || any(alpha < 0 | alpha > 1) || any(!is.finite(alpha))) stop("The parameter 'alpha' must be a numeric vector containing two or more values between 0 and 1")

  if(length(n) == 0 || any(is.na(n)) || any(n <= 0) || any(!is.finite(n))) stop("The parameter 'n' must be a numeric vector containing values > 0")
  if(length(n) != 1 && length(n) != length(alpha)) stop("The parameter 'n' must be either a single number for all coefficients or a numeric vector with values for each coefficient")
  if(length(n) == 1) n <- rep(n, length(alpha))

  if(length(indep) != 1 || is.na(indep) || !is.logical(indep)) stop("The parameter 'indep' must be TRUE or FALSE")

  if(!indep) {
    if(is.matrix(r)) {
      if(nrow(r) != ncol(r)) stop("The parameter 'r' must be a quadratic matrix")
      if(any(r[upper.tri(r)] < -1 | r[upper.tri(r)] > 1)) stop("The parameter 'r' must be a matrix with numbers between -1 and 1 in the upper triangle")
      if(nrow(r) != length(alpha) || ncol(r) != length(alpha)) stop("The parameter 'r' must be a matrix that contains as many rows and columns as there are alpha coefficients specified")

      diag(r) <- 1
      r[lower.tri(r)] <- NA
    } else {
      if(length(r) != 1 || is.na(r) || r < -1 || r > 1) stop("The parameter 'r' must be a matrix or a single number between -1 and 1")
      if(length(alpha) != 2) stop("The parameter 'r' can only be a single number if there are two alpha coefficients specified")

      r <- rbind(
        c(1,r),
        c(NA,1)
      )
    }
  }

  if(length(items) == 0 || any(is.na(items)) || any(items <= 0) || any(!is.finite(items))) stop("The parameter 'items' must be a numeric vector containing values > 0")
  if(length(items) != 1 && length(items) != length(alpha)) stop("The parameter 'items' must be either a single number for all coefficients or a numeric vector with values for each coefficient")
  if(length(items) == 1) items <- rep(items, length(alpha))
  if(length(los) != 1 || is.na(los) || los < 0 || los > 1) stop("The parameter 'los' must be a single number between 0 and 1")

  m <- length(alpha)
  mean.alpha <- sum((1 - alpha)^(-1/3))/m

  if(indep) {
    nt <- (items - 1) * n / (items + 1) #nt = n with tilde

    variance <- 2/(9 * (nt - 1) * (1 - alpha)^(2/3))
    mean.variance <- mean(variance)

    statistic <- sum(((1 - alpha)^(-1/3) - mean.alpha)^2 / mean.variance)
  } else {
    n.mean <- mean(n)
    if(n.mean != n[1]) warning("The dependent groups have unequal sizes. Continuing with mean group size.")

    nt <- (items - 1) * n.mean / (items + 1) #nt = n with tilde

    mean.items <- harmonic.mean(items) #harmonic mean of all test lengths
    mean.nt <- (mean.items - 1) * n.mean / (mean.items + 1)

    variance <- 2/(9 * (mean.nt - 1) * (1 - alpha)^(2/3))
    mean.variance <- mean(variance) #mean.variance = mean of covariance diagonal

    covariance <- matrix(nrow=m, ncol=m, dimnames=list(alpha,alpha))
    for(i in 1:m) {
      for(j in i:m) {
        covariance[i,j] <- 2 * r[i,j]^2 /(9 * (mean.nt - 1) * (1 - alpha[i])^(1/3) * (1 - alpha[j])^(1/3))
      }
    }
    mean.covariance <- mean(covariance[upper.tri(covariance)])

    statistic <- sum(((1 - alpha)^(-1/3) - mean.alpha)^2 / (mean.variance - mean.covariance))
  }

  distribution <- "chisq"
  df <- m - 1
  alternative <- "greater"
  p.value <- get.p.value(statistic, alternative, distribution, df)

  result <- new("cocron.n.coefficients",
    alpha=alpha,
    n=n,
    items=items,
    indep=indep,
    los=los,
    alternative=alternative,
    df=df,
    distribution=distribution,
    statistic=statistic,
    p.value=p.value
  )
  if(!indep) result@r <- r

  result
}
