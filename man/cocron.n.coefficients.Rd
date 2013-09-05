\name{cocron.n.coefficients}
\alias{cocron.n.coefficients}
\title{Statistical comparisons of n alpha coefficients}
\usage{
  cocron.n.coefficients(alpha, n, items = NULL,
    dep = FALSE, r = NULL, los = 0.05, conf.level = 0.95)
}
\arguments{
  \item{alpha}{A numeric vector containing the alpha
  coefficients.}

  \item{n}{A numeric vector containing the number of
  individuals who provided the data for the test for which
  alpha coefficients were determined.}

  \item{items}{A numeric vector containing the number of
  items the alpha coefficients are based on.}

  \item{dep}{A logical indicating whether the alpha
  coefficients are based on dependent groups of individuals
  (default is \code{FALSE}).}

  \item{r}{A matrix that contains in the upper triangle all
  correlations between the scores the alpha coefficients
  are based on (see examples). Only required if the alpha
  coefficients are computed for dependent groups of
  individuals (\code{dep = TRUE}).}

  \item{los}{A number indicating the level of significance
  (default is \code{.05}).}

  \item{conf.level}{A number defining the level of
  confidence for the confidence intervals of the alpha
  coefficients (default is \eqn{.95}; see
  \link{cronbach.alpha.CI}). The confidence intervals serve
  as additional information only, they are not used for the
  test of significance.}
}
\value{
  Returns an object of the class
  "\code{cocron.n.coefficients}" with the following slots:
  \item{alpha}{Input parameter} \item{n}{Input parameter}
  \item{items}{Input parameter} \item{dep}{Input parameter}
  \item{r}{Input parameter} \item{los}{Input parameter}
  \item{conf.level}{Input parameter} \item{statistic}{The
  value of the test statistic} \item{distribution}{The
  distribution of the test statistic} \item{df}{The degrees
  of freedom of the distribution of the test statistic}
  \item{p.value}{The p-value of the test}
  \item{conf.int}{The confidence intervals of the alpha
  coefficients}
}
\description{
  Performs a test of significance for the difference
  between \eqn{n} alpha coefficients (Cronbach, 1951). The
  function expects alpha coefficients as input.
}
\details{
  To compare \eqn{n} dependent or independent alpha
  coefficients (Cronbach, 1951), the methods by Feldt,
  Woodruff, and Salih (1987) are implemented.
}
\examples{
# independent alpha coefficients
cocron.n.coefficients(alpha=c(.784,.875,.936), items=c(5,5,5), n=c(51,101,151),
dep=FALSE)

# dependent alpha coefficients
r <- rbind(
 c(1,.8,.6,.75),
 c(NA,1,.65,.7),
 c(NA,NA,1,.55),
 c(NA,NA,NA,1)
)
cocron.n.coefficients(alpha=c(.857,.875,.800,.833), items=c(50,40,35,25), n=100,
dep=TRUE, r=r)
}
\references{
  Cronbach, L. J. (1951). Coefficient alpha and the
  internal structure of tests. \emph{Psychometrika},
  \emph{16}, 297-334.

  Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987).
  Statistical inference for coefficient alpha.
  \emph{Applied Psychological Measurement}, \emph{11},
  93-103.
}
\seealso{
  \link{cocron}, \link{cocron.two.coefficients}
}

