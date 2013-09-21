\name{cronbach.alpha.CI}
\alias{cronbach.alpha.CI}
\title{Confidence interval for Cronbach's Alpha}
\usage{
  cronbach.alpha.CI(alpha, n, items, conf.level = 0.95)
}
\arguments{
  \item{alpha}{A numeric specifying the alpha coefficient.}

  \item{n}{A numeric defining the number of individuals who
  provided the data for the test for which the alpha
  coefficient was determined.}

  \item{items}{A numeric specifying the number of items the
  alpha coefficient is based on.}

  \item{conf.level}{A number defining the level of
  confidence for the confidence interval (default is
  \eqn{.95}).}
}
\value{
  Returns a confidence interval for Cronbach's alpha as a
  numeric vector.
}
\description{
  Calculates a confidence interval for Cronbach's alpha
  (Cronbach, 1951).
}
\details{
  The lower bound of a confidence interval for an
  \eqn{\alpha} that is based on the data of \eqn{n}
  individuals who responded to \eqn{k} items is defined as
  \deqn{L = 1 - \left((1 - \alpha) F(1 - c/2)\right)}{L = 1
  - ((1 - \alpha) F(1 - c/2))} where \eqn{c} is the level
  of confidence and \eqn{F(1 - c/2)} the \eqn{100(1 - c/2)}
  percentile of the F-distribution with \eqn{df_1 = n - 1}
  and \eqn{df_2 = (n - 1)(k - 1)} (Feldt, Woodruff, &
  Salih, 1987, p. 95, formula 6). The upper bound of the
  confidence interval is computed as \deqn{U = 1 - \left((1
  - \alpha) F(c/2)\right)}{U = 1 - ((1 - \alpha) F(c/2))}
  (Feldt et al., 1987, p. 95, formula 7).
}
\examples{
cronbach.alpha.CI(alpha=.83, n=100, items=20, conf.level=.95)
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
  \link{cronbach.alpha}
}

