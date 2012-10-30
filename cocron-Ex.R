pkgname <- "cocron"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('cocron')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("cocron.n.coefficients")
### * cocron.n.coefficients

flush(stderr()); flush(stdout())

### Name: cocron.n.coefficients
### Title: Statistical comparisons of n alpha coefficients
### Aliases: cocron.n.coefficients

### ** Examples

# independent alpha coefficients
cocron.n.coefficients(alpha=c(.784,.875,.936), items=c(5,5,5), n=c(51,101,151),
indep=TRUE)

# dependent alpha coefficients
r <- rbind(
 c(1,.8,.6,.75),
 c(NA,1,.65,.7),
 c(NA,NA,1,.55),
 c(NA,NA,NA,1)
)
cocron.n.coefficients(alpha=c(.857,.875,.800,.833), items=c(50,40,35,25), n=100,
indep=FALSE, r=r)



cleanEx()
nameEx("cocron.two.coefficients")
### * cocron.two.coefficients

flush(stderr()); flush(stdout())

### Name: cocron.two.coefficients
### Title: Statistical comparisons of two alpha coefficients
### Aliases: cocron.two.coefficients

### ** Examples

# independent alpha coefficients
cocron.two.coefficients(alpha=c(.78,.71), n=c(41,151), indep=TRUE)

# dependent alpha coefficients
cocron.two.coefficients(alpha=c(.82,.89), n=27, indep=FALSE, r=.74)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
