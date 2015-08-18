\name{predict.abcrf}
\alias{predict.abcrf}
\title{Predict and evaluate the posterior probability of the MAP
for new data using an ABC-RF object}
\usage{
\method{predict}{abcrf}(object, sumsta, obs,...)
}
\description{
Using an ABC-RF object this function predict the model for
new data and evaluate the posterior probability of the MAP.
}
\arguments{
\item{object}{an \code{\link{abcrf}} object.}
\item{sumsta}{a data frame or a matrix containing the summary statistics
of the reference table.}
\item{obs}{a data frame or a matrix containing the summary statistics of the 
observed data sets.}
\item{...}{additional arguments to be passed to or from methods.}
}
\value{
  An object of class \code{abcrfpredict}, which is a list with the
  following components:
  \item{allocation}{a vector containing the index of the selected model for each
  observed data set.}
  \item{post.prob}{a vector containing the ABC-RF approximation of the posterior 
  probability of the selected model.}
}
\references{
Pudlo, P., Marin, J.-M., Estoup, A., Cornuet, J.-M., Gautier, M., & Robert, 
C. P. (2014). 
\emph{ABC model choice via random forests}. arXiv preprint arXiv:1406.6288.
\url{http://arxiv.org/abs/1406.6288}
}
\seealso{
\code{\link{abcrf}, \link{plot.abcrf}}
}
\examples{
\dontrun{
data(snp)
data(snp.obs)
mc.rf <- abcrf(snp[1:1e3, 1], snp[1:1e3, -1])
predict(mc.rf, snp[1:1e3, -1], snp.obs)
}
}
