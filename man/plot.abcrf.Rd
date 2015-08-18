\name{plot.abcrf}
\alias{plot.abcrf}
\title{Plot of an ABC-RF object}
\usage{
\method{plot}{abcrf}(x, sumsta=NULL, n.var=20, main="", ...)
}
\description{
\code{plot.abcrf} gives both variable importance plot of a model choice ABC-RF object
and the projection of the reference table on the LDA axes.
}
\arguments{
\item{x}{an \code{abcrf} object.}
\item{sumsta}{summary statistics of the reference table (\code{NULL} to 
remove the projection on the LDA axes).}
\item{n.var}{number of variables in the variable importance representation.}
\item{main}{title of the plot.}
\item{...}{other graphical parameters to be passed on to \code{\link{varImpPlot}}.}
}
\note{
The graph of the reference table projected
on the LD axes is shown only if LD axes has
been added to the set of summary statistics
in the call of \code{abcrf}.
}
\references{
Pudlo, P., Marin, J.-M., Estoup, A., Cornuet, J.-M., Gautier, M., & Robert, 
C. P. (2014). 
\emph{ABC model choice via random forests}. arXiv preprint arXiv:1406.6288.
\url{http://arxiv.org/abs/1406.6288}
}
\seealso{
\code{\link{abcrf}}, 
\code{\link{predict.abcrf}}, \code{\link{varImpPlot}}
}
\examples{
\dontrun{
data(snp)
mc.rf <- abcrf(snp[1:1000, 1], snp[1:1000, -1])
plot(mc.rf, snp[1:1000, -1])
}
}
