\name{plot.abcrf}
\alias{plot.abcrf}
\title{Plot of an ABC-RF object}
\usage{
\method{plot}{abcrf}(x, obs=NULL, n.var=20, main="", pdf=FALSE,...)
}
\description{
\code{plot.abcrf} gives both variable importance plot of a model choice ABC-RF object
and the projection of the reference table on the LDA axes.
}
\arguments{
\item{x}{an \code{abcrf} object.}
\item{obs}{a vector containing the summary statistics of an observed data set that
will be added to the graph of the projected reference table (black star or vertical line).}
\item{n.var}{number of variables in the variable importance representation.}
\item{main}{title of the importance variables plot.}
\item{pdf}{a boolean that indicates if a pdf version of the graph(s) should be saved in the current directory.}
\item{...}{additional arguments to be passed on to \code{\link{varImpPlot}}.}
}
\note{
The graph of the reference table projected
on the LD axes is shown only if LD axes has
been added to the set of summary statistics
in the call of \code{abcrf}.
}
\references{
Pudlo, P., Marin, J.-M., Estoup, A., Cornuet, J.-M., Gautier, M. & Robert, 
C. P. (2015). 
\emph{Reliable ABC model choice via random forests}. Bioinformatics.
\url{http://bioinformatics.oxfordjournals.org/content/early/2015/12/23/bioinformatics.btv684}
}
\seealso{
\code{\link{abcrf}}, 
\code{\link{predict.abcrf}}, 
\code{\link{err.abcrf}},
\code{\link{varImpPlot}}
}
\examples{
data(snp)
modindex <- snp[1:500, 1]
sumsta <- snp[1:500, -1]
model.rf <- abcrf(modindex, sumsta, ntree=100)
plot(model.rf)
data(snp.obs)
plot(model.rf, snp.obs[1,])
}
