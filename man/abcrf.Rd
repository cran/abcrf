\name{abcrf}
\alias{abcrf}
\alias{abcrf.formula}
\alias{abcrf.factor}
\alias{print.abcrf}

\title{ABC model choice via random forests}
\description{
\code{abcrf} constructs a random forest from a reference table towards performing
an ABC model choice.
}
\usage{
\method{abcrf}{formula}(formula, data=NULL, ...)
\method{abcrf}{factor}(modindex, sumsta, lda=TRUE, ntree=500, sampsize=min(1e5, length(modindex)), ...) 
\method{print}{abcrf}(x, ...)
}
\arguments{
  \item{formula}{a formula: left of ~, variable representing 
  the model index; right of ~, summary statistics.}
  \item{data}{an optional data frame containing the reference table.}
  \item{modindex}{a factor containing the model indices of the reference table.}
  \item{sumsta}{a matrix or a data frame containing the summary statistics 
  of the reference table.}
  \item{lda}{should LDA scores be added to the list of summary statistics?}
  \item{ntree}{number of trees to grow in the forest}
  \item{sampsize}{size of the sample from the reference table to grow a tree
  of the forest. If \code{sampsize} is a vector of length the number of models,
  then sampling is stratified by models, and the elements of \code{sampsize} indicate
  the numbers to be drawn from the models.}
  \item{x}{an \code{abcrf} object to be printed.}
  \item{...}{optional parameters to be passed to \code{randomForest} or \code{print}.}
}
\value{
  An object of class \code{abcrf}, which is a list with the
  following components:
  \item{call}{the original call to \code{abcrf},}
  \item{model.lda}{the LDA model used to add the LDA scores to the summary statistics,}
  \item{model.rf}{an object of class \code{randomForest} containing the trained 
  forest with the reference table,}
  \item{prior.err}{prior error rates of model selection on the reference
  table, estimated with the "out-of-bag" error of the forest.}
}
\references{
Pudlo, P., Marin, J.-M., Estoup, A., Cornuet, J.-M., Gautier, M., & Robert, 
C. P. (2014). 
\emph{ABC model choice via random forests}. arXiv preprint arXiv:1406.6288.
\url{http://arxiv.org/abs/1406.6288}
}
\seealso{
\code{\link{plot.abcrf}}, 
\code{\link{predict.abcrf}}, \code{\link{randomForest}}
}
\examples{
data(snp)
mc.rf <- abcrf(snp[1:1e3, 1], snp[1:1e3, -1])
print(mc.rf)
}
