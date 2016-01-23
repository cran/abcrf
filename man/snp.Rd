\name{snp}
\alias{snp}
\alias{snp.obs}
\title{A simulated example in population genetics}

\description{
The simulated example of population genetics using SNP loci in Pudlo et al. 
(2015): \code{snp} contains the reference table on which to perform ABC model choice.
\code{snp.obs} contains two pseudo-observed data sets. The first one 
(\code{favorable}) should be easily allocated to a model, while the
second one (\code{unfavorable}) is designed to be more tricky to allocate 
to a model.
}
\usage{
data(snp)
data(snp.obs)
}
\format{
\code{snp} is a data frame containing an ABC reference table of 20,000 
simulations from a 
Bayesian prior predictive model (see Pudlo et al., 2015, for a description
of the model choice issue). The first column, named \code{modindex} is a \code{factor} containing 
the model indices. The other columns represent the summary statistics.

\code{snp.obs} is a data frame with the summary statistics of two
pseudo-observed data sets.
}
\source{
Pudlo, P., Marin, J.-M., Estoup, A., Cornuet, J.-M., Gautier, M., & Robert, 
C. P. (2015). 
\emph{Reliable ABC model choice via random forests}. Bioinformatics.
\url{http://bioinformatics.oxfordjournals.org/content/early/2015/12/23/bioinformatics.btv684}
}
\examples{
data(snp)
data(snp.obs)
}
