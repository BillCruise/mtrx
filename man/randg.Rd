\name{randg}
\alias{randg}
\title{
    Random Gamma Matrix
}
\description{
    Creates an m x n matrix with random elements that follow a gamma distribution.
}
\usage{
    randg(alpha, m, n = m, seed = NULL)
}
\arguments{
  \item{alpha}{
    passed to rgamma as shape parameter
  }
  \item{m}{
    number of rows in the matrix
  }
  \item{n}{
    number of columns in the matrix
  }
  \item{seed}{
    initial seed value for the random generator
  }
}
\value{
    Returns an m x n matrix with random elements that follow a gamma distribution.
}
\author{
    Bill Cruise
}
