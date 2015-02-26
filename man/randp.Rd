\name{randp}
\alias{randp}
\title{
    Random Poisson Matrix
}
\description{
    Creates an m x n matrix with random elements that follow a Poisson distribution.
}
\usage{
    randp(lambda, m, n = m, seed = NULL)
}
\arguments{
  \item{lambda}{
    mean value parameter
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
    Returns an m x n matrix with random elements that follow a Poisson distribution.
}
\author{
    Bill Cruise
}
