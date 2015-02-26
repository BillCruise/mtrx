\name{randn}
\alias{randn}
\title{
    Random Normal Matrix
}
\description{
    Creates an m x n matrix with normally distributed random elements.
}
\usage{
    randn(m, n = m, seed = NULL)
}
\arguments{
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
    Returns an m x n matrix with random elements that follow a normal distribution.
}
\author{
    Bill Cruise
}
