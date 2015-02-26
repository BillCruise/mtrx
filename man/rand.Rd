\name{rand}
\alias{rand}
\title{
    Random Matrix
}
\description{
    Creates a matrix with random elements uniformly distributed on the interval (0, 1).
}
\usage{
    rand(m, n = m, seed = NULL)
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
    Returns a matrix with random elements uniformly distributed on the interval (0, 1).
}
\author{
    Bill Cruise
}
