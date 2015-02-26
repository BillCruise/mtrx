\name{rande}
\alias{rande}
\title{
    Random Exponential Matrix
}
\description{
    Creates a matrix with exponentially distributed random elements.
}
\usage{
    rande(m, n = m, seed = NULL)
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
    Returns a matrix with exponentially distributed random elements.
}
\author{
    Bill Cruise
}
