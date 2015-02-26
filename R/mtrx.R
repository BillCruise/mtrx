# Simple matrix-creation functions.


#' Creates an identity matrix.
#' @param n the size of both matrix dimensions
#' @return an n x n square matrix with ones on the main diagonal and zeros elsewhere.

eye <- function(n) {
    diag(x=1, nrow=n, ncol=n)
}


#' Creates an m x n matrix whose elements are all 1.
#' If invoked with a single integer argument, creates a square matrix.
#' @param m number of rows in the matrix
#' @param n number of columns in the matrix
#' @return a matrix or N-dimensional array whose elements are all 1.

ones <- function(m, n = m) {
    matrix(1, nrow=m, ncol=n)
}


#' Creates an m x n matrix whose elements are all 0.
#' If invoked with a single integer argument, it creates a square matrix.
#' @param m number of rows in the matrix
#' @param n number of columns in the matrix
#' @return a matrix or N-dimensional array whose elements are all 0.

zeros <- function(m, n = m) {
    mat.or.vec(nr=m, nc=n)
}


#' Creates an m x n matrix with uniformly distributed random elements.
#' @param m number of rows in the matrix
#' @param n number of columns in the matrix
#' @param seed initial seed value for the random generator
#' @return a matrix with random elements uniformly distributed on the interval (0, 1).

rand <- function(m, n = m, seed = NULL) {
    set.seed(seed=seed)
    matrix( runif(n=m*n, min=0, max=1), m, n )
}


#' Creates an m x n matrix with random integer elements.
#' @param imax the maximum integer value in the range
#' @param m number of rows in the matrix
#' @param n number of columns in the matrix
#' @param seed initial seed value for the random generator
#' @return random integers in the range 1:imax.

randi <- function(imax, m, n = m, seed = NULL) {
    set.seed(seed=seed)
    matrix( sample(1:imax, size=m*n, replace=TRUE), m, n )
}


#' Creates an m x n matrix with normally distributed random elements.
#' @param m number of rows in the matrix
#' @param n number of columns in the matrix
#' @param seed initial seed value for the random generator
#' @return a matrix with normally distributed random elements having zero mean and variance one.

randn <- function(m, n = m, seed = NULL) {
    set.seed(seed=seed)
    matrix( rnorm(n=m*n, mean=0, sd=1), m, n )
}


#' Creates an m x n matrix with exponentially distributed random elements.
#' @param m number of rows in the matrix
#' @param n number of columns in the matrix
#' @param seed initial seed value for the random generator
#' @return a matrix with exponentially distributed random elements.

rande <- function(m, n = m, seed = NULL) {
    set.seed(seed=seed)
    matrix( rexp(n=m*n), m, n )
}


#' Creates an m x n matrix with random elements that follow a Poisson distribution.
#' @param lambda mean value parameter
#' @param m number of rows in the matrix
#' @param n number of columns in the matrix
#' @param seed initial seed value for the random generator
#' @return a matrix with Poisson distributed random elements with mean value given by the first argument, lambda.
randp <- function(lambda, m, n = m, seed = NULL) {
    set.seed(seed=seed)
    matrix( rpois(n=m*n, lambda=lambda), m, n )
}


#' Creates an m x n matrix with random elements that follow a gamma distribution.
#' @param alpha passed to rgamma as shape parameter
#' @param m number of rows in the matrix
#' @param n number of columns in the matrix
#' @param seed initial seed value for the random generator
#' @return a matrix with gamma(alpha, 1) distributed random elements.

randg <- function(alpha, m, n = m, seed = NULL) {
    set.seed(seed=seed)
    matrix( rgamma(n=m*n, shape=alpha), m, n )
}
