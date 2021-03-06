% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{mm_xpf}
\alias{mm_xpf}
\title{Dot Product SparseMatrix*DenseArray}
\usage{
mm_xpf(x, y_, lsel)
}
\arguments{
\item{x}{A list, sparse matrix of type slam}

\item{y_}{A numeric 3d array}

\item{lsel}{An integer vector}
}
\value{
An array with dimensions (m x len(lsel) x k), i.e. it is permuted on the fly.
}
\description{
Dot product of simple triplet matrix x (m x n) (measurement matrix) and a dense array y (n x k x l).
Only slices of y_ from lsel vector are used.
}
