% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transcribe.R
\name{transcribe}
\alias{transcribe}
\title{Transcribe \R{} object assert conditions}
\usage{
transcribe(x, desc = NULL, seal = TRUE, detail = FALSE, ...)
}
\arguments{
\item{x}{\R{} object}

\item{desc}{test name. Default adopt object name.}

\item{seal}{which output testthat script}

\item{detail}{\code{Logical}. Only data frame.
If you chose \emph{TRUE}, to make a detailed record for each variables in data frame.}

\item{...}{Arguments passed on to \code{seal}
\describe{
  \item{load_testthat}{include \code{library(testthat)} when \emph{TRUE}}
  \item{clip}{If \emph{TRUE} will overwrite the system clipboard.
When clipr is not available, The clip arguments is forcibly \emph{FALSE}.}
  \item{ts}{include comments that timestamp?}
  \item{mask_seal}{Whether to comment out after executing the function.
Default \emph{FALSE}. This option is effective only when using RStudio.}
}}
}
\description{
Transcribe \R{} object assert conditions
}
\examples{
\dontrun{
x <- 1:3L; transcribe(x)
transcribe(3.14, seal = FALSE)
transcribe(letters, load_testthat = TRUE, ts = FALSE)
x <- iris$Species; transcribe(x)
transcribe(iris)
}
}
