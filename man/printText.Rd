% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printText.R
\name{printText}
\alias{printText}
\alias{printLines}
\alias{printMsg}
\alias{warningText}
\alias{wrapText}
\title{Printing Functions for supporting mStats package}
\usage{
printText(x, txt, split = NULL)

printLines(x = "=", width = 80, nextLine = TRUE)

printMsg(txt = NULL)

warningText(txt = NULL)

wrapText(txt, width = 80, sep = "\\n")
}
\arguments{
\item{x}{vector, matrix, dataframe or separator (in case of printLines)}

\item{txt}{texts}

\item{split}{separator for printText}

\item{width}{desired character length to display}

\item{nextLine}{logical value, indicating of next line,}

\item{sep}{separator for line break}
}
\description{
Printing Functions to format the outputs from mStats package
}
\examples{
\dontrun{
x <- rnorm(100)
plot(x)
clear()
}
}
\author{
Myo Minn Oo (Email: \email{dr.myominnoo@gmail.com} |
Website: \url{https://myominnoo.github.io/})
}
\keyword{support}
