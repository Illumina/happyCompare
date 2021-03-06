% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{read_samplesheet_}
\alias{read_samplesheet_}
\title{Load data from happyCompare samplesheets}
\usage{
read_samplesheet_(samplesheet, lazy = TRUE)
}
\arguments{
\item{samplesheet}{A \code{happyCompare} samplesheet (\code{data.frame}). Required fields:
\code{Group.Id}, \code{Sample.Id}, \code{Replicate.Id}, \code{happy_prefix}. Optional fields: \code{build_metrics}. Also supports
additional metadata fields supplied as extra columns.}

\item{lazy}{Do not load larger hap.py results until needed. Default: \code{TRUE}.}
}
\value{
A \code{happy_compare} object, with the following fields:
\itemize{
\item{\code{samplesheet}: the original samplesheet, stored as a \code{data.frame}}.
\item{\code{happy_results}: a \code{happy_result_list}
object that contains individual \code{happy_result} objects as defined in
\code{happyR}}.
\item{\code{build_metrics}: a \code{build_metrics_list} that contains a list of \code{data.frames} created from the csv
files provided under the \code{build_metrics} column of the samplesheet}.
\item{\code{ids}: a vector of unique identifiers for each result.}
}
}
\description{
Load hap.py results, build metrics and custom metadata into a \code{happy_compare} object using \code{happyR}.
}
\examples{

\dontrun{
happy_compare <- read_samplesheet(samplesheet_path = 'happyCompare_samplesheet.csv')
}

}
