% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_write_html.R
\name{write_html}
\alias{write_html}
\title{Write HTML output}
\usage{
write_html(
  data,
  dic,
  click,
  class_title,
  class_body,
  text_result_null = NULL,
  id = NULL,
  ...
)
}
\arguments{
\item{data}{A dataset to display.}

\item{dic}{A dictionary used to format the output.}

\item{click}{A vector of column names to apply click events to.}

\item{class_title}{A CSS class to apply to the title.}

\item{class_body}{A CSS class to apply to the body.}

\item{text_result_null}{Text to display if the data is empty.}

\item{id}{An optional ID to apply to the HTML output.}

\item{...}{Additional arguments to pass to \code{\link[dplyr]{select}}.}
}
\value{
An HTML output.
}
\description{
This function generates HTML output based on a dataset and a dictionary, and applies click events to selected elements.
}
