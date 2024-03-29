% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EFA-functions.R
\name{make_sankey_EFA}
\alias{make_sankey_EFA}
\title{Make Sankey Diagram from EFA Loadings}
\usage{
make_sankey_EFA(
  loadings,
  custom_html = TRUE,
  sankey_title = NULL,
  guess_title = TRUE,
  multi_loading_caption = TRUE,
  ...
)
}
\arguments{
\item{loadings}{Factor loadings from EFA}

\item{custom_html}{Logical; should custom tooltips, titles, captions, etc. be added to the diagram?}

\item{sankey_title}{A string indicating the title to be used. Default is NULL to work with next option.}

\item{guess_title}{Logical; if sankey_title is not specified, should a title be created? Default is "Sankey Diagram (cutoff = VALUE)".}

\item{multi_loading_caption}{Logical; if any items load on more than one factor, should a note be added to the diagram?}

\item{...}{Options to be passed to create_links_EFA}
}
\description{
Make Sankey Diagram from EFA Loadings
}
