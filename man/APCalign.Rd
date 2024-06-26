% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/APCalign-package.R
\docType{package}
\name{APCalign}
\alias{APCalign-package}
\alias{APCalign}
\title{Standardising Taxonomic Names in Australian Plants}
\description{
The process of standardising taxon names is necessary when working with
biodiversity data. 'APCalign' uses the Australian Plant Name Index (APNI)
and the Australian Plant Census (APC) to align and update plant taxon names
to current, accepted standards. 'APCalign' can also supply information about
the established status of plant taxa across different states/territories.
}
\section{Functions}{

\strong{Standarise taxon names}
\itemize{
\item \link{load_taxonomic_resources}
\item \link{create_taxonomic_update_lookup}
\item \link{align_taxa}
\item \link{update_taxonomy}
}

\strong{Established status by region}
\itemize{
\item \link{state_diversity_counts}
\item \link{create_species_state_origin_matrix}
\item \link{native_anywhere_in_australia}
}
}

\references{
If you have any questions, comments or suggestions, please
submit an issue at our
\href{https://github.com/traitecoevo/APCalign/issues}{GitHub repository}
}
\seealso{
Useful links:
\itemize{
  \item \url{https://traitecoevo.github.io/APCalign/}
  \item \url{https://github.com/traitecoevo/APCalign}
  \item Report bugs at \url{https://github.com/traitecoevo/APCalign/issues}
}

}
\author{
\strong{Maintainer}: Daniel Falster \email{daniel.falster@unsw.edu.au} (\href{https://orcid.org/0000-0002-9814-092X}{ORCID}) [copyright holder]

Authors:
\itemize{
  \item Elizabeth Wenk \email{e.wenk@unsw.edu.au} (\href{https://orcid.org/0000-0001-5640-5910}{ORCID}) [contributor]
  \item Will Cornwell \email{w.cornwell@unsw.edu.au} (\href{https://orcid.org/0000-0003-4080-4073}{ORCID}) [contributor]
  \item Fonti Kar \email{f.kar@unsw.edu.au} (\href{https://orcid.org/0000-0002-2760-3974}{ORCID}) [contributor]
}

Other contributors:
\itemize{
  \item Carl Boettiger \email{cboettig@gmail.com} (\href{https://orcid.org/0000-0002-1642-628X}{ORCID}) [contributor]
}

}
\keyword{internal}
