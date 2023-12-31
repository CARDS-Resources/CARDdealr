% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/src_brfss.R
\name{src_brfss}
\alias{src_brfss}
\title{Behavioral Risk Factor Surveillance System (BRFSS) dataset}
\source{
\itemize{
\item \href{https://data.cdc.gov/Behavioral-Risk-Factors/Behavioral-Risk-Factor-Surveillance-System-BRFSS-P/dttw-5yxu}{Data Source}
}
}
\usage{
src_brfss(...)
}
\arguments{
\item{\dots}{passed to \code{\link[readr:read_delim]{readr::read_csv()}} and useful for limiting
the number of rows read for testing or glimpsing data.}
}
\description{
The Behavioral Risk Factor Surveillance System (BRFSS) is the nation’s premier system of health-related telephone surveys that collect state data about U.S. residents regarding their health-related risk behaviors, chronic health conditions, and use of preventive services.
Established in 1984 with 15 states, BRFSS now collects data in all 50 states as well as the District of Columbia and three U.S. territories.
BRFSS completes more than 400,000 adult interviews each year, making it the largest continuously conducted health survey system in the world.
}
\details{
Fact sheet is available \href{https://www.cdc.gov/brfss/factsheets/pdf/DBS_BRFSS_survey.pdf}{here}.
}
\examples{

brfss = src_brfss(n_max=1000)

colnames(brfss)

dplyr::glimpse(brfss)

}
\seealso{
Other data accessors: 
\code{\link{src_acr_lung_cancer_screening_data}()},
\code{\link{src_bls_unemployment_data}()},
\code{\link{src_cdc_places_data}()},
\code{\link{src_epa_superfund_data}()},
\code{\link{src_epa_tri_data}()},
\code{\link{src_fda_mammography_data}()},
\code{\link{src_hrsa_facility_data}()},
\code{\link{src_nppes_gi_data}()},
\code{\link{src_nppes_onco_data}()}
}
\author{
Sean Davis \href{mailto:seandavi@gmail.com}{seandavi@gmail.com}
}
\concept{data accessors}
