% https://cran.r-project.org/doc/manuals/r-release/R-exts.html
% https://developer.r-project.org/Rds.html

\name{SQRL-package}
\alias{SQRL-package}
\alias{SQRL}
\docType{package}
\title{
\packageTitle{SQRL}
}
\description{
Wrapper of convenience about \pkg{RODBC}.
Supports parameterised multi-statement \acronym{SQL} scripts with embedded \R.
Generates like-named user-interfaces for \acronym{ODBC} (Open Database
Connectivity) data sources.
Implicitly manages connection handles and communication parameters.
Provides visual indication of open channels and queries in progress.
Detects lost connections and attempts recovery.
}
\details{
On load, the package will look for data source names (\acronym{DSN}s) via
\pkg{RODBC}, and attempt to create a like-named interface function
for each (additional data sources can be defined at any time).
These interfaces dispense with the need for explicit connection parameters on
every query.
They support multi-statement \acronym{SQL} scripts, with or without embedded \R
expressions.
}
\author{
\packageAuthor{SQRL}
}
\seealso{
\code{\link{sqrlInterface}},
\code{\link{sqrlUsage}},
\code{\link[RODBC:RODBC-package]{RODBC}}
}
\examples{
require(SQRL)

# Show (pre)defined data sources.
sqrlSources()

# Define a new data source.
sqrlSource("mysource",
            driver = "MYSQL ODBC 5.3 ANSI Driver",
            server = "localhost",
            user = "<uid>",
            password = "<pwd>")

# Obtain help on usage.
mysource("help")

\dontrun{
# Submit a query to the new source.
mysource("select 1")

# Submit a parameterised query from file.
mysource("transactions.sqrl", customerid = 111111)
}
}
\keyword{database}
\keyword{interface}
\keyword{package}
