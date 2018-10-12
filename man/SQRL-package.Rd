\name{SQRL-package}
\alias{SQRL-package}
\alias{SQRL}
\docType{package}
\title{
\packageTitle{SQRL}
}
\description{
Facilitates exploratory work and rapid prototyping with Open Database Connectivity (\acronym{ODBC}) data sources.
}
\details{
The package is a wrapper about \pkg{RODBC}.
On load, it looks for data source names (\acronym{DSN}s) and attempts to
create a like-named interface function for each.
Additional data-source interfaces can be defined at any time.
These functions support multi-statement \acronym{SQL} scripts, with or without 
parameters or embedded \R expressions, and dispense with the need to provide
explicit communication details with every query.
}
\section{Version}{
0.5.0
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

# Obtain help on usage (in text format).
mysource("help", "text")

\dontrun{
# Submit a query to the new source.
mysource("select 1")

# Submit a parameterised query from file.
mysource("transactions.sqrl", customerid = 111111)

# Define a source from file.
sqrlSource("ratatoskr", "~/yggdrasil")

# Submit a query to that source.
ratatoskr("select messages from vedfolnir ",
          "where addressee = 'nidhogg'")
}
}
\keyword{database}
\keyword{interface}
\keyword{package}
