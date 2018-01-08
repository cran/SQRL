\name{sqrlSources}
\alias{sqrlSources}
\title{
List Data Sources and their Interfaces
}
\description{
Calls '\link[RODBC:odbcDataSources]{RODBC:odbcDataSources}' to acquire
local user and system DSNs, then returns a data frame of defined sources.
This includes any additional sources that have been user-defined via
'\link{sqrlSource}'.
}
\usage{
sqrlSources()
}
\value{
Returns a data frame of data sources.

This frame may have zero rows, if no registered data source names (DSNs) were
found and no other sources have been manually defined.
}
\author{
\packageAuthor{SQRL}
}
\note{
This runs automatically on package load (with output suppressed).

Sources need only to have been defined; they need not actually exist.

DSNs with 'Access', 'dBASE', or 'Excel' in their names are specifically
excluded from automatic inclusion, but can be manually added.
}
\seealso{
\code{\link{sqrlInterface}}, \code{\link{sqrlSource}}
}
\examples{
require(SQRL)
sqrlSources()

\dontrun{
# Sample sqrlSources() output:

   name interface open  uid                     driver
1 chaos     chaos    N mike       PostgreSQL ANSI(x64)
2 order      <NA>    N mike MySQL ODBC 5.3 ANSI Driver

# Here, there are two data sources; 'order' and 'chaos'.
# The interface to 'chaos' is a function of the same name.
# No interface has yet been defined for 'order' (the default
# name of 'order' would clash with the base::order function,
# and so is prohibited by SQRL). The user can define an
# interface whenever they choose. Neither source is open.
}
}
\keyword{ database }
\keyword{ interface }
