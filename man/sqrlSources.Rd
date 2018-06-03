\name{sqrlSources}
\alias{sqrlSources}
\title{
List Data Sources and their Interfaces
}
\description{
Returns a summary of defined data sources.
These will consist of system and user \acronym{DSN}s, plus any additional
sources defined via \code{\link{sqrlSource}}.
}
\usage{
sqrlSources(...)
}
\arguments{
  \item{...}{
An optional character string.
If set to one of \dQuote{all}, \dQuote{user}, or \dQuote{system}, then a call is
made to \code{\link[RODBC:odbcDataSources]{RODBC:odbcDataSources}} (with the
corresponding \var{type} value) to re-examine that class of data source names
(\acronym{DSN}s) and import all those found.
If set to \dQuote{remove}, then all currently defined sources are deregistered. 
}
}
\value{
Returns a data frame of data source details.
}
\note{
The return frame may have zero rows, if no data sources are defined.

Sources need only to have been defined; they need not actually exist.

\acronym{DSN}s with \dQuote{Access}, \dQuote{dBASE}, or \dQuote{Excel} in their
names are not automatically imported.
They can be manually added via \code{\link{sqrlSource}}.
}
\seealso{
\code{\link{sqrlInterface}},
\code{\link{sqrlSource}},
\code{\link[RODBC:odbcDataSources]{RODBC:odbcDataSources}}
}
\examples{
# Review defined sources.
sqrlSources()

\dontrun{
# Sample sqrlSources() output:

   name interface open    uid                     driver
1 chaos     chaos    N zarkov       PostgreSQL ANSI(x64)
2 order      <NA>    N zarkov MySQL ODBC 5.3 ANSI Driver

# Here, there are two data sources; 'order' and 'chaos'.
# The interface to 'chaos' is a function of the same name.
# No interface has yet been defined for 'order' (use of
# that name is prevented due to its conflicting with the 
# base:order function). Neither source (channel) is open.

# Contents of the 'uid' column can be incorrect when the
# UID is contained within a DSN. As such, this column is
# likely to be absent in the next release.
}

# Remove all sources.
sqrlSources("remove")

# Reload user DSNs.
sqrlSources("user")
}
\keyword{database}
\keyword{interface}
