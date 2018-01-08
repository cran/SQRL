\name{SQRL-package}
\alias{SQRL-package}
\alias{SQRL}
\docType{package}
\title{
\packageTitle{SQRL}
}
\description{
Handles multi-statement 'SQL' scripts, optionally containing embedded 'R'.
Enables parameterized 'SQL' queries.
Stops on remote 'SQL' errors.
Automatic generation of a like-named user-interface to each data source.
Manages multiple connections, their interfaces, and their parameters.
Supports data-source/connection configuration files.
Protects connection handles from deletion.
Provides visual indication of which connections/channels are open.
Provides visual indication of queries-in-progress (platform dependent).
Performs implicit pasting of multiple expressions into single statements.
Detects closure of connection channels by remote servers.
Attempts reconnection after network outages.
Wrapper to 'RODBC' (and, as such, purely an option of convenience).
}
\details{
\tabular{ll}{ \tab \cr
Package:  \tab 'SQRL' \cr
Version:  \tab 0.1.1 \cr
Date:     \tab 2018-01-08 \cr
Licence:  \tab GPL-3 \cr
Depends:  \tab 'R' (>= 3.2.0) \cr
Imports:  \tab 'base', 'RODBC' \cr
Suggests: \tab 'utils' \cr
}

\packageIndices{SQRL}

On load, the package will look for data source names (DSNs) via 'RODBC', and
attempt to create a like-named interface function for each (additional data
sources can be defined at any time).
These interfaces dispense with the need for explicit connection parameters on
every query.
They support multi-statement 'SQL' scripts, with or without embedded 'R'
expressions.
}
\author{
\packageAuthor{SQRL}
}
\keyword{ database }
\keyword{ interface }
\keyword{ package }
\seealso{
\code{\link[RODBC:RODBC-package]{RODBC}}
}
\examples{
require(SQRL)
sqrlSources()
sqrlSource("entropy",
            "dbcname=EntSrvr",
            "driver=PostgreSQL ANSI(x64)",
            "uid=<uid>;pwd=<pwd>")
entropy("config")

\dontrun{
entropy("select ", 1)
}
}
