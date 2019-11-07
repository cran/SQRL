\name{sqrlOff}
\alias{sqrlOff}
\title{
Close Connections and Deactivate the Package
}
\description{
Closes all connections, detaches the interface environment (\var{SQRL:Face})
from the search path, and unloads the \pkg{SQRL} namespace.
No further communication with any data source will be possible through
\pkg{SQRL} (until it is reloaded).
}
\usage{
sqrlOff()
}
\value{
Returns invisible \code{NULL}.
}
\note{
Calls to \code{\link[RODBC:odbcClose]{RODBC::odbcCloseAll}} will close any
connection channels open in \pkg{SQRL}.
}
\seealso{
\pkg{\link[SQRL:SQRL-package]{SQRL}}
}
\examples{
\dontrun{
# Calling sqrlOff() will deactivate and unload SQRL.
sqrlOff()
}
}
\keyword{database}
