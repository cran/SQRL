\name{sqrlOff}
\alias{sqrlOff}
\title{
Close Connections and Deactivate Package
}
\description{
Closes connections, detaches the interface environment ('SQRL:Face') from the
search path, and unloads the 'SQRL' namespace.
No further communication with any data source will be possible through 'SQRL'
(unless it is reloaded).
}
\usage{
sqrlOff(ensure = FALSE)
}
\arguments{
  \item{ensure}{
This argument has been deprecated, and will be removed in a future release.
If set to TRUE, the function will additionally call
'\link[RODBC:odbcCloseAll]{RODBC:odbcCloseAll}' to ensure all 'RODBC'
connections are closed.
This will also close any 'RODBC' connections not belonging to 'SQRL'.
}
}
\value{
Returns invisible NULL.
}
\author{
\packageAuthor{SQRL}
}
\note{
Calls to '\link[RODBC:odbcCloseAll]{RODBC:odbcCloseAll}' will close any 
connection channels open in 'SQRL'.
}
\seealso{
\code{\link[RODBC:RODBC-package]{RODBC}}
}
\examples{
require(SQRL)

\dontrun{
# Calling sqrlOff() will deactivate and unload SQRL.
sqrlOff()
}
}
\keyword{ database }
