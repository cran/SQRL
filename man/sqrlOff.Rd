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
If this is set to TRUE, the function will additionally call
'\link[RODBC:odbcCloseAll]{RODBC:odbcCloseAll}' to ensure all 'RODBC'
connections are closed.
However, this will also close any 'RODBC' connections not belonging to 'SQRL'.
Use of this option should not normally be necessary.
}
}
\value{
Returns invisible NULL.
}
\author{
\packageAuthor{SQRL}
}
\seealso{
\code{\link[RODBC:RODBC-package]{RODBC}}
}
\examples{
require(SQRL)

# Calling sqrlOff() will deactivate and unload SQRL.
sqrlOff()
}
\keyword{ database }
