\name{sqrlAll}
\alias{sqrlAll}
\title{
Broadcast a Command to All Data Sources
}
\description{
Passes a single command to every \pkg{SQRL} data source in turn. 
}
\usage{
sqrlAll(...)
}
\arguments{
  \item{\dots}{
The command to broadcast (as per \code{\link{sqrlUsage}}).
}
}
\value{
Returns a named list containing the result of the command for each data source.
The list is invisible, except when retrieving (getting) a named parameter value.
}
\note{
The command is passed to all \pkg{SQRL} data sources, whether or not they
have defined interfaces.

The command can be a \acronym{SQL} query.
}
\seealso{
\code{\link{sqrlOff}},
\code{\link{sqrlUsage}}
}
\examples{
# Show all interfaces (visible return).
sqrlAll("interface")

# Do not convert strings to factors.
sqrlAll("stringsAsFactors", FALSE)

# Enable all connection indicators.
sqrlAll(visible = TRUE)

# Close all open channels.
sqrlAll("close")

# Remove all defined sources.
sqrlAll("remove")
}
\keyword{misc}
