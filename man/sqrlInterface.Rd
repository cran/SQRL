\name{sqrlInterface}
\alias{sqrlInterface}
\title{
Creates Data Source Interfaces
}
\description{
Creates, renames, and removes data-source interface functions.

Communications with data sources (including \acronym{SQL} queries) are conducted
through these interfaces.
}
\usage{
sqrlInterface(...)
}
\arguments{
  \item{\dots}{
The name of a registered data source, and the name to use for its interface.
}
}
\details{
The source and interface names may be supplied as two character strings,
\code{("source", "interface")}, or as a \code{(source = "interface")}, or
\code{("source" = "interface")}, parameter-value pair.

The setting of an interface whose name would clash with that of any other
function already on the \R search path is prevented.
An error will be thrown if a potential conflict is detected.
Conversely, a successful call of this function guarantees both the existence of
the new interface, and the uniqueness of its name (amongst functions).

If the interface name is specified as either \code{NULL} or \code{"remove"},
then any existing interface is deleted (and no new interface is created).

If only a single string is supplied, the name of that source's interface
function is returned.

Use of this functions is not ordinarily required, except when a registered
data source does not already have an interface function.
}
\value{
Returns the name of the source's interface function (visibly on get, invisibly
on set).
}
\note{
Interfaces are stored in a publicly accessible environment, \code{SQRL:Face}.
This is attached to the \R search path when the package is loaded.
}
\seealso{
\code{\link{sqrlSource}}
\code{\link{sqrlSources}}
}
\examples{
# Define a new data source, named 'entropy'.
sqrlSource("entropy", uid = "ludwig",
           driver = "{SQL Server Native Client 11.0}",
           server = "Clausius", database = "Gibbs")

# The source comes with an interface of the same name.
sqrlInterface("entropy")
entropy("sources")

# Change the name of the interface function.
sqrlInterface(entropy = "S")

# An equivalent alternative form.
sqrlInterface("entropy", "S")

# The name of the source remains unchanged.
sqrlInterface("entropy")
S("sources")

# Submit a dummy 'query', via the interface.
# Normally, you'd put some SQL in here, but
# the source would have to exist for that.
S("<R> 'hello, world'")

# Remove the source's interface function.
sqrlInterface(entropy = NULL)

# An equivalent alternative form.
sqrlInterface("entropy", "remove")

# The source remains, but has no interface.
sqrlInterface("entropy")
sqrlSources()
}
\keyword{interface}
