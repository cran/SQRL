\name{sqrlInterface}
\alias{sqrlInterface}
\title{
Creates Data Source Interfaces
}
\description{
Creates, renames, and/or removes data source interface functions.

Communications with data sources (including \acronym{SQL} queries) are conducted
through these interfaces.
}
\usage{
sqrlInterface(...)
}
\arguments{
  \item{\dots}{
The name of a defined data source, and the name to use for its interface.
}
}
\details{
The source and interface names may be supplied as two character strings,
(\dQuote{source}, \dQuote{interface}), or as a (source = \dQuote{interface}), or
(\dQuote{source} = \dQuote{interface}), pair.

The setting of an interface whose name would clash with that of any other object 
already on the \R search path is prevented.
An error will be thrown if a potential conflict is detected.
Conversely, a successful call of this function guarantees both the existence of
the new interface, and the uniqueness of its name.

If the interface name is specified as either \code{NULL} or \dQuote{remove},
then any existing interface is deleted (and no new interface is created).

If only a single string is supplied, the name of that source's interface
function is returned.
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
}
\examples{
# Define a new data source, named 'entropy'.
sqrlSource("entropy", uid = "ludwig", pwd = "<pwd>",
           driver = "{SQL Server Native Client 11.0}",
           server = "Clausius", database = "Gibbs")

# The source comes with an interface of the same name.
sqrlInterface("entropy")
entropy("sources")

# Change the name of the interface function.
sqrlInterface("entropy", "S")

# The name of the source remains unchanged.
sqrlInterface("entropy")
S("sources")

\dontrun{
# Submit a query, via the interface.
S("select 1")

# Submit a script, via the interface.
S("My/SQL/file.sql")

# Submit a parameterised script, via the interface.
S("My/SQRL/file.sqrl", month = "April")
}

# Remove the source's interface function.
sqrlInterface("entropy", "remove")

# The source remains, but has no interface.
sqrlInterface("entropy")
sqrlSources()
}
\keyword{interface}
