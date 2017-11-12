\name{sqrlInterface}
\alias{sqrlInterface}
\title{
Creates Data Source Interfaces
}
\description{
Creates, renames, and/or deletes interfaces to data sources.
Communications with data sources (i.e., 'SQL' queries) are performed through
these interface functions.
}
\usage{
sqrlInterface(datasource = "", interface = "", delete = FALSE)
}
\arguments{
  \item{datasource}{
The name of a defined data source (character string).
}
  \item{interface}{
The name to use for the data source's interface function (character string).
If omitted, the data source name is used by default.
}
  \item{delete}{
If set to TRUE, the 'interface' argument is ignored and any existing interface
to the data source is deleted.
}
}
\value{
Returns the interface function's definition, invisibly.
}
\author{
\packageAuthor{SQRL}
}
\note{
The (this) 'sqlrInterface' function will block the use of any interface name
that clashes with that of an object already on the 'R' search path.
It will throw an error should such a conflict be detected.
A successful call of 'sqrlInterface' guarantees the new interface exists.

Interfaces are stored in a publicly accessible environment, 'SQRL:Face',
attached to the 'R' search path when the package is loaded.
Interference with that environment may have consequences.
}
\seealso{
\code{\link{sqrlSources}}, \code{\link{sqrlUsage}}
}
\examples{
require(SQRL)
sqrlSource("entropy", "dbcname=EntSrvr;driver=PostgreSQL ANSI(x64)")
sqrlSources()
entropy("name")
sqrlInterface("entropy", "ent")
sqrlSources()
ent("name")

\dontrun{

ent("select ", 1)
ent("My/SQL/", file, ".sql")
}
}
\keyword{ interface }
