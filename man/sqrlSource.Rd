\name{sqrlSource}
\alias{sqrlSource}
\title{
Define New Data Sources
}
\description{
Defines a new data source (or redefines an existing one), and creates an
interface function for communicating with it.}
\usage{
sqrlSource(name = "", ...)
}
\arguments{
  \item{name}{
A character string, to be used as the name of the data source and its interface.
}
  \item{\dots}{
One or more character strings, defining either a connection string, or the path
to a configuration file (from which all parameters will be read).
}
}
\details{
Items in ... are pasted together with 'sep = ""'.
If that results in the name of an existing data source, a copy of that source is
made under the new name.
If that results in the path to a 'SQRL' configuration file, then the source is defined from that.
Otherwise, the items are concatenated into a semi-colon delimited connection
string and the source is defined by that.
}
\value{
Returns invisible NULL.
}
\author{
\packageAuthor{SQRL}
}
\note{
The defined source need not exist.

Redefinition (overwriting) is allowed, although many (channel defining)
parameters cannot be changed while the source is open.
Attempting to alter one of these will produce an error.

If a source has both a defined DSN (a non-empty 'dsn' parameter value) and a
defined connection string (a non-empty 'connection' parameter value), then the
string takes precedence (see '\link{sqrlConfig}').

Components of the connection string are 'DSN', 'DBCName', 'Driver', 'UID',
'Username' (an alias for 'UID'), 'PWD', and 'Password' (an alias for 'PWD').
These are not case sensitive.
Each of these may be defined as a (lower case only) placeholder; '<dsn>',
'<dbcname>', '<driver>', '<uid>', and '<pwd>', to be substituted with the
corresponding parameter values on the opening of a channel (see '\link{sqrlConfig}').

Defining a connection string via 'DSN', rather than 'DBCName' and 'Driver', is
not recommended in 'MySQL', since 'SQRL' will fail to identify the driver,
leading to an incorrect colQuote (and forcing manual setting of that parameter).
This problem should not occur when connecting to a 'MySQL' source via a locally
registered DSN (rather than by a connection string).

Use of usernames, driver names, etc., containing '=', ';', '{', or '}', is not
recommended (and is prohibited on many systems).

In 'Rgui', the minimum definition of the connection string is 'DBCName' and
'Driver' (or 'DSN').
The user should be prompted for 'UID' and 'PWD' when a channel is opened (a
feature provided by 'RODBC').
On other 'R's, 'UID' and 'PWD' should be included in the string (it is often
preferable to do this in 'Rgui' as well).

The (this) 'sqrlSource' function prohibits the use of a name that clashes with
that of any other object on the search path, and will throw an error on such a
conflict.
A successful call of this function consequently guarantees the existence of 
the named data source interface function.

The interface can be changed after source definition, if mismatched (source,
interface) names are required.

In addition to the usual manner, file paths can be specified as 'A<<B'.
Such paths are interpreted as start from directory 'A' (or the current
directory, if 'A' is omitted), and climb back up toward root until 'B' is found.
}
\seealso{
\code{\link{sqrlConfig}}, \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}
}
\examples{
require(SQRL)
sqrlSource("entropy",
            "DBCName=EntSrvr;Driver=<driver>",
            "UID=alfie;PWD=pass")
sqrlSources()
sqrlSource("entcopy", "entropy")
sqrlSources()

\dontrun{

sqrlSource("entropy", "path/to/entropy.config")
sqrlSource("entropy", "path/", "to/", otherfile, ".txt")
}
}
\keyword{ database }
