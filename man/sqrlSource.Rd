\name{sqrlSource}
\alias{sqrlSource}
\title{
Define New Data Sources
}
\description{
Defines new data sources and creates the interface functions for communicating
with them.
Can also redefine existing sources.
}
\usage{
sqrlSource(...)
}
\arguments{
  \item{\dots}{
A name and definition for the source (refer to the details section, below).
}
}
\details{
The arguments may be specified as a single \var{name = definition} pair.
In this usage, \var{definition} may be a connection string, a configuration
file, a data source name (\acronym{DSN}), or the name of an existing \pkg{SQRL}
source.
In the case of a configuration file, the new source (\var{name}) is defined by
importing all the parameter settings of that file (see
\code{\link{sqrlConfig}}).
In the case of an existing \pkg{SQRL} source, the new source is defined as a
copy of the original.

Alternatively, the arguments may be supplied as a list of strings.
In this usage, the first string is taken for the new source name while the
remainder comprise the definition (connection strings and file paths may be
provided as components).

Lastly, the arguments may instead be supplied as a single string followed by at
least one \var{parameter = value} pair.
In this usage, the string is taken for the new source name, and the pairs as
components of the defining connection string (or \acronym{DSN}).

Whichever form is used, the new source name must not conflict with that of any
other object on the \R search path (or else an error will be thrown).
Redefinition of an existing source is allowed, provided it is closed.
}
\value{for
Returns invisible \code{NULL}, after creating an interface to the newly defined
source.
}
\note{
Source definitions are not checked for validity (specified connection strings
need not be correct, specified \acronym{DSN}s need not exist).

Connection strings may include placeholders; \dQuote{<dsn>}, \dQuote{<driver>},
\dQuote{<uid>}, and \dQuote{<pwd>}, to be replaced with the corresponding
parameter values on the opening of a channel.
These placeholders are case sensitive (see \code{\link{sqrlParams}}).

In \file{Rgui.exe} (only), username and/or password credentials may be omitted
from the connection string (because \pkg{RODBC} provides a facility, via the 
\acronym{ODBC} driver,  prompting for that information when the channel is
opened).
}
\seealso{
\code{\link{sqrlConfig}},
\code{\link{sqrlParams}},
\code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}
}
\examples{
# Define a new source from a DSN.
sqrlSource("daedalus", dsn = "Knossos")

# Define another source as a copy of the former.
sqrlSource(icarus = "daedalus")

# Redefine an existing source by a connection string.
# (This example is for a Windows-system client.)
sqrlSource("icarus",
            driver = "PostgreSQL ANSI(x64)",
            server = "localhost",
            port = 5432,
            uid = "asterion",
            pwd = "moo")

# Define a new source by a connection string.
# (This example is for a GNU/Linux-system client,
#  and employs the <pwd> password placeholder.)
sqrlSource("knossos",
            "dbcname=Knossos;uid=theseus;pwd=<pwd>",
            "driver=/opt/teradata/client/16.10/lib64/tdata.so")

\dontrun{
# Define a new source from a configuration file.
sqrlSource(minos = "path/to/minos.config")

# Same thing, in an alternative form.
sqrlSource("minos", "path/to/", "minos.config")
}

# Review defined sources.
sqrlSources()
}
\keyword{database}
