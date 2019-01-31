\name{sqrlSource}
\alias{sqrlSource}
\title{
Define New Data Sources
}
\description{
Defines new data sources and creates the interface functions for communicating
with them.
Can also redefine and/or delete existing sources.
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
The arguments may be supplied as a single \var{name = definition} pair, or as
a sequence of unnamed strings.
In the latter usage, the leading string is taken for the new source \var{name},
while the remainder constitute its \var{definition}.
In either usage, the \var{definition} may be a connection string, a
configuration file, a data source name (\acronym{DSN}), or the name of an
existing \pkg{SQRL} source.
To be precise, if the \var{definition} specifies the path of an existing file,
then the source is configured from that file.
Otherwise, if the \var{definition} names an existing source, then the new source
is created as a copy of that.
Otherwise, if the \var{definition} consists of multiple terms, or if it contains
an equals character (=), then it is assumed to represent a connection string.
When none of these conditions apply, the \var{definition} is assumed to specify
a \acronym{DSN}.

Alternatively, the arguments may instead be supplied as a single string followed
by at least one named \var{parameter = value} pair.
In this usage, the leading string is taken for the new source name, while the 
named pairs constitute its definition.
The named pairs may represent components of a connection string, and/or
\pkg{SQRL} parameter values to be applied (see \code{\link{sqrlParams}}).
If one of the pairs is named \dQuote{copy}, then the new source is
created as a copy of the corresponding preexisting source.
If one of the pairs is named \dQuote{config}, then the new source is
configured from the corresponding file (see \code{\link{sqrlConfig}}).
If neither of those applies, and if no pair is named \dQuote{connection}
(i.e., if no explicit value was given for the connection string), and if either
the remaining names do not include \dQuote{dsn}, or they do include some other
name not recognised as being a \pkg{SQRL} parameter, then those pairs are
concatenated into a connection string.

Whichever form is used, the new interface name (which defaults to the source 
name) must not conflict with that of any other object on the \R search path (or
else an error will be thrown).

Redefinition of an existing source is allowed, provided it is closed.

When the source name is \sQuote{remove}, the definition is interpreted as a list
of sources to be deregistered.
This precludes the use of \sQuote{remove} as a source name.
Alternatively, redefining a source to \code{NULL} also deregisters the source.
}
\value{
Invisibly returns the interface function name, after defining the new source and
creating that interface.
In general, this need not match the name of the source itself. 
}
\note{
Source definitions are not checked for validity (specified connection strings
need not be correct, specified \acronym{DSN}s need not exist).

Connection strings may include placeholders; \dQuote{<dsn>}, \dQuote{<driver>},
\dQuote{<uid>}, and \dQuote{<pwd>}, to be replaced with the corresponding
parameter values on the opening of a channel.
These placeholders are case sensitive (see \code{\link{sqrlParams}}).

In \file{Rgui.exe}, the \acronym{ODBC} driver may, via \pkg{RODBC}, prompt for
missing connection details (username, password, etc.).
In other \R applications, those details will need to be complete (no prompting
occurs).
}
\seealso{
\code{\link{sqrlConfig}}
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

# Define a new source, ariadne, as a copy of the existing
# source, minos, then apply the configuration file conf.txt
# over that, and then set both the connection string and
# interface function name (parameter values) over those.
sqrlSource("ariadne",
            copy = "minos",
            config = "conf.txt",
            connection = "DSN=Knossos",
            interface = "a")
            
# Source names may conflict with those of preexisting
# R objects, provided that the config file defines a 
# conflict-free name for the source's interface function.
sqrlSource("c", "path/to/", "c.config")
}

# Review defined sources.
sqrlSources()

# Remove two of the sources.
sqrlSource("remove", c("daedalus", "knossos"))

# Remove another.
sqrlSource(icarus = NULL)
}
\keyword{database}
