\name{sqrlSource}
\alias{sqrlSource}
\title{
Define New Data Sources
}
\description{
Defines (registers) new data sources and creates the interface functions for
communicating with them.
For \acronym{DSN}s, this process occurs automatically when \pkg{SQRL} is loaded,
thereby making the manual use of this function unnecessary for those sources.
The function can also redefine or delete (deregister) existing sources.
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
The arguments must contain (at least) a name and definition for the source.
In simplest form, these could be given as either (\var{name}, \var{definition})
or (\var{name} = \var{definition}), where both \var{name} and \var{definition}
are single character strings.
In decreasing order of precedence, the definition can be the path of a
configuration file (containing a connection string or \acronym{DSN}, as per
\code{\link{sqrlConfig}}), the name of an existing \pkg{SQRL} source (to copy
all settings from), an \acronym{ODBC} connection string (as a character vector
of components, or as a single string containing the equals character; =), or
as the name of a \acronym{DSN}.

When clarity is required, the keywords \code{config}, \code{copy},
\code{connection} and \code{dsn} can be used to explicitly specify a
configuration file, existing source, connection string or \acronym{DSN},
respectively.
If the definition is given as multiple terms, and none of these four keywords
are present, or if one of the named terms does not correspond to the name of
an \pkg{RODBC}/\pkg{SQRL} parameter, then the terms are assumed to be
components of a connection string.
If, instead, the definition is given as multiple terms and at least one of these
four keywords is present, and when all of the remaining terms appear to be
\pkg{RODBC}/\pkg{SQRL} parameters, then those remaining terms will be treated
as such (rather than as connection string components).
The examples (below) should illustrate these statements.

Whichever form of definition is employed, the new interface name (which defaults
to the source name) must not conflict with that of any function on the \R search
path (or else an error will be thrown).

Redefinition of an existing source is allowed, provided it is closed at the
time.

When the source name is \dQuote{remove}, the definition is interpreted as a list
of sources to be deregistered.
This precludes the use of \dQuote{remove} as a source name.
Alternatively, redefining a source to \code{NULL} also deregisters the source.
}
\value{
An invisible list of the new source's parameter values.
This is a change from previous versions of \pkg{SQRL}, wherein only the name of
the source's interface function was returned.
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
# Define a new source, 'A', by a connection string. Alternatively,
# the string could be replaced with the name of a DSN, the path of
# a config file, or the name of an existing source (to be copied).
# This particular connection string would be for a GNU/Linux system
# upon which the unixODBC driver alias 'MariaDB' has been defined,
# in addition to the 'MDB' alias for the server address.
sqrlSource(A = "Driver=MariaDB;Server=MDB;User=zarkov;Password=zenith")

# Redefine source 'A', by a connection string given in sections.
# This is for a GNU/Linux system without a unixODBC driver alias.
sqrlSource("A", "dbcname=planet;uid=zakharov;pwd=$tdwallet(planet)",
                "driver=/opt/teradata/client/16.10/lib64/tdata.so")

# Define a new source, by way of named connection-string components.
# This example is for a Windows-system client, and uses the '<pwd>'
# placeholder (it remains to set a value for the pwd parameter before
# connecting to the ODBC source).
sqrlSource("jumbo",
           driver = "PostgreSQL ANSI(x64)",
           server = "localhost",
           port = 5432,
           uid = "admin",
           pwd = "<pwd>")

# Define another source, as a vector of connection string
# components, along with some RODBC/SQRL parameter values.
sqrlSource("mydb", believeNRows = FALSE, autoclose = TRUE,
           connection = c(Driver = "SQLite3 ODBC Driver",
                          Database = "C:/mydatabase.db",
                          Timeout = 10000,
                          StepAPI = 1))

# Define another source, from a DSN (rather than a connection
# string), while also setting an RODBC/SQRL parameter value.
sqrlSource("ak", dsn = "Akademgorodok", as.is = TRUE)

# Redefine the source. This time, the dsn term is treated as a
# connection string component (because the server term must be).
sqrlSource("ak", dsn = "Akademgorodok", server = "Novosibirsk")

# Define another source, 'Huma', by a list of components, which
# includes an explicit source name, a vector of connection string
# components, a config list of RODBC/SQRL parameter values, and
# one more RODBC/SQRL parameter value outside of that config list.
# This list format is convenient for programmatic source creation.
sqrlSource(list(name = "Huma",
                connection = c(DRIVER = "Firebird/InterBase(r) driver",
                               DBNAME = "C:\\\\Database\\\\myDB.fdb",
                               UID = "MCSSITE",
                               PWD = "mcssite"),
                config = list(as.is = TRUE, scdo = FALSE),
                visible = TRUE))

# Define a source called 'source'. Although the name 'source' clashes
# with that of the base::source function, this definition is allowed
# because we simultaneously set an interface, 'S', that does not clash.
# Note the mixture of named and unnamed connection string components.
sqrlSource(source = list(connection = c("DSN=Source", UID = "me"),
                         interface = "S"))

# Another source, defined by a list of named connection string
# components (without setting any other parameter values).
sqrlSource(sf = list(driver = "{SnowflakeDSIIDriver}",
                     server = "xyz.eu-central-1.snowflakecomputing.com",
                     uid = "me", pwd = "guess", port = 443))

# Create a configuration file.
config.file <- tempfile()
writeLines(c("autoclose = TRUE",
             "readOnlyOptimize = TRUE",
             "visible = FALSE"),
           config.file)

# Define a new source, 'papango', as a copy of the existing source,
# 'Huma', then apply the above configuration file over that, and
# then set values for the dsn and interface parameters over those.
# In this example, the value of the 'visible' parameter inherited
# from Huma is overwritten by the value from the config file.
# Setting the dsn parameter likewise erases the value of the
# connection parameter inherited from Huma, while the inherited
# uid and pwd values survive. Alternatively, we could have set
# 'connection = "dsn=Aythya"' in place of 'dsn = "Aythya"', and
# this would ignore the inherited uid and pwd values.
sqrlSource("papango",
           copy = "Huma",
           config = config.file,
           dsn = "Aythya",
           interface = "P")

# Review all defined sources.
sqrlSources()

# Review the configuration of the papango source.
P("config")

# Review details of how we connect to the source.
P("source")

# Clean-up (various methods of source removal).
sqrlSource("remove", "jumbo")
sqrlSource(remove = c("mongo", "papango"))
sqrlSource(Huma = NULL)
unlink(config.file)
}
\keyword{database}
