\name{sqrlUsage}
\alias{sqrlUsage}
\title{
How to Use the Interface Functions
}
\description{
This material does not describe a single function, but (rather) how to use
\pkg{SQRL} interfaces, once created.
These functions do not have their own help files, since their names are not
pre-determined.
}
\details{
Once you have a named interface, created either automatically (on loading of the
\pkg{SQRL} namespace) or manually (via
\code{\link[SQRL:sqrlSource]{sqrlSource()}}), it can be used to communicate with
the associated data source.
Connection handles and communication parameters are managed under the hood.

Subsequent sections provide usage examples for an interface called \code{chaos}.
The names of your own interface functions can be discovered by calling
\code{\link[SQRL:sqrlSources]{sqrlSources()}}.
}
\section{Opening and Closing}{
\preformatted{
# Open a connection to the data source.
chaos()

# Alternative method (explicit form).
chaos("open")

# Doing this is fine (the channel survives).
rm(list = ls(all.names = TRUE))

# Check if the connection is open.
chaos("isopen")

# Close the connection.
chaos("close")
}
Opening connections in the above way isn't usually necessary, since this occurs
automatically as required.

The \command{isopen} command \dQuote{pings} the data source, to reliably
establish whether or not the connection really is open (including after a
network outage or remote closure).
}
\section{Submitting Queries}{
\preformatted{
# Submit a query.
chaos("select 1")

# Submit another query.
chaos("select ", sample(6, 1), " from dual")

# Submit a query from file.
chaos("my/", "file.sql")

# Submit a parameterised query from file.
chaos("emissions.sqrl", make = "VEB", model = "Trabant 601")
}
If necessary, a connection channel will be opened beforehand.
The connection will remain open afterwards.

If a file called (say) \file{use database} should exist, then
\code{chaos("use database")} submits the content of that file (rather than the
apparent command).
}
\section{Querying Metadata}{
\preformatted{
# Get information on source data types.
chaos("typeinfo")

# Get a list of source tables.
chaos("tables")

# Get information on the columns of a particular table.
chaos("columns", "my.table")
}
The \command{typeinfo}, \command{tables}, and \command{columns} commands are
simple wrappers to \code{\link[RODBC:sqlTypeInfo]{RODBC:sqlTypeInfo}},
\code{\link[RODBC:sqlTables]{RODBC:sqlTables}}, and
\code{\link[RODBC:sqlColumns]{RODBC:sqlColumns}} (respectively).
}
\section{Reviewing Settings}{
\preformatted{
# Get the associated source definition.
chaos("source")

# Get the value of one named parameter.
chaos("uid")

# Alternative method (pings the source).
chaos()$uid

# List the values of all parameters.
chaos("config")
}
Passwords are returned obliterated.
}
\section{Setting Parameters}{
\preformatted{
# Enable visible indication of open connections.
chaos("visible", TRUE)

# Define the ping statement for the data source.
chaos("ping", "use database")

# Do not convert strings to factors.
chaos("stringsAsFactors FALSE")

# Set (opening and closing) table-name quotes.
chaos(tabQuote = c("`", "'"))

# Setting multiple parameters at once.
chaos(as.is = TRUE, na.strings = c("NA", "-", ""))

# Set one (named) parameter from a file.
chaos("ping" = "path/to/file")

# Import an entire configuration file.
chaos("config", "path/", "to/", "file")

# Reset parameters to their default values.
chaos("reset", c("as.is", "na.strings"))
}
Calls of the form \code{chaos("name value")},
\code{chaos("name", "value")}, \code{chaos("name", value)},
\code{chaos(name = value)}, \code{chaos("name" = value)},
\code{chaos(name = "value")} and \code{chaos("name" = "value")}
are largely interchangeable.

The \var{driver} and \var{dsn} parameters accept file paths as their values.
For all other parameters, values are extracted from within any specified files.

Assigning \var{visible} \code{TRUE} authorises modification of the global
\code{prompt} option.
When running \file{R.exe}, \file{Rterm.exe} or \file{Rgui.exe} on a
\dQuote{Windows} operating system, this also authorises modification of the
\R window title.
}
\section{Changing the Interface}{
\preformatted{
# Change the interface.
chaos("interface", "K")

# Change it back.
K(interface = "chaos")
}
If the proposed new interface name already belongs to some other object within
the \R search path, then the change request will be denied (unless that name is
\dQuote{remove}, in which case the current interface function will be deleted).

A successful change deletes the previous interface.
}
\section{Listing Data Sources}{
\preformatted{
# See the data sources and their interfaces.
chaos("sources")
}
This is equivalent to calling \code{\link[SQRL:sqrlSources]{sqrlSources()}}.
}
\section{Getting Help}{
\preformatted{
# Get help on 'chaos'.
chaos("help")

# Alternative form.
chaos("?")
}
The above calls will attempt to provide help tailored for the specific
interface, and will fall back to these notes (\code{help(sqrlUsage)} or
\code{?sqrlUsage}) should that fail.
}
\section{Removing the Source}{
\preformatted{
# Deregister the associated source.
chaos("remove")
}
This closes any open connection to the data source, deletes the
(\code{chaos}) interface function, and deregisters the source from \pkg{SQRL}.
}
\seealso{
\code{\link{sqrlAll}},
\code{\link{sqrlConfig}},
\code{\link{sqrlInterface}},
\code{\link{sqrlParams}},
\code{\link{sqrlScript}},
\code{\link{sqrlSource}},
\code{\link{sqrlSources}}
}
\keyword{database}
\keyword{interface}
