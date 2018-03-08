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
chaos("select ", 1, " from dual")

# Submit a query from file.
chaos("my/", "file.sql")

# Submit a parameterised query from file.
chaos("emissions.sqrl", make = "VEB", model = "Trabant 601")

# Get information on source data types.
chaos("typeinfo")

# Get a list of source tables. 
chaos("tables")

# Get information on the columns of a particular table.
chaos("columns", "my.table")
}
If necessary, a connection channel will be opened beforehand.
The connection will remain open afterwards.

If a file called (say) \dQuote{select 1} should exist, then
\code{chaos("select 1")} submits the content of that file (rather than the
apparent query).

The \command{typeinfo}, \command{tables}, and \command{columns} commands are 
simple wrappers to \code{\link[RODBC:sqlTypeInfo]{RODBC:sqlTypeInfo}}, 
\code{\link[RODBC:sqlTables]{RODBC:sqlTables}}, and
\code{\link[RODBC:sqlColumns]{RODBC:sqlColumns}} (respectively).
}
\section{Reviewing Settings}{
\preformatted{
# Get the value of one named parameter.
chaos("uid")

# List the values of all parameters.
chaos("config")
}
Passwords are never returned.
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
chaos("driver" = "path/to/file")

# Import an entire configuration file.
chaos("config", "path/", "to/", "file")
}
Calls of the form \code{chaos("name value")}, 
\code{chaos("name", "value")}, \code{chaos("name", value)},
\code{chaos(name = value)}, \code{chaos("name" = value)},
\code{chaos(name = "value")} and \code{chaos("name" = "value")}
are largely interchangeable.

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
the \R search path, then the change request will be denied.

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
# View these notes.
chaos("help")

# Alternative form.
chaos("?")
}
The above are simply alternatives to \code{help(sqrlUsage)} and
\code{?sqrlUsage}.
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
