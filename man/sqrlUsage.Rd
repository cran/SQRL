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

Subsequent sections provide usage examples for an interface called \code{thoth}.
The names of your own interface functions can be discovered by calling
\code{\link[SQRL:sqrlSources]{sqrlSources()}}.
}
\section{Opening and Closing}{
\preformatted{
# Open a connection to the data source.
thoth()

# Alternative method (explicit form).
thoth("open")

# Doing this is fine (the channel survives).
rm(list = ls(all.names = TRUE))

# Check if the connection is open.
thoth("isopen")

# Open a connection and confirm status.
thoth()$isopen

# Close the connection.
thoth("close")

# Close the connection when not in use.
thoth(autoclose = TRUE)
}
Opening connections in the above way isn't usually necessary, since this occurs
automatically as required.

The \command{isopen} command \dQuote{pings} the data source, to reliably
establish whether or not the connection really is open (including after a
network outage or remote closure).

With \code{autoclose = TRUE}, \command{isopen} will normally return
\code{FALSE}, since the connection is closed after every data source command
sequence (including the \command{open} command).
}
\section{Submitting Queries}{
\preformatted{
# Submit a query.
thoth("select 1")

# Submit another query.
thoth("select ", sample(6, 1), " from dual")

# Submit a query from file.
thoth("my/", "file.sql")

# Submit a parameterised query from file.
thoth("emissions.sqrl", make = "VEB", model = "Trabant 601")

# Submit a multi-statement query.
thoth(query = "use necronomicon; select top
               <R> N </R> shoggoths from pit",
      N = 5)

# Ensure input is treated only as a file name.
thoth(file = "create table")
}
If necessary, a connection channel will be opened beforehand.
The connection will remain open afterwards.

If a query should fail because of an unexpectedly lost connection, an attempt
will be made to re-connect and re-submit.
Unless credentials are required for authentication, this should go unnoticed by
the user.

When a query returns no data (as would \sQuote{use database}), the interface
function returns invisibly.

If a file called (say) \file{use database} should exist, then
\code{thoth("use database")} submits the content of that file (rather than the
apparent command).
This renders use of the \var{file} argument mostly unnecessary.

Use of either the \var{query} or \var{file} argument forces interpretation of
the corresponding value as a query or file path (from which to read a query),
respectively.
The \var{query} argument uses the \var{verbose} option and allows multiple
statements and embedded \R (as with \code{\link{sqrlScript}} files), whereas
unnamed queries do not (instead, they allow and concatenate multiple strings).
}
\section{Querying Metadata}{
\preformatted{
# Get information on source data types.
thoth("typeinfo")

# List all tables.
thoth("tables")

# List all tables within a database (schema).
thoth("tables", "mydatabase")

# Get information on the columns of a particular table.
thoth("columns", "my.table")
}
The \command{typeinfo}, \command{tables}, and \command{columns} commands are
simple (reduced functionality) wrappers about
\code{\link[RODBC:sqlTypeInfo]{RODBC:sqlTypeInfo}},
\code{\link[RODBC:sqlTables]{RODBC:sqlTables}}, and
\code{\link[RODBC:sqlColumns]{RODBC:sqlColumns}} (respectively).
}
\section{Reviewing Settings}{
\preformatted{
# Get the associated source definition.
thoth("source")

# Get the value of one named parameter.
thoth("uid")

# Alternative method (pings the source).
thoth()$uid

# List the values of all parameters.
thoth("config")
}
Passwords are returned obliterated.
}
\section{Setting Parameters}{
\preformatted{
# Enable visible indication of open connections.
thoth("visible", TRUE)

# Define the ping statement for the data source.
thoth("ping", "use database")

# Do not convert strings to factors.
thoth("stringsAsFactors FALSE")

# Set (opening and closing) table-name quotes.
thoth(tabQuote = c("`", "'"))

# Setting multiple parameters at once.
thoth(as.is = TRUE, na.strings = c("NA", "-", ""))

# Set one (named) parameter from a file.
thoth("ping" = "path/to/file")

# Import an entire configuration file.
thoth("config", "path/", "to/", "file")

# Reset parameters to their default values.
thoth("reset", c("as.is", "na.strings"))
}
Calls of the form \code{thoth("name value")},
\code{thoth("name", "value")}, \code{thoth("name", value)},
\code{thoth(name = value)}, \code{thoth("name" = value)},
\code{thoth(name = "value")} and \code{thoth("name" = "value")}
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
thoth("interface", "H")

# Change it back.
H(interface = "thoth")
}
If the proposed new interface name already belongs to some other object within
the \R search path, then the change request will be denied (unless that name is
\dQuote{remove}, in which case the current interface function will be deleted).

A successful change deletes the previous interface.
}
\section{Listing Data Sources}{
\preformatted{
# See the data sources and their interfaces.
thoth("sources")
}
This is equivalent to calling \code{\link[SQRL:sqrlSources]{sqrlSources()}}.
}
\section{Getting Help}{
\preformatted{
# Get help on 'thoth'.
thoth("help")

# Alternative form.
thoth("?")
}
The above calls will attempt to provide help tailored for the specific
interface, and will fall back to these notes (\code{help(sqrlUsage)} or
\code{?sqrlUsage}) should that fail.

Either of the commands \command{text} or \command{html} may be appended to
\command{help} if a specific output format is required.
}
\section{Removing the Source}{
\preformatted{
# Deregister the associated source.
thoth("remove")
}
This closes any open connection to the data source, deletes the
(\code{thoth}) interface function, and deregisters the source from \pkg{SQRL}.
}
\seealso{
\code{\link{sqrlAll}},
\code{\link{sqrlConfig}},
\code{\link{sqrlParams}},
\code{\link{sqrlScript}}
}
\keyword{database}
\keyword{interface}
