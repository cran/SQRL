\name{sqrlUsage}
\alias{sqrlUsage}
\title{
How to Use the Interface Functions
}
\description{
This material does not describe a single function, but (rather) how to use
\pkg{SQRL} interfaces (once created).
These interface functions do not have their own static help files, since their
names are not known at build time.
}
\details{
Once you have a named interface (created either automatically, on loading of the
\pkg{SQRL} namespace, or manually, via \code{\link{sqrlSource}}), it can be used
to communicate with the associated data source.
Connection handles and communication parameters are managed under the hood.

The following sections provide usage examples for an interface called
\code{owl}.
The names of your own interface functions can be discovered by calling
\code{\link[SQRL:sqrlSources]{sqrlSources()}}.
}
\section{Opening and Closing}{
\preformatted{
# Open a connection to the data source.
owl()

# Alternative method (explicit form).
owl("open")

# This is fine (the channel survives).
rm(list = ls(all.names = TRUE))

# Check if the connection is open.
owl("isopen")

# Open a connection and confirm status.
owl()$isopen

# Close the connection.
owl("close")

# Close the connection when not in use.
owl(autoclose = TRUE)
}
Opening connections in the above way isn't usually necessary, since this occurs
automatically, as and when required.

When necessary, the \command{isopen} command \sQuote{pings} the data source, to
reliably establish whether or not the connection really is open (including after
a network outage or remote closure).

With \code{autoclose = TRUE}, \code{owl("isopen")} should always return
\code{FALSE}, whereas \code{owl()$isopen} will often return \code{TRUE}.
This is because the latter command attempts to open a connection, with its
return value being the connectivity status immediately after that attempt
(before \var{autoclose} takes effect and closes the connection).
This provides a test of data source reachability and responsiveness, regardless
of the \var{autoclose} setting.
}
\section{Submitting Queries}{
\preformatted{
# Submit a query.
owl("select 1")

# Submit another query.
owl("select '", sample(letters, 1), "' from dual")

# Submit a multi-statement query.
owl("use necronomicon; select top ",
    sample(6, 1), " shoggoths from pit")

# Filtering against a set of values.
owl("select columnA from database.table ",
    "where columnB in (", c(2, 3, 5), ")")

# Supplying a query as a list of components.
owl(list("select ", c("'red'", "'blue'")))

# A parameterised query (SQL with R in it).
owl("select <R> a + b </R>", a = 0, b = 1)

# Explicit form.
owl(query = list("select ", "<R>sin(x)</R>"), x = 0)

# Verbatim query.
owl(verbatim = "select 1 as N")

# Recall the result of the last successful query.
owl("result")
}
If necessary, a connection channel will be opened automatically.
The connection will remain open afterwards, unless \var{autoclose} is
\code{TRUE}.

When a query is supplied as components, the pieces are pasted together without
any intervening whitespace.
To facilitate their use with the \acronym{SQL} \code{in} operator, any atomic
vectors are collapsed to comma-separated values, beforehand.
This (default) behaviour can be altered with the \var{aCollapse} and
\var{lCollapse} parameters (as described in \code{\link{sqrlParams}}).

Using the \var{query} keyword overrides the order of precedence (as detailed
below).
Whereas \var{query} arguments go through the usual \pkg{SQRL} concatenation,
parsing, and \R-substitution process, \var{verbatim} arguments are submitted
directly and without alteration.
Consequently, the \var{verbatim} command accepts only a single character string,
which cannot be parameterised via embedded \R.

As \pkg{SQRL} aims to be flexible on input formatting, the above examples can be
extrapolated.
For instance, the explicit \var{query} could have been a single string.

When a query returns no data (as would \dQuote{use database}), the interface
function returns invisibly.

If a query should fail due to an unexpectedly lost connection, one further
attempt will be made to reconnect and resubmit (provided the \var{retry}
parameter is \code{TRUE}).
Unless user input is required for authentication, this should go unnoticed.
If temp tables were in use, then these will have been dropped along with the
original connection, and an error may still occur.
}
\section{Submitting Queries from File}{
\preformatted{
# Submit a query from file.
owl("my/file.sql")

# Equivalent alternative forms.
owl("my/", "file.sql")
owl(c("my/", "file.sql"))

# Explicit alternative forms.
owl(file = "my/file.sql")
owl(file = c("my/", "file.sql"))
}
Using the \var{file} keyword overrides the order of precedence (as detailed
below).
In its absence, unnamed arguments are treated as a file path when they point to
a readable file.

In the above examples, \code{list(...)} works just as well as \code{c(...)}.
Either way, the path components are pasted together without any intervening
whitespace (the path not being a literal query).
}
\section{Submitting Parameterised Queries from File}{
\preformatted{
# Submit a parameterised query from file.
owl("my/file.sqrl", day = 1, month = "May")

# Supplying the arguments in a list.
owl("my/file.sqrl", list(day = 1, month = "May"))

# Supplying the arguments in an explicitly named list.
owl("my/file.sqrl", args = list(day = 1, month = "May"))

# Supplying both the query and its arguments in a list.
owl(list(file = "my/file.sqrl", day = 1, month = "May"))
owl(list(file = c("my/", "file.sqrl"),
         args = list(day = 1, month = "May")))
}
To be clear, the phrase \dQuote{parameterised query} is not meant in the sense
of prepared or parameterised statements (as per package \pkg{RODBCext}).
In \pkg{SQRL}, parameter substitution occurs within \R (locally), with the
resulting string being passed to the \acronym{ODBC} driver as an ordinary query.
Refer to \code{\link{sqrlScript}} for the details.

The use of the \var{args} keyword is optional when all list members have
syntactically valid names (in the sense of
\code{\link[base:make.names]{base::make.names}}).
Any such lists are automatically interpreted as collections of named arguments
(and are unpacked to those collections).
Query arguments called, for instance, \var{file}, \var{proc}, or \var{query},
may need to be wrapped in \var{args} to ensure they are treated as intended,
and not as query (script) specifiers.

In keeping with \pkg{SQRL}'s intended flexibility around input formatting, any
of the file path specification methods of the previous section could also be
used here.
}
\section{Stored Procedures}{
\preformatted{
# Import procedures from file.
owl(library = "my/library.sqrl")

# List procedure names.
owl("library")

# List procedure definitions.
owl("Library")

# Run a named procedure.
owl("my procedure")

# Equivalent explicit form.
owl(proc = "my procedure")

# Run a parameterised procedure.
owl("Cropp River Rainfall", date = Sys.Date() - 1)

# Empty the library.
owl(library = NULL)
}
As detailed below, procedures top the order of precedence.
Consequently, the \var{proc} keyword is an entirely optional transparency
device.
In its absence, unnamed arguments are treated as the name of a procedure when
they name a procedure within the library.

The library-file procedure-definition format is described in
\code{\link{sqrlScript}}.
The path to such a file (i.e., the value of the \var{library} keyword) can be
supplied in any of the file-path formats of the previous sections (that is, as
a list or vector of components).

Any of the file-path and/or query-argument specification formats seen in the
previous two sections can equally be used with procedure names.
The only difference is to replace any \var{file} keyword with the \var{proc}
keyword.
}
\section{Querying Metadata}{
\preformatted{
# List all tables.
owl("tables")

# List all tables within a database (schema).
owl("tables", "mydatabase")

# Get information on the columns of a particular table.
owl("columns", "mydatabase.table")

# Get information on the primary keys of a particular table.
owl("primarykeys mydatabase.table")

# Get information on source data types.
owl("typeinfo")
}
The \command{tables}, \command{columns}, \command{primarykeys} and
\command{typeinfo} commands are simple (reduced functionality) wrappers about
\pkg{RODBC}'s \code{\link[RODBC:sqlTables]{sqlTables}},
\code{\link[RODBC:sqlColumns]{sqlColumns}},
\code{\link[RODBC:sqlColumns]{sqlPrimaryKeys}}, and
\code{\link[RODBC:sqlTypeInfo]{sqlTypeInfo}}, respectively.
These features are dependent upon the support of your \acronym{DBMS} and driver.
For some sources, the \var{believeNRows} parameter may need to be \code{FALSE}.
}
\section{Reviewing Settings}{
\preformatted{
# Get the associated source definition.
owl("source")

# Get the value of one named parameter.
owl("uid")

# Alternative method (pings the source).
owl()$uid

# List the values of all parameters.
owl("config")

# List a subset of parameter values.
owl("settings")
}
The \command{settings} subset is intended for restoring \pkg{RODBC} and/or
\pkg{SQRL} parameter values at the end of a script that changed some.
An example of this is given in \code{\link{sqrlScript}}.
It can also be used to transfer parameter values between sources.
}
\section{Setting Parameters}{
\preformatted{
# Set a parameter value.
owl(stringsAsFactors = FALSE)

# Set multiple parameter values.
owl(max = 1000, na.strings = c("NA", "-", ""))

# Set multiple values from a list.
owl(list(case = "toupper", scdo = FALSE))

# Set values from a list (explicit form).
owl(config = list(visible = TRUE, autoclose = TRUE))

# Import values from source 'wol'.
owl(config = wol("settings"))

# Import values from a configuration file.
owl(config = "my/config/file.txt"))

# Import one value from a file.
owl(pwd = c("path/", "to/", "file", ".txt"))

# Reset parameters to their default values.
owl(reset = c("errors", "nullstring"))
}
The \var{driver} and \var{dsn} parameters accept file paths as their values.
For all other parameters, values are extracted from within any specified files.

Assigning \var{visible} \code{TRUE} authorises modification of the global
\code{prompt} option.
When running \file{R.exe}, \file{Rterm.exe} or \file{Rgui.exe} on a Windows
operating system, this also authorises modification of the\R window title.

Calling \command{reset} on its own, as in \code{owl("reset")}, does nothing.

Further alternative input formats appear in the examples section of
\code{\link{sqrlParams}}.
}
\section{Changing the Interface}{
\preformatted{
# Change the interface.
owl(interface = "O")

# Change it back.
O(interface = "owl")
}
Changing the interface is just a particular case of setting a parameter.

If the proposed new interface name already belongs to some other function within
the \R search path, then the change request will be denied (unless that name is
\command{remove}, in which case the current interface function will be deleted).

A successful change deletes the previous interface.
}
\section{Listing Data Sources}{
\preformatted{
# See the data sources and their interfaces.
owl("sources")
}
This is equivalent to calling \code{\link[SQRL:sqrlSources]{sqrlSources()}}.
}
\section{Getting Help}{
\preformatted{
# Get help on 'owl' (alternative forms).
owl("help")
owl("?")

# Obtain help, in specific formats.
owl("help text")
owl("help html")
}
The above calls will attempt to provide help tailored for the specific
interface, and will fall back to these notes (\code{help(sqrlUsage)} or
\code{?sqrlUsage}) should that fail.
}
\section{Removing the Source}{
\preformatted{
# Deregister the associated source.
owl("remove")
}
This closes any open connection to the data source, deletes the interface
function (\code{owl}), and deregisters the source from \pkg{SQRL}.
}
\section{Order of Precedence}{
When unnamed arguments are supplied, such as in \code{owl("something")},
\pkg{SQRL} interprets those arguments with the following order of precedence:
\enumerate{
\item Procedure names (in the \var{library}),
\item File paths (of \acronym{SQL} or \acronym{SQRL} scripts),
\item Special words (\dQuote{close}, \dQuote{config}, etc.),
\item Parameter names (optionally followed by values),
\item Literal \acronym{SQRL} script (including pure \acronym{SQL}).
}
Hence, if a file called (say) \file{use database} should exist, then
\code{owl("use database")} submits the content of that file (rather than the
apparent \acronym{SQL} command).
Such conflicts can be resolved by assigning the unnamed arguments to the
appropriate keyword (\var{file}, \var{proc}, or \var{query}).
In this case, the new command would be \code{owl(query = "use database")}.
}
\seealso{
\code{\link{sqrlAll}},
\code{\link{sqrlConfig}},
\code{\link{sqrlParams}},
\code{\link{sqrlScript}}
}
\examples{
# Some default values, for demonstration purposes.
x <- 1; y <- 2

# Define a new SQRL source and its interface. The DSN need
# not exist, but this will fail (by design) if a function
# called 'owl' already exists on the R search path.
sqrlSource("owl", dsn = "HundredAcreWood")

# This (ordinarily pointless) SQRL script contains no SQL,
# which allows it to run even when the DSN does not exist
# (there being no need to open a connection). In this case,
# the x and y variables are implicitly inherited.
owl("<R> x * y")

# Explicitly assign x, while inheriting y.
owl("<R> x * y", x = 2)

# An alternative arrangement of arguments.
owl(x = 3, query = "<R> x * y", y = 1)

# Write the script to file.
myfile <- tempfile()
writeLines("<R> x * y", myfile)

# Run the script from file, inheriting variables.
owl(myfile)

# Run the script from file, with explicit arguments.
# (These are all equivalent.)
owl(myfile, x = 2, y = 3)
owl(myfile, list(x = 2, y = 3))
owl(myfile, args = list(x = 2, y = 3))
owl(file = myfile, x = 2, y = 3)
owl(file = myfile, list(x = 2, y = 3))
owl(file = myfile, args = list(x = 2, y = 3))
owl(list(file = myfile, x = 2, y = 3))
owl(list(file = myfile, args = list(x = 2, y = 3)))

# With the file path specified as components.
owl(dirname(myfile), "/", basename(myfile), x = 2, y = 3)
owl(file = c(dirname(myfile), "/", basename(myfile)),
    args = list(x = 2, y = 3))

# Construct a library file (of procedure definitions).
mylibraryfile <- tempfile()
writeLines(c("<proc 'proc-a'> <R> x * y </proc>",
             "<proc 'proc-b'> <R> x + y </proc>"),
           mylibraryfile)

# Import procedures from file (to owl's library).
owl(library = mylibraryfile)

# Run the imported procedures.
owl("proc-a")
owl("proc-b", x = 2, y = c(3, 4))

# Review the last result.
owl("result")

# Clean-up.
unlink(c(myfile, mylibraryfile))
owl("remove")
}
\keyword{database}
\keyword{interface}
