\name{sqrlParams}
\alias{sqrlParams}
\title{
Control and Connection Parameters
}
\description{
This material does not describe a function, but (rather) the various parameters
governing \acronym{ODBC} communications and package behaviour.
The majority of these are passed through to \pkg{RODBC}.
}
\section{Parameters}{
\describe{
\item{\var{as.is}:}{
A logical vector, or a numeric vector (of column indices), or a  character
vector (of column names).
Argument to \code{\link[RODBC:sqlQuery]{RODBC:sqlQuery}} (see also
\code{\link[utils:read.table]{utils:read.table}}).
Tells \pkg{RODBC} which character columns of a table, as returned by a query
to the \acronym{ODBC} connection, \emph{not} to convert to some other data type
(i.e., which character columns to leave as is).
Due to \pkg{SQRL}'s set-and-forget approach to parameters, it is inconvenient to
change \var{as.is} on a query-by-query basis.
That being the case, it is usually defined as a logical singleton (either
\code{TRUE} or \code{FALSE}).
Default value is \code{FALSE} (convert all character columns).
}
\item{\var{autoclose}:}{
A logical singleton.
Tells \pkg{SQRL} whether or not to automatically close the data source
connection after each query (in general, a sequence of multiple statements).
The default value is \code{FALSE}, which leaves the connection open.
When set to \code{TRUE}, connections will normally only open for the duration of
each query, but may sometimes remain open if a mid-query exception is thrown.
Setting \code{TRUE} has no immediate effect upon a connection that is already
open.
When user input is required for authentication each time a new connection is
opened, the default setting will be more convenient.
}
\item{\var{believeNRows}:}{
A logical singleton.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Tells \pkg{RODBC} whether or not to trust the nominal number of rows returned by
the \acronym{ODBC} connection.
Locked while the connection is open.
Default value is \code{TRUE}.
}
\item{\var{buffsize}:}{
A positive integer.
Argument to \code{\link[RODBC:sqlQuery]{RODBC:sqlQuery}}.
Specifies the number of rows (of a query result) to fetch at a time.
Default value is 1000.
}
\item{\var{case}:}{
A character string, specifically one of \dQuote{nochange}, \dQuote{toupper},
\dQuote{tolower}, \dQuote{mysql}, \dQuote{postgresql}, or \dQuote{msaccess}.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Specifies case-changing behaviour for table and column names.
Locked while the connection is open.
Default value is \dQuote{nochange}.
}
\item{\var{channel}:}{
An \pkg{RODBC} connection handle.
Returned by \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Argument to \code{\link[RODBC:sqlQuery]{RODBC:sqlQuery}}.
This parameter is read-only.
}
\item{\var{colQuote}:}{
A character vector of length 0, 1, or 2, or \code{NULL}.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Specifies the quote character(s) for column names.
A vector of length zero means no quotes, of length one means apply the specified
quote character at both ends of a name, and of length two means apply the first
specified character to the start of the name and the second specified character
to the end of the name.
Locked while the connection is open.
The default value is a backtick for MySQL, and a double-quote for everything
else.
}
\item{\var{connection}:}{
A character string.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcDriverConnect}}.
Specifies an \acronym{ODBC} connection string.
The content of this string will be database-management system (\acronym{DBMS})
dependent.
Overrides \var{dsn}, should both be defined.
Locked while the connection is open.
Defaults to the empty string (connect via \acronym{DSN} instead).
Will accept \code{NULL} as an alias for the empty string.
Setting \var{connection} resets \var{dsn}, unless \var{connection} contains the
\dQuote{<dsn>} placeholder.
}
\item{\var{DBMSencoding}:}{
A character string.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Names the encoding returned by the \acronym{DBMS}.
Locked while the connection is open.
Default value is the empty string (use encoding of the \R locale).
Will accept \code{NULL} as an alias for the empty string.
}
\item{\var{dec}:}{
A character string (typically a single character).
Argument to \code{\link[RODBC:sqlQuery]{RODBC:sqlQuery}}.
Defines the decimal-place marker to be used when converting data from text to
numeric format.
The default value is \code{options("dec")}, as set by \pkg{RODBC}.
}

\item{\var{driver}:}{
A character string.
The name or file path of the \acronym{ODBC} driver for the source (either
currently in use, or to be used when a channel is opened).
This determines the requisite dialect of \acronym{SQL}.
Locked while the connection channel is open.
Defaults to the empty string.
Will accept \code{NULL} as an alias for the empty string.
}

\item{\var{dsn}:}{
A character string.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Specifies the data source name (\acronym{DSN}) to connect to.
Can be a file path.
Overridden by \var{connection}, when that parameter is defined.
Setting \var{dsn} resets \var{connection}, unless \var{connection} contains the
\dQuote{<dsn>} placeholder.
Setting \var{dsn} also sets \var{driver}, if the \acronym{DSN} exists and the
associated driver can be identified.
Locked while the connection is open.
Defaults to the empty string.
Will accept \code{NULL} as an alias for the empty string.
}
\item{\var{errors}:}{
A logical singleton.
Argument to \code{\link[RODBC:sqlQuery]{RODBC:sqlQuery}}.
Controls whether or not to throw \R errors in response to
\acronym{DBMS}/\acronym{ODBC} exceptions.
Default value is \code{TRUE} (this differs from the \pkg{RODBC} default).
}
\item{\var{interface}:}{
A character string, or \code{NULL}.
The name of the \pkg{SQRL} interface function for this data source (see
\code{\link{sqrlInterface}}).
Setting \code{NULL} or \dQuote{remove} removes the interface.
Default value is \code{NULL} (undefined).
}
\item{\var{interpretDot}:}{
A logical singleton.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Locked while the connection is open.
Controls whether or not to interpret table names of the form \dQuote{aaa.bbb} as
table \dQuote{bbb} in schema/database \dQuote{aaa}.
The default value is \code{TRUE}.
}
\item{\var{max}:}{
An integer.
Argument to \code{\link[RODBC:sqlQuery]{RODBC:sqlQuery}}.
Caps the number of rows fetched back to \R.
The default value is 0 (meaning unlimited; retrieve all rows).
}
\item{\var{na.strings}:}{
A character vector.
Argument to \code{\link[RODBC:sqlQuery]{RODBC:sqlQuery}}.
Specifies strings to be mapped to \code{NA} within character data.
The default value is \dQuote{NA}.
}
\item{\var{name}:}{
A character string.
The name of this \pkg{SQRL} data source.
While often identical to the names of both the underlying \acronym{ODBC}
data source and the \pkg{SQRL} interface function, it need match neither in
general.
Multiple \pkg{SQRL} sources may interface with the same \acronym{ODBC} source.
This parameter is write once, and cannot be changed after creation of the
\pkg{SQRL} source.
There is no default value.
}
\item{\var{nullstring}:}{
A character string.
Argument to \code{\link[RODBC:sqlQuery]{RODBC:sqlQuery}}.
The string with which to replace \code{SQL_NULL_DATA} items within character
columns.
The default value is \code{NA_character_}.
}
\item{\var{ping}:}{
A character string.
Defines a simple \acronym{SQL} statement used by \pkg{SQRL} to verify source
connections.
This enables \pkg{SQRL} to make one automatic reconnection attempt after a
network outage or other unexpected channel closure (this process is silent
unless user-input is required for authentication).
The default value is \acronym{DBMS}-dependent.
Manual definition may be necessary in the event that \pkg{SQRL} fails to
identify an appropriate statement for the particular \acronym{DBMS} of the
source.
}
\item{\var{prompt}:}{
A character string (typically a single character).
Defines an indicator to be applied to the \R command prompt when the connection
is open and \var{visible} is \code{TRUE}.
Defaults to the first character of \var{name}.
Single-letter indicators are recommended since, if two sources are open and one
indicator is a substring of the other, then \pkg{SQRL} may fail to correctly
update the prompt when one source is closed.
Can be set to an empty string, in which case nothing is applied to the prompt.
Will accept \code{NULL} as an alias for the empty string.
}
\item{\var{pwd}:}{
A character string.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Specifies the password for authentication.
Locked while the connection is open.
Defaults to the empty string (interpreted as do not supply a password to the 
\acronym{ODBC} driver).
Will accept \code{NULL} as an alias for the empty string.
Write-only.
}
\item{\var{readOnlyOptimize}:}{
A logical singleton.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Specifies whether or not to optimise the \acronym{ODBC} connection for read-only
access.
Locked while the connection is open.
Default value is \code{FALSE}.
}
\item{\var{rows_at_time}:}{
A positive integer, between 1 and 1024.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Specifies the number of rows to fetch at a time when retrieving query results.
Locked while the connection is open.
The default value is 100.
Manually setting 1 may be necessary with some \acronym{ODBC} drivers.
}
\item{\var{stringsAsFactors}:}{
A logical singleton.
Argument to \code{\link[RODBC:sqlQuery]{RODBC:sqlQuery}}.
Controls the conversion of character to factor columns within query results,
excluding those columns covered by \var{as.is}.
Defaults to \code{default.stringsAsFactors()}.
}
\item{\var{tabQuote}:}{
A character vector of length 0, 1, or 2, or \code{NULL}.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Specifies the quote character(s) for table names.
A vector of length zero means no quotes, of length one means apply the specified
quote character at both ends of a name, and of length two means apply the first
specified character to the start of the name and the second specified character
to the end of the name.
Locked while the connection is open.
Defaults to the value of \var{colQuote}.
}
\item{\var{uid}:}{
A character string.
Argument to \code{\link[RODBC:odbcConnect]{RODBC:odbcConnect}}.
Specifies the user identity (\acronym{UID}, user name) to use on the data
source.
Locked while the connection is open.
Defaults to the local name of the \R user (\code{Sys.info()["user"]}).
Will accept \code{NULL} as an alias for the empty string (which is interpreted
as do not pass a \acronym{UID} to the \acronym{ODBC} driver).
May be inaccurate when the \acronym{UID} is specified within a \acronym{DSN}.
}
\item{\var{verbose}:}{
A logical singleton.
Controls whether or not to display verbose output while processing \acronym{SQL}
or \acronym{SQRL} files.
The default is \code{FALSE} (verbose output disabled).
When \code{TRUE}, usually only the \code{\link[utils:head]{head}} of each 
intermediate object is displayed.
Sufficiently exotic objects may fail to print.
}
\item{\var{visible}:}{
A logical singleton.
Toggles display of the \var{wintitle} and \var{prompt} indicators (while an
open connection channel exists to the source).
The default value is \code{FALSE} (do not show indicators).
Changing this to \code{TRUE} authorises modification of the \dQuote{prompt}
global option (see \code{\link[base:options]{base:options}}).
}
\item{\var{wintitle}:}{
A character string, possibly empty.
Will accept \code{NULL} as an alias for the empty string.
Defines an indicator that, unless empty, is displayed on the \R window title bar
while a connection channel is open to the source, and provided \var{visible} is
\code{TRUE}.
An asterisk (*) is appended to the indicator while a query is running on the
source (including connection-testing pings), and a plus-sign (+) is appended
while results are being retrieved from it.
If two sources are open and one indicator is a substring of the other, then
\pkg{SQRL} may fail to correctly update the title when one source is closed.
Only works with \file{R.exe}, \file{Rterm.exe} and \file{Rgui.exe}, and then
only while running on a \dQuote{Windows} operating system.
Works with both \acronym{MDI} and \acronym{SDI} modes, but does not work with
\dQuote{RStudio}.
}
}
}
\note{
Each \pkg{SQRL} data source has its own set of the above parameters.
Altering the value of a parameter (e.g., \var{stringsAsFactors}) for one source
does not affect the value for any other source.
Use \code{\link[SQRL:sqrlAll]{sqrlAll()}} to make blanket changes.
}
\seealso{
\code{\link{sqrlUsage}},
\pkg{\link[RODBC:RODBC-package]{RODBC}}
}
\keyword{misc}
