\name{sqrlConfig}
\alias{sqrlConfig}
\title{
Parameters and Configuration Files
}
\description{
This material does not describe a function, but the file format used to create
and configure 'SQRL' interfaces and 'RODBC' communications.

Configuration files can be used to define new data sources (via
'\link{sqrlSource}'), set blanket parameter values for existing data sources
(from their interfaces), or set individually named parameter values (from source
interfaces).
}
\format{
This is a sample configuration file, setting all of the parameters that can be.
A file need not specify all of these (default values are in place).

\preformatted{## START OF SAMPLE CONFIG FILE

# Parameters for RODBC::odbcConnect, RODBC::odbcDriverConnect,
# and RODBC::sqlQuery. (Refer to the RODBC manual.)
dsn                 =
uid                 = julie
pwd                 = pass
connection          = "dbcname=<dbcname>;driver=<driver>;uid=<uid>;pwd=<pwd>"
case                = nochange
believeNRows        = TRUE
colQuote            = "
tabQuote            = "
interpretDot        = TRUE
DBMSencoding        =
rows_at_time        = 100
readOnlyOptimize    = FALSE
dbcname             = EntSrvr

# Additional parameters for RODBC::sqlQuery.
# (Refer to the RODBC manual.)
errors              = TRUE
as.is               = TRUE
max                 = 0
buffsize            = 1000
nullstring          = NA_character_
na.string           = NA
na.string           =
dec                 = .
stringsAsFactors    = FALSE

# Parameters for SQRL.

# Choose an ODBC driver (from those locally installed).
driver              = PostgreSQL ANSI(x64)

# Define a simple SQL statement to be used for testing the
# data source.
ping                = select 1

# Toggle verbose output during submission of SQL from file.
verbose             = FALSE

# Toggle visibility of query-in-progress and open-connection
# indicators.
visible             = TRUE

# Set the prompt open-connection indicator.
prompt              = S

# Set the window-title open-connection indicator.
wintitle            = (Entropy)

## END OF SAMPLE CONFIG FILE
}

It is possible to use a file of only one line, for setting the value of
only one named parameter.
In this case, the file need only contain the new parameter value (e.g., a
username), and not the full 'parameter = value' format.
}
\author{
\packageAuthor{SQRL}
}
\note{
Quote marks are literal within the file (for an empty string, leave
the right hand side of the equals blank).

If the 'pwd' parameter is assigned (non-empty), 'SQRL' will make one attempt to
reconnect when a query fails due to a network outage or the remote server
having closed the connection.

The value for 'colQuote' or 'tabQuote' can be 'NULL', in which case it will
be coerced to an actual NULL.

If the 'errors' parameter is set to FALSE, 'SQRL' will no longer throw local
('R') errors on remote ('SQL') exceptions.

The value for 'nullstring' can be 'NA' or 'NA_character_', either of which will
be coerced to an actual NA_character_.

Each 'na.string' line adds its value to the 'na.strings' parameter value (a
character vector).
Alternatively, 'na.strings' can be used to set a single string only.

The value of 'ping' should be a simple 'SQL' statement, accepted by the source.
This is source-dependent, but 'SQRL' will try to identify one for you.
The 'ping' is used to verify an open-connection to the source.
If 'SQRL' is unexpectedly closing your connection, review the 'ping' parameter,
and set it to something the source accepts.

Setting 'visible' TRUE authorises 'SQRL' to alter your 'R' prompt and your
'R' window title.
This enables visual indication of which channels are open and which source, if
any, is currently processing your last query (the latter feature only works on
Windows OS, and not in R Studio).

The 'prompt' parameter holds the string that will be applied to the 'R' prompt
when 'visible' is TRUE and a connection is open.
Setting this to an empty string disables prompt indication even when 'visible'
is TRUE.
One-letter strings are recommended since, if two sources are open and one
'prompt' is a substring of the other, then 'SQRL' is may not update the `R' 
prompt correctly when one source is closed.

The 'wintitle' parameter holds the string that will be applied to the 'R' window
title when 'visible' is TRUE and a connection is open.
Setting this to an empty string disables window-title indication even when
'visible' is TRUE.
The same caveat regarding overlapping 'wintitles' applies as with 'prompt'.
Regardless, 'wintitle' will never be visible on some platforms and some 'R'
distributions.
}
\seealso{
\code{\link{sqrlSource}},
\code{\link{sqrlUsage}},
\code{\link[RODBC:RODBC-package]{RODBC}}
}
\examples{
require(SQRL)
sqrlSource("entropy", "dbcname=EntSrvr;driver=PostgreSQL ANSI(x64)")

# View the current configuration (review parameter values).
entropy("config")

\dontrun{

# Configure existing source from a config file.
entropy("config", "my/config/file")

# Set one named parameter from a one-line file.
entropy("pwd", "my/password/file")

# Define (and configure) source from config file.
sqrlSource("jungle", "my/other/config/", file, ".txt")

# Set only one named parameter from a config file (containing many others).
jungle("stringsAsFactors", "my/config/file")
}
}
\keyword{ file }
