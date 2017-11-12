\name{sqrlUsage}
\alias{sqrlUsage}
\title{
How to Use the Interface Functions
}
\description{
This material does not describe a function, but how to use 'SQRL' interfaces,
once created.
These do not have their own help files, since their names are not static, but
reflect those of local data sources.
}
\details{
Once you have a named interface, either created automatically on loading of the
'SQRL' namespace, or manually by 'sqrlSource', it can be used to communicate
with the associated data source.
An advantage of this approach, is that connection handles, pasting, and 'RODBC'
parameters are implicit, rather than explicit.
This makes for faster exploratory work.
It also shifts the emphasis to the data sources by name, and away from those 
of the various functions used to communicate with them.

\preformatted{# Call sqrlSources to see which interfaces are defined.
sqrlSources()

# Let's say you have an interface called 'chaos'.

# Open a connection to the data source.
# This will be called automatically when you first
# submit a query, and is not strictly necessary.
chaos() (or chaos("open"))

# Doing this is fine; the channel handle is hidden.
rm(list = ls(all.names = TRUE))

# Is the connection open?
# This makes sure, by pinging the source.
# If TRUE, we don't simply think it's open from our end.
chaos("isopen")

# Enable visible indication of open connections.
# Since this requires modifying global options (the prompt
# and the window title), the default is FALSE (disabled).
# Assigning TRUE authorises SQRL to read and write these
# parameters (and nothing else). Window-title indication
# is only supported in Rgui and Rterm, on Windows OS.
chaos("visible", TRUE)

# Close the connection.
chaos("close")

# Submit a query (connection will re-open).
chaos("select ", 1, " from dual")

# Submit a query from file.
# See the help for sqrlSource() regarding file paths.
chaos("my/", script, ".sqrl")

# While the query is running, an asterisk is
# appended to the open-connection indicator on
# the window title (in Windows only), which is
# useful when you want to know if a hold-up is
# on the remote server or in your subsequent R.

# Import a configuration file (see sqrlConfig).
chaos("config", filepath)

# See your current configuration.
chaos("config")

# Change the interface.
chaos("interface", "CHAOS")

# Change it back.
CHAOS("interface", "chaos")

# Call sqrlSources().
chaos("sources")

# Set your ping (similarly for other parameters).
chaos("ping", "use database")

# Set your password from file (similarly for other parameters).
chaos("pwd", "password/file.txt")

# View your (data source) username (similarly for other
# parameters, except for your password).
chaos("uid")

# Set the colQuote parameter to NULL (similarly for tabQuote).
chaos("colQuote", "NULL")

# Add 'NULL' and '' to the RODBC na.strings list.
chaos("na.string", "NULL")
chaos("na.string", "")

# Set the na.strings list to 'NA'.
chaos("na.strings", "NA")

# Get database information (simple RODBC wrappers).
chaos("typeinfo")
chaos("tables")
chaos("columns", "my.table")

# Turn SQRL off (with ensure = FALSE).
# (See the help for sqrlOff().)
chaos("off")

# Turn SQRL off (with ensure = TRUE).
# (See the help for sqrlOff().)
chaos("off*")

# These last two ('off's) affect connections besides that of
# chaos(), and are perhaps best avoided in production code.
}
}
\author{
\packageAuthor{SQRL}
}
\seealso{
\code{\link{sqrlConfig}},
\code{\link{sqrlInterface}},
\code{\link{sqrlOff}},
\code{\link{sqrlScript}},
\code{\link{sqrlSource}},
\code{\link{sqrlSources}}
}
\keyword{ database }
\keyword{ interface }
