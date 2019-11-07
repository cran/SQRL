\name{sqrlConfig}
\alias{sqrlConfig}
\title{
Configuration Files
}
\description{
This material does not describe a function, but (rather) the file format used to
configure \pkg{SQRL} interfaces and \pkg{RODBC} communications.

Configuration files can be used to define new data sources, set blanket
parameter values for existing sources, or set individually named parameter
values.
}
\section{Example Configuration File}{
\preformatted{
# Parameters for RODBC::odbcConnect/RODBC::odbcDriverConnect.
dsn                 =
uid                 = Blake
pwd                 = C:/some/other/file.txt
connection          = "driver=<driver>;dbalias=quag;uid=<uid>;pwd=<pwd>;"
case                = "nochange"
believeNRows        = TRUE
colQuote            = c("`", "'")
tabQuote            = "
interpretDot        = TRUE
DBMSencoding        = ""
rows_at_time        = 100
readOnlyOptimize    = FALSE

# Additional parameters for RODBC::sqlQuery.
errors              = TRUE
as.is               = TRUE
max                 = 0
buffsize            = 1000
nullstring          = NA_character_
na.strings          = c("NA", "-", "")
dec                 = .
stringsAsFactors    = FALSE

# Parameters for SQRL.
aCollapse           = ,
autoclose           = TRUE
driver              = "{IBM DB2 ODBC DRIVER}"
interface           = "Z"
lCollapse           = "\\n"
library             = my/library/file.sqrl
ping                = "select 1 from dual"
verbose             = FALSE
visible             = TRUE
prompt              = "Z"
scdo                = TRUE
wintitle            = "(Zen)"
}
}
\section{Commentary on Example File}{
This is a sample configuration file, exhibiting almost all parameters.
In general, a file need not include all of these (default values are in place).

Parameters may be defined as the path to some other file.
The \var{driver} and \var{dsn} parameters will take that path as their value.
For all other parameters, a value will be read from within the file.
Such files may contain only a single line with nothing but the value on it, or
they may adhere to the full (multiple line) \sQuote{parameter = value}
\pkg{SQRL} configuration file format (as described here).
Unexpected results may occur if a file called (say) \dQuote{TRUE} should exist.

File paths should not be wrapped in quotes.
Other strings may be wrapped, although this is not mandatory unless they clash
with the name of an object within the \R base namespace.

No action is taken when the right hand side of the equals is blank (as for the
\var{dsn} parameter, above).
That is, no assignment is made to the parameter on the left hand side.
}
\seealso{
\code{\link{sqrlParams}},
\code{\link{sqrlSource}}
}
\examples{
# Define a new source (not from file).
sqrlSource("Orac",
           "DBQ=StarOne;UID=Avon;PWD=<pwd>",
           "Driver={Oracle in OraDB18Home1}")

# Review its configuration (parameter values).
Orac("config")

# Create a configuration file.
my.file <- tempfile()
writeLines(c("aCollapse = ', '",
             "as.is = TRUE"),
           my.file)

# Import that file.
Orac(config = my.file)

# Confirm imported values.
Orac("aCollapse")
Orac("as.is")

# Alternative forms.
Orac("config", my.file)
Orac(paste("config", my.file))

# Create a single-line file.
writeLines("c('NA', 'N-A', 'N/A')",
           my.file)

# Import that file.
Orac(na.strings = my.file)

# Confirm the imported value.
Orac("na.strings")

# Create another configuration file.
writeLines(c("aCollapse = ,", "as.is = T",
             "uid = Ensor", "dsn = 'Aristo'"),
           my.file)

# Define and configure a new source from file.
sqrlSource("Caro", my.file)

# Confirm source creation.
Caro("config")

# Delete the file.
unlink(my.file)
}
\keyword{file}
