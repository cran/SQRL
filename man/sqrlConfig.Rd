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
# Parameters for RODBC:odbcConnect/RODBC:odbcDriverConnect.
dsn                 =
uid                 = stanislaus
pwd                 = D:/some/other/file.txt
connection          = "driver=<driver>;dbalias=quag;uid=<uid>;pwd=<pwd>;"
case                = "nochange"
believeNRows        = TRUE
colQuote            = c("`", "'")
tabQuote            = "
interpretDot        = TRUE
DBMSencoding        = ""
rows_at_time        = 100
readOnlyOptimize    = FALSE

# Additional parameters for RODBC:sqlQuery.
errors              = TRUE
as.is               = TRUE
max                 = 0
buffsize            = 1000
nullstring          = NA_character_
na.strings          = c("NA", "-", "")
dec                 = .
stringsAsFactors    = FALSE

# Parameters for SQRL.
autoclose           = TRUE
driver              = "{IBM DB2 ODBC DRIVER}"
interface           = "Q"
ping                = "select 1 from dual"
verbose             = FALSE
visible             = TRUE
prompt              = "Q"
wintitle            = "(Quag)"
}
}
\section{Commentary on Example File}{
This is a sample configuration file, exhibiting all parameters.
In general, a file need not include all of these (default values are in place).

Parameters may be defined as the path to some other file.
The \var{driver} and \var{dsn} parameters will take paths as their values.
For all other parameters, a value will be read from within the file.
Such files may contain only a single line with only the one parameter value on
it, or they may adhere to the full (multiple) \sQuote{parameter = value} format.
Unexpected results may occur should a file called (say) \dQuote{TRUE} exist.

File paths should not be wrapped in quotes.
Other strings may be wrapped, although this is not mandatory unless they clash
with the name of an object within the \R base namespace.

When the right hand side of the equals is left blank, no action is taken (the
parameter on the left hand side is not assigned to).
}
\seealso{
\code{\link{sqrlParams}},
\code{\link{sqrlSource}}
}
\examples{
# Define a new source (not from file).
sqrlSource("Orac", "Dbq=Delphi;Uid=Pythia;Pwd=<pwd>",
            "Driver={Oracle in OraClient11g_home1}")

# Review the current configuration (parameter values).
Orac("config")

\dontrun{
# Configure an existing source from file.
Orac("config", "my/config/file")

# Set one named parameter from file.
Orac(connection = "my/other/file")

# Define and configure a new source from file.
sqrlSource("Zen", "my/other/", "file")
}
}
\keyword{file}
