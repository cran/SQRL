\name{SQRL-package}
\alias{SQRL-package}
\alias{SQRL}
\docType{package}
\title{
Database Query Interfaces
}
\description{
Facilitates exploratory work and rapid prototyping with Open Database
Connectivity (\acronym{ODBC}) data sources.
}
\details{
Automatically creates like-named interface functions to \acronym{ODBC} data
source names (\acronym{DSN}s).
These functions support multi-statement \acronym{SQL} scripts, with or without
arguments, embedded \R expressions, or flow-control structures.

Additional interfaces can be defined at any time.
Set-and-forget communication parameters are managed behind the scenes.
The package is a wrapper about \pkg{RODBC}.
}
\section{Version}{
0.6.1
}
\section{Changes}{
\itemize{
\item Scripts support conditionals, loops and returns.
\item Option for closing connections between queries.
\item Requires R-3.3, or newer.
}
}
\author{
Mike Lee
}
\seealso{
\code{\link{sqrlSources}},
\code{\link{sqrlUsage}}
}
\examples{
require(SQRL)

# Show (automatic) data sources.
sqrlSources()

\dontrun{
# If ratatoskr were one of those sources
# (i.e., if a DSN of that name was found),
# then a query could be submitted like so:
ratatoskr("select messages from vedfolnir ",
          "where addressee = 'nidhogg' ",
          "limit ", 5)
}

# Define a new data source.
sqrlSource("mysource",
            driver = "MYSQL ODBC 5.3 ANSI Driver",
            server = "localhost",
            user = "<uid>",
            password = "<pwd>")

\dontrun{
# Obtain help on usage.
mysource("help")

# Submit a query to the new source.
mysource("select * from database.table")

# Submit a parameterised query from file.
mysource("transactions.sqrl", customerid = 111111)
}
}
\keyword{database}
\keyword{interface}
\keyword{package}
