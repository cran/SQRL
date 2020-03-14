\name{SQRL-package}
\alias{SQRL-package}
\alias{SQRL}
\docType{package}
\title{
Enhanced Database Interactivity
}
\description{
Streamlines exploratory work and short-order ad hoc jobs on \acronym{ODBC} data
sources.
}
\details{
Automatically generates a dedicated and like-named interface function to each
\acronym{ODBC} \acronym{DSN} (Open DataBase Connectivity Data Source Name).
These functions manage communications behind the scenes, whilst supporting
multi-statement \acronym{SQL} scripts, with or without embedded \R.
Hybrid (\acronym{SQRL}) scripting syntax enables parameterisation of queries,
the addition of flow-control structures within and around \acronym{SQL}, and
the use of libraries of stored scripts.

Additional sources and interfaces can be defined at any time.
The package is a wrapper about \pkg{RODBC}.
This is version 0.7.1.
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
# If 'ratatoskr' were one of those sources (i.e.,
# if a DSN of that name was found), then a multi-
# statement query could be submitted like so:
ratatoskr("use yggdrasil; select messages from ",
          "vedfolnir where addressee = 'nidhogg' ",
          "limit ", 5)

# Submit a parameterised query from file.
ratatoskr("messages.sqrl", year = 950)

# Obtain help on usage.
ratatoskr("help")
}

# Define a new data source (interface).
sqrlSource("mysource",
           driver = "MYSQL ODBC 5.3 ANSI Driver",
           server = "localhost",
           user = "<uid>",
           password = "<pwd>")

\dontrun{
# Submit a query to the new source.
mysource("select * from database.table")
}
}
\keyword{database}
\keyword{interface}
\keyword{package}
