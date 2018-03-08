\name{sqrlScript}
\alias{sqrlScript}
\title{
Combined Language Scripts
}
\description{
This material does not describe a function, but (rather) the \pkg{SQRL} script
file format for \acronym{SQL} with embedded \R.

For instructions on how to submit (run) these scripts from file, refer to
\code{\link{sqrlUsage}}.

The following (very simple) example scripts won't necessarily work with
your own version of \acronym{SQL} or your own data source.
}
\section{Example Script #1}{
\preformatted{
-- My file
select 1;
select 2;
select 3;
}
\subsection{Commentary on Example Script #1}{
Multi-statement \acronym{SQL} files can be copied directly from
\dQuote{SQL Developer} (or similar application).
Each of the (above) three queries will be submitted in turn.
Only the final result will be returned to the \R calling environment (in this
case, a data frame containing a single value of 3).
}
}
\section{Example Script #2}{
\preformatted{
-- This is SQL
select
  <R>
    # This is R
    if (exists("x")) {
      x
    } else {
      0
    }
  </R>
/* This is SQL */
from
  dual
}
\subsection{Commentary on Example Script #2}{
\pkg{SQRL} supports the embedding of \R within \acronym{SQL}, via
\acronym{XML}-style tags (\code{<R> ... </R>}), as above.
This enables (explicitly and/or implicitly) parameterised \acronym{SQL} queries.

In this example (above), if the variable \var{x} was explicitly passed to the
query, then the supplied value will be used (see \code{\link{sqrlUsage}}).
If \var{x} was not explicitly passed, then it can be inherited (implicitly
passed) from the calling environment.
In this case, if \var{x} was not explicitly supplied and also does not \var{x}
exist within the calling environment, then a default value of 0 will be used.

\acronym{SQL} comments are allowed within \R (\code{<R> ... </R>}) sections, so
that \acronym{SQL} syntax highlighting can be better applied to the file.
\R comments are not allowed within \acronym{SQL} sections

Nested \acronym{SQL} queries can be made from within \code{<R> ... </R>} tags.
However, \code{<R> ... </R>} tags cannot be nested themselves.
}
}
\section{Example Script #3}{
\preformatted{
-- SQL statement, ending on a semi-colon.
use mydatabase;

/* SQL statement, ending on a <do> tag. */
create table mynewtable
as select columnA, columnB
from originaltable
<do>

-- SQL query, ending on a <result> (assignment) tag.
-- In this case, the result of the query (a data frame)
-- is assigned to an R variable 'a' (within a temporary
-- working environment communal to this script).
select max(columnA)
from mynewtable
<result -> a>

# Manipulate the result in R, then
# proceed to the next SQL statement.
b <- runif(1)
a <- as.numeric(a) + b
<do>

-- Use the earlier result in a new query.
select columnA, columnB from mynewtable
where columnA > <R> a * b </R>
and columnB < <R> z <- 5; z + 2 </R>
<result -> x>

-- Only the result of the final operation
-- is returned on the end of the script.
return(list(minA = min(x$columnA),
            maxB = max(x$columnB))
}
\subsection{Commentary on Example Script #3}{
\acronym{SQL} statements are terminated by any one of a semicolon, a \code{<do>}
tag, a \code{<result>} tag, or the end of the file.
After a \code{<result>} tag, statements are interpreted as \R (as opposed to
\acronym{SQL}) until either a \code{<do>} tag, or the end of the file, is 
encountered.
Following a \code{<do>} tag, statements are interpreted as \acronym{SQL}.

In the case of a \code{<result -> var>} tag, the \acronym{SQL} query result is
assigned to an \R variable \var{var} within the script's temporary working 
environment.
This variable can be arbitrarily manipulated in \R, and the result of that 
manipulation used as part of a subsequent \acronym{SQL} query.

With the exception of the name of the target (assignment) variable within a
\code{result} tag, tags are not case sensitive.
When the name of the target variable is either \dQuote{null} or \dQuote{NULL},
then the query result is not assigned into the working environment (like with a 
\code{<do>} tag), but the subsequent script is still interpreted as \R (unlike
with a \code{<do>} tag).

After the end of the script, the working environment is lost, along with any
variables within it.
Only the final result is returned to the calling environment.
}
}
\section{Example Script #4}{
\preformatted{
-- Empty query, nothing submitted.
-- Don't assign the result.
<result -> null>

-- Set temporary R variables here.
columns <- list("columnA", "columnB")
wordlist <- list("'red'", "'blue'")
conditions <- c("and columnA < 2", "and columnD > 4")
<do>

-- Now the query.
select
  <R> columns </R>
from
  dbname.tablename
where
  columnC in (<R> wordlist </R>)
  <R> conditions </R>
  
-- End the script here.
-- The statements below are ignored.
<stop>
  and columnA > 0
}
\subsection{Commentary on Example Script #4}{
To set \R variables prior to submitting any \acronym{SQL}, use an empty query.

Lists are inserted comma collapsed.
Vectors are inserted newline collapsed.

A \code{<stop>} tag imitates the end of the file, and can be used to interrupt a
script for debugging.
}
}
\note{
The \var{verbose} parameter toggles verbose output when running scripts (see
\code{\link{sqrlParams}}).
}
\seealso{
\code{\link{sqrlConfig}},
\code{\link{sqrlUsage}}
}
\examples{
# Define a new data source.
sqrlSource("mire", "dsn=Mire")

\dontrun{
# Submit a SQL script to the source.
mire("my/script.sql")

# Submit a SQRL script to the source.
mire("my/script.sqrl")

# Submit a SQRL script, with explicit parameter values.
mire("my/parameterised/script.sqrl",
      day = Sys.Date(), colour = "'blue'")
}
}
\keyword{file}
