\name{sqrlScript}
\alias{sqrlScript}
\title{
Combined 'SQL' with 'R' Scripts
}
\description{
This material does not describe a function, but (rather) the 'SQRL' file format
for 'SQL' with embedded 'R'.
}
\format{
'SQRL' supports multi-statement 'SQL' files.
These can be copied directly from 'SQL Developer' (or similar application).

The following (very simple) samples won't necessarily work in your own 'SQL' on
your own database.

In the sample below, only the final result, a data frame containing a single
value of 3, is returned to the calling environment.

\preformatted{## START OF SAMPLE FILE #1

-- My file
select 1;
select 2;
select 3;

## END OF SAMPLE FILE #1
}

'SQRL' allows embedding of 'R' into these 'SQL' files, within 'XML'-style tags.
This enables ('R') parameterized 'SQL' queries, and wrapper functions can be
written to make those parameters explicit, if required.

In the sample below, a variable 'x' will be inherited from the calling
environment (or a default value of 0 will be used if 'x' is not found).
'SQL' comments are allowed within these 'R' sections, so that 'SQL' syntax
highlighting can be better applied to the file.
'R' comments are allowed within 'R' sections, but not within 'SQL' sections.

\preformatted{## START OF SAMPLE FILE #2

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

## END OF SAMPLE FILE #2
}

'SQRL' can run one query from a script, manipulate the result in 'R', use that
result in subsequent query in the same script, and so on.
Only the final result is returned to the calling environment.

\preformatted{## START OF SAMPLE FILE #3

-- SQL query, ending on the semi-colon.
select 1;

/* SQL query, ending on a 'do' tag. */
select 1
<do>

-- SQL query, ending on a 'result' (assignment) tag.
select 1

-- Submit query, assign the result to an R variable 'a'
-- within the SQRL working environment (not the calling).
<result -> a>

-- Increment a
# R comments apply in R sections, SQL in both.
a <- a + 1

# Advance to next SQL statement
<do>

-- Use the earlier result in a new query.
select <R> b <- runif(1); a + b </R>

-- This time, don't assign the result in R.
-- (More appropriate after, say, create table.)
<result -> null>

-- Perform arbitrary R manipulation.
a <- a * 2

-- Final result, returned to calling environment.
return(a)

## END OF SAMPLE FILE #3
}

To set 'R' variables before submitting any 'SQL', use an empty query.
A 'stop' tag can be used to interrupt a script (for debugging).

\preformatted{## START OF SAMPLE FILE #4

-- Empty query, nothing submitted.
-- Don't assign the result.
<result -> null>

-- Set R variables here.
x <- 5

-- Stop tag; script ends here.
-- (5 is returned, statements below are not run.)

<stop>

-- Not run, not returned.
x <- 9

## END OF SAMPLE FILE #4
}

Lists are inserted comma collapsed.
Vectors are inserted newline collapsed.

\preformatted{## START OF SAMPLE FILE #5

<result -> null>
columns <- list("columnA", "columnB")
wordlist <- list("'red'", "'blue'")
conditions <- c("and columnA < 2", "and columnD > 4")
<do>

select <R> columns </R>
from dbname.tablename
where columnC in (<R> wordlist </R>)
<R> conditions </R>

## END OF SAMPLE FILE #5
}
}
\author{
\packageAuthor{SQRL}
}
\note{
The 'verbose' parameter toggles verbose output when running scripts.

Tags are not case sensitive (other than the name of the assignment variable in a
'result' tag).

'R' ('<R> .. </R>') tags cannot be nested.

Nested queries can be made from within 'R' ('<R> .. </R>') tags.
}
\seealso{
\code{\link{sqrlConfig}}, \code{\link{sqrlUsage}}
}
\examples{
require(SQRL)
sqrlSource("entropy", "dbcname=EntSrvr;driver=PostgreSQL ANSI(x64)")

\dontrun{
entropy("my/query.sqrl")
}
}
\keyword{ file }
