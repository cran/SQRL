\name{sqrlScript}
\alias{sqrlScript}
\title{
Combined Language Scripts
}
\description{
This material does not describe a function, but (rather) the \acronym{SQRL}
script file format for \acronym{SQL} with embedded \R.

For instructions on how to submit (run) these scripts from file, refer to
\code{\link{sqrlUsage}}.

The following (very simple) example scripts won't necessarily work with
your own version of \acronym{SQL} or your own data source.
}
\section{Example Script #1 (Multiple Statements)}{
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
\section{Example Script #2 (Embedded \R)}{
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
However, \code{<R> ... </R>} tags cannot be nested.
}
}
\section{Example Script #3 (Manipulation and Feedback)}{
\preformatted{
-- SQL statement, ending on a semicolon.
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
-- is returned at the end of the script.
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
then the query result is not assigned into the working environment (as with a
\code{<do>} tag), but the subsequent script is still interpreted as \R (unlike
with a \code{<do>} tag).

After the end of the script, the working environment is lost, along with any
variables within it.
Only the final result is returned to the calling environment.
}
}
\section{Example Script #4 (\R and Lists)}{
\preformatted{
-- R sections can be used to set temporary variables.
<R>
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
Whereas \code{<R> ... </R>} sections are embedded within \acronym{SQL} (which
may be blank), \code{<R> ... <do>} sections exist outside of any \acronym{SQL}.
While the result of an \code{<R> ... </R>} section is substituted back into the
surrounding \acronym{SQL} (and will form part of the query), an
\code{<R> ... <do>} section is simply evaluated within the script's working
environment.
Although a \code{<result -> null> ... <do>} section may be preceded by
\acronym{SQL} (which will be submitted prior to evaluating the section), an
\code{<R> ... <do>} section may not be (the two sections being otherwise
equivalent).

Lists are inserted comma collapsed.
Vectors are inserted newline collapsed.

A \code{<stop>} tag imitates the end of the file, and can be used to interrupt a
script for debugging.
}
}
\section{Example Script #5 (If, While, and Return)}{
\preformatted{
/* Implements drop-if-exists in a SQL that doesn't. */

-- Pull details of temporary tables.
help volatile table
<result -> a><do>

-- If those details are a data frame, then
-- at least one temporary table exists.
<if (class(a) == class(data.frame()))>
  <R> i <- 1 <do>

  -- Drop each table in turn.
  <while (i <= nrow(a))>
    drop table <R> a[i, "Table Name"] </R> <do>
    <R>
      print(paste("dropped", a[i, "Table Name"]))
      i <- i + 1
    <do>
  </while>

  -- Exit from here.
  <return (paste(i - 1, "tables dropped"))>
</if>

-- Otherwise, we had no temps to drop.
<return ("no temps found")>
}
\subsection{Commentary on Example Script #5}{
Within \acronym{SQL} sections, \code{<if>} and \code{<while>} tags can be used
to control the (repeated) submission of queries.
The parentheses enclosing the conditional \R expression are mandatory, and that
expression must evaluate to a Boolean singleton.

Within \acronym{SQL} sections, \code{<return>} tags can be used to exit early
from a script.
A return value (\R expression) is mandatory, and it must be enclosed in
parentheses.
Calling the base \code{return} function inside an \R section does not exit the
script; it merely sends a value to the temporary working environment.

A difference between \code{<return>} and \code{<stop>}, is that the latter
ignores conditionals; \code{<stop>} applies even when nested within an
\code{<if>} that evaluated to \code{FALSE}.
}
}
\section{Example Script #6 (Compact Variant)}{
\preformatted{
help volatile table <result -> a>;
<if (i <- 0; class(a) == class(data.frame()))>
  <while (i <- i + 1; i <= nrow(a))>
    drop table <R> a[i, "Table Name"] </R>;
    <R> print(paste("dropped", a[i, "Table Name"]));;
  </while>
  <return (paste(i - 1, "tables dropped"))>
</if>
<R> "no temps found"
}
\subsection{Commentary on Example Script #6}{
This is an alternative implementation of example #5.

Multiple \R statements are allowed within conditional tag expressions.
In the case of while loops, all statements within the tag expression are
evaluated on each iteration.
The condition takes the value of the final statement, which must evaluate to
either \code{TRUE} or \code{FALSE}.

Semicolons can be used to terminate \R sections (as well as \acronym{SQL}
statements).
Within an \R section (following either an <R> or a <result> tag), if there is
nothing but whitespace (or nothing at all) between a semicolon and the start of
its line, the previous semicolon, or the start of the \R section (whichever
comes first, heading left from the semicolon), then that semicolon marks the
end of the \R section and is equivalent to a \code{<do>} tag (the subsequent
script is read as \acronym{SQL}).
So located semicolons are not accepted by \R's parser.
}
}
\section{Example Script #7 (Else and Else If)}{
\preformatted{
select
  <if (dow == "Monday")>
    ColumnA
  <else if (dow \%in\% c("Saturday", "Sunday"))>
    ColumnB, ColumnC
  <else>
    *
  </if>
from
  some.table
}
\subsection{Commentary on Example Script #7}{
The closing tags, \code{</if>} and \code{</while>}, do not imply \code{<do>}.
This allows switching column names, or other \code{SQL} fragments, in and out of
a larger query.

The same end can be achieved through embedded \R (\code{<R> ... </R>} tags), or
other mechanisms.
In the above, \var{dow} is an externally supplied parameter (see example #2).

The parser is simple, and does not enforce correct nesting structure.
Unintuitive output may appear when nesting violations occur.
}
}
\note{
The \var{verbose} parameter toggles extended (intermediate) output when running
scripts (see \code{\link{sqrlParams}}).
}
\seealso{
\code{\link{sqrlUsage}}
}
\examples{
# Define a new data source.
sqrlSource("mire", "dsn=Mire")

\dontrun{
# Submit a SQL script to the source.
mire("my/script.sql")

# Submit a SQRL script, with explicit parameter values.
mire("my/parameterised/script.sqrl",
      day = Sys.Date(), colour = "'blue'")
}
}
\keyword{file}
