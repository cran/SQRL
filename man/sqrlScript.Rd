\name{sqrlScript}
\alias{sqrlScript}
\title{
Hybrid Script Syntax
}
\description{
This material does not describe a function, but (instead) the \acronym{SQRL}
scripting syntax for \acronym{SQL} with embedded \R.

For instructions on how to submit (run) these scripts from file, refer to
\code{\link{sqrlUsage}}.
}
\section{Script #01 (Pure SQL)}{
\preformatted{
use database;
select * from table;
}
\subsection{Commentary on Script #01}{
Multi-statement \acronym{SQL} scripts can be taken directly from
\file{SQL Developer} (or similar application), and (usually) run without
modification.
The \pkg{SQRL} parser identifies statement-terminating semicolons, and submits
each statement in turn, as those semicolons (or the end of the script) are
encountered.
}
}
\section{Script #02 (SQL, with R in it)}{
\preformatted{
select isotope from periodic_table
where atomic_number = <R> Z </R>
}
\subsection{Commentary on Script #02}{
Scripts can be parameterised, via embedded \R.
In \acronym{SQRL}, \acronym{XML}-style tags mark the beginning and end of an \R
block.
As shown in \code{\link{sqrlUsage}}, values can either be passed explicitly, or
inherited from the calling environment.
Embedded \R is not restricted to the insertion of values for filtering, and can
be used to specify column names, clauses, or entire \acronym{SQL} statements.

Tags are not case sensitive.
}
}
\section{Script #03 (Comments, and Arbitrary R)}{
\preformatted{
/* SQL-comment within SQL */
-- SQL-comment within SQL
select
  <R>
    /* SQL-comment within R */
    -- SQL-comment within R
    # R comment within R
    letter <- sample(letters, 1)
    paste0("'", letter, "'")
  </R>
}
\subsection{Commentary on Script #03}{
The parser supports \R comments, and two kinds of \acronym{SQL} comments.
For syntax-highlighting purposes, \acronym{SQL} comments may be used within \R
sections (but \R comments cannot be used within \acronym{SQL}).
Since \code{a -- b} is legitimate \R, it would need to be rewritten with a space
between the minus signs, or as \code{a + b}, to avoid being interpreted as a
comment.
% In the PDF manual, \code{a -- b} and \code{a - -b} both appear as 'a --b',
% hence the wording of the above sentence.

Otherwise-arbitrary \R script is allowed within (tag-delimited) \R sections.
Evaluation of such \R script takes place in a temporary child environment of the
calling environment.
The final result of that evaluation is then pasted into the surrounding
\acronym{SQL}, before its submission.
This mechanism cannot be used to insert additional tags into the \acronym{SQRL}
script.
}
}
\section{Script #04 (R, out of SQL)}{
\preformatted{
<R>
  threshold <- 9000
  sq <- function(x) paste0("'", x, "'")
<do> -- This tag terminates the R section.

<R>
  if (!exists("date", inherits = FALSE))
    date <- format(Sys.Date(), "\%Y-\%m-\%d");
; -- This semicolon terminates the R section.

select * from database.table
where event_date = <R> sq(date) </R>
and event_size > <R> threshold </R>
}
\subsection{Commentary on Script #04}{
As in scripts #02 and #03, when an \R section ends with \code{</R>}, the result
of evaluating that section is pasted into the surrounding \acronym{SQL} (or
implied \acronym{SQL}, if those surroundings are blank).
However, an \R section can also be terminated by a \code{<do>} tag, or by a
non-syntactical semicolon (that being one with nothing but whitespace between it
and the previous semicolon, newline, or \code{<R>} tag).
Such sections are evaluated in the same way as those ending in \code{</R>} tags,
but the results are not pasted into \acronym{SQL}.
These sections can only appear between, not within, \acronym{SQL} statements.

In this script, the first out-of-\acronym{SQL} \R section defines a numerical
constant, and also a function for wrapping strings in single quotes.
The second out-of-\acronym{SQL} \R section supplies a default value for a
variable, \var{date}, if none was explicitly passed in (see
\code{\link{sqrlUsage}}).
These two \R sections could be combined into one.
The function and values from these sections are applied in the subsequent
\acronym{SQL}.
}
}
\section{Script #05 (Temporary Parameter Settings)}{
\preformatted{
<with>
  na.strings = c('NA', "-")   -- comment
  x <- 5; max = x + 1         /* comment */
  verbose = TRUE               # comment
  as.is = "configfile.txt"     # read from file
</with>

-- Run this query with the above settings.
select * from database.table
}
\subsection{Commentary on Script #05}{
Tags \code{<with>} and \code{</with>} delimit special-purpose blocks of \R.
They are used to set temporary parameter values, which remain in effect only
for the duration of the script.
To be clear, temporary values survive beyond the \code{</with>} tag, but not
beyond the end of the script.
The exception is that temporary values of \var{ping} are retained if and only if
the connection would otherwise be left open without a defined \var{ping}.

This mechanism only works for a limited number of parameters:
\var{aCollapse}, \var{as.is}, \var{buffsize}, \var{dec}, \var{errors},
\var{lCollapse}, \var{max}, \var{na.strings}, \var{nullstring}, \var{ping},
\var{pwd}, \var{retry}, \var{scdo}, \var{stringsAsFactors}, and \var{verbose}.
}
}
\section{Script #06 (Procedural Language Blocks)}{
\preformatted{
select 1 from dual; -- This semicolon terminates the statement.

declare
  x integer := 0;
begin
  for i in 1..10 loop
    x := x + 1;
    if x > 7 then
      x := 0;
    else
      begin
        null;
      end;
    end if;
  end loop;
end; -- This semicolon terminates the block.

select 2 as N from dual
}
\subsection{Commentary on Script #06}{
As with statement-terminating semicolons (refer to script #01), the \pkg{SQRL}
parser attempts to identify procedural-language block-terminating semicolons,
and submits each such block as those semicolons are encountered.
The parser also submits (when necessary) upon reaching the end of the script.
Although trailing semicolons are not usually mandatory for \acronym{SQL}
statements, they usually are mandatory at the end of a procedural block.

If you have a script that came with forward-slashes, /, at the end
of procedural blocks, those slashes will normally need to be removed (as, say,
\file{SQL Developer} would do for you).

When a script contains multiple statements and/or blocks, only the final result
is returned.
In this case, that would be \code{data.frame(N = 2L)}.
}
}
\section{Script #07 (Parser Control)}{
\preformatted{
-- Change parser to fallback mode.
<with>
  scdo = FALSE
</with>

-- An outside-of-SQL R section.
<R>
  N <- 2;
<do> -- This tag terminates the section.

-- The default (scdo = TRUE) SQRL parser would
-- not find the end of this procedural block.
begin
  null;
end /**/ ;
<do> -- This tag terminates the block.

select 1 from dual; -- Semicolon ignored.
<do> -- This tag terminates the statement.

select <R> N </R> from dual
}
\subsection{Commentary on Script #07}{
While the parser detects terminal semicolons at the ends of \acronym{SQL}
statements and \R sections, it remains unsophisticated, and ignorant of
\acronym{DBMS}-specific procedural-language syntaxes.
That being the case, if you are working with procedural-language extensions to
\acronym{SQL}, sooner or later the parser will fail to detect the end of a
block.
Presently, a simple thing that causes this is the presence of a comment between
an \code{end} and its semicolon (as appears, above).

Setting the \var{scdo} parameter to FALSE causes the parser to ignore semicolons
and to conclude (out of \acronym{SQL}) \R sections, \acronym{SQL} statements,
and procedural blocks only upon encountering a \code{<do>} tag.
This provides a robust multi-statement capability when need be, but will
require appropriate modification of any scripts originally developed for some
other application.

Note that \code{<do>} tags function irrespective of the \var{scdo} setting.
That being the case, the use of these can be preferable to semicolons when
a \acronym{SQRL} script is developed from scratch.
When \var{scdo} is \code{TRUE}, and a terminating semicolon is followed by a
\code{<do>} tag, with nothing but whitespace between them, they are treated as a
single \code{<do>} (only one submission is made).
An implied \code{do} tag exists at the end of every script.
}
}
\section{Script #08 (Manipulation of Results)}{
\preformatted{
select
  calDate, Snowfall
from
  Weather.SparseDailySnowfall
where
  calYear = <R> year </R>

-- Submit the above, assign the result to 'a',
-- and immediately begin an R section.
<result -> var>

  names(var)[names(var) == "calDate"] <- "Date"
  var$Snowfall <- as.numeric(var$Snowfall)
  first <- as.Date(paste(year, 1, 1, sep = "-"))
  last <- as.Date(paste(year, 12, 31, sep = "-"))
  alldates <- data.frame(Date = seq(first, last, 1))
  merge(alldates, var, all.x = TRUE)
}
\subsection{Commentary on Script #08}{
A \code{<result>} tag acts as a combination of the \code{<do>} and \code{<R>}
tags, wherein the result of the query is assigned to an object within the
working (script evaluation) environment.
Any syntactically valid \R variable name could be used in place of \var{var}.
Whilst the main tag is not case sensitive, the name of the variable is.

An \R section begins immediately after the \code{<result>} tag.
This can be useful when preferred \R column names are reserved \acronym{SQL}
keywords (such as \dQuote{date}), when dates come back in unconventional
formats (and need conversion), when strict type-conversion control is required
(typically in combination with \code{as.is = TRUE}), or when data is sparse
(zero-valued entries are not stored) and results need to be expanded to
explicitly include the implied zero-valued data.
The net effect is a single script, combining \acronym{SQL} data extraction with
\R reformatting.

Using \dQuote{null} or \dQuote{NULL} as the variable name stops assignment of
the query result (but the \R section still begins).

Only the final value of any \acronym{SQRL} script is returned.
In this example, that value is the merged data frame.

Note that \code{<result>} tags cannot be used to conclude \R sections.
Also, \R sections beginning with \code{<result>} tags cannot end in \code{</R>}.

When \var{scdo} is \code{TRUE}, and a terminating semicolon is followed by a
\code{<result>} tag, with nothing but whitespace between them, they are treated
as a single \code{<result>} tag (only one query submission is made).
However, when a \code{<do>} tag is followed by a \code{<result>} tag, two
submissions are made, irrespective of \var{scdo}.
First, the query before the \code{<do>} is submitted, and then the (blank) query
between the \code{<do>} and the \code{<return>}.
In \pkg{SQRL}, blank queries (blank \acronym{SQL} statements) are always
allowed, and return no value.
}
}
\section{Script #09 (Feedback of Intermediate Results)}{
\preformatted{
select distinct customer_number from our.customers
where town in (<R> paste0("'", towns, "'") </R>)

<result -> z>; -- Same thing as <result -> z><do>

select sum(transactions) from online.orders
where customer_number in (<R> z$customer_number </R>)
}
\subsection{Commentary on Script #09}{
This script takes one parameter, \var{towns}, a character vector.
By default, atomic vectors are pasted into \acronym{SQL} as comma-separated
values.
The first query returns a data frame, which we save, as \var{z}, but have no
need to manipulate.
Its (integer-valued) \var{customer_number} column is then applied to the second
query, once again as comma-separated values.
Only the final result (of the second query) is returned.

Of course, this particular example could instead have been implemented with a
join, or temp table.

The pasting behaviour of atomic objects is controlled by the \var{aCollapse}
parameter.
Similarly, the pasting behaviour of list-like objects is controlled by the
\var{lCollapse} parameter (which defaults to the empty string).
To restore \pkg{SQRL}-0.6.3 pasting rules, insert
\code{<with> aCollapse = "\n"; lCollapse = ", " </with>} at the top of a script.
See \code{\link{sqrlUsage}} and \code{\link{sqrlParams}} for further detail.
}
}
\section{Script #10 (Combining Data Sources)}{
\preformatted{
<R>
  n <- OtherSource("select distinct customer_number ",
                   "from our.customers where town in "
                   "(" paste0("'", towns, "'") ")")
<do>

select sum(transactions) from online.orders
where customer_number in (<R> n$customer_number </R>)
}
\subsection{Commentary on Script #10}{
This is a repeat of script #09, except now the tables of the first and second
queries are hosted on two entirely separate data sources.
The function \code{OtherSource} is the \pkg{SQRL} interface to the data source
of the first query, while this script should be run from the interface to the
final source.
Again, there are other ways to do this, but the general idea is for the main \R
script (performing the modelling and analysis) to make a single call
\dQuote{get me this data}, without clutter, or concern for the true horror of
where that data comes from.
}
}
\section{Script #11 (The sqrl Function)}{
\preformatted{
<R>
  -- Check this is the intended source.
  stopifnot(sqrl("name") == "MySource")

  -- Remember initial parameter values.
  initials <- sqrl("settings")

  -- Set some parameter values.
  sqrl(stringsAsFactors = FALSE,
       wintitle = "[MySource]",
       visible = TRUE)
;

-- Submit the query, record the result.
select * from database.table
<result -> out>;

<R>
  -- Restore initial parameter values.
  sqrl(initials)

  -- Return the query result.
  out
}
\subsection{Commentary on Script #11}{
The function \code{sqrl(...)} is a special interface that is automatically
defined into the temporary script-evaluation (working) environment.
It acts as a cut-down \code{\link{sqrlAll}}, relaying its
arguments only to whichever \pkg{SQRL} data source is executing the script.
It works even when the name of the invoking interface function changes, or
when that interface function does not exist.
Consequently, \code{sqrl} should be adopted in preference to hard-coding
interface names into scripts.

Here, the \code{sqrl} function is first used to provide an assert, which, in
this case, verifies the script is being run only on the intended data source.
A second call then takes a snapshot of the current \pkg{RODBC}/\pkg{SQRL}
parameter settings (the \command{settings} command is described in
\code{\link{sqrlUsage}}).
A third call then sets new values for three of those parameters, prior to
submission of the \acronym{SQL} query.

Due to \pkg{SQRL}'s set-and-forget approach, these values are persistent, and
(unless they are changed again) will remain in effect after execution of the
script has completed.
That being the case, another call of \code{sqrl} is made after the query, in
order to restore the original parameter values.

Note that (temporary) parameter values set via \code{<with> ... </with>} take
precedence over (persistent) values set via \code{sqrl(...)}, irrespective of
the order in which those settings are made within the script.
Also, whilst (for example) \code{sqrl(max = 1)} sets the persistent value for
parameter \var{max}, \code{sqrl("max")} returns the value currently in effect
(which might be a temporary value).
Hence, for purposes of setting script-specific parameter values, using
\code{<with>} is, when possible, generally preferable to calling \code{sqrl}.

As shown in script #17, the \code{sqrl} function can also be used to make nested
queries (its arguments could just as easily be another script).

When a script is being run from some particular interface, explicit calls to
that same interface function (as opposed to \code{sqrl}) are normally blocked.
Calls to \code{\link{sqrlAll}}, \code{\link{sqrlInterface}},
\code{\link{sqrlOff}}, \code{\link{sqrlSource}}, and \code{\link{sqrlSources}}
are also normally blocked.
If you really want a way around these blocks, calls can be made to
\code{SQRL::sqrlAll}, instead of just \code{sqrlAll}, and so on.
Alternatively, \code{<R> rm("sqrlAll") <do>} (et cetera) removes the block (this
also works for unblocking the interface function).
}
}
\section{Script #12 (Closing Connections)}{
\preformatted{
-- Ensure readOnlyOptimise is TRUE.
<R>
  if (!sqrl("readOnlyOptimize"))
  {
    sqrl("close")
    sqrl(readOnlyOptimize = TRUE)
  }
<do>

-- Pull data (reopening is automatic).
select * from database.table;

-- Close the connection.
<close>
}
\subsection{Commentary on Script #12}{
Many communications parameters are \dQuote{locked while open}, and cannot be
changed while a connection (channel) exists to the source (see
\code{\link{sqrlParams}}).
In this example, we want a particular value for one such parameter, namely
\var{readOnlyOptimize}.
If its value needs changing, we must first ensure the channel is closed.
Within the \R section, this is achieved with \code{sqrl("close")} (see
\code{\link{sqrlUsage}}).

When the \acronym{SQL} query is submitted, \pkg{SQRL} will automatically open a
new connection, if need be.

After the data is pulled, the \code{<close>} tag closes the channel (because, in
this example, we do not want to leave it open).
Unlike \code{<R>sqrl("close")<do>}, \code{<close>} tags return no value of their
own, which means the final value of this \acronym{SQRL} script is still that of
the query (as we require).

Be aware that \code{<close>} does not imply \code{<do>}, and it is an error to
use \code{<close>} in the presence of partially-formed, or unsubmitted,
\acronym{SQL}.
Conversely, \code{<R>sqrl("close")</R>} is allowed within \acronym{SQL}.

An alternative to putting \code{<close>} at the end of a script is to set the
\var{autoclose} parameter to \code{TRUE} (see \code{\link{sqrlParams}}).
Placing \code{<close>} at the beginning of a script can be used to ensure no
temporary tables are in existence (when no better mechanism is available).

When user-input is required for authentication on the opening of a new channel,
connection closures should be kept to a bare minimum.
}
}
\section{Script #13 (Returns)}{
\preformatted{
-- This selects 1 (the result of the embedded R expression).
select <R> return(1); 2 </R>

-- This return doesn't exit the script either (only the R section).
<R>
  return(1)
  print("this won't be printed")
<do>

-- Pull from a temporary table, and save the result.
select some_columns from temp_table
<result -> a>;

-- Drop the temp table (this returns a value).
drop temp_table;

-- Return the value of interest (exit the script).
<return (a)>

-- This is unreachable.
select 1
}
\subsection{Commentary on Script #13}{
The difference between a \code{<return (a)>} and \code{<R>return(a)<do>}, is
that the former exits the \acronym{SQRL} script, while the latter only exits
from the (embedded) \R section (back into \acronym{SQL}), before continuing with
the script.

Almost any valid \R expression is allowed between the (mandatory) parentheses of
a \code{<return>} tag (see script #03).
Note that \code{<return>} tags are not recognised within \R sections.
Also, \code{<return>} tags do not imply \code{<do>}, and it is an error to
attempt a \code{<return>} within partially-formed (unsubmitted) \acronym{SQL}.
Because \code{x <- y} returns invisibly, so too do \code{<R> x <- y <do>} and
\code{<return (x <- y)>} (see example script #16).

The next example shows how \code{<return>} tags become much more useful when
combined with conditional expressions.
}
}
\section{Script #14 (If, Else If, Else)}{
\preformatted{
-- Submit this when we have a 9-digit code.
<if (nchar(code) == 9)>
  select category from item_category
  where long_item_code = <R> code </R>
  <result -> k> k <- as.integer(k) <do>

-- Submit this when we have a 6-digit code.
<else if (nchar(code) == 6)>
  select category from item_category
  where short_item_code = <R> code </R>
  <result -> k> k <- as.integer(k) <do>

-- Exit here when we have neither
-- (because the next query would fail).
<else>
  <return (NULL)>
</if>

-- Obtain all items in the same category,
-- in the original (long or short) format.
select
  category,
  <if (nchar(code) == 9)>
    long_item_code
  <else>
    short_item_code
  </if>
from item_category
where category = <R> k </R>
}
\subsection{Commentary on Script #14}{
In this example, we have a table of (say) stock items, each of which has both a
long (9 digit) and short (6 digit) identity code, and is assigned to some kind
of category (with another integer identifier).
The script takes a single integer argument, \var{code}, which might be in either
the long or short format.

The \code{<if>} and \code{<else if>} tags are used to submit an appropriate
query, according to the type of identity code supplied.
In the event that the code is of an unrecognised type, the \code{<else>} tag is
used, with \code{<return>}, to exit cleanly (without submitting any query).
Provided that the code is of a recognised type, a second query is submitted,
wherein \code{<if>} and \code{<else>} control which column is selected.

In the final query, the appropriate column could instead have been named from
within \code{<R>} and \code{</R>} tags, but the earlier conditional submission
and conditional return cannot easily be achieved in that way.
Essentially arbitrary \R is allowed between the (mandatory) parentheses of an
\code{<if>} or \code{<else if>} tag (see script #03), but the final result of
evaluating that \R must be either \code{TRUE} or \code{FALSE}.
None of these tags are recognised within \R sections.
To enable their intra-statement application (as in the final query of the
example script), none of these tags imply \code{<do>}.
The space in \code{else if} is optional (i.e., \code{elseif} is equally valid).
}
}
\section{Script #15 (Caching)}{
\preformatted{
-- If we already have a cached copy of the
-- data, return that.
<if (exists(.cached, .GlobalEnv))>
  <return (get(.cached, .GlobalEnv))>
</if>

-- Otherwise, submit the query and cache the
-- result before returing it.
select * from table
<result -> x>
  assign(.cached, x, .GlobalEnv)
  x
}
\subsection{Commentary on Script #15}{
In this example, the result of a large query is cached locally, with \code{<if>}
and \code{<return>} logic being used to return that cached result should the
script be run again.

This is most useful when the data changes infrequently, the query takes a long
time to run, and the script is stored as a procedure in the library (see script
#18).
}
}
\section{Script #16 (While Loops)}{
\preformatted{
<R>
  batch <- split(ID, ceiling(seq_along(ID) / 1000))
  x <- NULL
  i <- 1
<do>

-- Pull and accumulate results, a thousand at a time.
<while (i <= length(batch))>
  select idnumber, name from identity_lookup
  where idnumber in (<R> batch[[i]] </R>)
  <result -> y>
    i <- i + 1
    x <- rbind(x, y)
  <do>
</while>
}
\subsection{Commentary on Script #16}{
In this example, we have a script with a single argument; \code{ID}, a vector of
integer codes.
Supposing that vector might be too long for the \acronym{SQL} \code{in}
operator, a \code{<while>} loop is used to pull the results in batches.

Essentially arbitrary \R is allowed between the (mandatory) parentheses of a
\code{<while>} tag (see script #03), but the final result of evaluating it must
be either \code{TRUE} or \code{FALSE}.
As with the \code{<if>} family, \code{<while>} and \code{</while>} tags are not
recognised within \R sections, and do not imply \code{<do>}.
Ordinarily, loops over \acronym{SQL} should be avoided, or used only as a last
resort, but there are use-cases (see script #18), including for the insertion of
rows.

Referring back to script #13, the final result of script #16 (that of
\code{x <- rbind(x, y)}) is invisible.
If visible output is required, \code{<R>x} or \code{<return (x)>} could be
appended to the script.

The parser is simple, and does not verify or enforce correct nesting structure.
Unintuitive output may appear when nesting violations occur.
}
}
\section{Script #17 (Procedures)}{
\preformatted{
<proc "random patients">
  select patient_number
    from patient_details
    order by rand()
    limit <R> N </R>
  <result -> a>
    a[, 1L]
</proc>

select days_in_hospital
from patient_history
where patient_number in
  (<R> sqrl("random patients", N = 200) </R>)
}
\subsection{Commentary on Script #17}{
It is possible to define reusable \acronym{SQRL} procedures within (between)
\code{<proc>} and \code{</proc>} tags.
Here, a (parameterised) procedure is employed as a nested-query alternative to
the sequential feedback mechanism of script #09.

Each procedure must be named (in its \code{<proc>} tag), with quotes (either
single or double) being mandatory about that name.
The end of a procedure definition acts in the same way as the end of a script
(as an implied \code{<do>}).
Note that \code{</proc>} tags are only recognised within \R sections, when (as
is the case in this example) the \R section is within a procedure definition
(i.e., under a \code{<proc>} tag).
Opening \code{<proc>} tags are never recognised within \R sections.
}
}
\section{Script #18 (Libraries)}{
\preformatted{
<proc "add na.strings">
  -- Takes one argument, 'add' (a character vector),
  -- and adds its strings to the na.strings parameter.
  <R>
    sqrl(na.strings = unique(c(sqrl("na.strings"), add)))
  <do>
</proc>

<proc "drop if exists">
  /* Takes one argument, 'tables', being a
     character vector of tables to be dropped. */
  -- Force as.is to be TRUE (for the query).
  <with>
    as.is = TRUE
  </with>
  -- Pull details of temporary tables.
  help volatile table
  <result -> v>
  <do>
  -- Exit here when the above query did not return
  -- a data frame (when no volatile table exists).
  <if (class(v) != class(data.frame()))>
    <return (invisible())>
  </if>
  -- Extract the names of all volatile tables
  -- in existence, retain only those (unique)
  -- tables to be dropped that actually exist,
  -- and initialise the iterator.
  <R>
    volatiles <- as.character(v[, "Table SQL Name"])
    tables <- unique(tables[tables \%in\% volatiles])
    i <- 0L
  <do>
  -- Drop each requested table (that exists).
  <while (i <- i + 1L; i <= length(tables))>
    drop table <R> tables[i] </R> <do>
  </while>
  -- Return invisible NULL.
  <return (invisible())>
</proc>
}
\subsection{Commentary on Script #18}{
The previous example, script #17, exhibited a procedure defined as a utility
within a larger \acronym{SQRL} script.
Such definitions are not persistent, with the procedures vanishing upon the
conclusion of their parent script.

However, as shown in \code{\link{sqrlUsage}}, it is possible to construct a
persistent library of procedures.
As is the case above, \acronym{SQRL} scripts intended for libraries must
consist only of procedure definitions; no other \acronym{SQL} or \R is allowed.
This example script defines two procedures (both parameterised).
The first, \dQuote{add na.strings}, adds strings to the existing
\var{na.strings} vector (refer to \code{\link{sqrlParams}}).
The second, \dQuote{drop if exists}, implements that capability for Teradata
\acronym{SQL} (which doesn't).

Following \code{\link{sqrlUsage}}, let's say the above script is recorded in a
file \file{library.sqrl}, and that we have a \pkg{SQRL} interface function
called \code{owl}.
The library is then established with \code{owl(library = "library.sqrl")}, and
the procedures are called with (for instance)
\code{owl("add na.strings", add = c("N/A", "-"))}, and
\code{owl("drop if exists", tables = c("tableA", "tableB"))}.

Ultimately, procedures do not confer any capability beyond that of
\acronym{SQRL} files.
Procedure libraries merely allow the consolidation of multiple files into one.
They also offer another slight advantage in that when you change working
directory, they come along.
}
}
\section{Script #19 (Stop)}{
\preformatted{
select 1

-- This ends the script.
<stop>

-- This is unreachable.
select 2
}
\subsection{Commentary on Script #19}{
Lastly, \code{<stop>} tags act as an early end-of-script (with its implied
\code{<do>}).
They apply within \R sections, as well as \acronym{SQL}, and even on the inside
of a \code{FALSE} conditional block (i.e., the \code{<stop>} still functions in
\code{<if (FALSE)><R><stop>}).
They are intended as a troubleshooting aid.
}
}
\section{Summary of Tags}{
\describe{
\item{\code{<R>}:}{
Begins an \R section (leaves \acronym{SQL}).
Once begun, only \code{</R>}, \code{<do>}, \code{;}, \code{<stop>}, \code{EOS},
and \code{</proc>} are recognised (and can end the section).
The \code{</proc>} case is only recognised when the initiating \code{<R>} tag is
within a procedure definition (beneath a \code{<proc>} tag).
}
\item{\code{</R>}:}{
Ends an \R section, causing that section to be evaluated.
The result of that evaluation is pasted back into the surrounding \acronym{SQL}.
}
\item{\code{<do>}:}{
If inside an \R section, ends and evaluates that section (without pasting the
result back into \acronym{SQL}).
If outside of an \R section, causes the preceding \acronym{SQL} (which may be
blank) to be submitted.
}
\item{\code{<result -> name>}:}{
Submits the preceding \acronym{SQL}, and assigns the result of that submission
to the \R variable \code{name}.
Any syntactically valid \R name, or \dQuote{NULL}, is allowed (in place of
\code{name}).
An \R section begins immediately after the tag.
That section concludes with any of the tags listed (above) for \code{<R>},
besides \code{</R>}.
}
\item{\code{<close>}:}{
Closes the \acronym{ODBC} channel (connection) to the data source.
}
\item{\code{<return (Rexp)>}:}{
Evaluates \code{Rexp}, which can be any \R expression, and returns the resulting
value (exits the script).
The parentheses are mandatory.
}
\item{\code{<with>}:}{
Begins a special \R section, for assigning temporary values to
\pkg{RODBC}/\pkg{SQRL} parameters.
Once begun, only \code{</with>}, \code{<stop>}, \code{EOS}, and \code{</proc>}
are recognised (and can end the section).
The \code{</proc>} case is only recognised when the initiating \code{<with>} tag
lies within a procedure definition (beneath a \code{<proc>} tag).
Only the \var{aCollapse}, \var{as.is}, \var{buffsize}, \var{dec}, \var{errors},
\var{lCollapse}, \var{max}, \var{na.strings}, \var{nullstring}, \var{ping},
\var{pwd}, \var{retry}, \var{scdo}, \var{stringsAsFactors}, and \var{verbose}
parameters permit temporary value assignments.
}
\item{\code{</with>}:}{
Ends an \R section begun by \code{<with>}.
Causes the section to be evaluated, and assigns temporary parameter values
accordingly.
}
\item{\code{<if (Rexp)>}:}{
Evaluates \code{Rexp}, which can be any \R expression.
If that expression evaluated to \code{TRUE}, the (arbitrary) script beneath the
tag is acted upon.
If that expression evaluated to \code{FALSE}, the (arbitrary) script beneath the
tag is ignored (except for any \code{<stop>} tags).
The parentheses are mandatory.
}
\item{\code{<else if (Rexp)>}:}{
Acts as \code{<if (Rexp)>}, when the (mandatory) previous \code{<if ((Rexp)>},
and all the (multiple, optional) previous \code{<else if (Rexp)>} tags,
evaluated to \code{FALSE}.
Otherwise (when any of those evaluated to \code{TRUE}), acts as
\code{<if (FALSE)>}.
The parentheses are mandatory.
}
\item{\code{<else>}:}{
Acts as \code{<else if (TRUE)>}.
}
\item{\code{</if>}:}{
Marks the end of an \code{<if (Rexp)>} - \code{<else if (Rexp)>} - \code{<else>}
flow-control structure.
}
\item{\code{<while (Rexp)>}:}{
Acts in the manner of \code{<if (Rexp)>}.
The parentheses are mandatory.
}
\item{\code{</while>}:}{
Marks the end of a \code{<while (Rexp)>} flow-control loop.
If the previous \code{<while (Rexp)>} evaluated to \code{TRUE}, the parser
returns to that \code{<while (Rexp)>} tag, and re-evaluates the \R expression.
Otherwise (if the previous \code{<while (Rexp)>} evaluated to \code{FALSE}), no
action is performed (the parser continues from this point).
}
\item{\code{<proc "name">}:}{
Marks the beginning of the definition of a procedure called \var{name}.
Any character string can be used in place of \var{name}.
The quotation marks are mandatory (but can be singles or doubles).
}
\item{\code{</proc>}:}{
Marks the end of a procedure definition (acts as an \var{EOS} for that
procedure).
}
\item{\code{<stop>}:}{
Acts as \var{EOS}.
Applies even from within a \code{FALSE} conditional block.
Intended for debugging, only.
}
\item{\code{;}:}{
When the \var{scdo} parameter is at its default value of \code{TRUE} (see
\code{\link{sqrlParams}}), the \acronym{SQRL} parser attempts to identify
\acronym{SQL} statement terminating semicolons, procedural language block
terminating semicolons, and any extra semicolons within \R sections.
When detected, these all act as \code{<do>}.
}
\item{\var{EOS}:}{
The end of the script (\var{EOS}), acts as \code{<do>}.
As necessary, it also acts as \code{</if>} and \code{</proc>} (it does not act
as \code{</while>}).
}
}
}
\note{
The \var{verbose} parameter toggles extended output when running scripts (see
\code{\link{sqrlParams}}).
This includes the display of intermediate values.
}
\seealso{
\code{\link{sqrlUsage}}
}
\keyword{file}
