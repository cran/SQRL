####################################################################### SQRL ###

# Wrapper about RODBC. On load, SQRL automatically generates a like-named user-
# interface function to each DSN it finds on the system. These functions enable
# immediate interaction with each data source, since channels and communication
# parameters are managed behind the scenes. The general philosophy is to require
# the least possible typing from the user, while allowing the greatest possible
# flexibility in how commands are entered. The approach emphasises the source
# and query over concatenation functions and control parameters. The interfaces
# accept multi-statement SQL scripts, allowing the use of scripts developed in
# other applications without modification or multiple function calls. The script
# parser supports query parameterisation via embedded R expressions, conditional
# submission, loops, the feedback of intermediate results, and early returns.
# Secondary features include the protection of connection handles from rm(), the
# recovery of lost connections, the promotion of remote ODBC exceptions to local
# R errors, optional automatic closure of connections between queries, and the
# visual indication of which connections are open and of queries in progress.
# Mike Lee, South Titirangi, 30 January 2019.



#################################################################### HISTORY ###

# 1 February 2019. CRAN 0.6.1. Now requires R-3.3+ (for .traceback()) Removed an
# R-3.5 dependency, isFALSE(), that had also crept into the previous version.

# 31 January 2019. CRAN 0.6.0. Parser supports conditionals, loops and returns.
# Semicolons can be used in place of <do> tags. Added the autoclose parameter.

# 12 October 2014. CRAN 0.5.0. Various small improvements to the script parser,
# interface argument checkers, and the sqrlSources() summary function.

# 4 June 2018. CRAN 0.4.0. Enabled interface()$isopen and interface()$source.
# Invisible return of the trivial results of non-query SQL submissions.

# 16 April 2018. CRAN 0.3.0. Run-time generated help. Source deregistration.
# Parameter resetting. Interface()$parameter. SQRL-script <R> ... <do> blocks.

# 10 March 2018. CRAN 0.2.1. The driver parameter can be defined as a file path
# (as may be appropriate on GNU/Linux systems).

# 8 March 2018. CRAN 0.2.0. Support for kwargs and explicitly parameterised SQRL
# scripts. Complete documentation overhaul.

# 8 January 2018. CRAN 0.1.1. Removed calls of sqrlOff() from help-file examples
# for compliance with the upcoming R-3.5.

# 12 November 2017. CRAN 0.1.0. Completely rewritten for R 3.2+. No longer
# domain or DBMS specific.

# 15 April 2014. Packaged but not published. Support for SQL scripts (multiple
# statements, embedded R). Remains domain and DBMS specific.

# 7 January 2014. Unpackaged prototype script. R-2. Domain and DBMS specific.
# No support for SQL-scripts. Channels protected from rm(). Openness indicators.



################################################################### CONTENTS ###

# srqlHaus            Private. Environment. Stores data source parameters.
# srqlHelp            Private. Environment. Stores interface help temp files.

# SqrlAll()           Private. Broadcasts a command to every SQRL source.
# SqrlCache()         Private. Interfaces with srqlHaus (only point of contact).
# SqrlClose()         Private. Closes data source connection channels.
# SqrlConfig()        Private. Sets SQRL/RODBC parameters from a config file.
# SqrlDefault()       Private. Defines and returns default parameter values.
# SqrlDefile()        Private. Extracts parameter values from container files.
# SqrlDelegate()      Private. Relays data between interfaces and functions.
# SqrlDSNs()          Private. Registers existing DSN data sources with SQRL.
# SqrlFace()          Private. Interfaces with the SQRL:Face environment.
# SqrlFile()          Private. Sources SQL (and/or R) statements from a file.
# SqrlHelp()          Private. Generates run-time help for interface functions.
# SqrlHelper()        Private. Escapes strings for help-file compatibility.
# SqrlIndicator()     Private. Toggles display of open-connection indicators.
# SqrlInterface()     Private. Defines and/or deletes data source interfaces.
# SqrlIsOpen()        Private. Tests whether or not source channels are open.
# SqrlOff()           Private. Closes all channels, detaches and unloads SQRL.
# SqrlOpen()          Private. Opens connection channels to data sources.
# SqrlParam()         Private. Gets and sets data source SQRL/RODBC parameters.
# SqrlParams()        Private. Defines and returns various parameter groupings.
# SqrlPath()          Private. Checks if args are the path to an existing file.
# SqrlPing()          Private. Defines simple queries for pinging data sources.
# SqrlStatement()     Private. Assembles SQL statements from listed components.
# SqrlSource()        Private. Registers/defines new data sources with SQRL.
# SqrlSources()       Private. Look for, and summarise, known data sources.
# SqrlSubmit()        Private. Submits SQL, retrieves results, handles errors.
# SqrlSubScript()     Private. Relays data between SqrlFile() and SqrlSubmit().
# SqrlValue()         Private. Wrapper to SqrlParam(). Keeps secrets secret.

# sqrlAll()           Public.  Wrapper to SqrlAll(). See above.
# sqrlInterface()     Public.  Wrapper to SqrlInterface(). See above.
# sqrlOff()           Public.  Wrapper to SqrlOff(). See above.
# sqrlSource()        Public.  Wrapper to SqrlSource(). See above.
# sqrlSources()       Public.  Wrapper to SqrlSources(). See above.

# .onLoad()           Private. Attaches SQRL:Face and finds sources, on load.
# .onUnload()         Private. Detaches the SQRL:Face environment, on unload.



############################################################### ENVIRONMENTS ###

# Environment for caching data source parameters. Not exported. The user will
# not, without some effort, be able to view or modify objects within this.
srqlHaus <- base::new.env(parent = base::emptyenv())

# Environment for tracking temp files for the dynamic run-time help system. Not
# exported. The user will not be able to easily view or modify objects within.
srqlHelp <- base::new.env(parent = base::emptyenv())

# There will also exist a public environment, attached to the R search path as
# 'SQRL:Face', by the .onLoad() function, when the package is loaded.



########################################################## PRIVATE FUNCTIONS ###

SqrlAll <- function(...,
                    envir = base::parent.frame())
{
  # Applies the same command to each of the (currently defined) SQRL sources.
  # Args:
  #   ...   : A list of strings, as per (passed unaltered to) SqrlDelegate().
  #   envir : An environment. Only used by SqrlFile() (for processing R script).
  # Returns:
  #   A list (by SQRL source name) of the results of running the ... command(s)
  #   on each of the SQRL sources. NULL results are not explicit.
  # SQRL Calls:
  #   SqrlCache(), SqrlDelegate().
  # SQRL Callers:
  #   SqrlSources(), sqrlAll().
  # User:
  #   Has no direct access, but is able to supply (only) the ... arguments from
  #   sqrlAll(). Since ... is unrestricted, no validity checking is required.

  # Give the command to each data source, in turn. Retrieve the results. Fatal
  # errors will block sending the command to further sources. Opting not to wrap
  # in try(), because stopping may be preferable under many circumstances.
  results <- base::list()
  for (datasource in SqrlCache("*"))
  {
    result <- SqrlDelegate(datasource, ..., envir = envir)

    # Assigning NULL to a list element[[]] removes the element, whereas
    # assigning list(NULL) (to []) leaves the element with a NULL value.
    if (base::is.null(result))
    {
      results[datasource] <- base::list(NULL)
    } else
    {
      results[[datasource]] <- result
    }
  }

  # Return the results (listed by SQRL data source name).
  base::return(results)
}

SqrlCache <- function(datasource = "",
                      exists = NULL,
                      create = FALSE,
                      delete = FALSE)
{
  # Checks, creates, lists and gets data source cache environments.
  # Args:
  #   datasource : The name of a data source, or '*' for all known data sources.
  #   exists     : If set to TRUE or FALSE, test if a cache exists or doesn't.
  #   create     : If set to TRUE, create a cache for the data source.
  #   delete     : If set to TRUE, delete an existing data source cache.
  # Returns:
  #   Either an environment handle, a logical (when performing an existence
  #   check), a character vector (when listing all known data sources), or
  #   invisible NULL (after removing a data source's cache).
  # SQRL Calls:
  #   SqrlClose(), SqrlInterface(), SqrlParam(), SqrlParams(), srqlHaus.
  # SQRL Callers:
  #   SqrlAll(), SqrlDefault(), SqrlDelegate(), SqrlDSNs(), SqrlOff(),
  #   SqrlParam(), SqrlSource(), SqrlSources(), sqrlInterface().
  # User:
  #   Has no direct access, but is able to supply (only) the datasource argument
  #   via SqrlSource(), which verifies existence of that source before passing
  #   the argument on. Further validity checks are not required.

  # Defines the name for data source <datasource>'s cache environment.
  cachename <- base::paste(".", datasource, sep = "!")

  # If the exists argument was specified, return whether or not the cache exists
  # (whether or not the data source is known to SQRL). Use of exists as an
  # argument (variable) does not interfere with base::exists(), the function.
  # When exists is TRUE (FALSE), we return TRUE when the cache does (does not)
  # exist (i.e., the source is (is not) known).
  if (!base::is.null(exists))
  {
    ex <- base::exists(cachename, srqlHaus,
                        mode = "environment", inherits = FALSE)
    base::return(ex == exists)
  }

  # If the delete flag was set, close any open connection to the source, delete
  # its interface, delete its cache (this completes deregistration from SQRL),
  # and return invisible NULL. The garbage collector ought to take care of any
  # parameters within the cache.
  if (delete)
  {
    if (base::exists(cachename, srqlHaus,
                      mode = "environment", inherits = FALSE))
    {
      SqrlClose(datasource)
      SqrlInterface(datasource, "remove")
      SqrlParam(datasource, "reset", SqrlParams("secret"))
      SqrlParam(datasource, "reset", SqrlParams("semi-secret"))
      base::remove(list = cachename, pos = srqlHaus, inherits = FALSE)
    }
    base::return(base::invisible(NULL))
  }

  # If datasource is specified as '*', return a character vector of all data
  # source names for which a SQRL cache exists.
  if (datasource == "*")
  {
    cachenames <- base::objects(srqlHaus, all.names = TRUE, pattern = "^\\.!")
    if (base::length(cachenames) < 1L)
    {
      base::return(base::character(0L))
    }
    is.cache <- base::sapply(base::as.list(cachenames),
                                function(x) base::exists(x, srqlHaus,
                                                          mode = "environment",
                                                          inherits = FALSE))
    cachenames <- cachenames[is.cache]
    base::return(base::substring(cachenames, base::nchar(".!") + 1L))
  }

  # If the create argument was specified as TRUE, create a new cache for the
  # specified data source. Abort if the cache (or like-named object) exists.
  if (create)
  {
    if (base::exists(cachename, srqlHaus, inherits = FALSE))
    {
      base::stop("Source cache already exists.")
    }
    cache <- base::new.env(parent = base::emptyenv())
    base::assign(cachename, cache, srqlHaus)
    SqrlParam(datasource, "name", datasource)
    base::return(cache)
  }

  # Otherwise, abort if the cache does not exist.
  if (!base::exists(cachename, srqlHaus,
                      mode = "environment", inherits = FALSE))
  {
    base::stop("Cache does not exist.")
  }

  # The cache exists; return a handle to it.
  base::return(base::get(cachename, srqlHaus,
                           mode = "environment", inherits = FALSE))
}

SqrlClose <- function(datasource = "")
{
  # Closes the channel to the specified data source.
  # Args:
  #   datasource : The name of a data source whose channel is to be closed.
  # Returns:
  #   Invisible NULL, after closing the channel and removing the handle.
  # SQRL Calls:
  #   SqrlParam().
  # RODBC Calls:
  #   odbcClose().
  # SQRL Callers:
  #   SqrlCache(), SqrlDelegate(), SqrlIsOpen(), SqrlOff().
  # User:
  #   Has no direct access, unable to pass argument indirectly. No argument
  #   validity checks are required.

  # Return invisible NULL if the channel is already closed.
  if (base::is.null(SqrlParam(datasource, "channel")))
  {
    base::return(base::invisible(NULL))
  }

  # Attempt to close the channel (which may, or may not, actually be open).
  base::try(RODBC::odbcClose(SqrlParam(datasource, "channel")), silent = TRUE)

  # Whatever the situation, nullify the connection handle immediately. If the
  # channel somehow survived the close attempt, this makes it unusable. The
  # SqrlParam() function will remove any visible connection indicators.
  SqrlParam(datasource, "channel", NULL)

  # Return invisible NULL.
  base::return(base::invisible(NULL))
}

SqrlConfig <- function(datasource = "",
                        confile = "")
{
  # Assigns SQRL/RODBC parameter values, for a data source, from a file.
  # Args:
  #   datasource : The name of a known (to SQRL) data source.
  #   confile    : The path to a SQRL configuration file (optional).
  # Returns:
  #   The imported configuration, as an invisible list of (name, value) pairs.
  #   When no configuration file is specified, this function acts as a getter,
  #   and returns a list of all SQRL/RODBC parameters and their current values.
  # SQRL Calls:
  #   SqrlDefile(), SqrlInterface(), SqrlParams(), SqrlPath(), SqrlValue().
  # SQRL Callers:
  #   SqrlDelegate(), SqrlHelp(), SqrlSource().
  # User:
  #   Has no direct access, but is able to pass the confile and parameter
  #   arguments (only) via SqrlDelegate(). That function vets parameter, and
  #   ensures confile is a character string, but does not guarantee that confile
  #   specifies a path to an actual (existing) file.

  # If no config file was specified, return the data source's SQRL/RODBC
  # configuration as a list of parameter values, excepting the password (pwd).
  # We don't return a data frame, because we have mixed data types here,
  # including RODBC handles.
  if (base::nchar(confile) < 1L)
  {
    params <- SqrlParams("all")
    secret <- SqrlParams("secret")
    semisecret <- SqrlParams("semi-secret")
    config <- base::list()
    for (param in params)
    {
      # Retrieve parameter values, excepting those held secret (passwords).
      value <- SqrlValue(datasource, param)

      # Assigning NULL to a list element[[]] removes the element, whereas
      # assigning list(NULL) (to []) leaves the element with a NULL value.
      if (base::is.null(value))
      {
        config[param] <- base::list(NULL)
      } else
      {
        config[[param]] <- value
      }
    }
    base::return(config)
  }

  # Abort if a config file was specified, but that file does not exist.
  filepath <- SqrlPath(confile)
  if (base::is.null(filepath))
  {
    base::stop("File not found.")
  }

  # Read parameter values (to be set) from the config file.
  first = ""
  config <- base::list()
  for (lyne in base::readLines(filepath, warn = FALSE))
  {
    # Skip blank lines, and lines beginning with the comment symbol (#).
    if (!base::grepl("[[:graph:]]", lyne)
        || base::grepl("^[[:space:]]*#", lyne))
    {
      next
    }

    # Retain the first non-empty line, for use as a default value when no
    # explicit (name, value) pair is found for a specified parameter.
    if (base::nchar(first) < 1L)
    {
      first <- base::trimws(lyne)
    }

    # If the line does not contain '=', do not extract a (name, value) pair.
    if (!base::grepl("=", lyne))
    {
      next
    }

    # Extract a (parameter) (name, value) pair (required format: name = value).
    pos <- base::regexpr("=", lyne)
    param <- base::trimws(base::substring(lyne, 1L, pos - 1L))
    value <- base::trimws(base::substring(lyne, pos + base::nchar("=")))

    # Retain the (name, value) pair, provided the name is not blank, and not
    # 'channel' (blocks setting/overwriting of this parameter), and the value
    # is also not blank.
    if ((base::nchar(param) > 0L)
        && (base::nchar(value) > 0L)
        && (param != "channel"))
    {
      config[[param]] <- value
    }
  }

  # If 'interface' is among the parameters to be set, then set it first (since
  # it's the one most likely to fail). If this does fail, then no further
  # parameter values will be set (SqrlInterface() will throw an exception).
  if ("interface" %in% base::names(config))
  {
    value <- SqrlDefile("interface", config[["interface"]], evaluate = TRUE)
    SqrlInterface(datasource, value)
  }

  # Assign all values found (allows setting of user-invented parameter names).
  # The driver parameter is set last, to override any default driver set as a
  # side effect in the course of setting dsn (should that have been set).
  params <- base::names(config)[base::names(config) != "interface"]
  params <- base::c(params[params != "driver"], params[params == "driver"])
  for (parameter in params)
  {
    value <- SqrlDefile(parameter, config[[parameter]], evaluate = TRUE)
    config[[parameter]] <- SqrlValue(datasource, parameter, value)
  }

  # Return the (secrets-obscured) configuration, invisibly.
  base::return(base::invisible(config))
}

SqrlDefault <- function(datasource = "",
                        parameter = "")
{
  # Defines and returns default parameter values.
  # Args:
  #   datasource : The name of a known (to SQRL) data source.
  #   parameter  : The name of a single specific parameter.
  # Returns:
  #   The default value of the named parameter for the named data source.
  # SQRL Calls:
  #   SqrlCache(), SqrlParam().
  # SQRL Callers:
  #   SqrlParam().
  # User:
  #   Has no direct access, but is able to supply (only) parameter via
  #   SqrlDelegate(), which does the vetting. No further checks are required.

  # Obtain a handle to the data source's SQRL cache.
  cacheenvir <- SqrlCache(datasource)

  # Return the default value of the specified parameter.
  base::return(base::switch(parameter,

    # Parameters for RODBC::odbcConnect() and/or RODBC::odbcDriverConnect().
    "dsn"                 = "",
    "uid"                 = base::as.character(base::Sys.info()["user"]),
    "pwd"                 = "",
    "connection"          = "",
    "case"                = "nochange",
    "believeNRows"        = TRUE,
    "colQuote"            = base::ifelse(base::grepl("MySQL",
                                                SqrlParam(datasource, "driver"),
                                                ignore.case = TRUE),
                                          "`", "\""),
    "tabQuote"            = SqrlParam(datasource, "colQuote"),
    "interpretDot"        = TRUE,
    "DBMSencoding"        = "",
    "rows_at_time"        = 100L,
    "readOnlyOptimize"    = FALSE,

    # Parameters for RODBC::sqlQuery().
    # Also uses believeNRows and rows_at_time, as above.
    "channel"             = NULL,
    "errors"              = TRUE,
    "as.is"               = FALSE,
    "max"                 = 0L,
    "buffsize"            = 1000L,
    "nullstring"          = NA_character_,
    "na.strings"          = "NA",
    "dec"                 = base::as.character(base::getOption("dec")),
    "stringsAsFactors"    = base::default.stringsAsFactors(),
    # default.stringsAsFactors() throws an error if options() has been used to
    # set stringsAsFactors to a value that is neither TRUE nor FALSE.

    # Parameters for SQRL.
    "*"                   = base::objects(cacheenvir, all.names = TRUE),
    "autoclose"           = FALSE,
    "driver"              = "",
    "interface"           = NULL,
    "name"                = datasource,
    "ping"                = NULL,
    "prompt"              = base::substr(datasource, 1L, 1L),
    "verbose"             = FALSE,
    "visible"             = FALSE,
    "wintitle"            = base::paste0("(", datasource, ")"),

    # No other default parameter values are defined (abort and notify).
    base::stop("Unknown parameter.")))
}

SqrlDefile <- function(parameter = "",
                        value = "",
                        evaluate = FALSE)
{
  # Recursively substitutes file paths with contained parameter values.
  # Args:
  #   parameter : A single parameter name.
  #   value     : Either a final value or a file path.
  #   evaluate  : Whether or not to attempt to evaluate value as an expression.
  # Returns:
  #   A value for the parameter, either as supplied or as found within the
  #   supplied file (alternative).
  # SQRL Calls:
  #   SqrlDefile() (self), SqrlParams(), SqrlPath().
  # SQRL Callers:
  #   SqrlConfig(), SqrlDefile() (self), SqrlDelegate(), SqrlSource(),
  #   sqrlInterface().
  # User:
  #   Has no direct access, but is able to supply (only) parameter and value via
  #   SqrlParam() from SqrlDelegate() and/or SqrlConfig(). The parameter is
  #   guaranteed to be a string, and no further checks are required. The value
  #   may turn out to be unsuitable, but that is left for SqrlParam() to decide.

  # Return the unmodified value, if it is not of character type (SqrlPath()
  # doesn't like NULL input), if it is the empty string (evaluates to NULL), or
  # if it is one or more blank strings (also evaluates to NULL).
  if ((base::class(value) != base::class(base::character()))
      || (base::length(value) == 0)
      || base::all(!base::nzchar(base::trimws(value))))
  {
    base::return(value)
  }

  # If the (character) value is not a file path, or if the parameter can be path
  # valued, then return either the unmodified value, or else (if possible and so
  # requested) the evaluated value.
  if (base::is.null(SqrlPath(value))
      || (parameter %in% SqrlParams("path-valued")))
  {
    # The value is not a path. If it is not to be evaluated, return it as is.
    if (!evaluate)
    {
      base::return(value)
    }

    # Otherwise, if the value doesn't evaluate, or if it evaluates to something
    # odd (for example, 'ls' evaluates to a function), return it unmodified.
    evaluated <- base::try(base::eval(base::parse(text = value),
                                      base::new.env(parent = base::baseenv())),
                            silent = TRUE)
    if (base::inherits(evaluated, "try-error")
        || !(base::class(evaluated) %in% base::c(base::class(NULL),
                  base::class(base::logical()), base::class(base::character()),
                  base::class(base::numeric()), base::class(base::integer()))))
    {
      base::return(value)
    }

    # The value could be evaluated; return the evaluated value.
    base::return(evaluated)
  }

  # Search the file for the matching parameter name and value (config format).
  path <- value
  value <- NA
  for (lyne in base::readLines(path, warn = FALSE))
  {
    # Skip blank lines, and lines beginning with the comment symbol (#).
    if (!base::grepl("[[:graph:]]", lyne)
        || base::grepl("^[[:space:]]#", lyne))
    {
      next
    }

    # Retain the first non-empty line, for use as a default value when no
    # explicit (name, value) pair is found for a specified parameter.
    if (base::is.na(value))
    {
      value <- base::trimws(lyne)
    }

    # If the line does not contain '=', do not extract a (name, value) pair.
    if (!base::grepl("=", lyne))
    {
      next
    }

    # Extract a (parameter) (name, value) pair (required format: name = value).
    pos <- base::regexpr("=", lyne)
    param <- base::trimws(base::substring(lyne, 1L, pos - 1L))

    # Stop once the matching parameter value has been found.
    if (param == parameter)
    {
      value <- base::trimws(base::substring(lyne, pos + base::nchar("=")))
      break
    }
  }

  # Put the extracted value back into this function, in case it is another
  # file path (recursive call, infinite loops are possible). Given that the
  # current value is a file path, we evaluate the next value (since it is to be
  # read from file as text), whether or not the current value was evaluated.
  base::return(SqrlDefile(parameter, value, evaluate = TRUE))
}

SqrlDelegate <- function(datasource = "",
                          ...,
                          envir = base::parent.frame())
{
  # Interpret the command, and forward to the appropriate handler.
  # Args:
  #   datasource : The name of a known database.
  #   ...        : Some number of arguments, forming a command or file path.
  #   envir      : An R environment (only used when sourcing files).
  # Returns:
  #   The result of the command (normally a data frame, sometimes a string).
  # SQRL Calls:
  #   SqrlCahce(), SqrlClose(), SqrlConfig(), SqrlDefile(), SqrlFile(),
  #   SqrlHelp(), SqrlInterface(), SqrlIsOpen(), SqrlOpen(), SqrlParam(),
  #   SqrlParams(), SqrlPath(), SqrlSources(), SqrlSubmit(), SqrlValue().
  # RODBC Calls:
  #   sqlColumns(), sqlTables(), sqlTypeInfo().
  # SQRL Callers:
  #   SqrlAll() (and data source interfaces).
  # User:
  #   User has no direct access, but is able to supply (only) the ... arguments
  #   from sqrlAll() and/or any data source interface. Since ... is unrestricted
  #   (it could be SQL), no argument validity checking is required.

  # List the supplied arguments, and count their number.
  args.list <- base::list(...)
  args.count <- base::length(args.list)

  # If no command was given, open a channel to the data source. If no channel
  # exists, a new channel is opened. If a channel exists, but wasn't open after
  # all (after besure = TRUE pings the data source to check), we replace the
  # dead channel with a new one. If a channel exists and is open, we do nothing
  # else. If autoclose is set, the channel is closed immediately afterwards.
  # Returns the configuration invisibly, enabling interface()$parameter.
  if (args.count == 0L)
  {
    isopen <- SqrlIsOpen(datasource, besure = TRUE)
    if (!isopen)
    {
      SqrlOpen(datasource)
      isopen <- SqrlIsOpen(datasource)
    }
    if (SqrlParam(datasource, "autoclose"))
    {
      SqrlClose(datasource)
      isopen <- SqrlIsOpen(datasource)
    }
    config <- SqrlConfig(datasource)
    config[["source"]] <- SqrlValue(datasource, "source")
    config[["isopen"]] <- isopen
    base::return(base::invisible(config[base::order(base::names(config))]))
  }

  # Obtain the stated names of the supplied arguments. This may be NULL (no
  # names at all), or a character vector (with "" for any unnamed elements).
  args.names <- base::names(args.list)

  # When none of the arguments are named, attempt to interpret them as a list of
  # subcommands or file components (to be pasted together), or as specific SQRL
  # commands (consisting of a name and, optionally, a value to set).
  if (base::is.null(args.names)
      || base::all(base::nchar(args.names) == 0L))
  {
    # If the command specifies a file path, try sourcing SQL from that file.
    # If autoclose is set, the channel is closed immediately afterwards.
    file.path <- SqrlPath(...)
    if (!base::is.null(file.path))
    {
      result <- base::withVisible(
                                SqrlFile(datasource, file.path, envir = envir))
      if (SqrlParam(datasource, "autoclose"))
      {
        SqrlClose(datasource)
      }
      if (!result$visible)
      {
        base::return(base::invisible(result$value))
      }
      base::return(result$value)
    }

    # Extract the first word from the first supplied argument.
    first.word <- base::sub("^[^[:graph:]]*([[:graph:]]+).*$", "\\1",
                            args.list[[1L]])[1L]

    # If the first word looks like standard SQL, submit the unaltered command.
    # If autoclose is set, the channel is closed immediately afterwards.
    if (base::tolower(first.word) %in% SqrlParams("sql-keywords"))
    {
      result <- base::withVisible(SqrlSubmit(datasource, ...))
      if (SqrlParam(datasource, "autoclose"))
      {
        SqrlClose(datasource)
      }
      if (!result$visible)
      {
        base::return(base::invisible(result$value))
      }
      base::return(result$value)
    }

    # If the first supplied argument contains more than one word, the other
    # words consist of everything except the first word (pasted together).
    if (base::grepl("[[:graph:]]+[^[:graph:]]+[[:graph:]]+", args.list[[1L]]))
    {
      other.words <- base::trimws(base::sub(first.word, "",
                          base::paste(args.list, collapse = ""), fixed = TRUE))
      only.word <- ""

    # Otherwise (the first supplied argument is a single word), if only one
    # argument was supplied, then the (that) first word is the only word.
    } else if (args.count == 1L)
    {
      only.word <- first.word
      other.words <- ""

    # Otherwise, if precisely two arguments were supplied, then the other words
    # are the second argument verbatim (could be any object, not just a string).
    } else if (args.count == 2L)
    {
      only.word <- ""
      other.words <- args.list[[2L]]

    # Otherwise, the other words consist of all the supplied arguments besides
    # the first (paste these together).
    } else
    {
      only.word <- ""
      other.words <- base::paste(
                              args.list[base::seq(2L, base::length(args.list))],
                              collapse = "")
    }

    # If the only word is 'close', close the data source channel.
    if ("close" == only.word)
    {
      base::return(SqrlClose(datasource))
    }

    # If the first word is 'columns', call RODBC::sqlColumns() on the remainder.
    # If autoclose is set, the channel is closed immediately afterwards.
    if ("columns" == first.word)
    {
      SqrlOpen(datasource)
      SqrlIndicator(datasource, "query")
      result <- base::try(RODBC::sqlColumns(
                                    channel = SqrlParam(datasource, "channel"),
                                    sqtable = other.words,
                                    errors = SqrlParam(datasource, "errors"),
                                    as.is = TRUE),
                          silent = TRUE)
      SqrlIndicator(datasource, "done")
      if (SqrlParam(datasource, "autoclose"))
      {
        SqrlClose(datasource)
      }
      if (base::inherits(result, "try-error")
          && SqrlParam(datasource, "error"))
      {
        base::stop(result)
      }
      base::return(result)
    }

    # If the first word is 'config', get or set the configuration.
    if ("config" == first.word)
    {
      base::return(SqrlConfig(datasource, other.words))
    }

    # If the first word is 'help', or some multiple of '?', and the other words
    # are 'text', 'html', or absent, then provide help. We test for those other
    # words here, because (for example) 'help volatile table' is valid Teradata,
    # and "help 'contents'" is valid MySQL. Neither allows just 'help' alone.
    if ((("help" == first.word)
          || base::grepl("^[?]+$", first.word))
        && ((first.word == only.word)
            || base::identical(base::tolower(other.words), "html")
            || base::identical(base::tolower(other.words), "text")))
    {
      base::return(SqrlHelp(datasource, other.words))
    }

    # If the only word is 'interface', return the interface function name.
    if ("interface" == only.word)
    {
      base::return(SqrlValue(datasource, only.word))
    }

    # If the first word is 'interface', change the interface function.
    if ("interface" == first.word)
    {
      value <- SqrlDefile(first.word, other.words, evaluate = TRUE)
      base::return(SqrlInterface(datasource, value))
    }

    # If the only word is 'isopen' (or if words one and two are 'is open'),
    # return the channel's open status (TRUE for open, FALSE otherwise). This
    # calls with besure = TRUE, to ping the source and make certain of the
    # openness status. If autoclose is set, the channel is closed, not pinged.
    if (("isopen" == only.word)
        || (("is" == first.word)
            && ("open" == other.words)))
    {
      if (SqrlParam(datasource, "autoclose"))
      {
        SqrlClose(datasource)
        base::return(SqrlIsOpen(datasource))
      }
      base::return(SqrlIsOpen(datasource, besure = TRUE))
    }

    # If the only word is 'open', open a channel to the specified data source.
    # If autoclose is set, the channel is closed immediately afterwards.
    if ("open" == only.word)
    {
      if (SqrlParam(datasource, "autoclose"))
      {
        SqrlOpen(datasource)
        base::return(SqrlClose(datasource))
      }
      base::return(SqrlOpen(datasource))
    }

    # If the only word is 'remove', then deregister the source from SQRL.
    if ("remove" == only.word)
    {
      base::return(SqrlCache(datasource, delete = TRUE))
    }

    # If the first word is 'reset', then reset the stated parameters.
    if ("reset" == first.word)
    {
      base::return(SqrlParam(datasource, first.word, other.words))
    }

    # If the only word is source, return the (placeholder substituted, secrets
    # obliterated) source definition (either a DSN or a connection string).
    if ("source" == only.word)
    {
      base::return(SqrlValue(datasource, "source"))
    }

    # If the command is 'sources', return the data source summary table.
    if ("sources" == only.word)
    {
      base::return(SqrlSources())
    }

    # If the first word is 'tables', call RODBC::sqlTables() on the data source.
    # If autoclose is set, the channel is closed immediately afterwards.
    if ("tables" == first.word)
    {
      schema <- NULL
      if ("tables" != only.word)
      {
        schema <- other.words
      }
      SqrlOpen(datasource)
      SqrlIndicator(datasource, "query")
      result <- base::try(RODBC::sqlTables(
                                  channel = SqrlParam(datasource, "channel"),
                                  errors = SqrlParam(datasource, "errors"),
                                  as.is = TRUE,
                                  schema = schema),
                          silent = TRUE)
      SqrlIndicator(datasource, "done")
      if (SqrlParam(datasource, "autoclose"))
      {
        SqrlClose(datasource)
      }
      if (base::inherits(result, "try-error")
          && SqrlParam(datasource, "error"))
      {
        base::stop(result)
      }
      base::return(result)
    }

    # If the first word is 'typeinfo', call RODBC::sqlTypeInfo() on the others.
    # If autoclose is set, the channel is closed immediately afterwards.
    if ("typeinfo" == first.word)
    {
      SqrlOpen(datasource)
      type <- base::ifelse(first.word == only.word, "all", other.words)
      SqrlIndicator(datasource, "query")
      info <- base::try(RODBC::sqlTypeInfo(
                                  channel = SqrlParam(datasource, "channel"),
                                  type = type,
                                  errors = SqrlParam(datasource, "errors"),
                                  as.is = TRUE),
                        silent = TRUE)
      SqrlIndicator(datasource, "done")
      if (SqrlParam(datasource, "autoclose"))
      {
        SqrlClose(datasource)
      }
      if (base::inherits(info, "try-error")
          && SqrlParam(datasource, "error"))
      {
        base::stop(info)
      }
      base::return(info)
    }

    # When the first word is an SQRL/RODBC parameter, get or set that parameter.
    if (first.word %in% SqrlParams("all"))
    {

      # When getting, return the parameter's value (except for secrets, such as
      # passwords, which are returned obliterated), visibly.
      if (first.word == only.word)
      {
        base::return(SqrlValue(datasource, first.word))
      }

      # Allow getting, but not setting, of the channel parameter from here.
      if (first.word %in% SqrlParams("read-only"))
      {
        base::stop("Parameter is read-only.")
      }

      # Set the parameter's value to the supplied other words, then return the
      # (secrets-obscured) value, invisibly.
      value <- SqrlDefile(first.word, other.words, evaluate = TRUE)
      base::return(base::invisible(SqrlValue(datasource, first.word, value)))
    }

    # Otherwise, submit the original unaltered command.
    # If autoclose is set, the channel is closed immediately afterwards.
    result <- base::withVisible(SqrlSubmit(datasource, ...))
    if (SqrlParam(datasource, "autoclose"))
    {
      SqrlClose(datasource)
    }
    if (!result$visible)
    {
      base::return(base::invisible(result$value))
    }
    base::return(result$value)
  }

  # When all arguments are named, treat them as either parameterised queries to
  # be submitted, or as SQRL parameter values to be (re)set.
  if (base::all(base::nchar(args.names) > 0L))
  {
    # Prohibit the use of both of the names 'file' and 'query', since it is
    # unclear which refers to the query and which is a parameter to the other.
    if (base::all(base::c("file", "query") %in% args.names))
    {
      base::stop("The file and query arguments are mutually exclusive.")
    }

    # If one of the names in 'file', then submit a query from the file, and
    # treat any other (named) arguments as parameters to that query. It is this
    # function's responsibility to verify the existence and readability of the
    # file before passing it to SqrlFile(). If the autoclose parameter is set,
    # then the channel is closed immediately afterwards.
    if ("file" %in% args.names)
    {
      file.path <- SqrlPath(args.list[["file"]])
      if (base::is.null(file.path))
      {
        base::stop("File not found.")
      }
      params <- args.list[args.names != "file"]
      result <- base::withVisible(
                                SqrlFile(datasource, file.path, envir, params))
      if (SqrlParam(datasource, "autoclose"))
      {
        SqrlClose(datasource)
      }
      if (!result$visible)
      {
        base::return(base::invisible(result$value))
      }
      base::return(result$value)
    }

    # If one of the names is 'query', then pass the query to SqrlFile() (as a
    # script, not as a file name), with any other arguments as named parameters.
    # If autoclose is set, the channel is closed immediately afterwards.
    if ("query" %in% args.names)
    {
      script <- args.list[["query"]]
      params <- args.list[args.names != "query"]
      result <- base::withVisible(
                              SqrlFile(datasource, script, envir, params, TRUE))
      if (SqrlParam(datasource, "autoclose"))
      {
        SqrlClose(datasource)
      }
      if (!result$visible)
      {
        base::return(base::invisible(result$value))
      }
      base::return(result$value)
    }

    # Otherwise, interpret each name as that of a parameter, and assign each
    # value accordingly. The name 'reset' is a special case (reset specified
    # parameters to their default values). The driver parameter is set last,
    # to override any default driver that may have been set as a side effect
    # in the course of setting the dsn parameter.
    if (base::any(args.names %in% SqrlParams("read-only")))
    {
      base::stop("Parameter is read-only.")
    }
    result <- base::list()
    for (param in base::c(args.names[args.names != "driver"],
                          args.names[args.names == "driver"]))
    {
      if (param == "config")
      {
        result[[param]] <- SqrlConfig(datasource, args.list[[param]])
      } else if (param == "interface")
      {
        value <- SqrlDefile(param, args.list[[param]], evaluate = FALSE)
        result[[param]] <- SqrlInterface(datasource, value)
      } else if (param == "reset")
      {
        SqrlParam(datasource, param, args.list[[param]])
      } else
      {
        value <- SqrlDefile(param, args.list[[param]], evaluate = FALSE)
        result[[param]] <- SqrlValue(datasource, param, value)
      }
      if (base::is.null(result[[param]]))
      {
        result[param] <- base::list(NULL)
      }
    }
    if (base::length(result) < 2L)
    {
      result <- base::unlist(result)
    }
    base::return(base::invisible(result))
  }

  # When both named and unnamed arguments exist, and all named arguments trail
  # all unnamed arguments, then interpret the unnamed arguments as the path of
  # a SQRL script, and the named arguments as parameters of that script.
  # If autoclose is set, the channel is closed immediately afterwards.
  args.kindex <- base::which(base::nchar(args.names) > 0L)[1L]
  if (base::all(base::nchar(args.names[args.kindex:args.count]) > 0L))
  {
    file.path <- SqrlPath(
                        base::unlist(args.list[base::seq((args.kindex - 1L))]))
    if (base::is.null(file.path))
    {
      base::stop("File not found.")
    }
    params <- args.list[args.kindex:args.count]
    result <- base::withVisible(SqrlFile(datasource, file.path, envir, params))
    if (SqrlParam(datasource, "autoclose"))
    {
      SqrlClose(datasource)
    }
    if (!result$visible)
    {
      base::return(base::invisible(result$value))
    }
    base::return(result$value)
  }

  # At least one unnamed argument trails at least one named argument.
  # Abort and notify.
  base::stop("All unnamed arguments must precede all named arguments.")
}

SqrlDSNs <- function(import = "all")
{
  # Import data source names (DSNs), and create interfaces for them.
  # Args:
  #   import : The RODBC::odbcDataSources() type; 'all', 'user', or 'system'.
  # Returns:
  #   Invisible NULL, after registering DSNs with SQRL.
  # SQRL Calls:
  #   SqrlCache(), SqrlInterface(), SqrlParam(), SqrlParams().
  # RODBC Calls:
  #   odbcDataSources().
  # SQRL Callers:
  #   SqrlSources(), .onLoad().
  # User:
  #   Has no direct access. Is able to supply the argument from sqrlSources(),
  #   via SqrlSources(), but it is vetted there and no further validity checks
  #   are required.

  # Import a list of registered data sources (DSNs).
  sources <- RODBC::odbcDataSources(type = import)

  # Filter out Microsoft Access, dBASE, and Excel sources.
  unwanted <- base::paste(SqrlParams("unwanted-sources"), collapse = "|")
  sources <- sources[!base::grepl(unwanted, sources, ignore.case = TRUE)]

  # If any of the sources was previously unknown (has no associated cache), then
  # create a new cache for it. Store some valuables in the cache, then attempt
  # to generate an interface for the source (failure to do so is non-fatal).
  # A user-defined source will prevent importing a DSN of the same name.
  for (datasource in base::names(sources))
  {
    if (SqrlCache(datasource, exists = FALSE))
    {
      SqrlCache(datasource, create = TRUE)
      SqrlParam(datasource, "dsn", datasource)
      SqrlParam(datasource, "driver", sources[datasource])
      SqrlInterface(datasource, datasource, vital = FALSE)
    }
  }

  # Return invisible NULL.
  base::return(base::invisible(NULL))
}

SqrlFace <- function(interface = "",
                      set = NULL,
                      exists = NULL,
                      clashes = NULL,
                      delete = FALSE)
{
  # Checks, sets, gets, and removes data source user-interface functions.
  # Args:
  #   interface : The name (a string) of a SQRL interface function, or NULL.
  #   set       : Is supplied, this definition is assigned to the interface.
  #   exists    : If TRUE or FALSE, test whether or not the interface exists.
  #   clashes   : If Boolean, test for an object name conflict with interface.
  #   delete    : If TRUE, delete the interface.
  #   prime     : If TRUE, create and attach the interface environment.
  # Returns:
  #   Either the interface (function) definition, a logical existence or
  #   name-conflict indicator, or invisible NULL (when deleting).
  # SQRL Calls:
  #   SQRL:Face.
  # SQRL Callers:
  #   SqrlInterface(), SqrlSource().
  # User:
  #   Has no direct access, but is able to pass-in the interface argument (only)
  #   from SqrlInterface() or SqrlSource(). Both of these check that interface
  #   is a unique (non-clashing) and assignable name. No further checks needed.

  # When acting as a setter; make the assignment, return the result invisibly.
  # This does not alter the data source's 'interface' parameter.
  if (!base::is.null(set))
  {
    def <- base::eval(base::parse(text = set))
    base::assign(interface, def, "SQRL:Face")
    base::return(base::invisible(def))
  }

  # If the exists argument was specified, return whether or not the interface
  # exists (as a function). When exists is TRUE (FALSE), we return TRUE when the
  # interface does (does not) exist.
  if (!base::is.null(exists))
  {
    if (base::is.null(interface))
    {
      base::return(FALSE == exists)
    }
    ex <- base::exists(interface, "SQRL:Face",
                        mode = "function", inherits = FALSE)
    base::return(ex == exists)
  }

  # If the clashes argument was specified, return whether or not the interface
  # name is already taken by some other object, in sqrlFace, the global
  # environment, or any of their ancestor (parent, etc.) environments. When
  # clashes is TRUE (FALSE), we return TRUE when there is (not) a conflict.
  if (!base::is.null(clashes))
  {
    if (base::exists(interface, "SQRL:Face", inherits = TRUE)
        || base::exists(interface, base::globalenv(), inherits = TRUE))
    {
      base::return(clashes)
    }
    base::return(!clashes)
  }

  # Delete the interface function, on request. This does not alter the data
  # source's 'interface' parameter.
  if (delete)
  {
    base::suppressWarnings(
            base::remove(list = interface, pos = "SQRL:Face", inherits = FALSE))
    base::return(base::invisible(NULL))
  }

  # Acting as a getter; return the interface function.
  base::return(base::get(interface, "SQRL:Face",
                          mode = "function", inherits = FALSE))
}

SqrlFile <- function(datasource = "",
                      script.file = "",
                      envir = base::parent.frame(),
                      params = NULL,
                      is.script = FALSE)
{
  # Read a SQRL-script file and submit its content to a data source.
  # Args:
  #   datasource  : The name of a known data source.
  #   script.file : The file name (or path, or actual script), as a string.
  #   envir       : An R environment (script is executed in a child of this).
  #   params      : A named list of R parameters for the script.
  #   is.script   : If set to TRUE, script.file is taken as a literal script.
  # Returns:
  #   Result of submitting the script.
  # SQRL Calls:
  #   SqrlParam(), SqrlStatement(), SqrlSubScript().
  # SQRL Callers:
  #   SqrlDelegate().
  # utils Calls:
  #   head() (only if utils is attached).
  # User:
  #   Has no direct access, but is able to submit (only) the script.file
  #   argument (only) via SqrlDelegate(). That function verifies the existence
  #   of a file at script.file. No further checks are required.

  # If the supplied script.file has been flagged as actually being the script
  # (rather than the name of a containing file), then treat it as such.
  if (is.script)
  {
    script <- script.file

  # Otherwise, read the entirety of the script within script.file (slurp all
  # lines). No ordinary script would be so large that this should be a problem.
  } else
  {
    script <- base::paste(base::readLines(script.file, warn = FALSE,
                                          skipNul = TRUE), collapse = "\n")
  }

  # Script delimiter definitions (regular expression patterns).
  patterns <- base::list()
  patterns$tag.r.begin     <- "<r>"
  patterns$tag.r.end       <- "</r>"
  patterns$tag.do          <- "<do>"
  patterns$tag.stop        <- "<stop>"
  patterns$tag.result      <- "<result[[:blank:]]*->[[:blank:]]*[^[:space:]>]+>"
  patterns$tag.if          <- "<if[[:blank:]]*\\("
  patterns$tag.elseif      <- "<else[[:blank:]]*if[[:blank:]]*\\("
  patterns$tag.else        <- "<else>"
  patterns$tag.endif       <- "</if>"
  patterns$tag.while       <- "<while[[:blank:]]*\\("
  patterns$tag.endwhile    <- "</while>"
  patterns$tag.return      <- "<return[[:blank:]]*\\("
  patterns$end.expression  <- ")>"
  patterns$comment.begin   <- "/\\*"
  patterns$comment.end     <- "\\*/"
  patterns$comment.line    <- "--"
  patterns$comment.r       <- "#"
  patterns$end.of.line     <- "\n"
  patterns$quote.single    <- "'"
  patterns$quote.double    <- "\""
  patterns$semi.colon      <- ";"

  # Scan the script for delimiter positions (pos), types (pat), and character
  # sequence lengths (len). For example, one delim might be pat = 'tag.result',
  # starting at character pos = 145 of script, and len = 13 characters long
  # The actual delimiter is then substring(script, pos, pos + len - 1), which in
  # most cases (besides tag.result) is an invariant pattern. In our example, the
  # delimiter might be '<result -> x>' (13 characters).
  pos = NULL
  pat = NULL
  len = NULL
  for (pattern in base::names(patterns))
  {
    matches <- base::gregexpr(patterns[[pattern]],
                                script, ignore.case = TRUE)[[1L]]
    positions <- base::as.integer(matches)
    if ((base::length(positions) > 1L)
        || (positions > 0L))
    {
      pos <- base::c(pos, positions)
      pat <- base::c(pat, base::rep(pattern, base::length(positions)))
      len <- base::c(len, base::attr(matches, "match.length"))
    }
  }

  # Sort the delimiters (if any exist) into ascending (script) positional order.
  if (base::length(pos) > 1L)
  {
    ord <- base::order(pos)
    pos <- pos[ord]
    pat <- pat[ord]
    len <- len[ord]
  }

  # The total number of delimiters (of all kinds) found in the script.
  num.delims <- base::length(pos)

  # The total number of characters (invisible or otherwise) within the script.
  nchar.script <- base::nchar(script)

  # Boolean; whether or not to show verbose output.
  verbose <- base::interactive() && SqrlParam(datasource, "verbose")

  # Create a new environment as a child of the invoking environment.
  # SqrlFile() evaluates R expressions (including the post-processing) within
  # this environment (rather than the invoking environment) so as to avoid
  # overwriting variables within the invoking environment.
  sqrl.env <- base::new.env(parent = envir)

  # Assign any supplied parameters to the processing environment.
  for (param in base::names(params))
  {
    base::assign(param, params[[param]], sqrl.env)
  }

  # Default result. The result is a list of two components; value and visible,
  # as per withVisible(). This function will return the last non-empty value.
  result <- base::withVisible(base::invisible(base::character(0L)))

  # The SQL statement in progress (the script may contain multiple statements).
  statement <- base::list()

  # A stack, upon which to store (while) loop return (start) points.
  loop.points <- base::integer()

  # A stack, upon which to store the results of nested conditionals.
  cond.stack <- base::logical()

  # Result of evaluating the last (innermost nested) condition.
  cond.current <- TRUE

  # A stack, upon which to store whether or not any of the previous alternative
  # conditions within an if, else if, else structure have yet evaluated to TRUE.
  else.stack <- base::logical()

  # Delimiter counter/index (to pos, pat, and len). Range is [1 : num.delims].
  i <- 1L

  # Character counter/index (to script). Range is [1 : nchar.script].
  k <- 1L

  # Parse the script, submit SQL, evaluate and substitute R.
  while (i <= num.delims)
  {
    # Remove comments from SQL (both to-end-of-line and block).
    # The main reason for this, is that some data sources are (have been) known
    # to reject queries with more than one block of comments at the beginning.
    # A second reason is that RODBC's error messages may include the submitted
    # script, which is easier to read if we've cleaned it up. The flip side is
    # that our parsing (rather than the source's) had better get things right.
    if ((i <= num.delims)
        && (pat[i] %in% base::c("comment.line", "comment.begin")))
    {
      # Append any preceding fragment to the script, unless within the block of
      # an untrue conditional expression.
      if (cond.current)
      {
        # Isolate unappended (to the statement) script preceding this comment.
        phrase <- base::substring(script, k, pos[i] - 1L)

        # Remove trailing whitespace (including vertical) from the phrase.
        # (Only before to-end-of-line comments.)
        if (pat[i] == "comment.line")
        {
          phrase <- base::sub("[[:space:]]*$", "", phrase)
        }

        # Remove trailing whitespace from each internal line of the phrase.
        phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

        # Remove vertical whitespace from within the phrase.
        phrase <- base::gsub("\n+", "\n", phrase)

        # Append the phrase to the statement (unless the phrase is empty).
        if (base::nchar(phrase) > 0L)
        {
          statement <- base::append(statement, phrase)
        }
      }

      # Scan through the subsequent script delimiters, until the comment
      # concludes with either an end-of-file, or appropriate delimiter.
      end.marker <- base::switch(pat[i],
                                  comment.line = "end.of.line",
                                  comment.begin = "comment.end")
      i <- i + 1L
      while ((i <= num.delims)
              && (pat[i] != end.marker))
      {
        i <- i + 1L
      }

      # Reposition the start-of-phrase index immediately after the end of the
      # comment. When the comment ends with a newline, the index is placed on
      # that newline (so that the next phrase will begin with the newline).
      k <- base::ifelse(i <= num.delims,
            base::ifelse(end.marker == "end.of.line", pos[i], pos[i] + len[i]),
            nchar.script + 1L)

      # Advance to the next script delimiter.
      i <- i + 1L
    }

    # Incorporate (single & double) quote-enclosed strings verbatim within SQL.
    # That is; ignore anything that looks like a delimiter, but is in a string.
    if ((i <= num.delims)
        && (pat[i] %in% base::c("quote.single", "quote.double")))
    {
      # Append any preceding fragment to the script, unless within the block of
      # an untrue conditional expression.
      if (cond.current)
      {
        # Isolate unappended (to the statement) script preceding this string.
        phrase <- base::substring(script, k, pos[i] - 1L)

        # Remove trailing whitespace from each internal line of the phrase.
        phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

        # Remove vertical whitespace from within the phrase.
        phrase <- base::gsub("\n+", "\n", phrase)

        # Append the phrase to the statement (unless the phrase is empty).
        if (base::nchar(phrase) > 0L)
        {
          statement <- base::append(statement, phrase)
        }
      }

      # Reposition the start-of-phrase index on (including) the beginning quote.
      k <- pos[i]

      # Scan through the subsequent script delimiters, until the string
      # concludes with either an end-of-file, or matching quote delimiter.
      # We only test for \ escaped quotes here (once already in quote mode,
      # which also guarantees i > 1).
      # Some SQLs use doubled quotes within quotes to represent quote literals.
      # Such abominations are supported by this parser, at least in all testing
      # thus far, since 'x''' is read (here) as two strings: 'x' and '', which
      # which are later collapsed together with "", restoring 'x''' in the SQL.
      closing.quote <- pat[i]
      i <- i + 1L
      while ((i <= num.delims)
              && ((pat[i] != closing.quote)
                  || ((base::attr(base::regexpr(
                        base::paste0("\\\\*", patterns[[closing.quote]], "$"),
                        base::substring(script, pos[i - 1L], pos[i])),
                        "match.length") %% 2L) == 0L)))
      {
        i <- i + 1L
      }

      # Append the quoted string to the statement. Verbatim, quotes included.
      # Unless within the block of an untrue conditional expression.
      if (cond.current)
      {
        statement <- base::append(statement, base::ifelse(i <= num.delims,
                                            base::substring(script, k, pos[i]),
                                            base::substring(script, k)))
      }

      # Position the start-of-phrase index immediately after the closing quote.
      k <- base::ifelse(i <= num.delims, pos[i] + len[i], nchar.script + 1L)

      # Advance to the next script delimiter.
      i <- i + 1L
    }

    # Ignore remainder of script when encountering a 'stop' tag within SQL.
    # The 'stop' tag is mainly used to run partial scripts while bug hunting.
    if ((i <= num.delims)
        && (pat[i] == "tag.stop"))
    {
      if (cond.current)
      {
        # Isolate any unappended (to the statement) script preceding this stop.
        phrase <- base::substring(script, k, pos[i] - 1L)

        # Remove trailing whitespace (including vertical) from the phrase.
        phrase <- base::sub("[[:space:]]*$", "", phrase)

        # Remove trailing whitespace from each internal line of the phrase.
        phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

        # Remove vertical whitespace from within the phrase.
        phrase <- base::gsub("\n+", "\n", phrase)

        # Append the phrase to the statement (unless the phrase is empty).
        if (base::nchar(phrase) > 0L)
        {
          statement <- base::append(statement, phrase)
        }
      }

      # Advance the delimiter and phrase indices beyond the end of the script.
      # Break immediately (unnecessary). Statement will be submitted afterwards.
      # Note that stop tags apply even inside untrue conditional blocks.
      i <- num.delims + 1L
      k <- nchar.script + 1L
      break
    }

    # Submit statement (and retrieve result) on encountering an end-of-statement
    # marker (either a semi-colon or a 'do' tag) within SQL.
    if ((i <= num.delims)
        && (pat[i] %in% base::c("semi.colon", "tag.do")))
    {
      # Submit the statement, plus any unappended fragment, unless it is within
      # the block of an untrue conditional expression.
      if (cond.current)
      {
        # Isolate any unappended (to the statement) script preceding the marker.
        # Include the semi-colon itself, but do not include the 'do' tag.
        phrase <- base::ifelse(pat[i] == "semi.colon",
                              base::substring(script, k, pos[i] + len[i] - 1L),
                              base::substring(script, k, pos[i] - 1L))

        # Submit the statement (with phrase) and pull the result.
        dat <- base::withVisible(SqrlSubScript(datasource, statement, phrase))

        # If there was a result (there was a query), replace the overall result.
        if (!base::is.null(dat$value))
        {
          result <- dat
        }

        # Reset the statement (begin the next one afresh).
        statement <- base::list()
      }

      # Reposition the start-of-phrase index immediately after the semi-colon.
      k <- pos[i] + len[i]

      # Advance to the next script delimiter.
      i <- i + 1L
    }

    # Act upon condition end and else tags, encountered within SQL.
    if ((i <= num.delims)
        && (pat[i] %in% base::c("tag.endif", "tag.endwhile", "tag.else")))
    {
      # Remember the type of tag we've encountered.
      pat.type = pat[i]

      # Throw an exception if we're ending a loop that was never started.
      if ((pat.type == "tag.endwhile")
          && cond.current
          && (base::length(loop.points) < 1L))
      {
        if (SqrlParam(datasource, "autoclose"))
        {
          SqrlClose(datasource)
        }
        base::stop("End without while.")
      }

      # Throw an exception if we're ending a block that was never started.
      if ((pat.type == "tag.endif")
          && base::length(cond.stack) < 1L)
      {
        if (SqrlParam(datasource, "autoclose"))
        {
          SqrlClose(datasource)
        }
        base::stop("End without if.")
      }

      # Throw an exception if we've met an else but not a previous if.
      if (base::length(else.stack) < 1L)
      {
        if (SqrlParam(datasource, "autoclose"))
        {
          SqrlClose(datasource)
        }
        base::stop("Else without if.")
      }

      # Append any preceding fragment to the statement, unless ending (within)
      # the block of an untrue conditional expression.
      if (cond.current)
      {
        # Isolate any unappended (to the statement) script preceding this end.
        phrase <- base::substring(script, k, pos[i] - 1L)

        # Remove trailing whitespace (including vertical) from the phrase.
        phrase <- base::sub("[[:space:]]*$", "", phrase)

        # Remove trailing whitespace from each internal line of the phrase.
        phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

        # Remove vertical whitespace from within the phrase.
        phrase <- base::gsub("\n+", "\n", phrase)

        # Append the phrase to the statement (unless the phrase is empty).
        if (base::nchar(phrase) > 0L)
        {
          statement <- base::append(statement, phrase)
        }
      }

      # If we've reached the end of an active loop, pop the loop starting index
      # from the loop stack, and return to that point of the script.
      if ((pat.type == "tag.endwhile")
          && cond.current)
      {
        i <- loop.points[base::length(loop.points)]
        loop.points <- loop.points[base::seq(base::length(loop.points) - 1L)]
        k <- pos[i]

      # Otherwise, we've reached an else, the end of a (TRUE OR FALSE) if block,
      # or the end of an inactive (FALSE while) loop. Continue past the tag.
      } else
      {
        # Reposition the start-of-phrase index immediately after the end tag.
        k <- pos[i] + len[i]

        # Advance to the next script delimiter.
        i <- i + 1L
      }

      # In the case of an else tag, the condition becomes TRUE if all encasing
      # conditionals are TRUE (so the else lies within an active block) and all
      # previous alternatives (the parent if, and any else ifs) have evaluated
      # FALSE (so none of them have already applied).
      if (pat.type == "tag.else")
      {
        # Locate (grab the index of) the current (last) else stack entry.
        es <- base::length(else.stack)

        # The current condition becomes TRUE if all encasing conditionals are
        # TRUE (so the else belongs to an if-else structure that lies within an
        # active outer block) and all previous alternatives (the parent if, and
        # any else ifs) have evaluated FALSE (so none of those conditions has
        # already applied).
        new.cond <- (!else.stack[es]) && base::all(cond.stack)
        cond.current <- new.cond

        # The else condition becomes (or is) TRUE if the new condition is TRUE
        # (for this alternative) or if any previous else condition (alternative)
        # has already been TRUE.
        else.stack[es] <- new.cond || else.stack[es]

      # Otherwise, in the case of an end tag, pop (restore) the previous
      # (encasing) condition from the stack.
      } else
      {
        cond.current <- cond.stack[base::length(cond.stack)]
        cond.stack <- cond.stack[base::seq(1L, base::length(cond.stack) - 1L)]
      }

      # In the case of ending an if, remove the current (last) entry from the
      # else stack (all alternatives having been exhausted).
      if (pat.type == "tag.endif")
      {
        else.stack <- else.stack[base::seq(base::length(else.stack) - 1L)]
      }
    }

    # Act upon an if, else-if, while or return tag, encountered within SQL.
    if ((i <= num.delims)
        && (pat[i] %in%
                  base::c("tag.if", "tag.elseif", "tag.while", "tag.return")))
    {
      # Remember which type of tag we've encountered, and where we found it.
      tag.type <- pat[i]
      tag.pos <- i

      # Append any preceding fragment to the statement, unless within the block
      # of an untrue conditional expression.
      if (cond.current)
      {
        # Isolate any unappended (to the statement) script preceding this end.
        phrase <- base::substring(script, k, pos[i] - 1L)

        # Remove trailing whitespace from each internal line of the phrase.
        phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

        # Remove vertical whitespace from within the phrase.
        phrase <- base::gsub("\n+", "\n", phrase)

        # Append the phrase to the statement (unless the phrase is empty).
        if (base::nchar(phrase) > 0L)
        {
          statement <- base::append(statement, phrase)
        }
      }

      # Advance the start-of-phrase index to the opening expression parenthesis
      # (that being the last character of the tag).
      k <- pos[i] + len[i] - 1L

      # Counters for the number of left and right parentheses within the phrase.
      # We don't check the ordering of the parentheses.
      lpar <- 0L
      rpar <- 0L

      # Have not yet located the end (closing parenthesis) of the expression.
      complete <- FALSE

      # Remove any comments within the R expression (both R and SQL). Because
      # this involves parsing (to find the end of the section), we need to work
      # through this even when cond.current is FALSE.
      rscript <- base::list()
      i <- i + 1L
      while (i <= num.delims)
      {
        # Remove comments from R (including SQL line and block comments).
        # This is so we can use SQL comments within the R (looks better under
        # SQL syntax highlighting rules within your text editor), and is also
        # necessary to allow commenting out of quote markers, <do>, <stop>, and
        # </R> tags with R comments (as well as SQL).
        if ((i <= num.delims)
            && (pat[i] %in%
                        base::c("comment.line", "comment.begin", "comment.r")))
        {
          # Isolate any unappended script preceding this comment, and append it
          # to the R-script.
          fragment <- base::substring(script, k, pos[i] - 1L)
          rscript <- base::append(rscript, fragment)

          # Count the number of left and right parentheses within the fragment.
          lpar <- lpar + base::nchar(base::gsub("[^(]", "", fragment))
          rpar <- rpar + base::nchar(base::gsub("[^)]", "", fragment))

          # Scan through the subsequent script delimiters, until the comment
          # concludes with either an end-of-file, or appropriate delimiter.
          end.marker <- base::switch(pat[i],
                                      comment.r = "end.of.line",
                                      comment.line = "end.of.line",
                                      comment.begin = "comment.end")
          i <- i + 1L
          while ((i <= num.delims)
                  && (pat[i] != end.marker))
          {
            i <- i + 1L
          }

          # Reposition the start-of-phrase index immediately after the end of
          # the comment. When the comment ends with a newline, the index is
          # placed on that newline (the next phrase will begin with newline).
          k <- base::ifelse(i <= num.delims,
                              base::ifelse(end.marker == "end.of.line",
                                            pos[i], pos[i] + len[i]),
                              nchar.script + 1L)
        }

        # Skip over (single, double) quote-enclosed strings (include verbatim).
        # (Ignore anything that looks like a delimiter, but is inside a string.)
        if ((i <= num.delims)
            && (pat[i] %in% base::c("quote.single", "quote.double")))
        {
          # Isolate any unappended script preceding this string literal, and
          # append it to the R-script.
          fragment <- base::substring(script, k, pos[i] - 1L)
          rscript <- base::append(rscript, fragment)

          # Count the number of left and right parentheses within the fragment.
          lpar <- lpar + base::nchar(base::gsub("[^(]", "", fragment))
          rpar <- rpar + base::nchar(base::gsub("[^)]", "", fragment))

          # Reposition the start-of-phrase index on top of the opening quote.
          k <- pos[i]

          # Scan through the script delimiters until reaching the end of the
          # quote (ignore all other delimiters found in between). We only test
          # for \ escaped quotes here (once already within quote mode, which
          # also guarantees that i > 1).
          closing.quote <- pat[i]
          i <- i + 1L
          while ((i <= num.delims)
                  && ((pat[i] != closing.quote)
                      || ((base::attr(base::regexpr(
                          base::paste0("\\\\*", patterns[[closing.quote]], "$"),
                          base::substring(script, pos[i - 1L], pos[i])),
                          "match.length") %% 2L) == 0L)))
          {
            i <- i + 1L
          }

          # Append the quoted string literal to the R-script, without counting
          # any parentheses that may appear within it.
          rscript <- base::append(rscript, base::ifelse(i <= num.delims,
                                            base::substring(script, k, pos[i]),
                                            base::substring(script, k)))

          # Position the start-of-phrase index just after the closing quote.
          k <- base::ifelse(i <= num.delims, pos[i] + len[i], nchar.script + 1L)
        }

        # Upon finding an end-of-expression marker, establish whether or not it
        # terminates the expression. If not, keep looking. If so, evaluate it.
        if ((i <= num.delims)
            && (pat[i] == "end.expression"))
        {
          # Extract any un-appended fragment preceding (and including) the right
          # parenthesis (first character) of the end.expression sequence to the
          # expression (R-script).
          fragment <- base::substring(script, k, pos[i])
          rscript <- base::append(rscript, fragment)

          # Count the numbers of left and right parentheses within the fragment.
          lpar <- lpar + base::nchar(base::gsub("[^(]", "", fragment))
          rpar <- rpar + base::nchar(base::gsub("[^)]", "", fragment))

          # Reposition the start-of-phrase index immediately after the right
          # parenthesis (first character) of the end.expression sequence.
          k <- pos[i] + 1L

          # When we have equal numbers of left and right parentheses (both being
          # at least one), we consider the expression to be complete and attempt
          # to evaluate it. We don't check for correct parenthesis ordering.
          if (lpar <= rpar)
          {
            # The complete expression has been identified and isolated.
            complete <- TRUE

            # In the case of a first conditional tag (not an else-if), push the
            # current conditional mode (cond.current) to the condition stack.
            if (!(tag.type %in% base::c("tag.elseif", "tag.return")))
            {
              cond.stack <- base::c(cond.stack, cond.current)
            }

            # The expression must be evaluated, to determine the condition of
            # the upcoming nested block, if this is a leading (if, while, or
            # return) conditional and the current condition is TRUE, or if this
            # is an alternative condition (else-if), no previous alternative has
            # been TRUE (i.e., the current else state is FALSE), and the entire
            # conditional stack (which does not include the current condition)
            # is TRUE (i.e., we are within an active block of script).
            if ((cond.current && (tag.type != "tag.elseif"))
                || ((tag.type == "tag.elseif")
                      && !else.stack[base::length(else.stack)]
                      && base::all(cond.stack)))
            {
              # Collapse the expression (fragments) and remove the enclosing
              # parentheses (in case it contains multiple statements).
              cond <- base::paste0(rscript, collapse = "")
              expr <- base::substring(cond, 2L, base::nchar(cond) - 1L)

              # Evaluate the tag expression. On error, stop with exception,
              # after closing (if autoclose is set).
              tval <- base::try(base::withVisible(
                                base::eval(base::parse(text = expr), sqrl.env)),
                                silent = TRUE)
              if (base::inherits(tval, "try-error"))
              {
                if (SqrlParam(datasource, "autoclose"))
                {
                  SqrlClose(datasource)
                }
                base::stop(tval)
              }

              # In the case of a return tag, return the evaluated expression
              # (stop processing the SQRL script, and exit from this point).
              if (tag.type == "tag.return")
              {
                if (tval$visible)
                {
                  base::return(tval$value)
                }
                base::return(base::invisible(tval$value))
              }

              # Otherwise, the expression should be a logical condition,
              # replacing the previous condition.
              cond.current <- tval$value

              # If in verbose mode, output the expression and its evaluation.
              if (verbose)
              {
                base::cat("\n")
                base::cat(base::paste(base::trimws(cond), "is", cond.current))
                base::cat("\n")
              }

              # We require expressions to evaluate to Boolean singletons.
              if (!base::is.logical(cond.current)
                  || (base::length(cond.current) != 1L)
                  || base::is.na(cond.current))
              {
                if (SqrlParam(datasource, "autoclose"))
                {
                  SqrlClose(datasource)
                }
                base::stop("Condition neither TRUE nor FALSE.")
              }

              # If this was a while condition, and if that condition was TRUE,
              # then the loop is active and we push its starting location to the
              # loop (return point) stack.
              if ((tag.type == "tag.while")
                  && cond.current)
              {
                loop.points <- base::c(loop.points, tag.pos)
              }

              # If the tag is an else-if, update the current else condition. To
              # have arrived at this point with an else-if tag, the current else
              # condition must be FALSE. FALSE && cond.current is cond.current.
              if ((tag.type == "tag.elseif")
                  && cond.current)
              {
                else.stack[base::length(else.stack)] <- cond.current
              }

            # Otherwise, when an encasing conditional is untrue, do not evaluate
            # the expression (variables may be undefined). Instead, continue in
            # untrue mode until reaching the end of the encasing block. In the
            # case of a return tag within an untrue block, perform no action.
            } else if (tag.type != "tag.return")
            {
              cond.current <- FALSE
            }

            # In the case of an if, copy the current condition onto the else
            # stack, for any alternative (else-if, else) blocks to reference.
            if (tag.type == "tag.if")
            {
              else.stack <- base::c(else.stack, cond.current)
            }

            # Reposition the start-of-phrase index immediately after the
            # terminating (expression closing) tag.
            k <- pos[i] + len[i]

            # Advance to the next script delimiter.
            i <- i + 1L

            # Having processed the conditional tag, continue with the script.
            break
          }
        }

        # Advance to the next script delimiter.
        i <- i + 1L
      }

      # Throw an exception if we run out of script without ever finding the end
      # of the intra-tag expression.
      if (!complete)
      {
        if (SqrlParam(datasource, "autoclose"))
        {
          SqrlClose(datasource)
        }
        base::stop("Unterminated intra-tag expression.")
      }
    }

    # Evaluate embedded R (and insert into SQL or produce a result).
    if ((i <= num.delims)
        && pat[i] %in% base::c("tag.r.begin", "tag.result"))
    {
      # Remember the mode we're in (either embedded or post-processing).
      r.type <- pat[i]

      # Process any preceding fragment, unless within the block of an untrue
      # conditional expression.
      if (cond.current)
      {
        # Isolate any unappended (to the statement) script preceding this tag.
        phrase <- base::substring(script, k, pos[i] - 1L)

        # If this is R post-processing, submit any query beforehand.
        if (r.type == "tag.result")
        {
          # Extract the name of the intermediate variable to which the SQL
          # result is to be assigned within the R processing environment.
          intermediate <- base::gsub("^<result\\s*->\\s*|>$", "",
                          base::substring(script, pos[i], pos[i] + len[i] - 1L))

          # Submit the statement (with phrase) and pull the result.
          dat <- base::withVisible(SqrlSubScript(datasource, statement, phrase,
                                                  intermediate, sqrl.env))

          # If there was a result (was a query), use it as the overall result.
          if (!base::is.null(dat$value))
          {
            result <- dat
          }

          # Reset the statement (begin the next one afresh).
          statement <- base::list()

        # Otherwise (this is an R substitution into SQL), clean the phrase (but
        # do not remove pre-tag trailing whitespace) and then append it to the
        # statement. The phrase will never contain a string literal.
        } else
        {
          # Remove trailing whitespace from each internal line of the phrase.
          phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

          # Remove vertical whitespace from within the phrase.
          phrase <- base::gsub("\n+", "\n", phrase)

          # Append the phrase to the statement (unless the phrase is empty).
          if (base::nchar(phrase) > 0L)
          {
            statement <- base::append(statement, phrase)
          }
        }
      }

      # Reposition the start-of-phrase index immediately after this tag.
      k <- pos[i] + len[i]

      # Isolate the R section. Because this involves parsing (to find the end of
      # the section), we need to work through this, even when cond.current is
      # FALSE.
      rscript <- base::list()
      i <- i + 1L
      while ((i <= num.delims)
              && (!(pat[i] %in% base::c("tag.r.end", "tag.do"))))
      {
        # Remove comments from R (including SQL line and block comments).
        # This is so we can use SQL comments within the R (looks better under
        # SQL syntax highlighting rules within your text editor), and is also
        # necessary to allow commenting-out of quote markers, <do>, <stop>,
        # and </R> tags with R comments (as well as SQL).
        if ((i <= num.delims)
            && (pat[i] %in%
                      base::c("comment.line", "comment.begin", "comment.r")))
        {
          # Isolate any unappended script preceding this comment, and append
          # it to the R-script.
          rscript <- base::append(rscript,
                                  base::substring(script, k, pos[i] - 1L))

          # Scan through the subsequent script delimiters, until the comment
          # concludes with either an end-of-file, or appropriate delimiter.
          end.marker <- base::switch(pat[i],
                                      comment.r = "end.of.line",
                                      comment.line = "end.of.line",
                                      comment.begin = "comment.end")
          i <- i + 1L
          while ((i <= num.delims)
                  && (pat[i] != end.marker))
          {
            i <- i + 1L
          }

          # Reposition the start-of-phrase index immediately after the end of
          # the comment. When the comment ends with a newline, the index is
          # placed on that newline (the next phrase will begin with newline).
          k <- base::ifelse(i <= num.delims,
                              base::ifelse(end.marker == "end.of.line",
                                            pos[i], pos[i] + len[i]),
                              nchar.script + 1L)
        }

        # Skip over any (single or double) quote-enclosed strings (include
        # them verbatim). (Ignore anything that looks like a delimiter, but
        # is inside a string.)
        if ((i <= num.delims)
            && (pat[i] %in% base::c("quote.single", "quote.double")))
        {
          # Since we're not cleaning-up the R script, we merely scan through
          # the script delimiters until reaching the end of the quote (ignore
          # all other delimiters found in between). Appending to the R script
          # will only occur later (at a comment, or an end-of-R delimiter). We
          # only test for \ escaped quotes here (once already within quote mode,
          # which also guarantees that i > 1).
          closing.quote <- pat[i]
          i <- i + 1L
          while ((i <= num.delims)
                  && ((pat[i] != closing.quote)
                      || ((base::attr(base::regexpr(
                        base::paste0("\\\\*", patterns[[closing.quote]], "$"),
                          base::substring(script, pos[i - 1L], pos[i])),
                          "match.length") %% 2L) == 0L)))
          {
            i <- i + 1L
          }
        }

        # Semicolons may delimit R statements, or mark the end of the R section.
        if ((i <= num.delims)
            && (pat[i] == "semi.colon"))
        {
          # If there is nothing but whitespace between the semicolon and the
          # start of its line (or the R tag, or the result tag, or the previous
          # semicolon), then interpret the semicolon as a do tag (the R parser
          # won't accept it, anyway), and stop looking for one. We need not
          # append the whitespace to the R script.
          if (base::grepl("^[[:space:]]*$",
                          base::substring(script, k, pos[i] - 1L)))
          {
            break

          # Otherwise, the semicolon marks the end of an R statement. Append
          # any unappended script, up to and including the semicolon, to the
          # R-script. Advance the start-of-phrase index past the semicolon, and
          # resume searching for the end of the R section.
          } else
          {
            rscript <- base::append(rscript, base::substring(script, k, pos[i]))
            k <- pos[i] + len[i]
          }
        }

        # Ignore remainder of script when encountering a 'stop' tag within an
        # R post-processing section.
        if ((i <= num.delims)
            && (pat[i] == "tag.stop"))
        {
          # Append the last chunk of R (before the stop tag).
          rscript <- base::append(rscript,
                                    base::substring(script, k, pos[i] - 1L))

          # Ignore (skip over) everything else in the script.
          i <- num.delims + 1L
          k <- nchar.script + 1L
          break
        }

        # We append each line, as we reach its end, to the R-script, only to
        # simplify distinguishing R-section from R-statement semicolons.
        if ((i <= num.delims)
            && (pat[i] == "end.of.line"))
        {
          # Append any preceding R (up to and including the line end).
          rscript <- base::append(rscript, base::substring(script, k, pos[i]))

          # Move the start of phrase index to the beginning of the next line.
          k <- pos[i] + len[i]
        }

        # Advance to the next script delimiter.
        i <- i + 1L
      }

      # Evaluate the R script, and process the result, unless within the block
      # of an untrue conditional expression.
      if (cond.current)
      {
        # Append the final chunk to the R-script.
        phrase <- base::ifelse(i <= num.delims,
                                base::substring(script, k, pos[i] - 1L),
                                base::substring(script, k))
        rscript <- base::append(rscript, phrase)

        # Collapse the rscript (list) to a single string.
        rscript <- base::trimws(base::paste(rscript, collapse = ""))

        # In the case of embedded R, evaluate and append to the encasing SQL.
        if ((r.type == "tag.r.begin")
            && (i <= num.delims)
            && (pat[i] == "tag.r.end"))
        {
          sqlisedvalue <- base::try(SqrlStatement(
                            base::eval(base::parse(text = rscript), sqrl.env)),
                            silent = TRUE)
          if (base::inherits(sqlisedvalue, "try-error"))
          {
            if (SqrlParam(datasource, "autoclose"))
            {
              SqrlClose(datasource)
            }
            base::stop(sqlisedvalue)
          }
          statement <- base::append(statement, sqlisedvalue)

        # Otherwise (R post-processing), evaluate and retain the result.
        } else
        {
          # Stop if there's any unsubmitted SQL before an <R> ... <do> section.
          # (SQL is always submitted before a <result> ... <do> section.)
          if (base::any(base::grepl("[[:graph:]]", base::unlist(statement))))
          {
            if (SqrlParam(datasource, "autoclose"))
            {
              SqrlClose(datasource)
            }
            base::stop("Unsubmitted SQL preceding an <R> ... <do> section.")
          }

          # If verbose, output the script (prior to evaluation).
          if (verbose)
          {
            base::cat("\n")
            base::cat(rscript)
            base::cat("\n")
          }

          # Evaluate the script, and retain the result, only if the script is
          # non-empty (in the sense of containing no uncommented statements).
          parsed <- base::try(base::parse(text = rscript), silent = TRUE)
          if (base::inherits(parsed, "try-error"))
          {
            if (SqrlParam(datasource, "autoclose"))
            {
              SqrlClose(datasource)
            }
            base::stop(parsed)
          }
          if (!base::identical(base::as.character(parsed), base::character(0L)))
          {
            # Evaluate the script, retain the result. As above, stop with an
            # error should such occur. Close first, if autoclose is set.
            result <- base::try(base::withVisible(base::eval(parsed, sqrl.env)),
                                silent = TRUE)
            if (base::inherits(result, "try-error"))
            {
              if (SqrlParam(datasource, "autoclose"))
              {
                SqrlClose(datasource)
              }
              base::stop(result)
            }

            # If verbose, output (some of) the result. This could be any object,
            # with no guarantee head(), print() or other methods are defined.
            if (verbose)
            {
              printed <- FALSE
              if ("package::utils" %in% base::search())
              {
                printed <- !base::inherits(base::try(
                                        base::print(utils::head(result$value)),
                                        silent = TRUE), "try-error")
              }
              if (!printed)
              {
                rdim <- base::try(base::dim(result$value), silent = TRUE)
                rdims <- base::try(base::length(rdim), silent = TRUE)
                rlen <- base::try(base::length(result$value), silent = TRUE)
                if (!base::inherits(rdims, "try-error")
                    && rdims == 2L)
                {
                  base::try(base::print(result$value[
                                    base::seq(base::min(rdim[1], 1L),
                                              base::min(rdim[1], 6L)), ]),
                            silent = TRUE)
                } else if (!base::inherits(rlen, "try-error")
                            && rlen > 0L)
                {
                  base::try(base::print(result$value[
                            base::seq(1L, base::min(rlen, 6L))]), silent = TRUE)
                } else
                {
                  base::try(base::print(result$value), silent = TRUE)
                }
              }
              base::cat("\n")
            }
          }
        }
      }

      # Reposition the start-of-phrase index immediately after this R section.
      k <- base::ifelse(i <= num.delims, pos[i] + len[i], nchar.script + 1L)

      # Advance to the next script delimiter.
      i <- i + 1L
    }


    # Take no special action at an end-of-line, an end-of-intra-tag-expression,
    # or an intra-SQL R comment marker; merely advance to the next delimiter.
    if ((i <= num.delims)
        && (pat[i] %in% base::c("end.of.line", "end.expression", "comment.r")))
    {
      i <- i + 1L
    }
  }

  # Submit any remaining statement, unless within the block of an untrue
  # conditional expression (which would have to be missing its end tag).
  if (cond.current)
  {
    # Isolate any remaining unappended (to the statement) script.
    phrase <- base::substring(script, k)

    # Submit the statement (with phrase) and pull the result.
    dat <- base::withVisible(SqrlSubScript(datasource, statement, phrase))

    # If there was a result (there was a query), replace the overall result.
    if (!base::is.null(dat$value))
    {
      result <- dat
    }
  }

  # If the last result was invisible, return it invisibly.
  if (!result$visible)
  {
    base::return(base::invisible(result$value))
  }

  # Otherwise, (visibly) return whatever the last result was.
  base::return(result$value)
}

SqrlHelp <- function(datasource = "",
                      type = "",
                      clean = FALSE)
{
  # Generates run-time help for SQRL interface functions.
  # Args:
  #   datasource : The name of a known data source.
  #   type       : The requested help format ('text' or 'html').
  #   clean      : If set to TRUE, any old temp files are removed.
  # Returns:
  #   Invisible NULL, after displaying help.
  # SQRL Calls:
  #   SqrlConfig(), SqrlHelp() (self), SqrlHelper(), SqrlPath(), SqrlToUser(),
  #   srqlHelp.
  # SQRL Callers:
  #   SqrlDelegate(), SqrlHelp() (self), .onLoad(), .onUnload().
  # tools Calls:
  #   Rd2HTML(), Rd2txt() (only if the tools package is installed).
  # utils Calls:
  #   browseURL(), help(), installed.packages() (only if utils is attached).
  # User:
  #   Has no direct access, but is able to submit (only) the 'type' argument.
  #   That is coerced to an allowed value, and no further checks are required.

  # If the clean argument was set, prune any old temp files and return the temp
  # files list (after creating an empty list if the list does not yet exist).
  if (clean)
  {
    if (!base::exists("temps", srqlHelp, inherits = FALSE))
    {
      base::return(base::assign("temps", base::character(0L), srqlHelp))
    }
    temps <- base::get("temps", srqlHelp, inherits = FALSE)
    temps <- temps[base::file.exists(temps)]
    temps <- temps[!base::suppressWarnings(base::file.remove(temps))]
    base::return(base::assign("temps", temps, srqlHelp))
  }

  # Unless a supported help type was supplied, use the default help type.
  # This may be NULL valued. Types are not case sensitive.
  type <- base::tolower(type)
  if (!base::identical(type, "text")
      && !base::identical(type, "html"))
  {
    type <- base::tolower(base::getOption("help_type"))
  }

  # If the utils package (which provides the help() function) is not loaded,
  # print a link to the CRAN SQRL page (which has a PDF help file), and return.
  if (!("package:utils" %in% base::search()))
  {
    base::return(base::cat(
            "https://cran.r-project.org/web/packages/SQRL/index.html\n"))
  }

  # If the tools package (which converts Rd files) is unavailable, display the
  # pre-built (static) interface-usage help page, and return.
  if (!("tools" %in% base::rownames(utils::installed.packages())))
  {
    base::return(utils::help("sqrlUsage", help_type = type))
  }

  # The tools package is available. We shall dynamically generate tailored help
  # for the invoking (database's) interface function. This involves temp files.
  # Remove any existing SQRL temp files (from a previous SqrlHelp() call).
  temps <- SqrlHelp(clean = TRUE)

  # Obtain the current source configuration (all parameter values).
  config <- SqrlConfig(datasource)

  # Extract the driver from the configuration, and escape any % characters
  # (these are the Rd comment symbol, even with \preformatted{} sections).
  if (base::grepl("[[:graph:]]", config[["driver"]]))
  {
    driver <- base::paste0("\\file{", SqrlHelper(config[["driver"]]), "}")
  } else
  {
    driver <- "undefined"
  }

  # Extract and escape the data source's (SQRL) name.
  if (base::identical(config[["name"]], config[["interface"]]))
  {
    dsrc <- "of the same name"
  } else
  {
    dsrc <- base::paste0("\\file{", SqrlHelper(config[["name"]]), "}")
  }

  # Construct example queries, appropriate to the source's driver.
  if (base::grepl("oracle|db2", driver, ignore.case = TRUE))
  {
    query1 <- "select 1 from dual"
    query2 <- "\"select \", sample(6, 1), \" from dual\""
  } else
  {
    query1 <- "select 1"
    query2 <- "\"select \", sample(6, 1)"
  }

  # Extract and escape the interface function's name.
  iface <- SqrlHelper(config[["interface"]])

  # Escape and list all of the parameter values.
  csc <- base::character(0L)
  config <- SqrlHelper(config)
  for (name in base::names(config))
  {
    csc <- base::c(csc, base::paste(name, "=", config[[name]]))
  }

  # Construct the help text, in Rd (R man file) format. We don't gsub() on tags
  # in case the one of the parameter values happens to contain that sequence.
  helprd <- base::c(
    base::paste0("\\name{", iface, "}"),
    base::paste0("\\title{Interface Function \\sQuote{", iface, "}}"),
    "\\description{",
    base::paste0("The function \\code{", iface, "} is"),
    base::paste0("the interface to the data source ", dsrc,"."),
    base::paste0("The source's \\acronym{ODBC} driver is ", driver, "."),
    "}",
    "\\section{Listing Sources}{\\preformatted{",
    "# View the associated source definition.",
    base::paste0(iface, "(\"source\")"),
    "",
    "# See all data sources and their interfaces.",
    base::paste0(iface, "(\"sources\")"),
    "}}",
    "\\section{Opening and Closing}{\\preformatted{",
    "# Open a connection to the data source.",
    base::paste0(iface, "()"),
    "",
    "# Check if the connection is open.",
    base::paste0(iface, "(\"isopen\")"),
    "",
    "# Close the connection.",
    base::paste0(iface, "(\"close\")"),
    "",
    "# Close the connection when not in use.",
    base::paste0(iface, "(autoclose = TRUE)"),
    "}}",
    "\\section{Submitting Queries}{\\preformatted{",
    "# Submit a query.",
    base::paste0(iface, "(\"", query1, "\")"),
    "",
    "# Submit a compound query.",
    base::paste0(iface, "(", query2, ")"),
    "",
    "# Submit a query from file.",
    base::paste0(iface, "(\"my/file.sql\")"),
    "",
    "# Submit a parameterised query from file.",
    base::paste0(iface,
                  "(\"rhaphidophoridae.sqrl\", genus = \"gymnoplectron\")"),
    "",
    "# Force submission as a query.",
    base::paste0(iface, "(query = \"help\")"),
    "}}",
    "\\section{Communication Parameters}{\\preformatted{",
    "# Get a named parameter value.",
    base::paste0(iface, "(\"uid\")"),
    "",
    "# Set a named parameter value.",
    base::paste0(iface, "(visible = TRUE)"),
    "",
    "# Reset a parameter to its default value.",
    base::paste0(iface, "(reset = \"nullstring\")"),
    "",
    "# List all parameter values.",
    base::paste0(iface, "(\"config\")"),
    "",
    "# Set multiple parameter values from file.",
    base::paste0(iface, "(config = \"path/to/config/file\")"),
    "}}",
    "\\section{Further Assistance}{\\preformatted{",
    "# Additional usage examples.",
    "?sqrlUsage",
    "",
    "# Detailed parameter descriptions.",
    "?sqrlParams",
    "}}",
    "\\section{Current Settings}{\\preformatted{",
    csc,
    "}}")

  # Write the (Rd format) text to a temp file.
  rdfile <- base::tempfile(fileext = ".Rd")
  temps <- base::assign("temps", base::c(temps, rdfile), srqlHelp)
  base::writeLines(helprd, rdfile)

  # Detect and handle RStudio, which does things a bit differently (different
  # viewer, different style, different file location rules). For RStudio, the
  # 'type' argument is ignored (help is only provided in HTML format).
  if (base::nzchar(base::Sys.getenv("RSTUDIO_USER_IDENTITY")))
  {
    # Rstudio's viewer seems to want a style file in the same directory as the
    # help (HTML) file (and only wants the file name of that CSS file, not its
    # full path). That compels us to copy a style file to this temp file.
    csstemp <- base::tempfile(fileext = ".css")
    temps <- base::assign("temps", base::c(temps, csstemp), srqlHelp)

    # Set a default cascading style sheet. This won't exist within the temp
    # directory, in which case the viewer will apply default styling when it
    # does not find the CSS file (no harm done, but not aesthetically ideal).
    cssfile <- "R.css"

    # These locations are where we think the RStudio and R style files will be.
    # The two styles are different, so the RStudio file is preferred.
    cssfiles <- base::c(base::file.path(base::Sys.getenv("RSTUDIO_PANDOC"),
                                        "../../resources/R.css"),
                        base::file.path(base::R.home(),
                                        "library/base/html/R.css"))

    # If we find, and can copy, the RStudio file, use that. Otherwise, if we
    # find, and can copy, the base R file, use that. If we find neither, the
    # default style will apply.
    for (css in cssfiles)
    {
      if (!base::is.null(SqrlPath(css))
          && base::file.copy(css, csstemp))
      {
        cssfile <- csstemp
        break
      }
    }

    # Convert the Rd to HTML, write that to another temp file, open that in the
    # RStudio viewer, and return invisible NULL. Note the use of basename().
    htmlfile <- base::tempfile(fileext = ".html")
    base::assign("temps", base::c(temps, htmlfile), srqlHelp)
    if (base::inherits(base::try(tools::Rd2HTML(rdfile, htmlfile,
                        package = "SQRL", stylesheet = base::basename(cssfile)),
                        silent = TRUE), "try-error"))
    {
      base::return(utils::help("sqrlUsage"))
    }
    base::getOption("viewer")(htmlfile)
    base::return(base::invisible(NULL))
  }

  # If the help type is 'html', convert the Rd to HTML, write that to another
  # temp file, open that file in the default browser, and return NULL.
  if (base::identical(type, "html"))
  {
    htmlfile <- base::tempfile(fileext = ".html")
    base::assign("temps", base::c(temps, htmlfile), srqlHelp)
    cssfile <- base::paste0(base::R.home(), "/library/base/html/R.css")
    if (base::inherits(base::try(tools::Rd2HTML(rdfile, htmlfile,
        package = "SQRL", stylesheet = cssfile), silent = TRUE), "try-error"))
    {
      base::return(utils::help("sqrlUsage", help_type = "html"))
    }
    utils::browseURL(htmlfile)
    base::return(base::invisible(NULL))
  }

  # Otherwise, convert the Rd to text, write that to another temp file, open
  # that file in the default text viewer (pager), and return invisible NULL.
  txtfile <- base::tempfile(fileext = ".txt")
  base::assign("temps", base::c(temps, txtfile), srqlHelp)
  if (base::inherits(base::try(tools::Rd2txt(rdfile, txtfile, package = "SQRL"),
                                silent = TRUE), "try-error"))
  {
    base::return(utils::help("sqrlUsage", help_type = "text"))
  }
  base::file.show(txtfile)
  base::return(base::invisible(NULL))
}

SqrlHelper <- function(value = "")
{
  # Formats parameter values for inclusion in a (.Rd-format) help file.
  # Args:
  #   value : Either a list (source configuration) or a single character string.
  # Returns:
  #   The input values, converted to strings and with %s escaped.
  # SQRL Calls:
  #   None.
  # SQRL Callers:
  #   SqrlHelp().
  # User:
  #   Has no direct access, but is able to control the supplied value through
  #   parameter settings (interface name, ping query, and so on). These are
  #   converted to strings and escaped. No further checking is required.

  # Deparse and escape the entire configuration parameter-value list.
  if (base::class(value) == base::class(base::list()))
  {
    if (!base::is.null(value[["channel"]]))
    {
      value[["channel"]] <- base::as.numeric(value[["channel"]])
    }
    for (name in base::names(value))
    {
      value[[name]] <- base::gsub("%", "\\\\%", base::deparse(value[[name]]))
    }
    base::return(value)
  }

  # Escape a single character-string.
  base::return(base::gsub("%", "\\\\%", value))
}

SqrlIndicator <- function(datasource = "",
                          action = "",
                          marker = "all")
{
  # Alters the display-state of open-connection (channel) indicators.
  # Args:
  #   datasource : The name of a known (to SQRL) data source.
  #   action     : One of 'show', 'hide', 'busy', or 'done'.
  #   marker     : One of 'prompt', 'wintitle', or 'all' (the default).
  # Returns:
  #   Invisible NULL, after making the requested indicator changes.
  # SQRL Calls:
  #   SqrlParam().
  # utils Calls:
  #   getWindowTitle(), setWindowTitle() (only if the utils package is attached,
  #   and these two functions exist within it on the current OS/platform).
  # SQRL Callers:
  #   SqrlParam(), SqrlSubmit().
  # User:
  #   Has no direct access, and is unable to indirectly supply any of the
  #   arguments. Argument validity checks are not required.

  # TRUE if the indicators are potentially visible (when the data source's
  # channel is open). No test of openness is made here; that should be performed
  # (where necessary) before calling this function.
  visible <- (base::interactive()
              && SqrlParam(datasource, "visible"))

  # TRUE if, and only if, the prompt is to be altered.
  do.prompt <- (visible
                && (marker %in% base::c("all", "prompt")))

  # TRUE if, and only if, the window title is to be altered.
  # The get/setWindowTitle() functions only exist on Windows versions of R,
  # and only work with Rgui, R Console, and Rterm (not with RStudio).
  # We test ("package:utils" %in% base::search()), rather than
  # requireNamespace("utils", quietly = TRUE), because, if utils is attached,
  # we then need to look inside it to see whether or not the get & set functions
  # exist. This doesn't work without attachment (having the namespace available
  # does not suffice). We could promote our utils reliance from suggests to
  # depends, in the package description file, but would rather not have this
  # strict requirement (this indicator feature is nice to have, but not
  # absolutely necessary). Utils is normally attached on start-up, anyhow.
  do.title <- (visible
                && (marker %in% base::c("all", "wintitle"))
                && ("package:utils" %in% base::search())
                && base::exists("getWindowTitle", where = "package:utils",
                                  mode = "function", inherits = FALSE)
                && base::exists("setWindowTitle", where = "package:utils",
                                  mode = "function", inherits = FALSE))

  # When the action is 'show', apply (append and/or prepend) the indicator(s).
  if (action == "show")
  {
    # Append window title-bar open-channel indicator.
    if (do.title)
    {
      indic <- SqrlParam(datasource, "wintitle")
      if (base::grepl("[[:graph:]]", indic))
      {
        utils::setWindowTitle(title = base::sub("\\s+$", "",
          base::paste(base::sub("\\s+$", "", utils::getWindowTitle()), indic)))
      }
    }
    # Prepend command-prompt open-channel indicator.
    if (do.prompt)
    {
      indic <- SqrlParam(datasource, "prompt")
      base::options(prompt = base::paste0(indic, base::getOption("prompt")))
    }
    # Return invisible NULL.
    base::return(base::invisible(NULL))
  }

  # When the action is 'hide', remove the indicator(s). This will work (as in,
  # does nothing, quietly) if the indicators aren't actually on to begin with.
  # Where this can go wrong, is when the open indicators are defined as, say,
  # 'A', and 'AB'. Removal of 'A" from 'ABA' might leave 'BA'. So don't do that.
  if (action == "hide")
  {
    # Remove one open-channel indicator from the window title.
    if (do.title)
    {
      indic <- SqrlParam(datasource, "wintitle")
      if (base::grepl("[[:graph:]]", indic))
      {
        windowtitle <- utils::getWindowTitle()
        if (base::grepl(indic, windowtitle, fixed = TRUE))
        {
          position <- base::max(
                        base::gregexpr(indic, windowtitle, fixed = TRUE)[[1L]])
          before <- base::sub("\\s+$", "",
                                base::substring(windowtitle, 1L, position - 1L))
          after <- base::substring(windowtitle, position + base::nchar(indic))
          utils::setWindowTitle(title =
                            base::sub("\\s+$", "", base::paste0(before, after)))
        }
      }
    }
    # Remove one open-channel indicator from the R prompt.
    if (do.prompt)
    {
      indic <- SqrlParam(datasource, "prompt")
      if (base::nchar(indic) > 0L)
      {
        base::options(prompt =
                  base::sub(indic, "", base::getOption("prompt"), fixed = TRUE))
      }
    }
    # Return invisible NULL.
    base::return(base::invisible(NULL))
  }

  # When the action is 'query' or 'fetch', append a job-in-progress marker ('*'
  # or '+', respectively) to the data source's window title indicator, then
  # return invisible NULL. This will work (as in, does nothing, quietly) if the
  # indicator isn't actually on.
  if (action %in% base::c("query", "fetch"))
  {
    glyph <- base::switch(action, "query" = "*", "fetch" = "+")
    if (do.title)
    {
      indic <- SqrlParam(datasource, "wintitle")
      if (base::grepl("[[:graph:]]", indic))
      {
        utils::setWindowTitle(title = base::sub("\\s+$", "",
                                base::sub(indic, base::paste0(indic, glyph),
                                      utils::getWindowTitle(), fixed = TRUE)))
      }
    }
    base::return(base::invisible(NULL))
  }

  # When the action is 'done', remove a job-in-progress marker ('*' or '+') from
  # the data source's window title indicator, then return invisible NULL. This
  # will work (as in, does nothing, quietly) if no marker is actually present.
  if (action == "done")
  {
    if (do.title)
    {
      indic <- SqrlParam(datasource, "wintitle")
      if (base::grepl("[[:graph:]]", indic))
      {
        for (glyph in base::c("*", "+"))
        {
          glyphed <- base::paste0(indic, glyph)
          windowtitle <- utils::getWindowTitle()
          if (base::grepl(glyphed, windowtitle, fixed = TRUE))
          {
            utils::setWindowTitle(title = base::sub("\\s+$", "",
                        base::sub(glyphed, indic, windowtitle, fixed = TRUE)))
            break
          }
        }
      }
    }
    base::return(base::invisible(NULL))
  }

  # This should be unreachable, but if we were to arrive here, return NULL.
  base::return(base::invisible(NULL))
}

SqrlInterface <- function(datasource = "",
                          interface = "",
                          vital = TRUE)
{
  # Constructs a user-interface to a specified data source.
  # Args:
  #   datasource : The name of a known data source.
  #   interface  : The name to use for that data source's interface.
  #   vital      : When set to FALSE, name conflicts are non-fatal.
  # Returns:
  #   A function (named <interface>) for interacting with the data source.
  #   Any pre-existing interface to that data source will be deleted.
  #   If interface is not specified, the interface name defaults to the data
  #   source name (sans whitespace). When interface == "remove", no new
  #   interface is created, but any existing interface will be deleted. (There
  #   is no loss of generality, since "remove" is prohibited as an interface
  #   name due to its conflicting with the base::remove() function.)
  # SQRL Calls:
  #   SqrlFace(), SqrlInterface() (self), SqrlParam().
  # SQRL Callers:
  #    SqrlCache(), SqrlConfig(), SqrlDelegate(), SqrlDSNs(), SqrlInterface()
  #    (self), SqrlOff(), SqrlParam(), SqrlSource(), sqrlInterface().
  # User:
  #   Has no direct access, but is able to indirectly supply the datasource
  #   argument via sqrlInterface(), and through SqrlSources() by editing the
  #   registered data source names (DSNs) prior to loading SQRL. The user can
  #   indirectly supply the interface argument via sqrlInterface(),
  #   SqrlDelegate(), and through SqrlSources() by editing the DSNs prior to
  #   loading SQRL. The user cannot indirectly supply the vital argument. In
  #   all cases, existence of the datasource is established before calling this
  #   function. The interface value could be anything, and is checked here.

  # This is the user-interface function-body definition for the data source.
  uibody <- base::paste0("function(...) {base::return(SqrlDelegate(\"",
                        datasource, "\", ..., envir = base::parent.frame()))}")

  # Abort on invalid interface (name). Allowed values are NULL or a character
  # string. The requested name may, or may not, be available and assignable.
  if (!base::is.null(interface)
      && ((base::class(interface) != base::class(base::character()))
          || (base::length(interface) != 1L)
          || !base::nzchar(base::trimws(interface))))
  {
    if (!vital)
    {
      base::return(base::invisible(NULL))
    }
    base::stop("Invalid interface name.")
  }

  # Remove leading and trailing whitespace from the interface argument.
  # Applying trimws() to NULL would produce character(0).
  if (!base::is.null(interface))
  {
    interface <- base::trimws(interface)
  }

  # Isolate the previous interface (NULL when no interface was defined).
  preface <- SqrlParam(datasource, "interface")

  # On a request to delete the data source's interface, if we can confirm the
  # interface object retains its original SQRL definition, then we delete that
  # object. Either way, the interface is deregistered in the data source's
  # cache, and an invisible NULL is returned.
  if (base::is.null(interface)
      || base::identical(interface, "remove"))
  {
    if (!base::is.null(preface))
    {
      if (SqrlFace(preface, exists = TRUE))
      {
        fun <- base::paste(base::deparse(SqrlFace(preface)), collapse = "")
        if (base::gsub("[[:space:]]+", "", fun)
            == base::gsub("[[:space:]]+", "", uibody))
        {
          SqrlFace(preface, delete = TRUE)
        }
      }
      SqrlParam(datasource, "interface", NULL)
    }
    base::return(base::invisible(NULL))
  }

  # Check that the preface actually is a SQRL interface, and set NULL otherwise.
  # To be an interface, it must be registered within the source parameter cache,
  # exist as a function, and have the precise uibody definition (above).
  if (!base::is.null(preface)
      && (SqrlFace(preface, exists = FALSE)
          || (base::gsub("[[:space:]]+", "",
                base::paste(base::deparse(SqrlFace(preface)), collapse = ""))
              != base::gsub("[[:space:]]+", "", uibody))))
  {
    preface <- NULL
    SqrlParam(datasource, "interface", NULL)
  }

  # If no interface was specified, use the data source name (sans whitespace).
  if (base::nchar(interface) < 1L)
  {
    interface <- base::gsub("[[:space:]]+", "", datasource)
  }

  # If the interface already exists (under the same name), return it (silently).
  # The above chack on preface guarantees existence within envir when not NULL.
  if (!base::is.null(preface)
      && (preface == interface))
  {
    base::return(base::invisible(interface))
  }

  # Ensure the interface name is assignable. Non-assignability is usually fatal,
  # but when vital == FALSE the function exists normally (SqrlSources() uses
  # this when auto-generating functions, since 'A<<B' is a valid DSN name).
  if (interface != base::make.names(interface))
  {
    if (!vital)
    {
      base::return(base::invisible(NULL))
    }
    base::stop("Unassignable interface name.")
  }

  # Abort if some other object already exists under the chosen name.
  # Usually, these conflicts are fatal, but when vital == FALSE the function
  # exits normally (SqrlSources() uses this when auto-generating interfaces).
  if (SqrlFace(interface, clashes = TRUE))
  {
    if (!vital)
    {
      base::return(base::invisible(NULL))
    }
    base::stop("Interface name conflict.")
  }

  # If the data source already has an interface (under some other name), then
  # delete that existing interface (before continuing).
  if (!base::is.null(preface)
      && (preface != interface))
  {
    SqrlInterface(datasource, "remove")
  }

  # Assign the interface function to the chosen name. Note that changing the
  # interface (name) does not change the wintitle or prompt strings. Those are
  # both based upon the (invariant) data source name.
  SqrlFace(interface, uibody)

  # Register that assignment within the data source's cache. Again, this does
  # not alter the (data source name based) wintitle or prompt strings.
  SqrlParam(datasource, "interface", interface)

  # Return the name of the new user-interface function (invisibly).
  base::return(base::invisible(interface))
}

SqrlIsOpen <- function(datasource = "",
                        besure = FALSE)
{
  # Tests whether or not an open ODBC channel exists to the data source.
  # Args:
  #   datasource : The name of a data source.
  #   besure     : Check thoroughly (ping the source) when this is set to TRUE.
  # Returns:
  #   TRUE if the data source exists, and SQRL has an open channel to it.
  #   FALSE, otherwise.
  # SQRL Calls:
  #   SqrlClose(), SqrlParam(), SqrlSubmit().
  # RODBC Calls:
  #   odbcGetInfo().
  # SQRL Callers:
  #   SqrlDelegate(), SqrlOpen(), SqrlParam(), SqrlSources(), SqrlSubmit().
  # User:
  #   Has no direct access, and is unable to indirectly supply either argument.
  #   Argument validity checks are not required.

  # Attempt to obtain the channel parameter value for the specified data source.
  channel <- base::try(SqrlParam(datasource, "channel"), silent = TRUE)

  # Return FALSE when the datasource is invalid (does not exist => is not open).
  if (base::inherits(channel, "try-error"))
  {
    base::return(FALSE)
  }

  # Return FALSE when the channel is closed (and we knew that).
  if (base::is.null(channel))
  {
    base::return(FALSE)
  }

  # Return FALSE when the channel is not an RODBC handle (in which case we may
  # have mistakenly thought the channel was open, since it was non-null valued).
  if (base::class(channel) != "RODBC")
  {
    SqrlClose(datasource)
    base::return(FALSE)
  }

  # Attempt to obtain channel information. This will fail if the channel has
  # been closed from our end, or is not of RODBC class (repeating, in effect,
  # the test above), but will succeed if the channel has been closed from the
  # other end (and we were previously unaware of that).
  channel.info <- base::try(RODBC::odbcGetInfo(channel), silent = TRUE)

  # Return FALSE when the channel is closed (but we thought it was open).
  if (base::inherits(channel.info, "try-error"))
  {
    SqrlClose(datasource)
    base::return(FALSE)
  }

  # When besure is FALSE (the default), we only check openness from our end.
  # That being the case, return TRUE if we get to this point (the channel
  # appears to be open from our end) and we're not going to be more thorough.
  if (!base::identical(besure, TRUE))
  {
    base::return(TRUE)
  }

  # If we are being thorough (besure == TRUE) and have a ping, attempt to submit
  # that ping to the data source and see whether or not that produces an error.
  # It was confirmed, by SqrlPing() (at SqrlOpen() time), that the ping is valid
  # SQL for the data source (and does not produce an error when the channel is
  # open). If the user has since changed the ping, it is their responsibility to
  # ensure validity (otherwise, this test may close a channel that was open).
  ping <- SqrlParam(datasource, "ping")
  if (!base::is.null(ping)
      && (base::nchar(ping) > 0L))
  {
    # Submit the ping.
    response <- base::try(SqrlSubmit(datasource, ping, throw = TRUE,
                                      retry = FALSE, isping = TRUE),
                          silent = TRUE)

    # If the ping failed, the channel is closed (but we thought it was open).
    if (base::inherits(response, "try-error"))
    {
      SqrlClose(datasource)
      base::return(FALSE)
    }

    # Otherwise, the ping succeeded; the channel is open (like we thought).
    base::return(TRUE)
  }

  # When we want to be thorough, but don't have a ping, we submit a junk query
  # to deliberately produce an error, and scan that for anything that looks like
  # a socket error and/or a remotely closed channel. If we find something, we
  # conclude the channel is _probably_ closed (this is not utterly reliable).
  error <- base::try(SqrlSubmit(datasource, "1", throw = TRUE, retry = FALSE),
                      silent = TRUE)
  error <- base::tolower(base::attr(error, "condition")$message)
  for (word in base::c("sock", "libc", "connection", "reset", "open", "closed"))
  {
    if (base::grepl(word, error, fixed = TRUE))
    {
      SqrlClose(datasource)
      base::return(FALSE)
    }
  }

  # If it looks like some other kind of error, we interpret that as being
  # (_probably_) remotely generated (i.e., issued by the data source), and
  # conclude the channel is (_probably_) open (like we thought it was). If the
  # dummy query ('1') is actually valid SQL for the source, it will not have
  # thrown an error, in which case attr(error, "condition")$message returns
  # NULL, tolower() converts that to character(0), and we still end up here.
  base::return(TRUE)
}

SqrlOff <- function()
{
  # Close SQRL channels, deactivate SQRL.
  # Args:
  #   None.
  # Returns:
  #   Invisible NULL, after closing channels and detaching SQRL.
  # SQRL Calls:
  #   SqrlCache(), SqrlClose(), SqrlInterface().
  # RODBC Calls:
  #   odbcCloseAll().
  # SQRL Callers:
  #   sqrlOff().
  # User:
  #   User has no direct access, and there are no arguments.

  # SQRL data sources correspond to child environments of srqlHaus. For each
  # of these, close any open channel, remove any interface, and delete any data
  # within the source cache (may contain passwords and so on). The garbage
  # collector ought to take care of the cached data after we detach srqlHaus,
  # but it's best to be immediate and sure. Operations are wrapped in silent
  # try(), because we don't want one hiccup to block any other activity.
  for (datasource in SqrlCache("*"))
  {
    base::suppressWarnings(base::try(SqrlClose(datasource), silent = TRUE))
    base::suppressWarnings(
            base::try(SqrlInterface(datasource, "remove"), silent = TRUE))
    cache <- SqrlCache(datasource)
    base::suppressWarnings(base::try(base::remove(
                            list = base::objects(pos = cache, all.names = TRUE),
                            pos = cache), silent = TRUE))
  }

  # Detach the public SQRL:Face (interfaces) environment. The garbage collector
  # should handle the rest. Again, wrapped in try() so that a failure here won't
  # have knock-on effects.
  base::try(base::detach("SQRL:Face"))

  # Detach and unload the SQRL package. The .onUnload() function attempts to
  # detach SQRL:Face once again (but this doesn't matter).
  base::try(base::detach("package:SQRL", unload = TRUE))

  # Return invisible NULL.
  base::return(base::invisible(NULL))
}

SqrlOpen <- function(datasource = "")
{
  # Opens a channel to a data source.
  # Args:
  #   datasource : The name of a data source.
  # Returns:
  #   Invisible NULL, after creating and caching the data source channel.
  #   Will throw a fatal exception should the connection attempt fail.
  # SQRL Calls:
  #   SqrlIsOpen(), SqrlParam(), SqrlParams(), SqrlPing().
  # RODBC Calls:
  #   odbcConnect(), odbcDriverConnect()
  # SQRL Callers:
  #   SqrlDelegate(), SqrlSubmit().
  # User:
  #   Has no direct access. Is unable to supply the only argument.
  #   Argument validity checks are not required.

  # If an open channel already exists, do not attempt to open another.
  if (SqrlIsOpen(datasource, besure = TRUE))
  {
    base::return(base::invisible(NULL))
  }

  # RODBC will prompt the user (via dialog box) for missing information (uid,
  # pwd, etc.) only in Rgui. In Rterm, RStudio, etc., pwd must be contained in
  # the DSN or connection string, or the pwd parameter must be set, prior to
  # attempting to connect. Otherwise, a (connection failure) error will result.

  # If a connection string has been defined for this source, connect using that.
  connection <- base::as.character(SqrlParam(datasource, "connection"))
  if (base::nchar(connection) > 0L)
  {
    for (param in SqrlParams("substitutable"))
    {
      connection <- base::gsub(base::paste0("<", param, ">"),
                                SqrlParam(datasource, param),
                                connection, fixed = TRUE)
    }
    channel <- base::try(
                RODBC::odbcDriverConnect(
                  connection = connection,
                  case = SqrlParam(datasource, "case"),
                  believeNRows = SqrlParam(datasource, "believeNRows"),
                  colQuote = SqrlParam(datasource, "colQuote"),
                  tabQuote = SqrlParam(datasource, "tabQuote"),
                  interpretDot = SqrlParam(datasource, "interpretDot"),
                  DBMSencoding = SqrlParam(datasource, "DBMSencoding"),
                  rows_at_time = SqrlParam(datasource, "rows_at_time"),
                  readOnlyOptimize = SqrlParam(datasource, "readOnlyOptimize")))

  # Otherwise (no string), connect using the registered data source name (DSN).
  } else
  {
    # If a user-ID and/or password has been defined, use the defined value (this
    # overrides any corresponding value that may be defined within the DSN).
    # Otherwise, use '' (rather than the default values), which causes RODBC to
    # go with any values in the DSN (and to ask for missing values in Rgui).
    # We can't just send the default values, because these will override any
    # corresponding values on the DSN (which is unlikely to be the preferred
    # behaviour). We do still want a non-empty default user-id, since this is
    # useful when connecting via a string incorporating the <uid> placeholder.
    uid <- ""
    pwd <- ""
    if (SqrlParam(datasource, "uid", isdefined = TRUE))
    {
      uid <- SqrlParam(datasource, "uid")
    }
    if (SqrlParam(datasource, "pwd", isdefined = TRUE))
    {
      pwd <- SqrlParam(datasource, "pwd")
    }
    channel <- base::try(
                RODBC::odbcConnect(
                  dsn = SqrlParam(datasource, "dsn"),
                  uid = uid,
                  pwd = pwd,
                  case = SqrlParam(datasource, "case"),
                  believeNRows = SqrlParam(datasource, "believeNRows"),
                  colQuote = SqrlParam(datasource, "colQuote"),
                  tabQuote = SqrlParam(datasource, "tabQuote"),
                  interpretDot = SqrlParam(datasource, "interpretDot"),
                  DBMSencoding = SqrlParam(datasource, "DBMSencoding"),
                  rows_at_time = SqrlParam(datasource, "rows_at_time"),
                  readOnlyOptimize = SqrlParam(datasource, "readOnlyOptimize")))
  }

  # Halt and notify on failure to connect. Might just be an incorrect password,
  # but could also be a network or server outage, etc. Fatal error, regardless.
  if (base::inherits(channel, "try-error")
      || (base::class(channel) != "RODBC"))
  {
    base::stop("Connection attempt failed.")
  }

  # Looks like a valid connection channel was established. Record handle.
  SqrlParam(datasource, "channel", channel)

  # Double-check. If the connection attempt was unsuccessful, halt and notify.
  if (!SqrlIsOpen(datasource))
  {
    base::stop("Connection attempt failed.")
  }

  # Scrape uid, dsn, and driver from the channel's connection attribute (in case
  # the user should have entered something new). Mis-scraping will not kill the
  # open channel, but it will produce an incorrect view in SqrlConfig(), and
  # will prevent network drop-out recovery in SqrlSubmit(). We blank the uid
  # parameter first, because if it does not appear in the channel's connection
  # string, then it could be anything (when contained within a DSN, perhaps).
  SqrlParam(datasource, "uid", "", override = TRUE)
  cstring <- base::attr(channel, "connection.string")
  cstrings <- base::unlist(base::strsplit(cstring, ';'))
  for (param in SqrlParams("scrapeable-channel"))
  {
    pattern <- base::paste0("^", param, "=")
    matches <- base::grepl(pattern, cstrings, ignore.case = TRUE)
    if (base::any(matches))
    {
      index <- base::which(matches)[1L]
      value <- base::trimws(
                    base::sub(pattern, "", cstrings[index], ignore.case = TRUE))
      SqrlParam(datasource, param,
                base::trimws(base::gsub("^\\{|\\}$", "", value)),
                override = TRUE)
    }
  }

  # If no ping has been defined for this data source, attempt to find (set) one.
  if (base::is.null(SqrlParam(datasource, "ping")))
  {
    SqrlPing(datasource)
  }

  # Return invisible NULL.
  base::return(base::invisible(NULL))
}

SqrlParam <- function(datasource = "",
                      parameter = "",
                      set,
                      override = FALSE,
                      isdefined = NULL)
{
  # Gets and sets named SQRL/RODBC control parameters for a data source.
  # Args:
  #   datasource : The name of a known (to SQRL) data source.
  #   parameter  : The name of a SQRL or RODBC control parameter.
  #   set        : The value to assign to that parameter (optional).
  #   override   : If set to TRUE, open status does not block value changes.
  #   isdefined  : If set to TRUE, return whether or not a value is defined.
  # Returns:
  #   The value of the named parameter for the named data source. If the set
  #   argument is specified, then the new value is returned (invisibly) after
  #   its assignment to the parameter (new passwords are not returned).
  # SQRL Calls:
  #   SqrlCache(), SqrlDefault(), SqrlIndicator(), SqrlInterface(),
  #   SqrlIsOpen(), SqrlParam() (self), SqrlParams().
  # RODBC Calls:
  #   odbcDataSources().
  # SQRL Callers:
  #   SqrlCache(), SqrlClose(), SqrlDefault(), SqrlDelegate(), SqrlDSNs(),
  #   SqrlFile(), SqrlIndicator(), SqrlInterface(), SqrlIsOpen(), SqrlOpen(),
  #   SqrlParam() (self), SqrlPing(), SqrlSource(), SqrlSubmit(),
  #   SqrlSubScript(), SqrlValue(), sqrlInterface().
  # User:
  #   Has no direct access, but is able to supply (only) parameter and set via
  #   SqrlDelegate() and/or SqrlConfig(), by way of SqrlValue(). SqrlDelegate()
  #   vets parameter while the SqrlConfig() does not (although it will restrict
  #   parameter to being a string, and is write-only). Neither vets set, and
  #   that must be performed here. (SqrlValue() merely passes-through.)

  # Obtain a handle to the data source's SQRL cache.
  cacheenvir <- SqrlCache(datasource)

  # When the defined flag is either TRUE or FALSE, return only whether or not a
  # (default-overriding) value has been set (exists) for the parameter.
  if (!base::is.null(isdefined))
  {
    base::return(base::exists(parameter, cacheenvir, inherits = FALSE)
                  == isdefined)
  }

  # When the parameter is 'reset', the set argument should be a vector of
  # parameter names for which the default values are to be restored.
  if (base::identical(parameter, "reset"))
  {
    # Filter the supplied parameter names against the official list.
    params <- set[set %in% SqrlParam(datasource, "*")]

    # If we are left with no parameters to reset, return invisible NULL.
    if (base::length(params) < 1L)
    {
      base::return(base::invisible(NULL))
    }

    # Abort if any of the supplied parameters are write-protected ('name')
    # or read-only ('channel').
    if (base::any(params %in% SqrlParams("write-protected"))
        || base::any(params %in% SqrlParams("read-only")))
    {
      base::stop("Cannot reset protected parameter.")
    }

    # If the connection is open, we cannot reset any locked-while-open
    # parameters (abort if such a request has been made), and we must also
    # change any visible indicators (if those parameters are to be reset).
    if (SqrlIsOpen(datasource))
    {
      if (!override
          && base::any(params %in% SqrlParams('locked-while-open')))
      {
        base::stop("Cannot reset parameter while connection is open.")
      }
      if ("visible" %in% params)
      {
        SqrlParam(datasource, "visible",
                  SqrlDefault(datasource, "visible"), override)
      }
      if (SqrlParam(datasource, "visible"))
      {
        if ("prompt" %in% params)
        {
          SqrlParam(datasource, "prompt",
                    SqrlDefault(datasource, "prompt"), override)
        }
        if ("wintitle" %in% params)
        {
          SqrlParam(datasource, "wintitle",
                    SqrlDefault(datasource, "wintitle"), override)
        }
      }
    }

    # Interface removal is a special case, handled by SqrlInterface().
    # Failure to re-apply the original (default) interface is non-fatal.
    if ("interface" %in% params)
    {
      SqrlInterface(datasource, "remove")
      SqrlInterface(datasource, datasource, vital = FALSE)
      params <- params[params != "interface"]
      if (base::length(params) < 1L)
      {
        base::return(base::invisible(NULL))
      }
    }

    # Remove the parameter-value definitions (restores default values), then
    # return invisible NULL.
    base::remove(list = params, pos = cacheenvir)
    base::return(base::invisible(NULL))
  }

  # When the set argument is supplied, act as a setter (cache and return).
  if (!base::missing(set))
  {
    # Nullable-string parameters are string-types which accept a set value of
    # NULL as an alias for the empty string.
    if ((parameter %in% SqrlParams("nullable-string"))
        && base::is.null(set))
    {
      set <- ""
    }

    # Coerce set to the appropriate data type for the specified parameter.
    # Firstly, the channel parameter can be either NULL, or of RODBC class.
    # This can be set frequently, when the autoclose parameter value is TRUE.
    if (parameter %in% SqrlParams("rodbc/null-type"))
    {
      if (!base::is.null(set)
          && (base::class(set) != "RODBC"))
      {
        base::stop("New parameter value is not a connection handle.")
      }

    # Parameters that are (non-NA) character-strings. (These include all the
    # scrapeable-channel parameters that may be set with each new channel.)
    } else if (parameter %in% SqrlParams("string-type"))
    {
      set <- base::suppressWarnings(base::as.character(set))
      if ((base::length(set) != 1L)
          || base::is.na(set))
      {
        base::stop("New parameter value is not a character string.")
      }

    # Parameters that are logically-valued.
    } else if (parameter %in% SqrlParams("boolean-type"))
    {
      set <- base::suppressWarnings(base::as.logical(set))
      if (!base::identical(set, TRUE)
          && !base::identical(set, FALSE))
      {
        base::stop("New parameter value not a logical singleton.")
      }

    # Parameters that are integer-valued.
    } else if (parameter %in% SqrlParams("integer-type"))
    {
      set <- base::suppressWarnings(base::as.integer(set))
      if ((base::length(set) != 1L)
          || base::is.na(set))
      {
        base::stop("New parameter value is not an integer.")
      }

    # The interface parameter can be character-valued or null-valued.
    # Changing the parameter does not change the interface.
    } else if (parameter %in% SqrlParams("string/null-type"))
    {
      if (!base::is.null(set))
      {
        set <- base::suppressWarnings(base::as.character(set))
        if ((base::length(set) != 1L)
            || base::is.na(set))
        {
          base::stop("New parameter value is not a character string.")
        }
      }

    # The na.strings parameter is a character vector of any length, including 0.
    } else if (parameter %in% SqrlParams("character-type"))
    {
      set <- base::suppressWarnings(base::as.character(set))

    # The as.is parameter can be a logical, numerical, or character vector.
    } else if (parameter %in% SqrlParams("index-type"))
    {
      # This can be a logical (not NA), a natural number (integer or numeric
      # form), a character string (valid name form), or a vector of the same.
      # For now, we do nothing (accept raw input without checking).

    # The colQuote and tabQuote parameters can be either NULL, or character
    # vectors of length 0, 1, or 2.
    } else if (parameter %in% SqrlParams("quote-type"))
    {
      if (!base::is.null(set))
      {
        set <- base::suppressWarnings(base::as.character(set))
        if ((base::length(set) > 2L)
            || base::any(base::is.na(set)))
        {
          base::stop("New parameter value is not a quotation specifier.")
        }
      }

    # The nullstring parameter is a character string, possibly NA_character_.
    } else if (parameter %in% SqrlParams("string/na-type"))
    {
      set <- base::suppressWarnings(base::as.character(set))
      if ((base::length(set) != 1L))
      {
        base::stop("New parameter value is not a character string.")
      }

    # Block the assignment of parameters unknown to SqrlParams().
    } else
    {
      base::stop("Unrecognised parameter.")
    }

    # Prevent overwriting (changing) the channel while it is open, with the
    # exception that a channel can be nullified (dropped) at any time.
    if ((parameter == "channel")
        && base::exists(parameter, cacheenvir, inherits = FALSE)
        && !base::is.null(set)
        && SqrlIsOpen(datasource))
    {
      if (base::identical(set, SqrlParam(datasource, "channel")))
      {
        base::return(base::invisible(set))
      }
      base::stop("Channel cannot be changed while open.")
    }

    # Prevent changing write-protected parameter values.
    if ((parameter %in% SqrlParams("write-protected"))
        && base::exists(parameter, cacheenvir, inherits = FALSE))
    {
      if (base::identical(set, SqrlParam(datasource, parameter)))
      {
        base::return(base::invisible(set))
      }
      base::stop("Parameter is write-protected.")
    }

    # Prevent changing RODBC::odbcConnect() parameters while connection is open.
    # (Because those changes would only take effect on opening a new channel.)
    # The _default_ values of these 'locked-while-open' parameters cannot be
    # changed while the connection is open. Hence, it is permissible to replace
    # a currently default value with an identical static value at any time.
    # The override condition allows SqrlOpen() to alter some of these (to values
    # the user may have entered) when the connection channel is first opened.
    if (!override
        && (parameter %in% SqrlParams("locked-while-open"))
        && SqrlIsOpen(datasource))
    {
      # Throw an error on an attempt to change the parameter value. Must also
      # throw an error when attempting to set a secret or semi-secret parameter
      # to the value it already has, or else the value could be discovered by
      # trial and error.
      if ((parameter %in% base::c(SqrlParams("secret"),
                                    SqrlParams("semi-secret")))
          || !base::identical(set, SqrlParam(datasource, parameter)))
      {
        base::stop("Parameter is locked while connection is open.")
      }
      # Otherwise, if the current value is a default (when no static value is
      # defined), set the (identical) new value as an equivalent static
      # replacement (for the default).
      if (!base::exists(parameter, cacheenvir, inherits = FALSE))
      {
        base::assign(parameter, set, cacheenvir)
      }
      # Return the (unchanged) value.
      base::return(base::invisible(set))
    }

    # The channel parameter is a special case, because we want to toggle the
    # indicator state along with a change of channel existence (null/not).
    if (parameter == "channel")
    {
      # Current value of the channel parameter. NULL is no channel (closed),
      # anything else is we think the channel is open (it may or may not be).
      current <- SqrlParam(datasource, "channel")
      # No channel to channel; show indicators (conditional on settings, mode).
      if (base::is.null(current)
          && !base::is.null(set))
      {
        SqrlIndicator(datasource, "show")
      # Channel to no channel; hide indicators (conditional on settings, mode)
      } else if (!base::is.null(current)
                  && base::is.null(set))
      {
        SqrlIndicator(datasource, "hide")
      }
      # Set the new value. Return it invisibly.
      base::assign(parameter, set, cacheenvir)
      base::return(base::invisible(set))
    }

    # The connection parameter is a special case, since we want to extract
    # further parameter values from it, if we can. This may fail if any of the
    # parameter values contain = or ;, but none of the test systems allow these
    # characters in DSNs, passwords, etc. Does any system? See related 'scrape'
    # comments within SqrlOpen().
    if (parameter == "connection")
    {
      # Unless the connection string contains a DSN placeholder ('<dsn>'),
      # delete any dsn definition.
      if (!base::grepl("<dsn>", set))
      {
        SqrlParam(datasource, "reset", "dsn", override)
      }
      # RODBC::odbcConnect() likes to know the driver (from which it determines
      # whether or not it's dealing with MySQL). While we're doing that, we may
      # as well attempt to extract some other parameter values, first. We make
      # sure the driver parameter is done last, because setting a value for dsn
      # sets driver as a side effect, and we may want to override that.
      spars <- SqrlParams("scrapeable-string")
      for (param in base::c(spars[spars != "driver"], spars[spars == "driver"]))
      {
        if (base::grepl(base::paste0(param, "\\s*="), set, ignore.case = TRUE))
        {
          assignee <- base::paste0("^.*", param, "\\s*=")
          value <- base::sub(assignee, "", set, ignore.case = TRUE)
          value <- base::trimws(base::sub(";.*$", "", value))
          # 'user' and 'username' are connection string aliases for 'uid'.
          if (param %in% SqrlParams("uid-aliases"))
          {
            param <- "uid"
          # 'password' is a connection string alias for 'pwd'.
          } else if (param %in% SqrlParams("pwd-aliases"))
          {
            param <- "pwd"
          }
          # SQRL accepts <uid> (etc.) as connection string template place
          # holders (to be replaced with current values at connection time).
          # We don't want to override default or previous values with these.
          if (value != base::paste0("<", param, ">"))
          {
            SqrlParam(datasource, param, value, override)
          }
        }
      }
      # Set the (unaltered) connection string, return invisibly.
      base::assign(parameter, set, cacheenvir)
      base::return(base::invisible(set))
    }

    # Setting the dsn parameter is a special case, because we simultaneously
    # reset the connection parameter unless the connection string contains a
    # '<dsn>' placeholder. If the DSN is defined on the local system, then we
    # also set the driver parameter to the DSN's value, as obtained from
    # RODBC::odbcDataSources().
    if (parameter == "dsn")
    {
      if (!base::grepl("<dsn>", SqrlParam(datasource, "connection")))
      {
        SqrlParam(datasource, "reset", "connection", override)
      }
      base::assign(parameter, set, cacheenvir)
      sources <- RODBC::odbcDataSources("all")
      if (set %in% base::names(sources))
      {
        SqrlParam(datasource, "driver", sources[set], override)
      }
      base::return(base::invisible(set))
    }

    # The prompt and wintitle parameters are special cases, because, if the
    # old prompt or wintitle is currently visible, it must be removed before
    # changing the parameter value, and then the new value must be applied.
    if (parameter %in% base::c("prompt", "wintitle"))
    {
      set <- base::trimws(set)
      if (set != SqrlParam(datasource, parameter))
      {
        isopen <- SqrlIsOpen(datasource)
        if (isopen)
        {
          SqrlIndicator(datasource, "hide", parameter)
        }
        base::assign(parameter, set, cacheenvir)
        if (isopen)
        {
          SqrlIndicator(datasource, "show", parameter)
        }
      }
      base::return(base::invisible(set))
    }

    # The visible parameter is a special case, because, if the channel is open,
    # both prompt and window title changes (addition or removal) must be made.
    if (parameter == "visible")
    {
      if (set != SqrlParam(datasource, "visible"))
      {
        isopen <- SqrlIsOpen(datasource)
        if (isopen
            && !set)
        {
          SqrlIndicator(datasource, "hide")
        }
        base::assign(parameter, set, cacheenvir)
        if (isopen
            && set)
        {
          SqrlIndicator(datasource, "show")
        }
      }
      base::return(base::invisible(set))
    }

    # For all other cases, set and (invisibly) return the new parameter value.
    base::assign(parameter, set, cacheenvir)
    base::return(base::invisible(set))
  }

  # When a cached parameter value exists, retrieve and return it.
  if (base::exists(parameter, cacheenvir, inherits = FALSE))
  {
    # Take care with regard to whom we supply secret parameter values.
    if (parameter %in% SqrlParams("secret"))
    {
      # If we don't see an internal call of this function (i.e., it appears to
      # have been called from outside of the namespace), return the default.
      calls <- base::gsub("\\(.*", "", base::.traceback(0L))
      i <- base::which(calls == "SqrlParam")
      if (base::length(i) < 1L)
      {
        base::return(SqrlDefault(datasource, parameter))
      }

      # Likewise, if we don't see who called this function, return the default.
      i <- base::max(i) + 1L
      if (i > base::length(calls))
      {
        base::return(SqrlDefault(datasource, parameter))
      }

      # If the caller is aware, return either the default value (if such has
      # been set) or a dummy value (when some non-default value has been set).
      if (calls[i] %in% SqrlParams("aware"))
      {
        value <- base::get(parameter, cacheenvir, inherits = FALSE)
        if (base::identical(value, SqrlDefault(datasource, parameter)))
        {
          base::return(value)
        }
        base::return("*")
      }

      # If the caller is neither aware nor informed, return the default value.
      if (!(calls[i] %in% SqrlParams("informed")))
      {
        base::return(SqrlDefault(datasource, parameter))
      }

    # Take care with regard to whom we supply semi-secret parameter values.
    } else if (parameter %in% SqrlParams("semi-secret"))
    {
      # If we don't see an internal call of this function (i.e., it appears to
      # have been called from outside of the namespace), return the default.
      calls <- base::gsub("\\(.*", "", base::.traceback(0L))
      i <- base::which(calls == "SqrlParam")
      if (base::length(i) < 1L)
      {
        base::return(SqrlDefault(datasource, parameter))
      }

      # Likewise, if we don't see who called this function, return the default.
      i <- base::max(i) + 1L
      if (i > base::length(calls))
      {
        base::return(SqrlDefault(datasource, parameter))
      }

      # If the caller is neither aware nor informed, return the default value.
      if (!(calls[i] %in% base::c(SqrlParams("aware"), SqrlParams("informed"))))
      {
        base::return(SqrlDefault(datasource, parameter))
      }
    }

    # The parameter is not secret, or the caller is allowed to know its value.
    # Return the value.
    base::return(base::get(parameter, cacheenvir, inherits = FALSE))
  }

  # There is no cached value for the parameter. Return a default value instead.
  base::return(SqrlDefault(datasource, parameter))
}

SqrlParams <- function(group = "")
{
  # Returns any one of various useful parameter groupings.
  # Args:
  #   group : The (string) name (description) of a parameter group.
  # Returns:
  #   A character vector of the names of all parameters in the group.
  # SQRL Calls:
  #   None.
  # SQRL Callers:
  #   SqrlCache(), SqrlConfig(), SqrlDefile(), SqrlDelegate(), SqrlDSNs(),
  #   SqrlOpen(), SqrlParam(), SqrlSource(), SqrlSources(), SqrlValue(),
  #   sqrlAll().
  # User:
  #   Has no direct access, and is unable to supply the argument. Validity
  #   checks are not required.

  # Parameter-group definitions (find and return).
  base::return(base::switch(group,

    # All parameter names, whether RODBC or SQRL, except for 'interface'.
    "all"                   = base::c("as.is",
                                      "autoclose",
                                      "believeNRows",
                                      "buffsize",
                                      "case",
                                      "channel",
                                      "colQuote",
                                      "connection",
                                      "DBMSencoding",
                                      "dec",
                                      "driver",
                                      "dsn",
                                      "errors",
                                      "interface",
                                      "interpretDot",
                                      "max",
                                      "na.strings",
                                      "name",
                                      "nullstring",
                                      "ping",
                                      "prompt",
                                      "pwd",
                                      "readOnlyOptimize",
                                      "rows_at_time",
                                      "stringsAsFactors",
                                      "tabQuote",
                                      "uid",
                                      "verbose",
                                      "visible",
                                      "wintitle"),

    # Functions (not parameters) allowed to know whether or not secrets exist.
    "aware"                 = base::c("SqrlValue"),

    # Parameters of Boolean-singleton type (TRUE/FALSE, not NA).
    "boolean-type"          = base::c("autoclose",
                                      "believeNRows",
                                      "errors",
                                      "interpretDot",
                                      "readOnlyOptimize",
                                      "stringsAsFactors",
                                      "verbose",
                                      "visible"),

    # Parameters of character-vector type (any length, including zero).
    "character-type"        = base::c("na.strings"),

    # Parameters not to copy when duplicating an existing SQRL data source.
    "don't-copy"            = base::c("channel",
                                      "interface",
                                      "name",
                                      "prompt",
                                      "wintitle"),

    # Parameters of index type (logical, numerical, or character vectors).
    "index-type"            = base::c("as.is"),

    # Functions (not parameters) allowed to know secrets.
    "informed"              = base::c("SqrlOpen",
                                      "SqrlSource"),

    # Parameters of integer-singleton type (not NA).
    "integer-type"          = base::c("buffsize",
                                      "max",
                                      "rows_at_time"),

    # Parameters that cannot be changed while the connection channel is open.
    "locked-while-open"     = base::c("believeNRows",
                                      "case",
                                      "colQuote",
                                      "connection",
                                      "DBMSencoding",
                                      "driver",
                                      "dsn",
                                      "interpretDot",
                                      "pwd",
                                      "readOnlyOptimize",
                                      "rows_at_time",
                                      "tabQuote",
                                      "uid"),

    # String-type parameters that accept NULL as an aliaas for the empty string.
    "nullable-string"       = base::c("connection",
                                      "DBMSencoding",
                                      "driver",
                                      "dsn",
                                      "prompt",
                                      "pwd",
                                      "uid",
                                      "wintitle"),

    # Parameters that can be file-path valued (excluded from SqrlDefile()).
    "path-valued"           = base::c("driver",
                                      "dsn"),

    # Aliases for 'pwd' (within the 'scrapeable-string' parameter set).
    "pwd-aliases"           = base::c("password"),

    # Parameters of quote type can be NULL, or character-vectors of length <= 2.
    "quote-type"            = base::c("colQuote",
                                      "tabQuote"),

    # Parameters that cannot be set (written) by the user.
    "read-only"             = base::c("channel"),

    # Parameters that are of RODBC type (can be NULL valued).
    "rodbc/null-type"       = base::c("channel"),

    # Parameters that can have their values scraped from an open channel object.
    "scrapeable-channel"    = base::c("driver",
                                      "dsn",
                                      "uid"),

    # Parameters that can have their values scraped from a connection string.
    "scrapeable-string"     = base::c("driver",
                                      "dsn",
                                      "password",
                                      "pwd",
                                      "uid",
                                      "user",
                                      "username"),

    # Parameters whose actual values are never returned to the user.
    "secret"                = base::c("password",
                                      "pwd"),

    # Parameters whose values may contain a secret component.
    "semi-secret"           = base::c("connection"),

    # Parameters appearing in the data source summary table, in table column
    # order (not in alphabetical order).
    "source-table"          = base::c("name",
                                      "interface",
                                      "open",
                                      "driver"),

    # Keywords used for SQL script identification in SqrlDelegate().
    "sql-keywords"          = base::c("select",
                                      "create",
                                      "drop",
                                      "update",
                                      "insert"),

    # Parameters that are of character-string (singleton) type (non-NA).
    "string-type"           = base::c("case",
                                      "connection",
                                      "DBMSencoding",
                                      "dec",
                                      "driver",
                                      "dsn",
                                      "name",
                                      "prompt",
                                      "pwd",
                                      "uid",
                                      "wintitle"),

    # Parameters that are of character-string type, with NAs allowed.
    "string/na-type"        = base::c("nullstring"),

    # Parameters that are of string type, or else can be NULL valued.
    "string/null-type"      = base::c("interface",
                                      "ping"),

    # Parameters that can take template-form within a connection string.
    "substitutable"         = base::c("driver",
                                      "dsn",
                                      "pwd",
                                      "uid"),

    # Aliases for 'uid' (within the 'scrapeable-string' parameter set).
    "uid-aliases"           = base::c("user",
                                      "username"),

    # Names to filter-out when obtaining DSNs.
    "unwanted-sources"      = base::c("Access",
                                      "dBASE",
                                      "Excel"),

    # Parameters that are write-once (even by SQRL, not just the user).
    "write-protected"       = base::c("name"),

    # This should never happen.
    base::stop("Unknown parameter group.")))
}

SqrlPath <- function(...)
{
  # Determines whether or not the arguments form a path to an existing file.
  # Args:
  #   ... : Components of a (possible) file path (to be pasted together).
  # Returns:
  #   The normalised file path, when ... appears to specify an existing file,
  #   or NULL, when ... does not appear to specify an existing file.
  # SQRL Calls:
  #   None.
  # SQRL Callers:
  #   SqrlConfig(), SqrlDefile(), SqrlDelegate(), SqrlHelp(), SqrlSource().
  # User:
  #   Has no direct access, but is able to supply arguments(s) indirectly, via
  #   SqrlDelegate(). There are no restrictions on these (no checks required).

  # Paste all arguments together.
  filepath <- base::paste0(..., collapse = "")

  # If the filepath does not have length one, then it cannot specify a file.
  if ((base::length(filepath) != 1L)
      || (base::nchar(filepath) < 1L))
  {
    base::return(NULL)
  }

  # If filepath actually does point to a readable file, return the (normalised)
  # path. Note that files '.' and '..' exist as directories, and that file '"'
  # exists but is not read accessible (the '4' tests for read access).
  if (base::file.exists(filepath)
      && (base::file.access(filepath, 4L) == 0L)
      && !(base::file.info(filepath)$isdir))
  {
    base::return(base::normalizePath(filepath))
  }

  # The arguments do not appear to specify a file path. Return NULL.
  base::return(NULL)
}

SqrlPing <- function(datasource)
{
  # Attempts to find a simple 'ping' statement for the data source.
  # Args:
  #   datasource : The name of a known data source.
  # Returns:
  #   An invisible string (either a short SQL statement, or the empty string).
  # SQRL Calls:
  #   SqrlParam(), SqrlSubmit().
  # SQRL Callers:
  #   SqrlOpen().
  # User:
  #   Has no direct access, and is unable to supply the argument. No argument
  #   validity checking is required.

  # Here, we define some 'pings', being very simple SQL statements. These are
  # used to ping the data source; confirming we're still connected when we get
  # the expected result back (or telling us we've lost the connection when we
  # don't). These, alas, are vendor dependent, so we have to guess, trial, and
  # see what works. Some vendor-independent method would be vastly preferable.

  # Ping for MySQL, PostgreSQL, SQL Server, SQLite, Teradata.
  p1 <- "select 1"

  # Ping for Oracle, MySQL, DB2.
  p2 <- "select 1 from dual"

  # Ping for Oracle.
  p3 <- "begin null; end;"

  # Arrange the pings into best-guess-first order, according to the driver.
  pings <- base::c(p1, p2, p3)
  driver <- base::tolower(SqrlParam(datasource, "driver"))
  if (base::grepl("oracle", driver, fixed = TRUE))
  {
    pings <- pings[base::c(3L, 2L, 1L)]
  } else if (base::grepl("db2", driver, fixed = TRUE))
  {
    pings <- pings[base::c(2L, 1L, 3L)]
  }

  # Try each of the pings, in (driver-dependent) order of decreasing preference,
  # until we find a ping that works (is valid SQL for the data source). These
  # could also fail if the connection has been unexpectedly closed.
  for (ping in pings)
  {
    a <- base::try(SqrlSubmit(datasource, ping, throw = TRUE,
                              retry = FALSE, isping = TRUE),
                    silent = TRUE)
    if (!base::inherits(a, "try-error"))
    {
      base::return(SqrlParam(datasource, "ping", ping))
    }
  }

  # Did not find a ping that works. Set and return the empty string. This causes
  # the pinging system to submit a junk query and scan the response for error
  # terms that suggest a lost connection (which is not completely reliable).
  base::return(SqrlParam(datasource, "ping", ""))
}

SqrlStatement <- function(...,
                          envir = base::parent.frame())
{
  # Constructs a SQL statement from the supplied list of arguments.
  # Args:
  #   ...   : A list of strings, expressions, etc., forming a SQL statement.
  #   envir : An R environment (in which the expressions will be evaluated).
  # Returns:
  #   The corresponding SQL statement. Differs from paste() in that lists are
  #   rewritten in comma-separated form and vectors in newline-separated form.
  # SQRL Calls:
  #   None.
  # SQRL Callers:
  #   SqrlFile(), SqrlSubmit().
  # User:
  #   Has no direct access. Can supply the ... argument(s), via SqrlDelegate(),
  #   but ... is unrestricted, and so no validity checks are required.

  # Retrieve list of items in the arguments list (this may include data-types,
  # object names, function calls, unevaluated expressions, you name it).
  arglist <- base::as.list(base::match.call())[-1L]

  # If no arguments were supplied, return an empty statement.
  if (base::length(arglist) == 0L)
  {
    base::return("")
  }

  # Internal function for evaluating and collapsing objects from the args list.
  SqrlStatement_Reduce <- function(obj)
  {
    # Evaluate function calls, get named objects, evaluate expressions.
    if (base::class(obj) %in% base::c(base::class(base::call("x")),
                                      base::class(base::as.name("x")),
                                      base::class(base::expression())))
    {
      obj <- base::eval(obj, envir)
    }

    # Any objects that are (or that evaluated to) lists, are now collapsed
    # to comma-separated (single string) form.
    if (base::class(obj) == base::class(base::list()))
    {
      obj <- base::paste(obj, collapse = ", ")
    }

    # Any vector objects (such as output from readLines()) are now collapsed
    # to newline-separated (single string) form.
    obj <- base::paste(obj, collapse = "\n")

    # Return the final (evaluated and collapsed) object.
    base::return(obj)
  }

  # Return the final (reduced and collapsed) statement.
  base::return(base::paste(base::lapply(arglist, SqrlStatement_Reduce),
                            collapse = ""))
}

SqrlSource <- function(...)
{
  # Defines (or re-defines) a data source and its interface.
  # Args:
  #   ... : A source name and definition (string or file), in that order.
  # Returns:
  #   The interface name, invisibly, after creating, or re-defining, the source
  #   and its interface.
  # SQRL Calls:
  #   SqrlCache(), SqrlConfig(), SqrlDefile(), SqrlFace(), SqrlInterface(),
  #   SqrlParam(), SqrlParams(), SqrlPath().
  # SQRL Callers:
  #   sqrlSource().
  # User:
  #   Has no direct access. Can supply all arguments via sqrlSource() (only).
  #   That function guarantees the existence of either at least two unnamed
  #   terms, or at least one named term. Additional checks (assignability,
  #   conflict, etc.) are performed here.

  # Separate the name from the definition component(s).
  def <- base::list(...)
  if (base::length(def) == 1L)
  {
    name <- base::trimws(base::names(def))
    base::names(def) <- NULL
  } else
  {
    name <- base::trimws(def[[1L]])
    def[[1L]] <- NULL
  }

  # Ensure either all terms are named, or that no term is named. When all terms
  # are named, ensure all names are different.
  if (!base::is.null(base::names(def)))
  {
    isnamed <- base::nzchar(base::names(def))
    if (!base::any(isnamed))
    {
      base::names(def) <- NULL
    } else if (!base::all(isnamed))
    {
      base::stop("Mixture of named and unnamed arguments.")
    } else if (base::length(base::unique(base::names(def)))
                != base::length(base::names(def)))
    {
      base::stop("Duplicated argument names.")
    }
  }

  # Accept source = NULL as an alias for remove = source.
  if ((base::length(def) == 1L)
      && base::is.null(def[[1L]]))
  {
    def <- base::list(name)
    name <- "remove"
  }

  # If the name is 'remove', treat the definition as a list of names of sources
  # to be removed (deregistered from SQRL). Do that, then return invisible NULL.
  # Non-existent sources are quietly skipped (no error is thrown).
  if (name == "remove")
  {
    datasources <- base::as.character(base::unlist(def))
    datasources <- datasources[datasources %in% SqrlCache("*")]
    for (datasource in datasources)
    {
      SqrlCache(datasource, delete = TRUE)
    }
    base::return(base::invisible(NULL))
  }

  # Abort if the source name is unassignable.
  if (name != base::make.names(name))
  {
    base::stop("Unassignable data-source name.")
  }

  # Prohibit redefinition of open sources. Always remove a preexisting source,
  # so that SqrlSource() begins from (defines onto) a clean slate.
  if (SqrlCache(name, exists = TRUE))
  {
    if (SqrlIsOpen(name))
    {
      base::stop("Cannot redefine an open source.")
    }
    SqrlCache(name, delete = TRUE)
  }

  # When none of the terms are named, establish the implied name (and value),
  # according to a sequential hierarchy.
  if (base::is.null(base::names(def)))
  {
    # If the terms specify the path of a readable file, interpret them as a
    # request to define and configure a source from that file.
    def <- base::as.character(base::unlist(def))
    if (!base::is.null(SqrlPath(def)))
    {
      def <- base::list(config = base::paste0(def, collapse = ""))

    # Otherwise, if there is only one term and it names an existing source,
    # interpret it as a request to make a copy of that source.
    } else if ((base::length(def) == 1L)
                && SqrlCache(def, exists = TRUE))
    {
      def <- base::list(copy = def)

    # Otherwise, if there are multiple terms, or if any term (string) contains
    # an equals sign, interpret them as components of a connection string.
    } else if ((base::length(def) > 1L)
                || base::any(base::grepl("=", def)))
    {
      def <- base::sub(";$", "", def)
      def <- base::list(connection = base::paste0(def, collapse = ";"))

    # Otherwise, there is only one term (string), it does not contain an equals
    # sign, and does not name an existing source; interpret it as a DSN.
    } else
    {
      def <- base::list(dsn = def)
    }
  }

  # Abort if an original source (to be copied) has been specified, but that
  # source does not exist within the SQRL cache.
  if (("copy" %in% base::names(def))
      && (!(def$copy %in% SqrlCache("*"))))
  {
    base::stop("Copy source original not found.")
  }

  # Abort if a configuration file has been specified, but that file cannot be
  # read (including file does not exist).
  if (("config" %in% base::names(def))
      && base::is.null(SqrlPath(def["config"])))
  {
    base::stop("Cannot read config file.")
  }

  # When the defining terms do not include a 'copy', 'config', or 'connection',
  # there is no possibility of a single term specifying a connection string. If,
  # additionally, we do not have a 'dsn' term, or if one of the terms does not
  # correspond to a SQRL/RODBC parameter, then we interpret all of the terms as
  # connection-string components, and construct the string from them.
  if ((!(base::any(base::c("copy", "config", "connection")
                    %in% base::names(def))))
      && ((!("dsn" %in% base::names(def)))
          || (!base::all(base::names(def) %in% SqrlParams("all")))))
  {
    def <- base::paste0(base::names(def), "=", base::sub(";$", "", def))
    def <- base::list(connection = base::paste0(def, collapse = ";"))
  }

  # Create a fresh cache for the new data source (if it previously existed, it
  # will have been deleted).
  SqrlCache(name, create = TRUE)

  # If we have a 'copy' term, perform the copy operation first (so that any
  # other terms will subsequently override copied values).
  if ("copy" %in% base::names(def))
  {
    # This only returns the set (non-default) parameters.
    params <- SqrlParam(def$copy, "*")

    # Don't copy parameters we shouldn't (name, interface, etc.).
    params <- params[!(params %in% SqrlParams("don't-copy"))]

    # Copy driver last, in case dsn was copied (and set a value for driver).
    # Secrets are copied without loss, because SqrlSource() is informed.
    params <- base::c(params[params != "driver"], params[params == "driver"])
    for (param in params)
    {
      SqrlParam(name, param, SqrlParam(def$copy, param))
    }

    # If the original driver wasn't set, ensure the copy's driver is also
    # undefined (in case dsn was defined and a copy driver has been set).
    if (!("driver" %in% params))
    {
      SqrlParam(name, "reset", "driver")
    }
  }

  # If we have a 'config' term, attempt to configure the source from the config
  # file. Values in the file override any vales that may already have been
  # copied from another source. The incomplete source is deleted on error.
  if ("config" %in% base::names(def))
  {
    result <- base::try(SqrlConfig(name, SqrlPath(def$config)), silent = TRUE)
    if (base::inherits(result, "try-error"))
    {
      SqrlCache(name, delete = TRUE)
      base::stop(result)
    }
  }

  # If we have an 'interface' term, attempt to apply the specified name. This
  # overrides any value that may have been set via config file. The incomplete
  # source is deleted upon error.
  if ("interface" %in% base::names(def))
  {
    result <- base::try(
                  SqrlInterface(name, SqrlDefile("interface", def$interface)),
                  silent = TRUE)
    if (base::inherits(result, "try-error"))
    {
      SqrlCache(name, delete = TRUE)
      base::stop(result)
    }
  }

  # Iterate over all other terms (besides 'copy', 'config', and 'interface'),
  # treating each as a SQRL/RODBC parameter. The incomplete source is deleted
  # upon any error.
  params <- base::names(def)
  params <- params[!(params %in% base::c("copy", "config", "interface"))]
  params <- base::c(params[params != "driver"], params[params == "driver"])
  for (param in params)
  {
    result <- base::try(SqrlParam(name, param, SqrlDefile(param, def[[param]])),
                        silent = TRUE)
    if (base::inherits(result, "try-error"))
    {
      SqrlCache(name, delete = TRUE)
      base::stop(result)
    }
  }

  # If no interface has been defined, attempt to apply the source name. The
  # incomplete source will be deleted if this is not possible.
  if (SqrlParam(name, "interface", isdefined = FALSE))
  {
    result <- base::try(SqrlInterface(name, name), silent = TRUE)
    if (base::inherits(result, "try-error"))
    {
      SqrlCache(name, delete = TRUE)
      base::stop(result)
    }
  }

  # Return the interface name (not the source name), invisibly.
  base::return(base::invisible(SqrlParam(name, "interface")))
}

SqrlSources <- function(import = "")
{
  # Returns a summary table of defined sources.
  # Args:
  #   import : Specifies the class of DSNs to import (default is do not import).
  # Returns:
  #   A data frame summarising locally defined data sources. There is no
  #   guarantee that any of these sources are presently available, or even that
  #   they exist. The data frame may be empty (have zero rows).
  # SQRL Calls:
  #   SqrlAll(), SqrlCache(), SqrlDSNs(), SqrlIsOpen(), SqrlParams(),
  #   SqrlValue().
  # SQRL Callers:
  #   SqrlDelegate(), sqrlSources().
  # User:
  #   The user has no direct access, but is able to supply the argument via
  #   sqrlSources(), which vets it as being one of "", "all", "user", or
  #   "system". Further argument validity checking is not required.

  # If the import argument is 'remove', then deregister (delete) all sources.
  if (import == "remove")
  {
    SqrlAll("remove", envir = base::parent.frame())
    base::return(base::invisible(NULL))
  }

  # If the import argument was something else, import the corresponding DSNs.
  if (base::nchar(import) > 0L)
  {
    SqrlDSNs(import)
  }

  # Retrieve and return a summary of sources (data frame).
  params <- SqrlParams("source-table")
  sumlist <- base::list()
  for (param in params)
  {
    sumlist[[param]] <- base::list(base::character(0L))
  }
  for (datasource in SqrlCache("*"))
  {
    for (param in params)
    {
      if (param == "open")
      {
        value <- base::c("N", "Y")[SqrlIsOpen(datasource, besure = FALSE) + 1L]
      } else
      {
        value <- SqrlValue(datasource, param)
        if (base::is.null(value))
        {
          value <- NA
        }
      }
      sumlist[[param]] <- base::append(sumlist[[param]], value)
    }
  }
  for (param in params)
  {
    sumlist[[param]] <- base::unlist(sumlist[[param]])
  }
  sumframe <- base::as.data.frame(sumlist, stringsAsFactors = FALSE)
  sumframe <- sumframe[base::order(sumframe[, 1L]), ]
  base::rownames(sumframe) <- NULL
  base::return(sumframe)
}

SqrlSubmit <- function(datasource = "",
                        ...,
                        throw = FALSE,
                        retry = TRUE,
                        isping = FALSE)
{
  # Submit a SQL statement to a connected data source.
  # Args:
  #   datasource : The name of a known data source.
  #   ...        : A SQL statement (in general, as a list of components).
  #   throw      : When set to TRUE, overrides the errors parameter.
  #   retry      : When set to FALSE, do not resubmit on failure.
  #   isping     : When set to TRUE, the SQL statement is treated as a ping.
  # Returns:
  #   Result of submitting the statement (typically a data frame).
  # SQRL Calls:
  #   SqrlIndicator(), SqrlIsOpen(), SqrlOpen(), SqrlParam(), SqrlStatement(),
  #   SqrlSubmit() (self).
  # RODBC Calls:
  #   sqlGetResults(), sqlQuery().
  # SQRL Callers:
  #   SqrlDelegate(), SqrlIsOpen(), SqrlPing(), SqrlSubmit() (self),
  #   SqrlSubScript().
  # User:
  #   Has no direct access. Is able to supply (only) the ... argument(s), via
  #   SqrlDelegate(), or from a SQRL file.

  # Abort, unless an open channel exists, or can be established, to the data
  # source. This is not a ping check, so the channel might still be closed.
  if (!SqrlIsOpen(datasource))
  {
    SqrlOpen(datasource)
    if (!SqrlIsOpen(datasource))
    {
      base::stop("Connection attempt failed.")
    }
  }

  # Combine the supplied components into a single SQL statement.
  statement <- base::try(SqrlStatement(...), silent = TRUE)
  if (base::inherits(statement, "try-error"))
  {
    if (SqrlParam(datasource, "autoclose"))
    {
      SqrlClose(datasource)
    }
    base::stop(statement)
  }

  # If the statement is empty, return NULL (emulates no-query in any SQL).
  if (!base::grepl("[[:graph:]]", statement))
  {
    base::return(NULL)
  }

  # Append query-in-progress indicator to the window-title connection indicator.
  SqrlIndicator(datasource, "query")

  # This is the maximum number of rows to fetch from RODBC::sqlQuery() (with
  # any remaining rows fetched by RODBC::sqlGetResults()). We'd actually prefer
  # this to be zero, but zero is a special value meaning all (see RODBC manual).
  # We could use RODBC::odbcQuery() for this, but the function is "likely to be
  # confined to the 'RODBC' namespace in the near future" (see RODBC manual).
  # This number may as well be one (rather than, say, ten), because RODBC::
  # sqlGetResults() will run, whether or not there are any more rows left to
  # fetch on the remote machine, so long as SqrlParam("max") is either zero (the
  # default) or greater than this number.
  onerow <- 1L

  # A valid connection exists, submit the statement.
  result <- base::try(
              RODBC::sqlQuery(channel = SqrlParam(datasource, "channel"),
                  query = statement,
                  errors = SqrlParam(datasource, "errors"),
                  as.is = SqrlParam(datasource, "as.is"),
                  max = onerow,
                  buffsize = SqrlParam(datasource, "buffsize"),
                  nullstring = SqrlParam(datasource, "nullstring"),
                  na.strings = SqrlParam(datasource, "na.strings"),
                  believeNRows = SqrlParam(datasource, "believeNRows"),
                  dec = SqrlParam(datasource, "dec"),
                  stringsAsFactors = SqrlParam(datasource, "stringsAsFactors"),
                  rows_at_time = SqrlParam(datasource, "rows_at_time")),
              silent = TRUE)

  # Remove query-in-progress indicator from the window title.
  SqrlIndicator(datasource, "done")

  # If, for example, the as.is parameter value has been set to the name of a
  # column, and that column does not exist in the data frame produced by the
  # query, then RODBC throws an exception. If the query was a ping, then we
  # resubmit it with safe (although non-default) values of as.is, believeNRows,
  # and stringsAsFactors. The user may have intentionally set as.is to 'foo'
  # for their main query, but this might not be compatible with their ping.
  if (isping
      && base::inherits(result, "try-error"))
  {
    SqrlIndicator(datasource, "query")
    result <- base::try(
                    RODBC::sqlQuery(channel = SqrlParam(datasource, "channel"),
                          query = statement,
                          errors = SqrlParam(datasource, "errors"),
                          as.is = TRUE,
                          max = onerow,
                          buffsize = SqrlParam(datasource, "buffsize"),
                          nullstring = SqrlParam(datasource, "nullstring"),
                          na.strings = SqrlParam(datasource, "na.strings"),
                          believeNRows = FALSE,
                          dec = SqrlParam(datasource, "dec"),
                          stringsAsFactors = FALSE,
                          rows_at_time = SqrlParam(datasource, "rows_at_time")),
                    silent = TRUE)
    SqrlIndicator(datasource, "done")

    # If that still doesn't work, try again with all (other) default values.
    if (base::inherits(result, "try-error"))
    {
      SqrlIndicator(datasource, "query")
      result <- base::try(
                    RODBC::sqlQuery(channel = SqrlParam(datasource, "channel"),
                                    query = statement,
                                    errors = SqrlDefault("errors"),
                                    as.is = TRUE,
                                    max = onerow,
                                    buffsize = SqrlDefault("buffsize"),
                                    nullstring = SqrlDefault("nullstring"),
                                    na.strings = SqrlDefault("na.strings"),
                                    believeNRows = FALSE,
                                    dec = SqrlDefault("dec"),
                                    stringsAsFactors = FALSE,
                                    rows_at_time = SqrlDefault("rows_at_time")),
                    silent = TRUE)
      SqrlIndicator(datasource, "done")
    }
  }

  # On success, RODBC::sqlQuery() returns a data frame or character string (both
  # possibly empty). On error, RODBC::sqlQuery() returns a character vector of
  # error messages (always with at least two elements, it seems) or an integer
  # (either -1 or -2). From R, see ?RODBC::sqlQuery. This detects such errors.
  # It might be preferable to call RODBC::sqlQuery() with errors = FALSE (which
  # unambiguously returns -1 on an error), and then retrieve the error message
  # via RODBC::odbcGetErrMsg(), but this function is 'likely to be confined to
  # the RODBC namespace in the near future' (according to the RODBC manual).
  # RODBC::sqlQuery() may also return an actual error, usually due to something
  # within RODBC, rather than within the data source.
  srcerr <- (base::inherits(result, "try-error")
              || (base::class(result) == base::class(base::integer()))
              || ((base::class(result) == base::class(base::character()))
                    && (base::length(result) > 1L)))

  # If there was an error, and the retry flag is set (to TRUE), and a double
  # check of the connection channel's open status (this time, with a ping of
  # the data source) reveals that it's actually closed, then we infer that the
  # channel was likely closed by the source and submit the statement one more
  # time (only). This call of SqrlSubmit() will attempt to reopen the channel.
  # This provides a (very) limited ability to recover from network drop-outs.
  # It is possible this recovery attempt will prompt the user for a password.
  # We are unable to distinguish between not knowing the password (not defined
  # within SQRL), the password being contained within a DSN (which may form
  # part of a connection string), or some kind of network authentication.
  if (srcerr
      && retry
      && !SqrlIsOpen(datasource, besure = TRUE))
  {
    base::return(SqrlSubmit(datasource, ..., throw = throw,
                            retry = FALSE, isping = isping))
  }

  # RODBC::sqlQuery() doesn't (often) throw proper errors (when these occur on
  # the data source side). Instead, it returns the error messages (as sent from
  # the data source) as a character vector. Trap and throw these (as fatal
  # errors), unless the throw argument and the error parameter are both FALSE.
  # Possible RODBC::sqlQuery() (& RODBC::sqlGetResults()) Return Values: On
  # success, a data frame (possibly with 0 rows) or a character string. On
  # failure, if errors = TRUE a character vector of message(s), otherwise an
  # invisible integer error code -1 (general, call odbcGetErrMsg for details)
  # or -2 (no data; which may not be an error. as some SQL statements do return
  # no data).
  if (srcerr
      && (throw
          || SqrlParam(datasource, "errors")))
  {
    if (SqrlParam(datasource, "autoclose"))
    {
      SqrlClose(datasource)
    }
    base::stop(base::paste(result, collapse = "\n"))
  }

  # When the result is not a data frame, there won't be any more rows to fetch,
  # so return it invisibly (it should be a character string, likely indicating
  # 'No data', or some such thing. Likewise, if the query was a ping, we don't
  # need to fetch any remaining rows (should such exist).
  if (isping
      || (base::class(result) != base::class(base::data.frame())))
  {
    base::return(base::invisible(result))
  }

  # Everything appears to have gone smoothly. If the maximum total number of
  # rows to fetch, SqrlParam("max"), is non-zero (zero means unlimited), then
  # subtract the number of rows already fetched from RODBC::sqlQuery() (to fetch
  # the remainder). If we've already retrieved the maximum number, return the
  # result from here (do not place a post-query call to RODBC::sqlGetResults()).
  fetchrows <- SqrlParam(datasource, "max")
  if (fetchrows > 0)
  {
    if (fetchrows <= onerow)
    {
      base::return(result)
    }
    fetchrows <- fetchrows - onerow
  }

  # Append fetch-in-progress indicator to the window-title connection indicator.
  SqrlIndicator(datasource, "fetch")

  # Retrieve all remaining rows. If a connection error occurs here, we cannot
  # easily recover, since pinging the source would destroy the waiting rows.
  restof <- base::try(
                RODBC::sqlGetResults(channel = SqrlParam(datasource, "channel"),
                  as.is = SqrlParam(datasource, "as.is"),
                  errors = SqrlParam(datasource, "errors"),
                  max = fetchrows,
                  buffsize = SqrlParam(datasource, "buffsize"),
                  nullstring = SqrlParam(datasource, "nullstring"),
                  na.strings = SqrlParam(datasource, "na.strings"),
                  believeNRows = SqrlParam(datasource, "believeNRows"),
                  dec = SqrlParam(datasource, "dec"),
                  stringsAsFactors = SqrlParam(datasource, "stringsAsFactors")),
                silent = TRUE)

  # Remove fetch-in-progress indicator from the window title.
  SqrlIndicator(datasource, "done")

  # If RODBC::sqlGetResults() appears to have returned an error (message or
  # code) and throw is TRUE, then stop with an exception. Otherwise, if the
  # function did not return a data frame (something seems to have gone wrong),
  # return that something invisibly. An exception might also have been thrown
  # by RODBC::sqlGetResults(), in the event of a network outage. The logic here
  # could perhaps be simplified.
  srcerr <- (base::inherits(restof, "try-error")
              || (base::class(restof) == base::class(base::integer()))
              || ((base::class(restof) == base::class(base::character()))
                    && (base::length(restof) > 1L)))
  if (srcerr
      && (throw
          || SqrlParam(datasource, "errors")))
  {
    if (SqrlParam(datasource, "autoclose"))
    {
      SqrlClose(datasource)
    }
    base::stop(base::paste(restof, collapse = "\n"))
  }
  if (base::class(restof) != base::class(base::data.frame()))
  {
    base::return(base::invisible(restof))
  }

  # RODBC::sqlQuery() and RODBC::sqlGetRows() have both returned data frames.
  # Bind them together (RODBC::sqlQuery() first), and return the full result.
  base::return(base::rbind(result, restof))
}

SqrlSubScript <- function(datasource = "",
                          statement = "",
                          phrase = "",
                          intermediate = "null",
                          envir = NULL)
{
  # Submits a SQL statement to a data source, and retrieves the result.
  # Args:
  #   datasource   : The name of data source, as known to SQRL.
  #   statement    : A list of strings, forming a (partial) SQL statement.
  #   phrase       : A single string, completing the SQL statement.
  #   intermediate : The name (string) of a variable to assign the result to.
  #   envir        : An environment, within which the assignment is made.
  # Returns:
  #   The result of submitting the statement (or NULL when the statement is
  #   blank). When the environment and intermediate are both non-null, the
  #   result (or NULL) is assigned to the intermediate within the environment.
  # SQRL Calls:
  #   SqrlParam(), SqrlSubmit().
  # SQRL Callers:
  #   SqrlFile().
  # utils Calls:
  #   head() (only if utils is attached).
  # User:
  #   Has no direct access, but is able to supply (only) the statement, phrase,
  #   and intermediate arguments via a SQRL script. These arguments will have
  #   already been parsed and worked into the correct format, by SqrlFile() and
  #   SqrlStatement(), so no argument validity checks should be required here.

  # Remove trailing whitespace (including vertical) from the phrase.
  # The phrase cannot (will never) contain quoted string literals.
  phrase <- base::sub("[[:space:]]*$", "", phrase)

  # Remove trailing whitespace from each internal line of the phrase.
  phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

  # Remove vertical whitespace from within the phrase.
  phrase <- base::gsub("\n+", "\n", phrase)

  # Remove any whitespace preceding a terminal semi-colon.
  phrase <- base::sub("[[:space:]]*;$", ";", phrase)

  # Append the phrase to the statement (unless the phrase is empty).
  if (base::nchar(phrase) > 0L)
  {
    statement <- base::append(statement, phrase)
  }

  # If the statement is non-empty, submit it and retrieve the result.
  if (base::length(statement) > 0L)
  {
    # Collapse the statement to a single string. Submit it if non-blank.
    statement <- base::trimws(base::paste(statement, collapse = ""))
    if (base::grepl("[[:graph:]]", statement))
    {
      # Boolean; whether or not to show verbose output.
      verbose <- base::interactive() && SqrlParam(datasource, "verbose")

      # If verbose, output the statement (prior to submission).
      if (verbose)
      {
        base::cat("\n\n\n")
        base::cat(statement)
        base::cat("\n")
      }

      # Submit the statement to the source, retrieve the result.
      result <- SqrlSubmit(datasource, statement)

      # If verbose, output (some of) the result. Coming from SqrlSubmit, this
      # should be a data frame, a short character vector, an integer, or NULL.
      # Methods for head() and print() are defined on all of these.
      if (verbose)
      {
        if ("package:utils" %in% base::search())
        {
          base::print(utils::head(result))
        } else if (base::class(result) == base::class(base::data.frame()))
        {
          base::print(result[base::seq(base::min(base::nrow(result), 1L),
                                        base::min(base::nrow(result), 6L)), ])
        } else
        {
          base::print(result)
        }
        base::cat("\n")
      }

      # Assign the result to the intermediate variable (unless null).
      if (!base::is.null(envir)
          && (base::tolower(intermediate) != "null"))
      {
        base::assign(intermediate, result, envir)
      }

    # If the result was 'No Data' (generated by RODBC in response to receiving
    # SQL_NO_DATA from the driver, after executing, say, drop table) or a zero-
    # zero-length character vector (sometimes produced by similar operations),
    # then return it invisibly.
    if (base::identical(result, "No Data")
        || base::identical(result, base::character(0L)))
    {
      base::return(base::invisible(result))
    }

    # Otherwise, return the result visibly.
    base::return(result)
    }
  }

  # There was actually no query (and, therefore, no result). If the result was
  # to have been assigned to some name, assign it the value NULL.
  if (!base::is.null(envir)
      && (base::tolower(intermediate) != "null"))
  {
    base::assign(intermediate, NULL, envir)
  }

  # Return NULL (signifying an undefined result, because there was no query).
  # SqrlSubmit(), and RODBC::sqlQuery() (to which it is a wrapper), are both
  # incapable of returning NULL (or NA). It doesn't actually matter whether or
  # not this is visible, since NULL is a special value; signifying to SqrlFile()
  # that the current overall result should not be replaced by this one.
  base::return(base::invisible(NULL))
}

SqrlValue <- function(datasource = "",
                      parameter = "",
                      set)
{
  # Output-safe (password obliterated) wrapper to SqrlParam().
  # Args:
  #   datasource : The name of data source, as known to SQRL.
  #   parameter  : The name of a SQRL or RODBC control parameter.
  #   set        : A value to assign to that parameter (optional).
  # Returns:
  #   The edited parameter value (with secrets kept secret).
  # SQRL Calls:
  #   SqrlParam(), SqrlParams(), SqrlValue() (self).
  # SQRL Callers:
  #   SqrlConfig(), SqrlDelegate(), SqrlSources(), SqrlValue() (self).
  # User:
  #   Has no direct access, but is able to supply (only) parameter and set via
  #   SqrlDelegate() and/or SqrlConfig(). The former vets parameter while the
  #   latter does not (although it will restrict parameter to being a string,
  #   and is write-only). Neither vets set. Both parameters are simply passed to
  #   SqrlParam(), and that function performs additional checking as required.
  #   All functions returning values to the user (outside of the SQRL namespace)
  #   should be sourcing their values from this, and not from SqrlParam().

  # This is the text with which secret information (pwd) is replaced.
  # Six asterisks have been chosen for consistency with RODBC.
  oblit <- "******"

  # A request for the (read-only) value of 'source' returns either the 'dsn'
  # parameter, or the 'connection' parameter, whichever defines the source,
  # with any placeholders substituted and any secrets obliterated.
  if (base::identical(parameter, "source"))
  {
    connection <- base::as.character(SqrlValue(datasource, "connection"))
    if (base::nchar(connection) > 0L)
    {
      for (spar in SqrlParams("substitutable"))
      {
        connection <- base::gsub(base::paste0("<", spar, ">"),
                                  SqrlValue(datasource, spar), connection)
      }
      base::return(connection)
    }
    dsn <- base::as.character(SqrlValue(datasource, "dsn"))
    # The conditional pasting below is as per RODBC::odbcConnect().
    dsn <- base::paste0("DSN=", dsn)
    if (SqrlParam(datasource, "uid", isdefined = TRUE)
        && (base::nchar(SqrlValue(datasource, "uid")) > 0L))
    {
      dsn <- base::paste0(dsn, ";UID=", SqrlValue(datasource, "uid"))
    }
    if (SqrlParam(datasource, "pwd", isdefined = TRUE)
        && (base::nchar(SqrlValue(datasource, "pwd")) > 0L))
    {
      dsn <- base::paste0(dsn, ";PWD=", SqrlValue(datasource, "pwd"))
    }
    base::return(dsn)
  }

  # Retrieve the parameter value, after setting it if so instructed.
  if (!base::missing(set))
  {
    if (base::nchar(datasource) < 1L)
    {
      value <- set
    } else
    {
      value <- SqrlParam(datasource, parameter, set)
    }
  } else
  {
    value <- SqrlParam(datasource, parameter)
  }

  # If the parameter is semi-secret (connection), it may contain secret (pwd)
  # values as substrings. In this case, locate and obliterate any secrets.
  if (parameter %in% SqrlParams("semi-secret"))
  {
    for (spar in SqrlParams("secret"))
    {
      pattern <- base::paste0("\\b", spar, "\\s*=")
      if (base::grepl(pattern, value, ignore.case = TRUE))
      {
        # Construct regular expression patterns for each of the non-secret
        # values (blank, <pwd>, and so on).
        ignorables <- SqrlParams("substitutable")
        ignorables <- ignorables[ignorables %in% SqrlParams("secret")]
        if (base::length(ignorables) > 0L)
        {
          ignorables <- base::paste0("\\s*<", ignorables, ">\\s*$")
        }
        ignorables <- base::paste0(pattern, base::c("\\s*$", ignorables))

        # Positions (first-character indices) and lengths of the (potential)
        # secret-containing sub-strings of the parameter-value string.
        ssubs <- base::gregexpr(base::paste0(pattern, "\\s*[^;]*"),
                                value, ignore.case = TRUE)[[1L]]
        slens <- base::c(0L, base::attr(ssubs, "match.length"))
        ssubs <- base::c(0L, ssubs)

        # Overwrite all non-ignorable (true) secrets with the replacement text.
        eds <- base::character(0L)
        for (i in base::seq(2L, base::length(ssubs)))
        {
          # Character positions (indices) within the value string; Start Of
          # Secret substring, End Of Secret substring, Start Of Previous
          # (non-secret) substring, End of Previous (non-secret) substring.
          sos <- ssubs[i]
          eos <- ssubs[i] + slens[i] - 1L
          sop <- ssubs[i - 1L] + slens[i - 1L]
          eop <- ssubs[i] - 1L

          # Isolate the potentially secret containing substring.
          ssub <- base::substring(value, sos, eos)

          # If the parameter value is apparently non-sensitive (ignorable),
          # then retain it unmodified (do not obliterate the value).
          ignore <- FALSE
          for (ignorable in ignorables)
          {
            if (base::grepl(ignorable, ssub, ignore.case = TRUE))
            {
              ignore <- TRUE
              break
            }
          }
          if (ignore)
          {
            eds <- base::c(eds, base::substring(value, sop, eos))

          # Otherwise, the sub-string contains potentially secret information.
          # Obliterate (replace) that information with the masking sequence.
          } else
          {
            pat <- base::paste0("(", spar, "\\s*=\\s*)[^;]+")
            eds <- base::c(eds, base::substring(value, sop, eop),
                            base::sub(pat, base::paste0("\\1", oblit),
                                      ssub, ignore.case = TRUE))
          }
        }

        # Append any final (trailing) non-secret sub-string.
        eds <- base::c(eds, base::substring(value,
                      ssubs[base::length(ssubs)] + slens[base::length(ssubs)]))
        value <- base::paste0(eds, collapse = "")
      }
    }
    base::return(value)
  }

  # If the parameter is secret, obliterate it entirely (unless it is empty).
  if (parameter %in% SqrlParams("secret"))
  {
    if (!base::nzchar(value))
    {
      base::return(value)
    }
    base::return(oblit)
  }

  # Otherwise (the parameter is non-secret), return the unmodified value.
  base::return(value)
}



########################################################### PUBLIC FUNCTIONS ###

sqrlAll <- function(...)
{
  # Sends the same command to each of the defined SQRL sources.
  # Args:
  #   ... : A sequence of strings, as per (to be supplied to) SqrlDelegate().
  # Returns:
  #   A (possibly invisible) list of the results of the command on each source.
  # SQRL Calls:
  #   SqrlAll(), SqrlParams().
  # User:
  #   Exported function. User has direct access. However, the argument(s) are
  #   unrestricted, and no checking is required (beyond that in SqrlDelegate()).

  # Return visibly when the command is a value request on either a single named
  # parameter or connection openness status.
  arglist <- base::list(...)
  if ((base::length(arglist) == 1L)
      && base::is.null(base::names(arglist))
      && (base::class(...) == base::class(base::character()))
      && (base::nchar(...) > 0L)
      && ((... %in% base::c(SqrlParams("all"), "source"))
          || base::grepl("^is\\s*open$", ...)))
  {
    base::return(SqrlAll(..., envir = base::parent.frame()))
  }

  # Apply the commands, return the results invisibly.
  base::return(base::invisible(SqrlAll(..., envir = base::parent.frame())))
}

sqrlInterface <- function(...)
{
  # Constructs a user-interface to a specified data source.
  # Args:
  #   ... : A source name and, optionally, a new interface name, in that order.
  # Returns:
  #   The name of the interface function to the specified source. When only a
  #   source name is supplied in the arguments, the function acts as a getter
  #   and returns the current interface name (or NULL when there is none). When
  #   both source and interface names are supplied, the new interface name is
  #   set before being returned. When the interface name is given as 'remove',
  #   no new interface is created, but any existing interface is deleted.
  # SQRL Calls:
  #   SqrlCache(), SqrlDefile(), SqrlInterface(), SqrlParam().
  # User:
  #   Exported function. User has direct access. The datasource name is checked
  #   for validity, but it is left to SqrlInterface() to establish the validity
  #   and usability of the interface name.

  # Either one or two arguments are expected.
  arglist <- base::list(...)
  if ((base::length(arglist) < 1L)
      || (base::length(arglist) > 2L))
  {
    base::stop("A source name and an interface name are expected.")
  }

  # Identify the data-source name and also the interface name (if specified).
  getname <- FALSE
  if (base::length(arglist) == 1L)
  {
    if (!base::is.null(base::names(arglist)))
    {
      datasource <- base::names(arglist)
      interface <- SqrlDefile("interface", arglist[[datasource]],
                              evaluate = FALSE)
    } else
    {
      datasource <- arglist[[1L]]
      getname <- TRUE
    }
  } else
  {
    datasource <- arglist[[1L]]
    interface <- SqrlDefile("interface", arglist[[2L]], evaluate = TRUE)
  }

  # Abort on non-existence of the specified data source.
  if ((base::class(datasource) != base::class(base::character()))
      || (base::length(datasource) != 1L)
      || (base::nchar(datasource) < 1L)
      || SqrlCache(datasource, exists = FALSE))
  {
    base::stop("Unrecognised data source.")
  }

  # In the absence of a specified interface name, get and return the name of the
  # current interface to the data source (returnsS NULL if none exists).
  if (getname)
  {
    base::return(SqrlParam(datasource, "interface"))
  }

  # Relay the arguments to SqrlInterface() (returns the new name invisibly).
  base::return(SqrlInterface(datasource, interface))
}

sqrlOff <- function()
{
  # Close SQRL (optionally also all other RODBC) channels, deactivate SQRL.
  # Args:
  #   None.
  # Returns:
  #   Invisible NULL, after closing channels and detaching SQRL.
  # SQRL Calls:
  #   SqrlOff().
  # User:
  #   Exported function. User has direct access, but there are no arguments.

  # Relay the command-option to SqrlOff() (returns invisible NULL).
  base::return(SqrlOff())
}

sqrlSource <- function(...)
{
  # Defines (or re-defines) a data source and its interface.
  # Args:
  #   ... : A source name and definition (string or file), in that order.
  # Returns:
  #   The interface name, invisibly, after creating, or re-defining, the source
  #   and its interface.
  # SQRL Calls:
  #   SqrlSource().
  # User:
  #   Exported function. User has direct access. Here, we ensure the existence
  #   of name and definition terms (in the form of multiple arguments, or at
  #   least one named argument). Additional checks are left to SqrlSource().

  # Abort unless we have at least a pair of terms (name, definition) or a
  # single named term (name = definition).
  def <- base::list(...)
  if ((base::length(def) < 2L)
      && base::is.null(base::names(def)))
  {
    base::stop("A name and definition are expected.")
  }

  # Pass the arguments to SqrlSource() (returns the interface name, invisibly).
  base::return(SqrlSource(...))
}

sqrlSources <- function(...)
{
  # Returns a summary table of defined data sources.
  # Args:
  #   ... : Argument to RODBC::odbcDataSources(), or empty (default).
  # Returns:
  #   A data frame summarising defined data sources. There is no guarantee that
  #   any of these are presently available, or even that they exist.
  # SQRL Calls:
  #   SqrlSources().
  # User:
  #   Exported function. User has direct access. Argument checking is required.

  # Ensure the argument is either omitted or takes one of the three allowed
  # values (strings). Each of the strings 'all', 'user', and 'system' cause
  # RODBC::odbcDataSources() to (re)import the corresponding set of local DSNs.
  # Omitting the argument simply returns the existing SQRL data source
  # definitions, without (re)importing DSNs.
  import <- base::list(...)
  if (base::length(import) == 0L)
  {
    import <- ""
  } else if ((base::length(import) == 1L)
              && (base::identical(import[[1L]], "all")
                  || base::identical(import[[1L]], "user")
                  || base::identical(import[[1L]], "system")
                  || base::identical(import[[1L]], "remove")))
  {
    import <- import[[1L]]
  } else
  {
    base::stop("Argument should be 'all', 'user', 'system', or 'remove'.")
  }

  # Pass to SqrlSources(), return the summary.
  base::return(SqrlSources(import))
}



###################################################### PRIVATE LOAD / UNLOAD ###

.onLoad <- function(libname = "",
                    pkgname = "")
{
  # Create data source interfaces within a public environment, on SQRL load.
  # Args:
  #   libname : The name of the package's directory, within the R library.
  #   pkgname : The name of the package.
  # Returns:
  #   Invisible NULL.
  # SQRL Calls:
  #   SqrlDSNs(), SqrlHelp(), SQRL:Face.

  # Attach a public environment, SQRL:Face, for holding data source interfaces
  # where the user can see them (on the R search path). The user will be able to
  # assign and modify objects within this environment (we would prefer that they
  # didn't, but must allow for the possibility). It doesn't seem possible to
  # attach a SQRL environment (such as srqlHaus), only a copy of one (with the
  # name attribute added to it).
  if (!("SQRL:Face" %in% base::search()))
  {
    base::attach(base::new.env(parent = base::emptyenv()), name = "SQRL:Face")
  }

  # Look for data source names (DSNs). Create an interface for each.
  SqrlDSNs("all")

  # Initiate an empty temp-file vector within the help environment.
  SqrlHelp(clean = TRUE)

  # Return invisible NULL.
  base::return(base::invisible(NULL))
}

.onUnload <- function(libpath = "")
{
  # Detaches the SQRL:Face environment whenever the SQRL package is unloaded.
  # Args:
  #   libpath : The complete path to the package.
  # Returns:
  #   Invisible NULL.
  # SQRL Calls:
  #   SqrlHelp(), SQRL:Face.

  # Remove any SQRL temp files from the R-session temp directory.
  SqrlHelp(clean = TRUE)

  # Attempt to detach the public SQRL:Face environment, if it not already done.
  if ("SQRL:Face" %in% base::search())
  {
    base::try(base::detach("SQRL:Face"))
  }

  # Return invisible NULL.
  base::return(base::invisible(NULL))
}



######################################################################## EOF ###
