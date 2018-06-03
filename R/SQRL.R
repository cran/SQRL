####################################################################### SQRL ###

# Wrapper to RODBC (SQRL provides only tools of convenience).
# Supports parameterised multi-statement SQL scripts with embedded R.
# Automatic generation of like-named user-interfaces to data sources.
# Manages multiple simultaneous connections and their communication parameters.
# Protects connection handles from rm(list = ls(all.names = TRUE)).
# Provides visual indication of which connections/channels are open.
# Provides visual indication of queries and fetches in progress (Windows only).
# Automatically attempts recovery after an unexpected loss of connection.
# Supports source/communications-defining configuration files.
# Promotes ODBC exceptions to local R errors (stops).
# Performs implicit concatenation (pasting).
# Works best in Rgui on Windows.

# Mike Lee, South Titirangi, 9 April 2018.



#################################################################### HISTORY ###

# 4 June 2018.
# No longer supplies the default UID when connecting via DSN, as this overrides
# any possible UID already contained within the DSN. SQL submissions to sources
# now return invisibly when the result is not a data frame (such as that of 'use
# database'). Added interface()$ispoen and interface()$source.

# 16 April 2018. CRAN v0.3.0.
# Implemented run-time-generated help for interface functions. Activated source
# deregistration and parameter resetting. Allowed configuration files to specify
# the interface function. Improved password protection. Support for <R> ... <do>
# in SQRL scripts. Differentiation of query-in-progress from fetch-in-progress
# in window-title activity indicators. Enabled interface()$parameter.

# 10 March 2018. CRAN v0.2.1.
# Allowed drivers to be defined as file paths (as they can be on GNU/Linux).

# 8 March 2018. CRAN v0.2.0.
# Added kwarg support, and the ability to explicitly pass parameters to .sqrl
# scripts. Complete documentation overhaul.

# 8 January 2018. CRAN v0.1.1.
# Patched help files for compliance with R-devel-2018-01-05 (upcoming R-3.5).
# (Specifically, removed active example calls of SqrlOff().)

# 12 November 2017. CRAN v0.1.0.
# Complete re-write for R-3.2+ (began on 30 August 2017). Dropped DBMS-specific
# features and removed the need for ahead-of-time knowledge of sources. Changed
# connection markers to invisible by default (since visibility requires altering
# the user's prompt global option).

# 15 April 2014 -- 12 June 2014. Packaged prototype.
# Packaged (but not published). Added support for multi-statement SQL files with
# embedded R. Introduced stripping of SQL comments prior to submission (on the
# DBMS of the time, too many leading comments led to rejection of the query).

# 7 January 2014. Script prototype.
# R-2. Original (unpackaged script) prototype. Primary objectives were to
# protect RODBC handles from rm(list = ls(all = TRUE)), and to provide named
# interfaces and visual indication of connection status for multiple sources.
# Secondary features required specific versions of SQL, and advanced (ahead of
# time) domain-specific knowledge of the available data sources.



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
  #   Has no direct access, unable to pass arguments indirectly. Argument
  #   validity checks are not required.

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
  # its interface, delete its cache (this complete deregistration from SQRL),
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
    if (base::length(cachenames) < 1)
    {
      base::return(base::character(0))
    }
    is.cache <- base::sapply(base::as.list(cachenames),
                                function(x) base::exists(x, srqlHaus,
                                                          mode = "environment",
                                                          inherits = FALSE))
    cachenames <- cachenames[is.cache]
    base::return(base::substring(cachenames, base::nchar(".!") + 1))
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
  # channel somehow survived the close attempt, this makes it unusable.
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
  if (base::nchar(confile) < 1)
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
    if (base::nchar(first) < 1)
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
    param <- base::trimws(base::substring(lyne, 1, pos - 1))
    value <- base::trimws(base::substring(lyne, pos + base::nchar("=")))

    # Retain the (name, value) pair, provided the name is not blank, and not
    # 'channel' (blocks setting/overwriting of this parameter), and the value
    # is also not blank.
    if ((base::nchar(param) > 0)
        && (base::nchar(value) > 0)
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
  for (parameter in base::names(config)[base::names(config) != "interface"])
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
    "rows_at_time"        = 100,
    "readOnlyOptimize"    = FALSE,

    # Parameters for RODBC::sqlQuery().
    # Also uses believeNRows and rows_at_time, as above.
    "channel"             = NULL,
    "errors"              = TRUE,
    "as.is"               = FALSE,
    "max"                 = 0,
    "buffsize"            = 1000,
    "nullstring"          = NA_character_,
    "na.strings"          = "NA",
    "dec"                 = base::as.character(base::getOption("dec")),
    "stringsAsFactors"    = base::default.stringsAsFactors(),

    # Parameters for SQRL.
    "*"                   = base::objects(cacheenvir, all.names = TRUE),
    "driver"              = "",
    "interface"           = NULL,
    "name"                = datasource,
    "ping"                = NULL,
    "prompt"              = base::substr(datasource, 1, 1),
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
  #   SqrlConfig(), SqrlDefile() (self), SqrlDelegate(), SqrlSource().
  # User:
  #   Has no direct access, but is able to supply (only) parameter and value via
  #   SqrlParam() from SqrlDelegate() and/or SqrlConfig(). The parameter is
  #   guaranteed to be a string, and no further checks are required. The value
  #   may turn out to be unsuitable, but that is left for SqrlParam() to decide.

  # Return the unmodified value, if it is not of character type (SqrlPath()
  # doesn't like NULL input) or if it is the empty string (evaluates to NULL).
  if ((base::class(value) != base::class(base::character()))
      || base::identical(value, ""))
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
    param <- base::trimws(base::substring(lyne, 1, pos - 1))

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
  # else. Returns the configuration invisibly, enabling interface()$parameter.
  if (args.count == 0)
  {
    isopen <- SqrlIsOpen(datasource, besure = TRUE)
    if (!isopen)
    {
      SqrlOpen(datasource)
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
      || base::all(base::nchar(args.names) == 0))
  {
    # If the command specifies a file path, try sourcing SQL from that file.
    file.path <- SqrlPath(...)
    if (!base::is.null(file.path))
    {
      base::return(SqrlFile(datasource, file.path, envir = envir))
    }

    # Extract the first word from the first supplied argument.
    first.word <- base::sub("^[^[:graph:]]*([[:graph:]]+).*$", "\\1",
                            args.list[[1]])[1]

    # If the first word looks like standard SQL, submit the unaltered command.
    if (base::tolower(first.word) %in% SqrlParams("sql-keywords"))
    {
      base::return(SqrlSubmit(datasource, ...))
    }

    # If the first supplied argument contains more than one word, the other
    # words consist of everything except the first word (pasted together).
    if (base::grepl("[[:graph:]]+[^[:graph:]]+[[:graph:]]+", args.list[[1]]))
    {
      other.words <- base::trimws(base::sub(first.word, "",
                          base::paste(args.list, collapse = ""), fixed = TRUE))
      only.word <- ""

    # Otherwise (the first supplied argument is a single word), if only one
    # argument was supplied, then the (that) first word is the only word.
    } else if (args.count == 1)
    {
      only.word <- first.word
      other.words <- ""

    # Otherwise, if precisely two arguments were supplied, then the other words
    # are the second argument verbatim (could be any object, not just a string).
    } else if (args.count == 2)
    {
      only.word <- ""
      other.words <- args.list[[2]]

    # Otherwise, the other words consist of all the supplied arguments besides
    # the first (paste these together).
    } else
    {
      only.word <- ""
      other.words <- base::paste(
                              args.list[base::seq(2, base::length(args.list))],
                              collapse = "")
    }

    # If the only word is 'close', close the data source channel.
    if ("close" == only.word)
    {
      base::return(SqrlClose(datasource))
    }

    # If the first word is 'columns', call RODBC::sqlColumns() on the remainder.
    if ("columns" == first.word)
    {
      SqrlOpen(datasource)
      base::return(RODBC::sqlColumns(channel = SqrlParam(datasource, "channel"),
                                      sqtable = other.words,
                                      errors = SqrlParam(datasource, "errors"),
                                      as.is = TRUE))
    }

    # If the first word is 'config', get or set the configuration.
    if ("config" == first.word)
    {
      base::return(SqrlConfig(datasource, other.words))
    }

    # If the first word is 'help', or some multiple of '?'. then provide help.
    if (("help" == first.word)
        || base::grepl("^[?]+$", first.word))
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
      base::return(SqrlInterface(datasource, other.words))
    }

    # If the only word is 'isopen' (or if words one and two are 'is open'),
    # return the channel's open status (TRUE for open, FALSE otherwise). This
    # calls with besure = TRUE, to ping the source and make certain of the
    # openness status.
    if (("isopen" == only.word)
        || (("is" == first.word)
            && ("open" == other.words)))
    {
      base::return(SqrlIsOpen(datasource, besure = TRUE))
    }

    # If the only word is 'open', open a channel to the specified data source.
    if ("open" == only.word)
    {
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

    # If the only word is 'tables', call RODBC::sqlTables() on the data source.
    if ("tables" == only.word)
    {
      SqrlOpen(datasource)
      base::return(RODBC::sqlTables(channel = SqrlParam(datasource, "channel"),
                                      errors = SqrlParam(datasource, "errors"),
                                      as.is = TRUE))
    }

    # If the first word is 'typeinfo', call RODBC::sqlTypeInfo() on the others.
    if ("typeinfo" == first.word)
    {
      SqrlOpen(datasource)
      type <- base::ifelse(first.word == only.word, "all", other.words)
      info <- RODBC::sqlTypeInfo(channel = SqrlParam(datasource, "channel"),
                                  type = type,
                                  errors = SqrlParam(datasource, "errors"),
                                  as.is = TRUE)
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
      if (first.word == "channel")
      {
        base::stop("Parameter is read-only.")
      }

      # Set the parameter's value to the supplied other words, then return the
      # (secrets-obscured) value, invisibly.
      value <- SqrlDefile(first.word, other.words, evaluate = TRUE)
      base::return(base::invisible(SqrlValue(datasource, first.word, value)))
    }

    # Otherwise, submit the original unaltered command.
    base::return(SqrlSubmit(datasource, ...))
  }

  # When all arguments are named, interpret each name as that of a parameter,
  # and assign each value accordingly. The name 'reset' is a special case
  # (reset parameters to default values).
  if (base::all(base::nchar(args.names) > 0))
  {
    result <- base::list()
    for (param in args.names)
    {
      if (param == "config")
      {
        result[[param]] <- SqrlConfig(datasource, args.list[[param]])
      } else if (param == "interface")
      {
        result[[param]] <- SqrlInterface(datasource, args.list[[param]])
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
    if (base::length(result) < 2)
    {
      result <- base::unlist(result)
    }
    base::return(base::invisible(result))
  }

  # When both named and unnamed arguments exist, and all named arguments trail
  # all unnamed arguments, then interpret the unnamed arguments as the path of
  # a SQRL script, and the named arguments as parameters of that script.
  args.kindex <- base::which(base::nchar(args.names) > 0)[1]
  if (base::all(base::nchar(args.names[args.kindex:args.count]) > 0))
  {
    file.path <- SqrlPath(base::unlist(args.list[base::seq((args.kindex - 1))]))
    if (base::is.null(file.path))
    {
      base::stop("File not found.")
    }
    params <- args.list[args.kindex:args.count]
    base::return(SqrlFile(datasource, file.path, envir, params))
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
      SqrlInterface(datasource, vital = FALSE)
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
                      params = NULL)
{
  # Read a SQRL-script file and submit its content to a data source.
  # Args:
  #   datasource  : The name of a known data source.
  #   script.file : The file name (or path), as a string.
  #   envir       : An R environment (script is executed in a child of this).
  #   params      : A named list of R parameters for the script.
  # Returns:
  #   Result of submitting the script.
  # SQRL Calls:
  #   SqrlParam(), SqrlStatement(), SqrlSubScript().
  # SQRL Callers:
  #   SqrlDelegate().
  # User:
  #   Has no direct access, but is able to submit (only) the script.file
  #   argument (only) via SqrlDelegate(). That function verifies the existence
  #   of a file at script.file. No further checks are required.

  # Read the entirety of the script within script.file (slurp all lines).
  # No ordinary script would be so large that slurping would be a problem.
  script <- base::paste(base::readLines(script.file, warn = FALSE,
                                              skipNul = TRUE), collapse = "\n")

  # Script delimiter definitions (regular expression patterns).
  patterns <- base::list()
  patterns$tag.r.begin     <- "<r>"
  patterns$tag.r.end       <- "</r>"
  patterns$tag.do          <- "<do>"
  patterns$tag.stop        <- "<stop>"
  patterns$tag.result      <- "<result\\s*->\\s*[[:graph:]]+>"
  patterns$comment.begin   <- "/\\*"
  patterns$comment.end     <- "\\*/"
  patterns$comment.line    <- "--"
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
                                script, ignore.case = TRUE)[[1]]
    positions <- base::as.integer(matches)
    if ((base::length(positions) > 1)
        || (positions > 0))
    {
      pos <- base::c(pos, positions)
      pat <- base::c(pat, base::rep(pattern, base::length(positions)))
      len <- base::c(len, base::attr(matches, "match.length"))
    }
  }

  # Sort the delimiters (if any exist) into ascending (script) positional order.
  if (base::length(pos) > 1)
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

  # Default result. This function will return the last non-empty result.
  result <- base::character(0)

  # The SQL statement in progress (the script may contain multiple statements).
  statement <- base::list()

  # Delimiter counter/index (to pos, pat, and len). Range is [1 : num.delims].
  i <- 1

  # Character counter/index (to script). Range is [1 : nchar.script].
  k <- 1

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
      # Isolate any unappended (to the statement) script preceding this comment.
      phrase <- base::substring(script, k, pos[i] - 1)

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
      if (base::nchar(phrase) > 0)
      {
        statement <- base::append(statement, phrase)
      }

      # Scan through the subsequent script delimiters, until the comment
      # concludes with either an end-of-file, or appropriate delimiter.
      end.marker <- base::switch(pat[i],
                                  comment.line = "end.of.line",
                                  comment.begin = "comment.end")
      i <- i + 1
      while ((i <= num.delims)
              && (pat[i] != end.marker))
      {
        i <- i + 1
      }

      # Reposition the start-of-phrase index immediately after the end of the
      # comment. When the comment ends with a newline, the index is placed on
      # that newline (so that the next phrase will begin with the newline).
      k <- base::ifelse(i <= num.delims,
            base::ifelse(end.marker == "end.of.line", pos[i], pos[i] + len[i]),
            nchar.script + 1)

      # Advance to the next script delimiter.
      i <- i + 1
    }

    # Incorporate (single & double) quote-enclosed strings verbatim within SQL.
    # That is; ignore anything that looks like a delimiter, but is in a string.
    if ((i <= num.delims)
        && (pat[i] %in% base::c("quote.single", "quote.double")))
    {
      # Isolate any unappended (to the statement) script preceding this string.
      phrase <- base::substring(script, k, pos[i] - 1)

      # Remove trailing whitespace from each internal line of the phrase.
      phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

      # Remove vertical whitespace from within the phrase.
      phrase <- base::gsub("\n+", "\n", phrase)

      # Append the phrase to the statement (unless the phrase is empty).
      if (base::nchar(phrase) > 0)
      {
        statement <- base::append(statement, phrase)
      }

      # Reposition the start-of-phrase index on (including) the beginning quote.
      k <- pos[i]

      # Scan through the subsequent script delimiters, until the string
      # concludes with either an end-of-file, or matching quote delimiter.
      # We only test for \ escaped quotes here (once already in quote mode).
      # Some SQLs use doubled quotes within quotes to represent quote literals.
      # Such abominations are supported by this parser, at least in all testing
      # thus far, since 'x''' is read (here) as two strings: 'x' and '', which
      # which are later collapsed together with "", restoring 'x''' in the SQL.
      closing.quote <- pat[i]
      i <- i + 1
      while ((i <= num.delims)
              && ((pat[i] != closing.quote)
                  || (base::substring(script, pos[i] - 1, pos[i] - 1) == "\\")))
      {
        i <- i + 1
      }

      # Append the quoted string to the statement. Verbatim, quotes included.
      statement <- base::append(statement, base::ifelse(i <= num.delims,
                                            base::substring(script, k, pos[i]),
                                            base::substring(script, k)))

      # Position the start-of-phrase index immediately after the closing quote.
      k <- base::ifelse(i <= num.delims, pos[i] + len[i], nchar.script + 1)

      # Advance to the next script delimiter.
      i <- i + 1
    }

    # Ignore remainder of script when encountering a 'stop' tag within SQL.
    # The 'stop' tag is mainly used to run partial scripts while bug hunting.
    if ((i <= num.delims)
        && (pat[i] == "tag.stop"))
    {
      # Isolate any unappended (to the statement) script preceding this stop.
      phrase <- base::substring(script, k, pos[i] - 1)

      # Remove trailing whitespace (including vertical) from the phrase.
      phrase <- base::sub("[[:space:]]*$", "", phrase)

      # Remove trailing whitespace from each internal line of the phrase.
      phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

      # Remove vertical whitespace from within the phrase.
      phrase <- base::gsub("\n+", "\n", phrase)

      # Append the phrase to the statement (unless the phrase is empty).
      if (base::nchar(phrase) > 0)
      {
        statement <- base::append(statement, phrase)
      }

      # Advance the delimiter and phrase indices beyond the end of the script.
      # Break immediately (unnecessary). Statement will be submitted after.
      i <- num.delims + 1
      k <- nchar.script + 1
      break
    }

    # Submit statement (and retrieve result) on encountering an end-of-statement
    # marker (either a semi-colon or a 'do' tag) within SQL.
    if ((i <= num.delims)
        && (pat[i] %in% base::c("semi.colon", "tag.do")))
    {
      # Isolate any unappended (to the statement) script preceding the marker.
      # Include the semi-colon itself, but do not include the 'do' tag.
      phrase <- base::ifelse(pat[i] == "semi.colon",
                              base::substring(script, k, pos[i] + len[i] - 1),
                              base::substring(script, k, pos[i] - 1))

      # Submit the statement (with phrase) and pull the result.
      dat <- SqrlSubScript(datasource, statement, phrase)

      # If there was a result (there was a query), replace the overall result.
      if (!base::is.null(dat))
      {
        result <- dat
      }

      # Reset the statement (begin the next one afresh).
      statement <- base::list()

      # Reposition the start-of-phrase index immediately after the semi-colon.
      k <- pos[i] + len[i]

      # Advance to the next script delimiter.
      i <- i + 1
    }

    # Evaluate embedded R (and insert into SQL or produce a result).
    if ((i <= num.delims)
        && pat[i] %in% base::c("tag.r.begin", "tag.result"))
    {
      # Isolate any unappended (to the statement) script preceding this tag.
      phrase <- base::substring(script, k, pos[i] - 1)

      # If this is R post-processing, submit any query beforehand.
      r.type <- pat[i]
      if (r.type == "tag.result")
      {
        # Extract the name of the intermediate variable to which the SQL result
        # is to be assigned within the R processing environment.
        intermediate <- base::gsub("^<result\\s*->\\s*|>$", "",
                          base::substring(script, pos[i], pos[i] + len[i] - 1))

        # Submit the statement (with phrase) and pull the result.
        dat <- SqrlSubScript(datasource, statement, phrase,
                                intermediate, sqrl.env)

        # If there was a result (there was a query), replace the overall result.
        if (!base::is.null(dat))
        {
          result <- dat
        }

        # Reset the statement (begin the next one afresh).
        statement <- base::list()

      # Otherwise (this is an R substitution into SQL), clean the phrase (but do
      # not remove pre-tag trailing whitespace) and append it to the statement.
      # The phrase will never contain a string literal.
      } else
      {
        # Remove trailing whitespace from each internal line of the phrase.
        phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

        # Remove vertical whitespace from within the phrase.
        phrase <- base::gsub("\n+", "\n", phrase)

        # Append the phrase to the statement (unless the phrase is empty).
        if (base::nchar(phrase) > 0)
        {
          statement <- base::append(statement, phrase)
        }
      }

      # Reposition the start-of-phrase index immediately after this tag.
      k <- pos[i] + len[i]

      # Remove any SQL comments within the R section (do not clean-up script).
      rscript <- base::list()
      i <- i + 1
      while ((i <= num.delims)
              && (!(pat[i] %in% base::c("tag.r.end", "tag.do"))))
      {
        # Remove SQL comments from R (both to-end-of-line and block).
        # This is merely so that we can use SQL comments within the R (looks
        # better under SQL syntax highlighting rules within your text editor).
        if ((i <= num.delims)
            && (pat[i] %in% base::c("comment.line", "comment.begin")))
        {
          # Isolate any unappended script preceding this comment, and append it
          # to the R-script.
          rscript <- base::append(rscript,
                                  base::substring(script, k, pos[i] - 1))

          # Scan through the subsequent script delimiters, until the comment
          # concludes with either an end-of-file, or appropriate delimiter.
          end.marker <- base::switch(pat[i],
                                      comment.line = "end.of.line",
                                      comment.begin = "comment.end")
          i <- i + 1
          while ((i <= num.delims)
                  && (pat[i] != end.marker))
          {
            i <- i + 1
          }

          # Reposition the start-of-phrase index immediately after the end of
          # the comment. When the comment ends with a newline, the index is
          # placed on that newline (the next phrase will begin with newline).
          k <- base::ifelse(i <= num.delims,
                              base::ifelse(end.marker == "end.of.line",
                                            pos[i], pos[i] + len[i]),
                              nchar.script + 1)
        }

        # Skip over (single, double) quote-enclosed strings (include verbatim).
        # (Ignore anything that looks like a delimiter, but is inside a string.)
        if ((i <= num.delims)
            && (pat[i] %in% base::c("quote.single", "quote.double")))
        {
          # Since we're not cleaning-up the R script, we merely scan through the
          # script delimiters until reaching the end of the quote (ignore all
          # other delimiters found in between). Appending to the R script will
          # only occur later (at a comment, or an end-of-R delimiter). We only
          # test for \ escaped quotes here (once already within quote mode).
          closing.quote <- pat[i]
          i <- i + 1
          while ((i <= num.delims)
                  && ((pat[i] != closing.quote)
                      || (base::substring(script, pos[i] - 1, pos[i] - 1)
                          == "\\")))
          {
            i <- i + 1
          }
        }

        # Ignore remainder of script when encountering a 'stop' tag within an
        # R post-processing section.
        if ((i <= num.delims)
            && (pat[i] == "tag.stop"))
        {
          # Append the last chunk of R (before the stop tag).
          rscript <- base::append(rscript,
                                    base::substring(script, k, pos[i] - 1))

          # Ignore (skip over) everything else in the script.
          i <- num.delims + 1
          k <- nchar.script + 1
          break
        }

        # Advance to the next script delimiter.
        i <- i + 1
      }

      # Append the final chunk to the R-script.
      phrase <- base::ifelse(i <= num.delims,
                              base::substring(script, k, pos[i] - 1),
                              base::substring(script, k))
      rscript <- base::append(rscript, phrase)

      # Collapse the rscript (list) to a single string.
      rscript <- base::trimws(base::paste(rscript, collapse = ""))

      # In the case of embedded R, evaluate and append to the encasing SQL.
      if ((r.type == "tag.r.begin")
          && (i <= num.delims)
          && (pat[i] == "tag.r.end"))
      {
        rvalue <- base::eval(base::parse(text = rscript), sqrl.env)
        statement <- base::append(statement, SqrlStatement(rvalue))

      # Otherwise (R post-processing), evaluate and retain the result.
      } else
      {
        # Stop if there's any unsubmitted SQL before an <R> ... <do> section.
        # (SQL is always submitted before a <result> ... <do> section.)
        if (base::any(base::grepl("[[:graph:]]", base::unlist(statement))))
        {
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
        parsed <- base::parse(text = rscript)
        if (!base::identical(base::as.character(parsed), base::character()))
        {
          # Evaluate the script, retain the result.
          result <- base::eval(parsed, sqrl.env)

          # If verbose, output (some of) the result.
          if (verbose)
          {
            if (base::length(base::dim(result)) == 2)
            {
              base::print(result[
                            base::seq(1, base::min(base::nrow(result), 10)), ])
            } else if (base::length(result) > 0)
            {
              base::print(result[
                            base::seq(1, base::min(base::length(result), 10))])
            } else if (base::length(result) == 0)
            {
              base::print(result)
            }
            base::cat("\n")
          }
        }
      }

      # Reposition the start-of-phrase index immediately after this R section.
      k <- base::ifelse(i <= num.delims, pos[i] + len[i], nchar.script + 1)

      # Advance to the next script delimiter.
      i <- i + 1
    }

    # Take no special action at an end-of-line (advance to next delimiter).
    if ((i <= num.delims)
        && (pat[i] == "end.of.line"))
    {
      i <- i + 1
    }
  }

  # Isolate any remaining unappended (to the statement) script.
  phrase <- base::substring(script, k)

  # Submit the statement (with phrase) and pull the result.
  dat <- SqrlSubScript(datasource, statement, phrase)

  # If there was a result (there was a query), replace the overall result.
  if (!base::is.null(dat))
  {
    result <- dat
  }

  # Return whatever the last result was (from SQL query or R post-processing).
  base::return(result)
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
      base::return(base::assign("temps", base::character(), srqlHelp))
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
  csc <- base::character()
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
                          base::gregexpr(indic, windowtitle, fixed = TRUE)[[1]])
          before <- base::sub("\\s+$", "",
                                base::substring(windowtitle, 1, position - 1))
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
      if (base::nchar(indic) > 0)
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
  #   function. The interface parameter is guaranteed to be a character string
  #   (singleton), but it is not assured to be usable (that is checked here).

  # This is the user-interface function-body definition for the data source.
  uibody <- base::paste0("function(...) {base::return(SqrlDelegate(\"",
                        datasource, "\", ..., envir = base::parent.frame()))}")

  # Isolate the previous interface (NULL when no interface was defined).
  preface <- SqrlParam(datasource, "interface")

  # Remove leading and trailing whitespace from the interface argument.
  interface <- base::trimws(interface)

  # On a request to delete the data source's interface, if we can confirm the
  # interface object retains its original SQRL definition, then we delete that
  # object. Either way, the interface is deregistered in the data source's
  # cache, and an invisible NULL is returned.
  if (base::identical(interface, "remove"))
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
  if (base::nchar(interface) < 1)
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
      && (base::nchar(ping) > 0))
  {
    # Submit the ping.
    response <- base::try(SqrlSubmit(datasource, ping, throw = TRUE,
                                      retry = FALSE), silent = TRUE)

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
  if (base::nchar(connection) > 0)
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
      index <- base::which(matches)[1]
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
  #   SqrlSubScript(), sqrlInterface().
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
    if (base::length(params) < 1)
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
      SqrlInterface(datasource, vital = FALSE)
      params <- params[params != "interface"]
      if (base::length(params) < 1)
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
    # Checks to move to user end functions once this fun hidden from user.
    if ((parameter %in% SqrlParams("write-protected"))
        && base::exists(parameter, cacheenvir, inherits = FALSE))
    {
      base::stop("Parameter is write-protected.")
    }

    # Prevent changing RODBC::odbcConnect() parameters while connection is open.
    # (Because those changes would only take effect on opening a new channel.)
    # The override condition allows SqrlOpen() to alter some of these (to values
    # the user may have entered) when the connection channel is first opened.
    if (!override
        && (parameter %in% SqrlParams("locked-while-open"))
        && SqrlIsOpen(datasource))
    {
      base::stop("Parameter is locked while connection is open.")
    }

    # Prevent overwriting (changing) the channel while it is open, with the
    # exception that a channel can be nullified (forced closed) at any time.
    if ((parameter == "channel")
        && !base::is.null(set)
        && SqrlIsOpen(datasource))
    {
      base::stop("Channel cannot be changed while open.")
    }

    # Coerce set to the appropriate data type for the specified parameter.
    # Firstly, parameters that are logically-valued.
    if (parameter %in% SqrlParams("boolean-type"))
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
      if ((base::length(set) != 1)
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
        if ((base::length(set) != 1)
            || base::is.na(set))
        {
          base::stop("New parameter value is not a character string.")
        }
      }

    # The channel parameter can be either NULL, or of RODBC class.
    } else if (parameter %in% SqrlParams("rodbc/null-type"))
    {
      if (!base::is.null(set)
          && (base::class(set) != "RODBC"))
      {
        base::stop("New parameter value is not a connection handle.")
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
        if ((base::length(set) > 2)
            || base::any(base::is.na(set)))
        {
          base::stop("New parameter value is not a quotation specifier.")
        }
      }

    # The nullstring parameter is a character string, possibly NA_character_.
    } else if (parameter %in% SqrlParams("string/na-type"))
    {
      set <- base::suppressWarnings(base::as.character(set))
      if ((base::length(set) != 1))
      {
        base::stop("New parameter value is not a character string.")
      }

    # Parameters that are (non-NA) character-strings. (And if we still haven't
    # had a match on our parameter after this, its set value is left as is.)
    } else if (parameter %in% SqrlParams("string-type"))
    {
      set <- base::suppressWarnings(base::as.character(set))
      if ((base::length(set) != 1)
          || base::is.na(set))
      {
        base::stop("New parameter value is not a character string.")
      }
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
      # as well attempt to extract some other parameter values, too.
      for (param in SqrlParams("scrapeable-string"))
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
      # If a DSN was specified without a driver, assume the driver associated
      # with the DSN (provided that DSN actually has a local definition).
      if (base::grepl("dsn\\s*=", set, ignore.case = TRUE)
          && !base::grepl("driver\\s*=", set, ignore.case = TRUE))
      {
        sources <- RODBC::odbcDataSources("all")
        dsn <- SqrlParam(datasource, "dsn")
        if (dsn %in% base::names(sources))
        {
          SqrlParam(datasource, "driver", sources[dsn], override)
        }
      }
      # Set the (unaltered) connection string, return invisibly.
      base::assign(parameter, set, cacheenvir)
      base::return(base::invisible(set))
    }

    # The dsn parameter is a special case, because we also reset the connection
    # parameter unless the connection string contains a '<dsn>' placeholder.
    if (parameter == "dsn")
    {
      if (!base::grepl("<dsn>", SqrlParam(datasource, "connection")))
      {
        SqrlParam(datasource, "reset", "connection", override)
      }
      base::assign(parameter, set, cacheenvir)
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

  # A cached parameter value exists. Retrieve and return it.
  if (base::exists(parameter, cacheenvir, inherits = FALSE))
  {
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
  #   SqrlOpen(), SqrlParam(), SqrlSource(), SqrlSources(), sqrlAll().
  # User:
  #   Has no direct access, and is unable to supply the argument. Validity
  #   checks are not required.

  # Parameter-group definitions (find and return).
  base::return(base::switch(group,

    # All parameter names, whether RODBC or SQRL, except for 'interface'.
    "all"                   = base::c("as.is",
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

    # Parameters of Boolean-singleton type (TRUE/FALSE, not NA).
    "boolean-type"          = base::c("believeNRows",
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

    # Parameters of integer-singleton type (not NA).
    "integer-type"          = base::c("buffsize",
                                      "max",
                                      "rows_at_time"),

    # Parameters that cannot be changed while the connection channel is open.
    "locked-while-open"     = base::c("believeNRows",
                                      "case",
                                      "colQuote",
                                      "connection",
                                      "driver",
                                      "DBMSencoding",
                                      "dsn",
                                      "interpretDot",
                                      "pwd",
                                      "readOnlyOptimize",
                                      "rows_at_time",
                                      "tabQuote",
                                      "uid"),

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
                                      "uid",
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
                                      "uid",
                                      "prompt",
                                      "pwd",
                                      "name",
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
  if ((base::length(filepath) != 1)
      || (base::nchar(filepath) < 1))
  {
    base::return(NULL)
  }

  # If filepath actually does point to a readable file, return the (normalised)
  # path. Note that files '.' and '..' exist as directories, and that file '"'
  # exists but is not read accessible (the '4' tests for read access).
  if (base::file.exists(filepath)
      && (base::file.access(filepath, 4) == 0)
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
    pings <- pings[base::c(3, 2, 1)]
  } else if (base::grepl("db2", driver, fixed = TRUE))
  {
    pings <- pings[base::c(2, 1, 3)]
  }

  # Try each of the pings, in (driver-dependent) order of decreasing preference,
  # until we find a ping that works (is valid SQL for the data source). These
  # could also fail if the connection has been unexpectedly closed.
  for (ping in pings)
  {
    a <- base::try(SqrlSubmit(datasource, ping, throw = TRUE, retry = FALSE),
                    silent = TRUE)
    if (!base::inherits(a, "try-error"))
    {
      base::return(SqrlParam(datasource, "ping", ping))
    }
  }

  # Did not find a ping that works. Set and return the empty string.
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
  arglist <- base::as.list(base::match.call())[-1]

  # If no arguments were supplied, return an empty statement.
  if (base::length(arglist) == 0)
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
  #   Invisible NULL, after creating, or re-defining, the source and interface.
  # SQRL Calls:
  #   SqrlCache(), SqrlConfig(), SqrlDefile(), SqrlFace(), SqrlInterface(),
  #   SqrlParam(), SqrlParams(), SqrlPath().
  # SQRL Callers:
  #   sqrlSource().
  # User:
  #   Has no direct access. Can supply all arguments via sqrlSource() (only).
  #   That function guarantees the existence of either at least two unnamed
  #   terms, or of at least one named term. Additional checks (assignability,
  #   conflict, etc.) are performed here.

  # Separate the name from the definition component(s).
  def <- base::list(...)
  if (base::length(def) == 1)
  {
    name <- base::trimws(base::names(def))
    base::names(def) <- NULL
  } else
  {
    name <- base::trimws(def[[1]])
    def[[1]] <- NULL
  }

  # If the name is 'remove', treat the definition as a list of names of sources
  # to be removed (deregistered from SQRL). Do that, then return invisible NULL.
  if (name == "remove")
  {
    for (datasource in base::unlist(def))
    {
      SqrlCache(datasource, delete = TRUE)
    }
    base::return(base::invisible(NULL))
  }

  # Abort if the name (used for both source and interface) is unassignable.
  # (So that successfully adding a new source guarantees a named interface.)
  if (name != base::make.names(name))
  {
    base::stop("Unassignable data-source name.")
  }

  # If the definition is a path to a file, configure the source from that file.
  confile <- base::paste0(def, collapse = "")
  if (!base::is.null(SqrlPath(confile)))
  {
    # If the configuration file defines (names) the interface, use that value.
    # Otherwise, make the interface name identical to the source name.
    config <- base::readLines(SqrlPath(confile))
    if (base::any(base::grepl("^\\s*interface\\s*=\\s*[[:graph:]]", config,
                              ignore.case = TRUE)))
    {
      interface <- SqrlDefile("interface", SqrlPath(confile), evaluate = TRUE)
    } else
    {
      interface <- name
    }

    # Abort if the interface name is not of the standard R name format.
    if (interface != base::make.names(interface))
    {
      base::stop("Unassignable interface name.")
    }

    # Abort if the interface name clashes with that of any pre-existing object.
    if (SqrlFace(interface, clashes = TRUE)
        && (SqrlCache(name, exists = FALSE)
            || SqrlParam(name, "interface") != interface))
    {
      base::stop("Interface name conflict.")
    }

    # If the source does not already exist, create it.
    if (SqrlCache(name, exists = FALSE))
    {
      SqrlCache(name, create = TRUE)
    }

    # Apply the interface and configuration (which may, or may not, define the
    # interface again), then return invisible NULL.
    SqrlInterface(name, interface)
    SqrlConfig(name, SqrlPath(confile))
    base::return(base::invisible(NULL))
  }

  # Abort if the name clashes with that of some object other than the interface
  # of a pre-existing data source of the same name (see comment below).
  if (SqrlFace(name, clashes = TRUE)
      && (SqrlCache(name, exists = FALSE)
          || SqrlParam(name, "interface") != name))
  {
    base::stop("Interface name conflict.")
  }

  # Create a cache for the data source, unless it already exists. (If it does
  # exist (see comment above), then we are simply adding or replacing some
  # parameter values and, possibly, the interface.)
  if (SqrlCache(name, exists = FALSE))
  {
    SqrlCache(name, create = TRUE)
  }

  # If def names an existing data source, make our new source a duplicate of it.
  # (Copy (almost) all non-default parameter values from the original (<def>)
  # data source's cache, to the new (<name>) data source's cache.)
  datasource <- base::paste0(def, collapse = "")
  if (SqrlCache(datasource, exists = TRUE))
  {
    params <- SqrlParam(datasource, "*")
    params <- params[!(params %in% SqrlParams("don't-copy"))]
    for (param in params)
    {
      SqrlParam(name, param, SqrlParam(datasource, param))
    }

  # Otherwise, concatenate the definition to a connection string. This may
  # turn out to be a DSN. Store the definition accordingly.
  } else
  {
    # If the user supplied the connection string as 'connection = ...', then
    # drop the argument name ('connection' itself is not part of the string).
    if (base::identical(base::names(def), "connection"))
    {
      base::names(def) <- NULL
    }

    # If the terms have names, include those in the string.
    if (!base::is.null(base::names(def)))
    {
      lex <- base::names(def)
      lex[base::nchar(lex) > 0] <- base::paste0(lex[base::nchar(lex) > 0], "=")
      def <- base::as.list(base::paste0(lex, def))
    }

    # Concatenate to a single semi-colon delimited string (each of the terms
    # may, or may not, already end in one).
    def <- base::sub(";$", "", def)
    def <- base::paste(def, collapse = ";")

    # If the definition is not in name = value form (i.e., is a single word),
    # then assume that word is the name of a DSN, and assign accordingly. We
    # reset the connection parameter in case we are redefining an existing
    # source defined by a connection string with a '<dsn>' placeholder.
    if (!base::grepl("=", def))
    {
      SqrlParam(name, "reset", "connection")
      SqrlParam(name, "dsn", def)

    # If the definition is of form 'dsn = word', assign 'word' (only) as a DSN.
    # We reset the connection parameter in case we are redefining an existing
    # source defined by a connection string with a '<dsn>' placeholder.
    } else if ((base::length(base::gregexpr("=", def)[[1]]) == 1)
                && (base::grepl("^\\s*dsn\\s*=", def, ignore.case = TRUE)))
    {
      def <- base::trimws(base::gsub("^\\s*dsn\\s*=|;$", "", def))
      SqrlParam(name, "reset", "connection")
      SqrlParam(name, "dsn", def)

    # Otherwise, assign the definition as a connection string. We don't reset
    # dsn, in case we're redefining an existing source and the new connection
    # string contains a '<dsn>' placeholder (if it does not, setting the string
    # will reset the dsn parameter anyway).
    } else
    {
      SqrlParam(name, "connection", def)
    }
  }

  # Generate the interface for the source. This ought to always succeed, since
  # we've verified the name is assignable and doesn't clash.
  SqrlInterface(name)

  # Return invisible NULL.
  base::return(base::invisible(NULL))
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
  if (base::nchar(import) > 0)
  {
    SqrlDSNs(import)
  }

  # Retrieve and return a summary of sources (data frame).
  params <- SqrlParams("source-table")
  sumlist <- base::list()
  for (param in params)
  {
    sumlist[[param]] <- base::list(base::character(0))
  }
  for (datasource in SqrlCache("*"))
  {
    for (param in params)
    {
      if (param == "open")
      {
        value <- base::c("N", "Y")[SqrlIsOpen(datasource, besure = TRUE) + 1]
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
  sumframe <- sumframe[base::order(sumframe[, 1]), ]
  base::rownames(sumframe) <- NULL
  base::return(sumframe)
}

SqrlSubmit <- function(datasource = "",
                        ...,
                        throw = FALSE,
                        retry = TRUE)
{
  # Submit a SQL statement to a connected data source.
  # Args:
  #   datasource : The name of a known data source.
  #   ...        : A SQL statement (in general, as a list of components).
  #   throw      : When set to TRUE, overrides the errors parameter.
  #   retry      : When set to FALSE, do not resubmit on failure.
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
  statement <- SqrlStatement(...)

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
  queryrows <- 1

  # A valid connection exists, submit the statement.
  result <- RODBC::sqlQuery(channel = SqrlParam(datasource, "channel"),
                  query = statement,
                  errors = SqrlParam(datasource, "errors"),
                  as.is = SqrlParam(datasource, "as.is"),
                  max = queryrows,
                  buffsize = SqrlParam(datasource, "buffsize"),
                  nullstring = SqrlParam(datasource, "nullstring"),
                  na.strings = SqrlParam(datasource, "na.strings"),
                  believeNRows = SqrlParam(datasource, "believeNRows"),
                  dec = SqrlParam(datasource, "dec"),
                  stringsAsFactors = SqrlParam(datasource, "stringsAsFactors"),
                  rows_at_time = SqrlParam(datasource, "rows_at_time"))

  # Remove query-in-progress indicator from the window title.
  SqrlIndicator(datasource, "done")

  # On success, RODBC::sqlQuery() returns a data frame or character string (both
  # possibly empty). On error, RODBC::sqlQuery() returns a character vector of
  # error messages (always with at least two elements, it seems) or an integer
  # (either -1 or -2). From R, see ?RODBC::sqlQuery. This detects such errors.
  # It might be preferable to call RODBC::sqlQuery() with errors = FALSE (which
  # unambiguously returns -1 on an error), and then retrieve the error message
  # via RODBC::odbcGetErrMsg(), but this function is 'likely to be confined to
  # the RODBC namespace in the near future' (according to the RODBC manual).
  srcerr <- ((base::class(result) == base::class(base::integer()))
              || ((base::class(result) == base::class(base::character()))
                    && (base::length(result) > 1)))

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
    base::return(SqrlSubmit(datasource, ..., throw = throw, retry = FALSE))
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
    base::stop(base::paste(result, collapse = "\n"))
  }

  # When the result is not a data frame, there won't be any more rows to fetch,
  # so return it invisibly (it should be a character string, likely indicating
  # 'No data', or some such thing.
  if (base::class(result) != base::class(base::data.frame()))
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
    if (fetchrows <= queryrows)
    {
      base::return(result)
    }
    fetchrows <- fetchrows - queryrows
  }

  # Append fetch-in-progress indicator to the window-title connection indicator.
  SqrlIndicator(datasource, "fetch")

  # Retrieve all remaining rows. If a connection error occurs here, we cannot
  # easily recover, since pinging the source would destroy the waiting rows.
  restof <- RODBC::sqlGetResults(channel = SqrlParam(datasource, "channel"),
                  as.is = SqrlParam(datasource, "as.is"),
                  errors = SqrlParam(datasource, "errors"),
                  max = fetchrows,
                  buffsize = SqrlParam(datasource, "buffsize"),
                  nullstring = SqrlParam(datasource, "nullstring"),
                  na.strings = SqrlParam(datasource, "na.strings"),
                  believeNRows = SqrlParam(datasource, "believeNRows"),
                  dec = SqrlParam(datasource, "dec"),
                  stringsAsFactors = SqrlParam(datasource, "stringsAsFactors"))

  # Remove fetch-in-progress indicator from the window title.
  SqrlIndicator(datasource, "done")

  # If RODBC::sqlGetResults() appears to have returned an error (message or
  # code) and throw is TRUE, then stop with an exception. Otherwise, if the
  # function did not return a data frame (something seems to have gone wrong),
  # return that something invisibly. The logic here could perhaps be simplified.
  srcerr <- ((base::class(restof) == base::class(base::integer()))
              || ((base::class(restof) == base::class(base::character()))
                    && (base::length(restof) > 1)))
  if (srcerr
      && (throw
          || SqrlParam(datasource, "errors")))
  {
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
  if (base::nchar(phrase) > 0)
  {
    statement <- base::append(statement, phrase)
  }

  # If the statement is non-empty, submit it and retrieve the result.
  if (base::length(statement) > 0)
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

      # If verbose, output (some of) the result.
      if (verbose)
      {
        if (base::class(result) == base::class(base::data.frame()))
        {
          base::print(result[base::seq(1, base::min(base::nrow(result), 10)), ])
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

    # Return the result.
    base::return(result)
    }
  }

  # If the result was to have been assigned to some name, but there was
  # actually no query (and, therefore, no result), assign the value NULL.
  if (!base::is.null(envir)
      && (base::tolower(intermediate) != "null"))
  {
    base::assign(intermediate, NULL, envir)
  }

  # Return NULL (signifying an undefined result, because there was no query).
  # SqrlSubmit(), and RODBC::sqlQuery() (to which it is a wrapper), are both
  # incapable of returning NULL (or NA).
  base::return(NULL)
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
    if (base::nchar(connection) > 0)
    {
      for (spar in SqrlParams("substitutable"))
      {
        connection <- base::gsub(base::paste0("<", spar, ">"),
                                  SqrlValue(datasource, spar), connection)
      }
      base::names(connection) <- "connection"
      base::return(connection)
    }
    dsn <- base::as.character(SqrlValue(datasource, "dsn"))
    base::names(dsn) <- "dsn"
    base::return(dsn)
  }

  # Retrieve the parameter value, after setting it if so instructed.
  if (!base::missing(set))
  {
    if (base::nchar(datasource) < 1)
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
        if (base::length(ignorables) > 0)
        {
          ignorables <- base::paste0("\\s*<", ignorables, ">\\s*$")
        }
        ignorables <- base::paste0(pattern, base::c("\\s*$", ignorables))

        # Positions (first-character indices) and lengths of the (potential)
        # secret-containing sub-strings of the parameter-value string.
        ssubs <- base::gregexpr(base::paste0(pattern, "\\s*[^;]*"),
                                value, ignore.case = TRUE)[[1]]
        slens <- base::c(0, base::attr(ssubs, "match.length"))
        ssubs <- base::c(0, ssubs)

        # Overwrite all non-ignorable (true) secrets with the replacement text.
        eds <- base::character(0)
        for (i in base::seq(2, base::length(ssubs)))
        {
          # Character positions (indices) within the value string; Start Of
          # Secret substring, End Of Secret substring, Start Of Previous
          # (non-secret) substring, End of Previous (non-secret) substring.
          sos <- ssubs[i]
          eos <- ssubs[i] + slens[i] - 1
          sop <- ssubs[i - 1] + slens[i - 1]
          eop <- ssubs[i] - 1

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
  if ((base::length(arglist) == 1)
      && base::is.null(base::names(arglist))
      && (base::class(...) == base::class(base::character()))
      && (base::nchar(...) > 0)
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
  #   SqrlCache(), SqrlInterface(), SqrlParam().
  # User:
  #   Exported function. User has direct access. All arguments are checked for
  #   validity (although this does not guarantee usability of the interface
  #   name, which is left for SqrlInterface() to establish).

  # Either one or two arguments are expected.
  arglist <- base::list(...)
  if ((base::length(arglist) < 1)
      || (base::length(arglist) > 2))
  {
    base::stop("A source name and an interface name are expected.")
  }

  # Identify both the data-source and interface names.
  if (base::length(arglist) == 1)
  {
    if (!base::is.null(base::names(arglist)))
    {
      datasource <- base::names(arglist)
      interface <- arglist[[datasource]]
    } else
    {
      datasource <- arglist[[1]]
      interface <- NULL
    }
  } else
  {
    datasource <- arglist[[1]]
    interface <- arglist[[2]]
  }

  # Abort on non-existence of the specified data source.
  if ((base::class(datasource) != base::class(base::character()))
      || (base::length(datasource) != 1)
      || (base::nchar(datasource) < 1)
      || SqrlCache(datasource, exists = FALSE))
  {
    base::stop("Unrecognised data source.")
  }

  # In the absence of a specified interface name, return the name of the
  # current interface to the data source (or NULL if none exists).
  if (base::is.null(interface))
  {
    base::return(SqrlParam(datasource, "interface"))
  }

  # Abort on invalid interface (name). Allowed values are NULL or a character
  # string. The requested name may, or may not, be available and assignable.
  if (!base::is.null(interface)
      && ((base::class(interface) != base::class(base::character()))
          || (base::length(interface) != 1)))
  {
    base::stop("Invalid interface name.")
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
  #   Invisible NULL, after creating, or re-defining, the source and interface.
  # SQRL Calls:
  #   SqrlSource().
  # User:
  #   Exported function. User has direct access. Here, we ensure the existence
  #   of name and definition terms (in the form of multiple arguments, or at
  #   least one named argument). Additional checks are left to SqrlSource().

  # Abort unless we have at least a pair of terms (name, definition) or a
  # single named term (name = definition).
  def <- base::list(...)
  if ((base::length(def) < 2)
      && base::is.null(base::names(def)))
  {
    base::stop("A name and definition are expected.")
  }

  # Pass the arguments to SqrlSource() (returns invisible NULL).
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
  if (base::length(import) == 0)
  {
    import <- ""
  } else if ((base::length(import) == 1)
              && (base::identical(import[[1]], "all")
                  || base::identical(import[[1]], "user")
                  || base::identical(import[[1]], "system")
                  || base::identical(import[[1]], "remove")))
  {
    import <- import[[1]]
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
