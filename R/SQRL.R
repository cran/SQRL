####################################################################### SQRL ###

# Wrapper about RODBC. On load, SQRL automatically generates a like-named user-
# interface function to each DSN it finds on the system. These functions enable
# immediate interaction with each data source, since channels and communication
# parameters are managed behind the scenes. The general philosophy is to require
# the least possible typing from the user, while allowing the greatest possible
# flexibility in how commands are entered. This approach emphasises the source
# and query over concatenation functions and control parameters. The interfaces
# accept multi-statement SQL scripts, allowing the use of scripts developed in
# other applications without modification or fragmentation. The script parser
# supports query parameterisation via embedded R expressions, the feedback of
# intermediate results, reusable procedures, conditional submission, loops, and
# early returns. Secondary features include the protection of connection handles
# from rm(), automatic recovery from lost connections, the promotion of remote
# ODBC exceptions to local R errors, optional automatic closure of connections
# between queries, and visual indication of which connections are open and of
# where and whether queries or fetches are in progress.
# Mike Lee, South Titirangi, 7 March 2020.



################################################################### CONTENTS ###

# srqlHaus            Private. Environment. Stores data-source parameters.
# srqlHelp            Private. Environment. Stores interface-help temp files.

# SqrlAll()           Private. Broadcasts a command to every SQRL source.
# SqrlCache()         Private. Interfaces with srqlHaus (only point of contact).
# SqrlClose()         Private. Closes data source connection channels.
# SqrlConfig()        Private. Sets SQRL/RODBC parameters from a config file.
# SqrlDefault()       Private. Defines and returns default parameter values.
# SqrlDefile()        Private. Extracts parameter values from container files.
# SqrlDelegate()      Private. Delegates commands to appropriate functions.
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
# SqrlPing()          Private. Sets and submits ping queries to data sources.
# SqrlPL()            Private. Detects procedural (PL) blocks within scripts.
# SqrlProc()          Private. Checks if a value names a stored procedure.
# SqrlShell()         Private. Relays commands from interfaces to delegator.
# SqrlStatement()     Private. Assembles SQL statements from listed components.
# SqrlSource()        Private. Registers/defines new data sources with SQRL.
# SqrlSources()       Private. Look for, and summarise, known data sources.
# SqrlSubmit()        Private. Submits SQL, retrieves results, handles errors.
# SqrlSubScript()     Private. Relays data between SqrlFile() and SqrlSubmit().
# SqrlTry()           Private. Silent error catching, with warning suppression.
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

SqrlAll <- function(argsl,
                    envir = base::parent.frame())
{
  # Applies the same command to each of the (currently defined) SQRL sources.
  # Args:
  #   argsl : A list of arguments, to be passed unaltered to SqrlDelegate().
  #   envir : An environment, beneath which any embedded R script is evaluated.
  # Returns:
  #   A list (by SQRL source name) of the results of running the argsl
  #   command(s) on each of the SQRL sources. SQRL source names are unique.
  # SQRL Calls:
  #   SqrlCache(), SqrlShell().
  # SQRL Callers:
  #   SqrlSources(), sqrlAll().
  # User:
  #   Has no direct access, but is able to supply (only) the argsl argument,
  #   from sqrlAll(). That function ensures argsl is a list. Since argsl is
  #   otherwise unrestricted, no validity checking is required at this stage.

  # Give the command to each data source in turn. Retrieve the results. Fatal
  # errors will block sending the command to further sources. Opting not to wrap
  # in try(), because stopping may be preferable under many circumstances.
  results <- base::list()
  for (datasource in SqrlCache("*"))
  {
    # Assigning NULL to a list element[[]] removes the element, whereas
    # assigning list(NULL) to element[] leaves the element with a NULL value.
    results[datasource] <- base::list(SqrlShell(datasource, envir, argsl))
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
  #   SqrlParam(), SqrlTry().
  # RODBC Calls:
  #   odbcClose().
  # SQRL Callers:
  #   SqrlCache(), SqrlDelegate(), SqrlFile(), SqrlIsOpen(), SqrlOff().
  # User:
  #   Has no direct access, unable to pass argument indirectly. No argument
  #   validity checks are required.

  # Return invisible NULL if the channel is already closed.
  if (base::is.null(SqrlParam(datasource, "channel")))
  {
    base::return(base::invisible(NULL))
  }

  # Attempt to close the channel (which may, or may not, actually be open).
  SqrlTry(RODBC::odbcClose(SqrlParam(datasource, "channel")), warn = FALSE)

  # Whatever the situation, nullify the connection handle immediately. If the
  # channel somehow survived the close attempt, this makes it unusable. The
  # SqrlParam() function will remove any visible connection indicators.
  SqrlParam(datasource, "channel", NULL)

  # Return invisible NULL.
  base::return(base::invisible(NULL))
}

SqrlConfig <- function(datasource = "",
                        config = "")
{
  # Assigns SQRL/RODBC parameter values, for a data source, from a file or list.
  # Args:
  #   datasource : The name of a known (to SQRL) data source.
  #   config     : The path of a configuration file, or a list of named values.
  # Returns:
  #   The imported configuration, as an invisible list of (name, value) pairs.
  #   When no configuration file is specified, this function acts as a getter,
  #   and returns a list of all SQRL/RODBC parameters and their current values.
  # SQRL Calls:
  #   SqrlDefile(), SqrlFile(), SqrlInterface(), SqrlParam(), SqrlParams(),
  #   SqrlPath(), SqrlValue().
  # SQRL Callers:
  #   SqrlDelegate(), SqrlHelp(), SqrlSource().
  # User:
  #   Has no direct access, but is able to pass the config argument (only) via
  #   SqrlDelegate(). That function vets the value, and ensures config is either
  #   a list (of named values) or a character string. In the latter case, the
  #   string ought to be the path of an actual (existing, readable) config file,
  #   but this is not guaranteed (and so is checked here).

  # Parameter values will be copied into this list, before setting or returning.
  conf <- base::list()

  # If no config was specified, return the data source's configuration as a list
  # of named SQRL/RODBC parameter values (with any secrets obliterated).
  if (base::identical(base::class(config), base::class(base::character()))
      && (base::nchar(config) < 1L))
  {
    params <- SqrlParams("all")
    params <- params[!(params %in% SqrlParams("omit-from-config"))]
    for (param in params)
    {
      conf[param] <- base::list(SqrlValue(datasource, param))
    }
    base::return(conf)
  }

  # If a list of named elements was supplied, extract parameter values from it.
  if (base::identical(base::class(config), base::class(base::list()))
      && !base::is.null(base::names(config))
      && base::all(base::grepl("[[:graph:]]", base::names(config))))
  {
    # If any name should be replicated, the final occurrence is taken
    for (i in base::seq_along(config))
    {
      conf[base::trimws(base::names(config)[i])] <- base::list(config[[i]])
    }

  # Otherwise, a file path should have been supplied as a character string.
  } else
  {
    # Abort if that file does not exist.
    filepath <- SqrlPath(config)
    if (base::is.null(filepath))
    {
      base::stop("File not found.")
    }

    # Slurp the file, and parse it as an R script.
    ftext <- base::readLines(filepath, warn = FALSE)
    flang <- base::parse(text = ftext, keep.source = FALSE)

    # An environment within which to evaluate the script. Inherits functions
    # from base, but not variables from the global environment.
    cfenv <- base::new.env(parent = base::baseenv())

    # Take each expression from the script, wrap it in a list, and evaluate.
    # Where this results in a named list, interpret the expression as a request
    # to set a value for the SQRL/RODBC parameter of that name, and add that
    # name-value pair to the config list. When a name appears within the script
    # multiple times, only the last value is used.
    for (expr in flang)
    {
      etext <- base::paste0(base::deparse(expr), collapse = "\n")
      lexp <- base::parse(text = base::paste0("list", "(", etext, ")"),
                          keep.source = FALSE)
      lval <- base::eval(lexp, cfenv)
      if ((base::length(lval) == 1L)
          && !base::is.null(base::names(lval)))
      {
        conf[base::names(lval)] <- lval
      }
    }
  }

  # Ignore any request to set the channel.
  conf <- conf[base::names(conf) != "channel"]

  # If 'interface' is among the parameters to be set, then set it first (since
  # it's the one most likely to fail). If this does fail, then no further
  # parameter values will be set (SqrlInterface() will throw an exception).
  if ("interface" %in% base::names(conf))
  {
    value <- SqrlDefile("interface", conf[["interface"]])
    SqrlInterface(datasource, value)
    conf["interface"] <- base::list(SqrlValue(datasource, "interface"))
  }

  # Defining the library is another special case.
  if ("library" %in% base::names(conf))
  {
    value <- SqrlDefile("library", conf[["library"]])
    if (base::is.null(value))
    {
      SqrlParam(datasource, "reset", "library")
    } else
    {
      SqrlFile(datasource, value, libmode = TRUE)
    }
    conf["library"] <- base::list(SqrlValue(datasource, "library"))
  }

  # Assign all other values found (besides those of 'interface' and 'library').
  # The driver parameter is set last, to override any default driver set as a
  # side effect in the course of setting dsn (should that have been set). By
  # the above construction, list-member (parameter) names are unique.
  params <- base::names(conf)
  params <- params[!(params %in% base::c("interface", "library"))]
  params <- base::c(params[params != "driver"], params[params == "driver"])
  for (parameter in params)
  {
    value <- SqrlDefile(parameter, conf[[parameter]])
    conf[parameter] <- base::list(SqrlValue(datasource, parameter, value))
  }

  # Return the (sorted, secrets-obscured) configuration, invisibly.
  base::return(base::invisible(conf[base::order(base::names(conf))]))
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
    "believeNRows"        = !base::grepl("SQLite",
                                          SqrlParam(datasource, "driver"),
                                          ignore.case = TRUE),
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
    "stringsAsFactors"    = FALSE,

    # Parameters for SQRL.
    "*"                   = base::objects(cacheenvir, all.names = TRUE),
    "aCollapse"           = ",",
    "autoclose"           = FALSE,
    "driver"              = "",
    "interface"           = NULL,
    "lCollapse"           = "",
    "library"             = base::character(),
    "libstack"            = base::list(),
    "name"                = datasource,
    "ping"                = NULL,
    "prompt"              = base::substr(datasource, 1L, 1L),
    "pstack"              = base::list(cacheenvir),
    "result"              = NULL,
    "retry"               = TRUE,
    "scdo"                = TRUE,
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
  #   value     : Either a final value or a file path (or components thereof).
  #   evaluate  : Whether or not to attempt to evaluate value as an expression.
  # Returns:
  #   A value for the parameter, either as supplied or as found within the
  #   supplied file (alternative).
  # SQRL Calls:
  #   SqrlDefile() (self), SqrlParams(), SqrlPath(), SqrlTry().
  # SQRL Callers:
  #   SqrlConfig(), SqrlDefile() (self), SqrlDelegate(), SqrlSource(),
  #   sqrlInterface().
  # User:
  #   Has no direct access, but is able to supply (only) parameter and value via
  #   SqrlParam() from SqrlDelegate() and/or SqrlConfig(). The parameter is
  #   guaranteed to be a string, and no further checks are required. The value
  #   may turn out to be unsuitable, but that is left for SqrlParam() to decide.

  # If the value is to represent a file path, it must be a non-empty, non-blank,
  # character vector (or list of character vectors). If the value is anything
  # besides these, return it unmodified. The reason for returning both blank and
  # empty character vectors here (before evaluation) is to take them as literal
  # values (rather than as R expressions; they would evaluate to NULL).
  if (!((base::identical(base::class(value), base::class(base::character()))
          && (base::length(value) > 0L)
          && base::any(base::nzchar(base::trimws(value))))
        || (base::identical(base::class(value), base::class(base::list()))
            && (base::length(value) > 0L)
            && base::rapply(base::rapply(value, base::class, how = "list"),
                            base::identical, classes = "ANY", deflt = NULL,
                            how = "unlist", base::class(base::character()))
            && base::any(base::nzchar(base::rapply(value, base::trimws))))))
  {
    base::return(value)
  }

  # See if the value corresponds to the path of a readable file. If so, this is
  # that path. If not, this is NULL.
  path <- SqrlPath(value)

  # If the value is not the path of a readable file, then return either the
  # unmodified value, or the (if possible and so requested) evaluated value.
  if (base::is.null(path))
  {
    # The value is not a path. If it is not to be evaluated, return it as is.
    if (!evaluate)
    {
      base::return(value)
    }

    # Otherwise, if the value doesn't evaluate, return it unmodified.
    evaluated <- SqrlTry(base::eval(
                            base::parse(text = value, keep.source = FALSE),
                            base::new.env(parent = base::baseenv())))
    if (evaluated$error)
    {
      base::return(value)
    }

    # If the value evaluated to something odd (for example, 'ls' evaluates to a
    # function), return it unmodified.
    eclass <- base::class(evaluated$value)
    if (!(base::identical(eclass, base::class(NULL))
          || base::identical(eclass, base::class(base::logical()))
          || base::identical(eclass, base::class(base::character()))
          || base::identical(eclass, base::class(base::numeric()))
          || base::identical(eclass, base::class(base::integer()))))
    {
      base::return(value)
    }

    # The value could be evaluated; return the evaluated value.
    base::return(evaluated$value)
  }

  # Otherwise, the value specifies the path of a readable file. If the parameter
  # is path-valued, then return that path.
  if (parameter %in% SqrlParams("path-valued"))
  {
    base::return(path)
  }

  # Slurp the file.
  ftxt <- base::readLines(path, warn = FALSE)

  # If the file can't be parsed and evaluated, or if no 'parameter = value'
  # assignment is found, assume the file text is a literal parameter value.
  value <- base::trimws(ftxt)
  value <- value[base::nzchar(value)]

  # Attempt to parse the file as an R script.
  fexp <- SqrlTry(base::parse(text = ftxt, keep.source = FALSE))

  # If the file parsed, attempt to evaluate its R expressions (within an
  # environment that inherits functions from base, but not variables from the
  # global environment, or the evaluation environment of any parent file).
  if (!fexp$error)
  {
    fenv <- base::new.env(parent = base::baseenv())
    for (expr in fexp$value)
    {
      etxt <- base::paste0(base::deparse(expr), collapse = "\n")
      lexp <- base::parse(text = base::paste0("list", "(", etxt, ")"),
                          keep.source = FALSE)
      lval <- SqrlTry(base::eval(lexp, fenv))

      # Should an error occur, revert to the default value (literal script).
      if (lval$error)
      {
        value <- base::trimws(ftxt)
        value <- value[base::nzchar(value)]
        break
      }

      # If the expression was of the form 'parameter = value', take that value.
      if (base::identical(base::names(lval$value), parameter))
      {
        value <- lval$value[[1L]]
      }
    }
  }

  # Put the extracted value back into this function, in case it is another
  # file path (recursive call, infinite loops are possible). Given that the
  # current value is a file path, we evaluate the next value (since it is to be
  # read from file as text), whether or not the current value was evaluated.
  base::return(SqrlDefile(parameter, value))
}

SqrlDelegate <- function(datasource = "",
                          envir = base::parent.frame(),
                          args.list)
{
  # Interpret the command, and forward to the appropriate handler.
  # Args:
  #   datasource : The name of a known data source.
  #   envir      : An R environment, from which variables are inherited.
  #   args.list  : A list of arguments, to be interpreted and actioned.
  # Returns:
  #   The result of the command (normally a data frame, sometimes a string).
  # SQRL Calls:
  #   SqrlCache(), SqrlClose(), SqrlConfig(), SqrlDefile(), SqrlFile(),
  #   SqrlHelp(), SqrlIndicator(), SqrlInterface(), SqrlIsOpen(), SqrlOpen(),
  #   SqrlParam(), SqrlParams(), SqrlPath(), SqrlProc(), SqrlSources(),
  #   SqrlStatement(), SqrlSubmit(), SqrlTry(), SqrlValue().
  # RODBC Calls:
  #   sqlColumns(), sqlTables(), sqlTypeInfo().
  # SQRL Callers:
  #   SqrlFile() (via sqrl()), SqrlShell().
  # User:
  #   User has no direct access, but is able to supply (only) the args.list
  #   argument from sqrlAll() and/or any data source interface (including
  #   intra-script sqrl() functions). Since args.list is unrestricted (it could
  #   be SQL), no argument validity checking is performed.

  # Count the number of supplied arguments.
  args.count <- base::length(args.list)

  # If no command was given, open a channel to the data source. If no channel
  # exists, a new channel is opened. If a channel exists, but wasn't open after
  # all (after besure = TRUE pings the data source to check), we replace the
  # dead channel with a new one. If a channel exists and is open, we do nothing
  # else. Returns the configuration invisibly, enabling interface()$parameter.
  if (args.count == 0L)
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
  # Names need not be unique. These names cannot be NAs.
  args.names <- base::names(args.list)

  # Expand lists of named arguments.
  if (base::is.null(args.names)
      || base::any(base::nchar(args.names) == 0L))
  {
    # Obtain the indices of any unnamed arguments.
    i <- base::seq(args.count)
    if (!base::is.null(args.names))
    {
      i <- i[base::nchar(args.names) == 0L]
    }

    # Unpack unnamed lists of (syntactically correctly) named members, except
    # where that would place a named argument before an unnamed argument.
    j <- base::length(i)
    while ((j > 0L)
            && base::identical(base::class(args.list[[i[j]]]),
                                base::class(base::list()))
            && (base::length(args.list[[i[j]]]) > 0L)
            && !base::is.null(base::names(args.list[[i[j]]]))
            && !base::any(base::is.na(base::names(args.list[[i[j]]])))
            && (base::all(base::names(args.list[[i[j]]]) ==
                      base::make.names(base::names(args.list[[i[j]]])))))
    {
      k <- base::seq_along(args.list)
      args.list <- base::c(args.list[k[k < i[j]]],
                            args.list[[i[j]]],
                            args.list[k[k > i[j]]])
      j <- j - 1L
    }

    # Update the number of (unpacked) arguments, and their names.
    args.count <- base::length(args.list)
    args.names <- base::names(args.list)
  }

  # When all arguments are named, treat them as either parameterised queries to
  # be submitted, or as SQRL parameter values to be (re)set.
  if (!base::is.null(args.names)
      && base::all(base::nchar(args.names) > 0L))
  {
    # When there is only one argument, and it is named 'verbatim', expect a
    # single character string to be submitted directly (unmodified, without
    # going through the SQRL concatenator, parser, or R-substitution process).
    if ((base::length(args.names) == 1L)
        && (args.names == "verbatim"))
    {
      if ((base::length(args.list) != 1L)
          || (base::class(args.list[[1L]]) != base::class(base::character()))
          || (base::length(args.list[[1L]]) != 1L))
      {
        base::stop("Verbatim query not a single character string.")
      }
      base::return(SqrlSubmit(datasource, args.list[[1L]]))
    }

    # Prohibit the use of more than one of the names 'file', 'proc', and
    # 'query', since it is unclear which refers to the script and which is a
    # (are) parameter(s) to the that.
    if (base::sum(base::c("file", "proc", "query") %in% args.names) > 1L)
    {
      base::stop("The file, proc, and query arguments are mutually exclusive.")
    }

    # If one of the names in 'proc', then submit the named procedure, and treat
    # any other (named) arguments as parameters to that procedure. It is this
    # function's responsibility to verify the existence of the procedure. When
    # multiple arguments are named 'proc', the first of them is taken as the
    # procedure while the others are treated as arguments to that procedure.
    if ("proc" %in% args.names)
    {
      index <- base::which(args.names == "proc")[1L]
      script <- SqrlProc(datasource, args.list[[index]])
      if (base::is.null(script))
      {
        base::stop("Procedure not defined.")
      }
      params <- args.list[base::seq_along(args.list) != index]
      result <- base::withVisible(
                    SqrlFile(datasource, script, envir, params, literal = TRUE))
      SqrlParam(datasource, "result", result$value, override = TRUE)
      if (!result$visible)
      {
        base::return(base::invisible(result$value))
      }
      base::return(result$value)
    }

    # If one of the names in 'file', then submit a query from the file, and
    # treat any other (named) arguments as parameters to that query. It is this
    # function's responsibility to verify the existence and readability of the
    # file before passing it to SqrlFile(). When multiple arguments are named
    # 'file', the first of them is taken as the query file while the others are
    # treated as arguments to that query.
    if ("file" %in% args.names)
    {
      index <- base::which(args.names == "file")[1L]
      file.path <- SqrlPath(args.list[[index]])
      if (base::is.null(file.path))
      {
        base::stop("File not found.")
      }
      params <- args.list[base::seq_along(args.list) != index]
      result <- base::withVisible(
                                SqrlFile(datasource, file.path, envir, params))
      SqrlParam(datasource, "result", result$value, override = TRUE)
      if (!result$visible)
      {
        base::return(base::invisible(result$value))
      }
      base::return(result$value)
    }

    # If one of the names is 'query', then pass the query to SqrlFile() (as a
    # script, not as a file name), with any other arguments as named parameters.
    # When multiple arguments are named 'query', the first of them is taken as
    # the query, while the others are treated as parameters of that query.
    if ("query" %in% args.names)
    {
      index <- base::which(args.names == "query")[1L]
      script <- SqrlTry(
                    SqrlStatement(datasource, base::list(args.list[[index]])))
      if (script$error)
      {
        base::stop(script$value)
      }
      script <- script$value
      params <- args.list[base::seq_along(args.list) != index]
      result <- base::withVisible(
                              SqrlFile(datasource, script, envir, params, TRUE))
      SqrlParam(datasource, "result", result$value, override = TRUE)
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
    # in the course of setting the dsn parameter. Names need not be unique.
    if (base::any(args.names %in% SqrlParams("read-only")))
    {
      base::stop("Parameter is read-only.")
    }
    result <- base::list()
    indices <- base::seq_along(args.list)
    drivers <- args.names == "driver"
    indices <- base::c(indices[!drivers], indices[drivers])
    for (index in indices)
    {
      param <- args.names[index]
      if (param == "config")
      {
        # SqrlConfig() returns a list with unique names.
        conf <- SqrlConfig(datasource, args.list[[index]])
        for (cpar in base::names(conf))
        {
          result[cpar] <- base::list(conf[[cpar]])
        }
      } else
      {
        if (param == "interface")
        {
          value <- SqrlDefile(param, args.list[[index]])
          result[param] <- base::list(SqrlInterface(datasource, value))
        } else if (param == "library")
        {
          if (base::is.null(args.list[[index]]))
          {
            SqrlParam(datasource, "reset", param)
          } else
          {
            path <- SqrlPath(args.list[[index]])
            if (base::is.null(path))
            {
              libdef <- SqrlTry(SqrlStatement(datasource,
                                              base::list(args.list[[index]])))
              if (libdef$error)
              {
                base::stop(libdef$value)
              }
              SqrlFile(datasource, libdef$value, envir,
                        libmode = TRUE, literal = TRUE)
            } else
            {
              SqrlFile(datasource, path, envir, libmode = TRUE, literal = FALSE)
            }
          }
          result[param] <- base::list(SqrlValue(datasource, param))
        } else if (param == "reset")
        {
          # SqrlValue() returns a list of default values with unique names.
          values <- SqrlValue(datasource, param, args.list[[index]])
          result[base::names(values)] <- values
        } else
        {
          value <- SqrlDefile(param, args.list[[index]])
          result[param] <- base::list(SqrlValue(datasource, param, value))
        }
      }
    }
    if (!base::is.null(base::names(result)))
    {
      result <- result[base::order(base::names(result))]
    }
    base::return(base::invisible(result))
  }

  # When both named and unnamed arguments exist, and all named arguments trail
  # all unnamed arguments, then interpret the unnamed arguments as defining a
  # script (one way or another), and the named arguments as its parameters.
  args.kindex <- base::which(base::nchar(args.names) > 0L)[1L]
  if (!base::is.null(args.names)
      && base::all(base::nchar(args.names[args.kindex:args.count]) > 0L))
  {
    unnamed <- args.list[base::seq((args.kindex - 1L))]
    params <- args.list[base::seq(args.kindex, args.count)]

    # If the unnamed arguments name a stored procedure, use that.
    if (!base::is.null(script <- SqrlProc(datasource, unnamed)))
    {
      literal <- TRUE

    # If, instead, the unnamed arguments define a file path, read and use that.
    } else if (!base::is.null(script <- SqrlPath(unnamed)))
    {
      literal <- FALSE

    # If, instead, the single unnamed argument is 'config', set and return that.
    } else if ((args.kindex == 2L)
                && base::identical(base::trimws(unnamed), "config"))
    {
      base::return(SqrlConfig(datasource, params))

    # Otherwise, treat the unnamed arguments as a literal script.
    } else
    {
      script <- SqrlTry(SqrlStatement(datasource, unnamed))
      if (script$error)
      {
        base::stop(script$value)
      }
      script <- script$value
      literal <- TRUE
    }

    # Submit the script and its parameters. Retrieve and return the result.
    result <- base::withVisible(
                          SqrlFile(datasource, script, envir, params, literal))
    SqrlParam(datasource, "result", result$value, override = TRUE)
    if (!result$visible)
    {
      base::return(base::invisible(result$value))
    }
    base::return(result$value)
  }

  # When even one unnamed argument trails at least one named argument, abort.
  if (!base::is.null(args.names)
      && base::any(base::nchar(args.names) > 0L))
  {
    base::stop("All unnamed arguments must precede all named arguments.")
  }

  # Otherwise (when none of the arguments are named), attempt to interpret them
  # as a list of subcommands, or as file-path components, or as a procedure
  # name, or as specific SQRL commands (consisting of a command word, or a
  # parameter name, and, optionally, a value to go with that).

  # If the entire command names a procedure, submit that stored procedure.
  procedure <- SqrlProc(datasource, args.list)
  if (!base::is.null(procedure))
  {
    result <- base::withVisible(
                        SqrlFile(datasource, procedure, envir, literal = TRUE))
    SqrlParam(datasource, "result", result$value, override = TRUE)
    if (!result$visible)
    {
      base::return(base::invisible(result$value))
    }
    base::return(result$value)
  }

  # If the entire command specifies a path, try sourcing SQL from that file.
  file.path <- SqrlPath(args.list)
  if (!base::is.null(file.path))
  {
    result <- base::withVisible(SqrlFile(datasource, file.path, envir))
    SqrlParam(datasource, "result", result$value, override = TRUE)
    if (!result$visible)
    {
      base::return(base::invisible(result$value))
    }
    base::return(result$value)
  }

  # When the first argument is not a single character string, interpret all the
  # arguments as components of a query, and submit that. This is performed here,
  # because the grepl() logic below gets upset when args.list[[1L]] is a vector.
  if (!base::identical(base::class(args.list[[1L]]),
                        base::class(base::character()))
      || (base::length(args.list[[1L]]) != 1L))
  {
    statement <- SqrlTry(SqrlStatement(datasource, args.list))
    if (statement$error)
    {
      base::stop(statement$value)
    }
    result <- base::withVisible(
                  SqrlFile(datasource, statement$value, envir, literal = TRUE))

    SqrlParam(datasource, "result", result$value, override = TRUE)
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
  if (base::tolower(first.word) %in% SqrlParams("sql-keywords"))
  {
    statement <- SqrlTry(SqrlStatement(datasource, args.list))
    if (statement$error)
    {
      base::stop(statement$value)
    }
    result <- base::withVisible(
                  SqrlFile(datasource, statement$value, envir, literal = TRUE))

    SqrlParam(datasource, "result", result$value, override = TRUE)
    if (!result$visible)
    {
      base::return(base::invisible(result$value))
    }
    base::return(result$value)
  }

  # If the first supplied argument contains more than one word, the other words
  # consist of everything except the first word (pasted together).
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

  # Otherwise, the other words consist of all the supplied arguments besides the
  # first (paste these together).
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
  if ("columns" == first.word)
  {
    if (first.word == only.word)
    {
      base::stop("Table not specified.")
    }
    SqrlOpen(datasource)
    SqrlIndicator(datasource, "query")
    result <- SqrlTry(RODBC::sqlColumns(
                                  channel = SqrlParam(datasource, "channel"),
                                  sqtable = other.words,
                                  errors = SqrlParam(datasource, "errors"),
                                  as.is = TRUE))
    SqrlIndicator(datasource, "done")
    if (result$error
        && SqrlParam(datasource, "errors"))
    {
      base::stop(result$value)
    }
    base::return(result$value)
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

  # If the only word is 'isopen' (or if words one and two are 'is open'), return
  # the channel's open status (TRUE for open, FALSE otherwise). This calls with
  # besure = TRUE, to ping the source and make certain of the openness status.
  if (("isopen" == only.word)
      || (("is" == first.word)
          && ("open" == other.words)))
  {
    base::return(SqrlIsOpen(datasource, besure = TRUE))
  }

  # If the first word is 'library', treat the other words as a library file or
  # literal script (to be imported). If the other word is a literal NULL, then
  # this is an alias for reset. The getter case, wherein 'library' is the only
  # word, is handled below (with the other SQRL parameters).
  if (("library" == first.word)
      && (first.word != only.word))
  {
    if (base::is.null(other.words))
    {
      SqrlParam(datasource, "reset", first.word)
    } else
    {
      SqrlFile(datasource, other.words, envir, libmode = TRUE,
                literal = base::is.null(SqrlPath(other.words)))
    }
    base::return(base::invisible(SqrlValue(datasource, "library")))
  }

  # If the only word is 'Library', return the full library definition.
  if ("Library" == only.word)
  {
    base::return(SqrlParam(datasource, "library"))
  }

  # If the only word is 'open', open a channel to the specified data source.
  if ("open" == only.word)
  {
    base::return(SqrlOpen(datasource))
  }

  # If the first word is 'primarykeys', then call RODBC::sqlPrimaryKeys() on the
  # remaining words (which ought to be table or database.table).
  if ("primarykeys" == first.word)
  {
    if (first.word == only.word)
    {
      base::stop("Table not specified.")
    }
    SqrlOpen(datasource)
    SqrlIndicator(datasource, "query")
    result <- SqrlTry(RODBC::sqlPrimaryKeys(
                                  channel = SqrlParam(datasource, "channel"),
                                  sqtable = other.words,
                                  errors = SqrlParam(datasource, "errors"),
                                  as.is = TRUE))
    SqrlIndicator(datasource, "done")
    if (result$error
        && SqrlParam(datasource, "errors"))
    {
      base::stop(result$value)
    }
    base::return(result$value)
  }

  # If the only word is 'remove', then deregister the source from SQRL.
  if ("remove" == only.word)
  {
    base::return(SqrlCache(datasource, delete = TRUE))
  }

  # If the first word is 'reset', then reset the stated parameters.
  if ("reset" == first.word)
  {
    base::return(
              base::invisible(SqrlValue(datasource, first.word, other.words)))
  }

  # If the only word is 'settings', return that subset of the configuration.
  if ("settings" == only.word)
  {
    s <- SqrlConfig(datasource)
    base::return(s[!(base::names(s) %in% SqrlParams("omit-from-settings"))])
  }

  # If the only word is 'source', return the (placeholder substituted, secrets
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
  if ("tables" == first.word)
  {
    schema <- NULL
    if ("tables" != only.word)
    {
      schema <- other.words
    }
    SqrlOpen(datasource)
    SqrlIndicator(datasource, "query")
    result <- SqrlTry(RODBC::sqlTables(
                                  channel = SqrlParam(datasource, "channel"),
                                  errors = SqrlParam(datasource, "errors"),
                                  as.is = TRUE,
                                  schema = schema))
    SqrlIndicator(datasource, "done")
    if (result$error
        && SqrlParam(datasource, "errors"))
    {
      base::stop(result$value)
    }
    base::return(result$value)
  }

  # If the first word is 'typeinfo', call RODBC::sqlTypeInfo() on the others.
  if ("typeinfo" == first.word)
  {
    SqrlOpen(datasource)
    type <- base::ifelse(first.word == only.word, "all", other.words)
    SqrlIndicator(datasource, "query")
    info <- SqrlTry(RODBC::sqlTypeInfo(
                                channel = SqrlParam(datasource, "channel"),
                                type = type,
                                errors = SqrlParam(datasource, "errors"),
                                as.is = TRUE))
    SqrlIndicator(datasource, "done")
    if (info$error
        && SqrlParam(datasource, "errors"))
    {
      base::stop(info$value)
    }
    base::return(info$value)
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

  # Otherwise, submit the original unaltered command, via the parser.
  statement <- SqrlTry(SqrlStatement(datasource, args.list))
  if (statement$error)
  {
    base::stop(statement$value)
  }
  result <- base::withVisible(
                  SqrlFile(datasource, statement$value, envir, literal = TRUE))

  SqrlParam(datasource, "result", result$value, override = TRUE)
  if (!result$visible)
  {
    base::return(base::invisible(result$value))
  }
  base::return(result$value)
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

  # Import a list (named character vector) of registered data sources (DSNs).
  sources <- RODBC::odbcDataSources(type = import)

  # The list may contain empty names when unixODBC is incorrectly configured.
  # Such names cause SqrlParam(datasource, "driver", sources[datasource])
  # (below) to throw an error (sources[""] is NA_character_). This prevents SQRL
  # from loading, and that prevents its installation. Removing such elements
  # re-enables both. However, the incorrect configuration will still prevent
  # SQRL (and RODBC) from connecting to any data source.
  sources <- sources[base::nzchar(base::names(sources))]

  # Filter out Microsoft Access, dBASE, and Excel sources.
  unwanted <- base::paste(SqrlParams("unwanted-sources"), collapse = "|")
  sources <- sources[!base::grepl(unwanted, sources, ignore.case = TRUE)]

  # If any of the sources was previously unknown (has no associated cache), then
  # create a new cache for it. Store some valuables in the cache, then attempt
  # to generate an interface for the source (failure to do so is non-fatal).
  # A user-defined source will prevent importing a DSN of the same name. Source
  # names might not be unique (multiple same-named DSNs may appear within a
  # unixODBC .odbc.ini file), in which case only the first instance is imported
  # (and this is also what unixODBC uses when a reference to that DSN is made).
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
  #   SqrlInterface().
  # User:
  #   Has no direct access, but is able to pass-in the interface argument (only)
  #   from SqrlInterface() or SqrlSource(). Both of these check that interface
  #   is a unique (non-clashing) and assignable name. No further checks needed.

  # When acting as a setter; make the assignment, return the result invisibly.
  # This does not alter the data source's 'interface' parameter.
  if (!base::is.null(set))
  {
    def <- base::eval(base::parse(text = set, keep.source = FALSE))
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
  # name is already taken by some other function, in SQRL:Face, the global
  # environment, or any of their ancestor (parent, etc.) environments. When
  # clashes is TRUE (FALSE), we return TRUE when there is (not) a conflict.
  if (!base::is.null(clashes))
  {
    if (base::exists(interface, "SQRL:Face",
                      mode = "function", inherits = TRUE)
        || base::exists(interface, base::globalenv(),
                        mode = "function", inherits = TRUE))
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
                      script = "",
                      envir = base::parent.frame(),
                      params = NULL,
                      literal = FALSE,
                      libmode = FALSE)
{
  # Read a SQRL-script file and submit its content to a data source.
  # Args:
  #   datasource : The name of a known data source.
  #   script     : The path of a script file, or an actual script, as a string.
  #   envir      : An R environment (script is executed in a child of this).
  #   params     : A named list of R parameters for the script.
  #   literal    : If set to TRUE, script is a literal script (not a file path).
  #   libmode    : If TRUE, scripts are copied to the library parameter.
  # Returns:
  #   Result of submitting the script.
  # SQRL Calls:
  #   SqrlClose(), SqrlDelegate (via sqrl()), SqrlParam(), SqrlParams(),
  #   SqrlPL(), SqrlStatement(), SqrlSubScript(), SqrlTry().
  # utils Calls:
  #   head() (only if utils is attached).
  # SQRL Callers:
  #   SqrlConfig(), SqrlDelegate().
  # User:
  #   Has no direct access, but is able to submit (only) the script argument
  #  (only) via SqrlDelegate(). When script is a file path, SqrlDelegate() will
  #  already have confirmed the file's existence and readability. When it's not,
  #  SqrlDelegate() will have set literal TRUE.

  # Expand the temporary library stack by one layer, and ensure that layer is
  # removed whenever, and however, this function exits (cleanly or otherwise).
  SqrlParam(datasource, "libstack", "expand", override = TRUE)
  base::on.exit(SqrlParam(datasource, "libstack", "contract", override = TRUE))

  # Expand the temporary parameter stack by one layer, and ensure that layer is
  # removed whenever, and however, this function exits (cleanly or otherwise).
  SqrlParam(datasource, "pstack", "expand")
  base::on.exit(SqrlParam(datasource, "pstack", "contract"), add = TRUE)

  # When the script argument is a file path, slurp the entirety of that file.
  # No ordinary script would be so large that this should be a problem.
  if (!literal)
  {
    script <- base::paste(base::readLines(script, warn = FALSE, skipNul = TRUE),
                          collapse = "\n")
  }

  # Script delimiter definitions (regular expression patterns).
  patterns <- base::c(
    tag.r           = "<r>",
    tag.endr        = "</r>",
    tag.do          = "<do>",
    tag.stop        = "<stop>",
    tag.result      = "<result[[:blank:]]*->[[:blank:]]*[^[:space:]>]+>",
    tag.if          = "<if[[:blank:]]*\\(",
    tag.elseif      = "<else[[:blank:]]*if[[:blank:]]*\\(",
    tag.else        = "<else>",
    tag.endif       = "</if>",
    tag.while       = "<while[[:blank:]]*\\(",
    tag.endwhile    = "</while>",
    tag.return      = "<return[[:blank:]]*\\(",
    tag.close       = "<close>",
    tag.proc        = "<proc",
    tag.endproc     = "</proc>",
    tag.with        = "<with>",
    tag.endwith     = "</with>",
    end.expression  = ")>",
    comment.begin   = "/\\*",
    comment.end     = "\\*/",
    comment.line    = "--",
    comment.r       = "#",
    end.of.line     = "\n",
    quote.single    = "'",
    quote.double    = "\"",
    semi.colon      = ";")

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
    matches <- base::gregexpr(patterns[pattern],
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

  # Create a new environment as a child of the invoking environment.
  # SqrlFile() evaluates R expressions (including the post-processing) within
  # this environment (rather than the invoking environment) so as to avoid
  # overwriting variables within the invoking environment.
  sqrl.env <- base::new.env(parent = envir)

  # When this is an initial (unnested) call, create an interface for making
  # nested calls, and block the regular interface and public sqrl functions.
  # Any nested calls will inherit these assignments.
  if (base::length(SqrlParam(datasource, "libstack")) == 1L)
  {
    # Prevent calling the invoking interface from within the script. This is for
    # correct autoclose behaviour, which is achieved by the tracking of call
    # nesting within SqrlDelegate(). The stop() function prepends the offending
    # function name to the error message (so we don't have to).
    if (!base::is.null(SqrlParam(datasource, "interface")))
    {
      base::assign(SqrlParam(datasource, "interface"), function(...)
                    {base::stop("Calls from within scripts are blocked.")},
                    sqrl.env)
    }

    # Block calling any of the public sqrlXXX() functions from within a script.
    # This is for autoclose behaviour (nesting tracking) and to prevent a call
    # of one data source's interface from modifying the settings of any other.
    # The stop() function prepends the offending function's name to the message.
    for (fun in base::c("All", "Off", "Interface", "Source", "Sources"))
    {
      base::assign(base::paste0("sqrl", fun), function(...)
                    {base::stop("Calls from within scripts are blocked.")},
                    sqrl.env)
    }

    # Assign an interface to whichever datasource is running the script, into
    # the working environment. This interface works even when the datasource has
    # no devoted (regular) interface (in which case the script must have been
    # passed from sqrlAll()). This function is an intra-script replacement of
    # the regular interface (blocked above). It preserves nesting and makes
    # scripts interface-name indifferent (i.e., improves portability). Whereas
    # regular interfaces call SqrlShell(), this replacement goes directly to
    # SqrlDelegate(), so that autoclosure only occurs upon exiting the initial
    # (un-nested) user's command-line call.
    base::assign("sqrl", base::eval(base::parse(text = base::paste0(
                  "function(...) {SqrlDelegate(\"", datasource,
                  "\", base::parent.frame(), base::list(...))}"),
                  keep.source = FALSE)), sqrl.env)
  }

  # Assign any supplied parameters to the processing environment. The supplied
  # parameter names might not be unique, in which case the last value applies.
  for (i in base::seq_along(params))
  {
    # When a parameter is called 'args', and is a non-empty list within which
    # every member has a legitimate R-variable name, then individiually assign
    # each of its members into the processing environment (rather than assigning
    # the whole list, 'args', as a single object).
    if ((base::names(params)[i] == "args")
        && base::identical(base::class(params[[i]]), base::class(base::list()))
        && (base::length(params[[i]]) > 0L)
        && (!base::is.null(base::names(params[[i]])))
        && (base::all(base::names(params[[i]]) ==
                      base::make.names(base::names(params[[i]])))))
    {
      for (j in base::seq_along(params[[i]]))
      {
        base::assign(base::names(params[[i]])[j], params[[i]][[j]], sqrl.env)
      }

    # Otherwise, assign the named object to the processing environment.
    } else
    {
      base::assign(base::names(params)[i], params[[i]], sqrl.env)
    }
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

  # Initialise the procedural language extension tracker.
  pl <- SqrlPL(NULL)

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

        # Update the procedural-language state tracker.
        pl <- SqrlPL(pl, phrase)

        # Append the phrase to the statement (unless the phrase is empty).
        # This is an error when the script is meant to define a library.
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

        # Update the procedural-language state tracker.
        pl <- SqrlPL(pl, phrase)

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
      # which also guarantees i > 1). Some SQLs use doubled quotes within quoted
      # strings to represent quote literals. This is supported here, via the
      # following mechanism: 'x''' is read as two adjacent strings, 'x' and '',
      # which are eventually collapsed together (with an empty string between),
      # restoring the original 'x''' in the final SQL statement (string).
      closing.quote <- pat[i]
      i <- i + 1L
      while ((i <= num.delims)
              && ((pat[i] != closing.quote)
                  || ((base::attr(base::regexpr(
                        base::paste0("\\\\*", patterns[closing.quote], "$"),
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

        # Update the procedural-language state tracker.
        pl <- SqrlPL(pl, phrase)

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

    # Transfer procedure definitions into either the permanent (source
    # parameter) or temporary (working stack) library, without modification.
    if ((i <= num.delims)
        && (pat[i] == "tag.proc"))
    {
      # Position of the character immediately before this potential proc tag. If
      # it turns out to be an actual proc tag, then this position is needed to
      # check for unsubmitted SQL.
      k.prime <- pos[i] - 1L

      # If this really is a proc tag, there must be nothing but horizontal
      # whitespace between the matched pattern and a quotation mark (single or
      # double). Since horizontal whitespace is not a matched pattern, that
      # quote mark must be the next matched pattern. If it's not, then this is
      # not a proc tag after all (is just SQL), and we continue with the next.
      i <- i + 1L
      if ((i > num.delims)
          || !(pat[i] %in% base::c("quote.single", "quote.double"))
          || !base::grepl("^[[:blank:]]*$", base::substring(script,
                            pos[i - 1L] + len[i - 1L], pos[i] - 1L)))
      {
        next
      }

      # Scan through the subsequent script delimiters, until the string
      # concludes with a matching quote delimiter, or we reach the end of the
      # file. We only test for \ escaped quotes on the inside of the string.
      j <- i
      i <- i + 1L
      while ((i <= num.delims)
              && ((pat[i] != pat[j])
                  || ((base::attr(base::regexpr(
                        base::paste0("\\\\*", patterns[pat[j]], "$"),
                        base::substring(script, pos[i - 1L], pos[i])),
                        "match.length") %% 2L) == 0L)))
      {
        i <- i + 1L
      }

      # Stop if the end of the (procedure name) string was not found.
      if ((i > num.delims)
          || (pat[i] != pat[j]))
      {
        base::stop("Unterminated procedure name.")
      }

      # Stop if the character immediately after the (name string) closing quote
      # is not a (tag-closing) angle bracket ('>').
      if (base::substring(script, pos[i] + 1L, pos[i] + 1L) != ">")
      {
        base::stop("Badly formatted proc tag (improperly terminated).")
      }

      # Stop if there's any unsubmitted SQL before the proc tag.
      if (base::any(base::grepl("[[:graph:]]", base::unlist(statement)))
          || base::any(base::grepl("[[:graph:]]",
                                    base::substring(script, k, k.prime))))
      {
        if (libmode)
        {
          base::stop("Text outside of a procedure definition.")
        }
        base::stop("Unsubmitted SQL preceding a procedure definition.")
      }

      # Extract the name of the procedure.
      proc.name <- base::substring(script, pos[j] + len[j], pos[i] - 1L)

      # Ensure the proc name is not empty or blank, and does not contain any
      # control characters (new line, carriage return, tab, vertical tab, etc.)
      if (!base::grepl("[[:graph:]]", proc.name)
          || base::grepl("[[:cntrl:]]", proc.name))
      {
        base::stop("Invalid procedure name.")
      }

      # We have found one proc tag within a SQL section (not within an R block).
      nproc <- 1L
      rblock <- FALSE

      # Reposition the start-of-phrase index immediately after the proc tag.
      k <- pos[i] + 2L

      # Scan the procedure definition (stepping by delimiter), to find its end.
      while ((i <= num.delims)
              && (nproc > 0L))
      {
        # Advance to the next delimiter.
        i <- i + 1L

        # The end of the script concludes the definition, as does a stop tag.
        if ((i > num.delims)
            || (pat[i] == "tag.stop"))
        {
          break
        }

        # Ignore delimiters within comments (advance to the end of the comment).
        if (pat[i] %in% base::c("comment.line", "comment.begin"))
        {
          end.marker <- base::switch(pat[i],
                                      comment.line = "end.of.line",
                                      comment.begin = "comment.end")
          i <- i + 1L
          while ((i <= num.delims)
                  && (pat[i] != end.marker))
          {
            i <- i + 1L
          }

        # Ignore delimiters within R comments (only when within an R section).
        } else if (rblock
                    && (pat[i] == "comment.r"))
        {
          i <- i + 1L
          while ((i <= num.delims)
                  && (pat[i] != "end.of.line"))
          {
            i <- i + 1L
          }

        # Ignore delimiters within quotes (advance to the end of the quote).
        } else if (pat[i] %in% base::c("quote.single", "quote.double"))
        {
          closing.quote <- pat[i]
          i <- i + 1L
          while ((i <= num.delims)
                  && ((pat[i] != closing.quote)
                      || ((base::attr(base::regexpr(
                            base::paste0("\\\\*", patterns[closing.quote], "$"),
                            base::substring(script, pos[i - 1L], pos[i])),
                            "match.length") %% 2L) == 0L)))
          {
            i <- i + 1L
          }

        # R sections begin with either an <R> or <result> tag.
        } else if ((i <= num.delims)
                    && (pat[i] %in% base::c("tag.r", "tag.result")))
        {
          rblock <- TRUE

        # R sections are terminated by </R> or <do> tags (revert to SQL).
        } else if (rblock
                    && (pat[i] %in% base::c("tag.endr", "tag.do")))
        {
          rblock <- FALSE

        # R sections are also terminated by an extra semicolon (revert to SQL).
        } else if (rblock
                    && (pat[i] == "semi.colon")
                    && (pat[i - 1L] %in% base::c("end.of.line", "semi.colon"))
                    && !base::grepl("[[:graph:]]", base::substring(script,
                                      pos[i - 1L] + len[i - 1L], pos[i] - 1L)))
        {
          rblock <- FALSE

        # Upon meeting an end-of-procedure tag, decrement the nested-procedures
        # counter. These tags are recognised both within SQL and R sections, and
        # terminate the later (reverting to SQL).
        } else if (pat[i] == "tag.endproc")
        {
          nproc <- nproc - 1L
          rblock <- FALSE

        # Upon meeting a start-of-procedure tag within a SQL section, increment
        # the nested-procedure counter. These tags are not recognised within R,
        # for consistency with the primary (extra-procedural) R-block parser
        # (below). That parser does not recognise </proc> tags either (which we
        # do here), but it is not applied within an open procedural definition.
        } else if (!rblock
                  && (pat[i] == "tag.proc")
                  && (pat[i + 1L] %in% base::c("quote.single", "quote.double"))
                  && base::grepl("^[[:blank:]]*$",
                    base::substring(script, pos[i] + len[i], pos[i + 1L] - 1L)))
        {
          closing.quote <- pat[i + 1L]
          j <- i + 1L
          i <- i + 2L
          while ((i <= num.delims)
                  && ((pat[i] != closing.quote)
                      || ((base::attr(base::regexpr(
                          base::paste0("\\\\*", patterns[closing.quote], "$"),
                          base::substring(script, pos[i - 1L], pos[i])),
                          "match.length") %% 2L) == 0L)))
          {
            i <- i + 1L
          }
          if (i <= num.delims)
          {
            pname <- base::substring(script, pos[j] + len[j], pos[i] - 1L)
            if (base::grepl("[[:graph:]]", pname)
                && !base::grepl("[[:cntrl:]]", pname)
                && base::substring(script, pos[i] + 1L, pos[i] + 1L) == ">")
            {
              nproc <- nproc + 1L
            }
          }
        }
      }

      # If no closing tag was found, the procedure ends with the script. Extract
      # the procedure, and move the start-of-phrase index beyond the end of the
      # script (indicating there's no unprocessed script remaining).
      if (i > num.delims)
      {
        proc.body <- base::substring(script, k, nchar.script)
        k <- nchar.script + 1L

      # Otherwise, the definition of the procedure ends immediately before the
      # closing tag (that was found). Extract the procedure, up to the tag.
      } else
      {
        proc.body <- base::substring(script, k, pos[i] - 1L)
        # If the definition was terminated by a stop tag, then ignore the rest
        # of the script (move the delimiter and phrase indices beyond its end).
        if (pat[i] == "tag.stop")
        {
          i <- num.delims + 1L
          k <- nchar.script + 1L
        # Otherwise, the definition was terminated by an end-of-procedure tag.
        # Advance the start-of-phrase index to the character after that tag.
        } else
        {
          k <- pos[i] + len[i]
        }
      }

      # Remove leading and trailing whitespace from the procedure. If it
      # originally contained one or more trailing newlines, restore one.
      tnl <- base::grepl("\\n[[:space:]]*$", proc.body)
      proc.body <- base::trimws(proc.body)
      if (tnl)
      {
        proc.body <- base::paste0(proc.body, "\n")
      }

      # It is possible, outside of library mode, that the procedure definition
      # might appear within the block of an untrue conditional, in which case it
      # should not be added to the stack.
      if (cond.current)
      {
        # Apply the name to the procedure.
        base::names(proc.body) <- proc.name

        # Add the procedure to either the library or the stack. This operation
        # requires the use of override = TRUE.
        if (libmode)
        {
          SqrlParam(datasource, "library", proc.body, override = TRUE)
        } else
        {
          SqrlParam(datasource, "libstack", proc.body, override = TRUE)
        }

        # If verbose, advise the user of the addition.
        if (base::interactive()
            && SqrlParam(datasource, "verbose"))
        {
          base::cat("\n")
          if (libmode)
          {
            base::cat(base::paste0("Added '", proc.name,
                                    "' to the library:\n"))
          } else
          {
            base::cat(base::paste0("Defined procedure '", proc.name, "':\n"))
          }
          base::cat(proc.body)
          base::cat("\n")
        }
      }

      # Advance to the next script delimiter.
      i <- i + 1L
    }

    # Submit the statement (and retrieve the result) on encountering a 'do' tag
    # within SQL.
    if ((i <= num.delims)
        && (pat[i] == "tag.do"))
    {
      # Prohibit query-submission in library mode.
      if (libmode)
      {
        base::stop("Text outside of a procedure definition.")
      }

      # Submit the statement, plus any unappended fragment, unless it is within
      # the block of an untrue conditional expression.
      if (cond.current)
      {
        # Isolate any unappended (to the statement) script preceding the tag.
        phrase <- base::substring(script, k, pos[i] - 1L)

        # Submit the statement (with phrase) and pull the result.
        dat <- base::withVisible(SqrlSubScript(datasource, statement, phrase))

        # If there was a result (there was a query), replace the overall result.
        if (!base::is.null(dat$value))
        {
          result <- dat
        }

        # Reset the statement (begin the next one afresh).
        statement <- base::list()

        # Reset the procedural-language state tracker.
        pl <- SqrlPL(NULL)
      }

      # Reposition the start-of-phrase index immediately after the tag.
      k <- pos[i] + len[i]

      # Advance to the next script delimiter.
      i <- i + 1L
    }

    # Act upon a semicolon, encountered within SQL. Dependent upon the current
    # situation, either submit a query or do nothing.
    if ((i <= num.delims)
          && (pat[i] == "semi.colon"))
    {
      # Prohibit query-submission in library mode.
      if (libmode)
      {
        base::stop("Text outside of a procedure definition.")
      }

      # Consider the posibility this semicolon might termintate a complete
      # query, only when the scdo parameter is TRUE.
      if (SqrlParam(datasource, "scdo"))
      {
        # Assess the nature of the semicolon, unless it is within the block of
        # an untrue conditional expression
        if (cond.current)
        {
          # Isolate any unappended (to the statement) script; up to, and
          # including, the semicolon.
          phrase <- base::substring(script, k, pos[i] + len[i] - 1L)

          # Remove trailing whitespace from each internal line of the phrase.
          phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

          # Remove vertical whitespace from within the phrase.
          phrase <- base::gsub("\n+", "\n", phrase)

          # Update the procedural-language state tracker.
          pl <- SqrlPL(pl, phrase)

          # Append the phrase here, to avoid running it through the state
          # tracker a second time, if no query is submitted below. The phrase
          # can't be empty, because it ends with the semicolon.
          statement <- base::append(statement, phrase)

          # The semicolon is considered to terminate a complete SQL statement,
          # if we're not in a PL block, or if the PL block has ended. When such
          # is the case, a query could be submitted (it might not be, just yet).
          if (!pl$block
              || ((pl$begins > 0L)
                  && (pl$ends >= pl$begins)))
          {

            # The terminal semicolon will be treated a do tag (the query will be
            # submitted, unless there's nothing but whitespace between it and a
            # subsequent do or result tag.
            do <- TRUE
            if (i < num.delims)
            {
              j <- base::which(pat %in% base::c("tag.do", "tag.result"))
              if (base::any(j > i))
              {
                j <- base::min(j[j > i])
                do <- !base::grepl("^[[:space:]]*$",
                          base::substring(script, pos[i] + len[i], pos[j] - 1L))
              }
            }

            # When the semicolon is not followed by a do or result tag, submit
            # query and retrieve the result.
            if (do)
            {
              # Submit the statement, and pull the result. There ought to be
              # something, because the query at least contains the semicolon.
              result <- base::withVisible(SqrlSubScript(datasource, statement))

              # Reset the statement (begin the next one afresh).
              statement <- base::list()

              # Reset the procedural-language state tracker.
              pl <- SqrlPL(NULL)
            }
          }
        }

        # Reposition the start-of-phrase index immediately after the marker.
        k <- pos[i] + len[i]
      }

      # Advance to the next script delimiter, whether or not scdo is TRUE, and
      # whether or not any query was submitted.
      i <- i + 1L
    }

    # Act upon condition end and else tags, encountered within SQL.
    if ((i <= num.delims)
        && (pat[i] %in% base::c("tag.endif", "tag.endwhile", "tag.else")))
    {
      # Prohibit R-execution (potential query-submission) in library mode.
      if (libmode)
      {
        base::stop("Text outside of a procedure definition.")
      }

      # Remember the type of tag we've encountered.
      pat.type = pat[i]

      # Throw an exception if we're ending a loop that was never started.
      if ((pat.type == "tag.endwhile")
          && cond.current
          && (base::length(loop.points) < 1L))
      {
        base::stop("End without while.")
      }

      # Throw an exception if we're ending a block that was never started.
      if ((pat.type == "tag.endif")
          && base::length(cond.stack) < 1L)
      {
        base::stop("End without if.")
      }

      # Throw an exception if we've met an else but not a previous if.
      if ((pat.type == "tag.else")
          && (base::length(else.stack) < 1L))
      {
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

        # Update the procedural-language state tracker.
        pl <- SqrlPL(pl, phrase)

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
      # Prohibit R-execution (potential query-submission) in library mode.
      if (libmode)
      {
        base::stop("Text outside of a procedure definition.")
      }

      # Remember which type of tag we've encountered, and where we found it.
      tag.type <- pat[i]
      tag.pos <- i

      # Abort on else-if without prior if (avoids an uncontrolled error later).
      if ((tag.type == "tag.elseif")
          && (base::length(else.stack) < 1L))
      {
        base::stop("Else-if without if.")
      }

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

        # Update the procedural-language state tracker.
        pl <- SqrlPL(pl, phrase)

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
                          base::paste0("\\\\*", patterns[closing.quote], "$"),
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

              # Evaluate the tag expression. On error, stop with exception.
              tval <- SqrlTry(base::withVisible(base::eval(
                                  base::parse(text = expr, keep.source = FALSE),
                                  sqrl.env)))
              if (tval$error)
              {
                base::stop(tval$value)
              }
              tval <- tval$value

              # In the case of a return tag, return the evaluated expression
              # (stop processing the SQRL script, and exit from this point).
              # That exit is an exception, if any unsubmitted SQL exists.
              if (tag.type == "tag.return")
              {
                if (base::any(base::grepl("[[:graph:]]",
                                          base::unlist(statement))))
                {
                  base::stop("Unsubmitted SQL preceding a <return> tag.")
                }
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
              if (base::interactive()
                  && SqrlParam(datasource, "verbose"))
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
        base::stop("Unterminated intra-tag expression.")
      }
    }

    # Evaluate embedded R (and insert into SQL or produce a result).
    if ((i <= num.delims)
        && pat[i] %in% base::c("tag.r", "tag.result"))
    {
      # Prohibit R-execution (potential query-submission) in library mode.
      if (libmode)
      {
        base::stop("Text outside of a procedure definition.")
      }

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

          # Reset the procedural-language state tracker.
          pl <- SqrlPL(NULL)

        # Otherwise (this is an R substitution into SQL), clean the phrase (but
        # do not remove pre-tag trailing whitespace) and then append it to the
        # statement. The phrase will never contain a string literal.
        } else
        {
          # Remove trailing whitespace from each internal line of the phrase.
          phrase <- base::gsub("[[:blank:]]+\n", "\n", phrase)

          # Remove vertical whitespace from within the phrase.
          phrase <- base::gsub("\n+", "\n", phrase)

          # Update the procedural-language state tracker.
          pl <- SqrlPL(pl, phrase)

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
              && !(pat[i] %in% base::c("tag.endr", "tag.do")))
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
                        base::paste0("\\\\*", patterns[closing.quote], "$"),
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
          # Provided scdo is TRUE, if there is nothing but whitespace between
          # the semicolon and the start of its line (or the R tag, or the result
          # tag, or the previous semicolon), then interpret the semicolon as a
          # do tag (the R parser won't accept it, anyway), and stop looking for
          # one. We need not append the whitespace to the R script.
          if (base::grepl("^[[:space:]]*$",
                              base::substring(script, k, pos[i] - 1L))
              && SqrlParam(datasource, "scdo"))
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
        if ((r.type == "tag.r")
            && (i <= num.delims)
            && (pat[i] == "tag.endr"))
        {
          sqlisedvalue <- SqrlTry(SqrlStatement(datasource,
                base::list(base::eval(
                  base::parse(text = rscript, keep.source = FALSE), sqrl.env))))
          if (sqlisedvalue$error)
          {
            base::stop(sqlisedvalue$value)
          }
          pl <- SqrlPL(pl, sqlisedvalue$value)
          statement <- base::append(statement, sqlisedvalue$value)

        # Otherwise (R post-processing), evaluate and retain the result.
        } else
        {
          # Stop if there's any unsubmitted SQL before an <R> ... <do> section.
          # (SQL is always submitted before a <result> ... <do> section.)
          if (base::any(base::grepl("[[:graph:]]", base::unlist(statement))))
          {
            base::stop("Unsubmitted SQL preceding an <R> ... <do> section.")
          }

          # If in verbose mode, output the script (prior to evaluation).
          if (base::interactive()
              && SqrlParam(datasource, "verbose"))
          {
            base::cat("\n")
            base::cat(rscript)
            base::cat("\n")
          }

          # Evaluate the script, and retain the result, only if the script is
          # non-empty (in the sense of containing no uncommented statements).
          parsed <- SqrlTry(base::parse(text = rscript, keep.source = FALSE))
          if (parsed$error)
          {
            base::stop(parsed$value)
          }
          if (!base::identical(base::as.character(parsed$value),
                                base::character(0L)))
          {
            # Evaluate the script, retain the result. As above, stop with an
            # error should such occur.
            result <- SqrlTry(
                          base::withVisible(base::eval(parsed$value, sqrl.env)))
            if (result$error)
            {
              base::stop(result$value)
            }
            result <- result$value

            # If verbose, output (some of) the result. This could be any object,
            # with no guarantee of either the head() or print() methods.
            if (base::interactive()
                && SqrlParam(datasource, "verbose"))
            {
              printed <- FALSE
              if ("package:utils" %in% base::search())
              {
                top <- SqrlTry(utils::head(result$value))
                if (!top$error)
                {
                  printed <- !SqrlTry(base::print(top$value))$error
                  if (printed
                      && !base::identical(top$value, result$value))
                  {
                    base::cat("(output truncated)\n")
                  }
                }
              }
              if (!printed)
              {
                base::cat(base::paste0("(object of class '",
                        base::paste0(base::class(result$value), collapse = " "),
                        "')\n"))
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

    # Assign temporary parameter values.
    if ((i <= num.delims)
        && (pat[i] == "tag.with"))
    {
      # Prohibit changing RODBC and/or SQRL parameters in library mode.
      if (libmode)
      {
        base::stop("Text outside of a procedure definition.")
      }

      # Stop if there's any unsubmitted SQL before the tag, unless within the
      # block of an untrue conditional expression.
      if ((cond.current)
          && base::any(base::grepl("[[:graph:]]", base::unlist(statement)))
          || base::any(base::grepl("[[:graph:]]",
                                    base::substring(script, k, pos[i] - 1L))))
      {
        base::stop("Unsubmitted SQL preceding a <with> block.")
      }

      # Reposition the start-of-phrase index immediately after the tag.
      k <- pos[i] + len[i]

      # Isolate the with section. This involves parsing (to find the end of the
      # section), even when cond.current is FALSE.
      withs <- base::list()
      i <- i + 1L
      while ((i <= num.delims)
              && !(pat[i] %in% base::c("tag.endwith", "tag.stop")))
      {
        # Remove SQL comments from the with block's R expressions.
        if ((i <= num.delims)
            && (pat[i] %in% base::c("comment.line", "comment.begin")))
        {
          # Isolate any unappended script preceding this comment, and append
          # it to the withs-script.
          withs <- base::append(withs, base::substring(script, k, pos[i] - 1L))

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
          # We only test for \ escaped quotes here (once already within quote
          # mode, which also guarantees that i > 1).
          closing.quote <- pat[i]
          i <- i + 1L
          while ((i <= num.delims)
                  && ((pat[i] != closing.quote)
                      || ((base::attr(base::regexpr(
                        base::paste0("\\\\*", patterns[closing.quote], "$"),
                          base::substring(script, pos[i - 1L], pos[i])),
                          "match.length") %% 2L) == 0L)))
          {
            i <- i + 1L
          }
        }

        # Advance to the next script delimiter.
        i <- i + 1L
      }

      # Process the withs script, unless within the block of an untrue
      # conditional expression.
      if (cond.current)
      {
        # Append the final chunk to the withs-script.
        phrase <- base::ifelse(i <= num.delims,
                                base::substring(script, k, pos[i] - 1L),
                                base::substring(script, k))
        withs <- base::append(withs, phrase)

        # Collapse the withs-script (list) to a single string.
        withs <- base::paste(withs, collapse = "\n")

        # Attempt to parse the entire withs-script to an R expression.
        # Expressions remain unevaluated at this point.
        withs <- SqrlTry(base::parse(text = withs, keep.source = FALSE))
        if (withs$error)
        {
          base::stop("Failed to parse <with> block.")
        }
        withs <- withs$value

        # Create a sub-environment of the main-script working environment
        # (sqrl.env), within which to evaluate expressions of the withs-script.
        w.env <- base::new.env(parent = sqrl.env)

        # Consistently apply the verbose mode in effect prior to the <with>
        # block (whether or not the verbose value is changed within the block).
        verbose <- base::interactive() && SqrlParam(datasource, "verbose")

        # In verbose mode, add vertical whitespace before showing the values.
        if (verbose)
        {
          base::cat("\n")
        }

        # Evaluate each item of the withs-script expression in turn.
        for (w.itm in withs)
        {
          # Deparse this item's expression to text, wrap it in a list, and parse
          # back to an expression. This ought to succeed, since the expression
          # has been parsed before (as a part of the whole block, just above).
          w.txt <- base::paste0(base::deparse(w.itm), collapse = "\n")
          w.exp <- base::parse(text = base::paste0("list", "(", w.txt, ")"),
                                keep.source = FALSE)

          # Evaluate the expression within the with-block sub-environment.
          # Errors here are fatal. Warnings are visible.
          w.val <- base::eval(w.exp, w.env)

          # Any successful result must be a list. When that list comprises a
          # single named member, we interpret it as an intended SQRL/RODBC
          # parameter value.
          if ((base::length(w.val) == 1L)
              && !base::is.null(base::names(w.val)))
          {
            w.par <- base::names(w.val)

            if ((w.par %in% SqrlParams("locked-while-open"))
                || (w.par %in% SqrlParams("no-temp-allowed")))
            {
              base::stop("Parameter does not accept temporary values.")
            }

            # Attempt to set the temporary parameter value.
            SqrlParam(datasource, "pstack", w.val)

            # If in verbose mode, print the temporary value.
            if (verbose)
            {
              w.val <- base::deparse(SqrlParam(datasource, w.par))
              base::cat("Using:", w.par, "=", w.val, "\n")
            }
          }
        }

        # In verbose mode, add vertical whitespace after showing the values.
        if (verbose)
        {
          base::cat("\n")
        }
      }

      # When the section ends with a stop tag, skip the rest of the script.
      if ((i <= num.delims)
          && (pat[i] == "tag.stop"))
      {
        i <- num.delims + 1L
        k <- nchar.script + 1L
        break
      }

      # Reposition the start-of-phrase index immediately after this section.
      k <- base::ifelse(i <= num.delims, pos[i] + len[i], nchar.script + 1L)

      # Advance to the next script delimiter.
      i <- i + 1L
    }

    # Process a close tag, encountered within SQL.
    if ((i <= num.delims)
        && (pat[i] == "tag.close"))
    {
      # Prohibit changing the connection status in library mode.
      if (libmode)
      {
        base::stop("Text outside of a procedure definition.")
      }

      # Perform the closure action, unless within an untrue conditional block.
      if (cond.current)
      {
        # Stop if there's any unsubmitted SQL before the close directive.
        if (base::any(base::grepl("[[:graph:]]", base::unlist(statement)))
            || base::any(base::grepl("[[:graph:]]",
                                      base::substring(script, k, pos[i] - 1L))))
        {
          base::stop("Unsubmitted SQL preceding a <close> tag.")
        }

        # Close the channel to the source.
        SqrlClose(datasource)

        # If verbose, notify of the closure.
        if (base::interactive()
            && SqrlParam(datasource, "verbose"))
        {
          base::cat("\nConnection channel closed.\n")
        }
      }

      # Reposition the start-of-phrase index immediately after this tag.
      k <- base::ifelse(i <= num.delims, pos[i] + len[i], nchar.script + 1L)

      # Advance to the next script delimiter.
      i <- i + 1L
    }

    # Take no special action at any other delimiter: end-of-line, end-of-intra-
    # tag-expression, intra-SQL R comment marker, SQL end-comment-block marker,
    # end-of-procedure-definition, or end-of-intra-SQL-embedded-R. In all cases
    # we assume this is legitimate SQL and proceed to the next delimiter. This
    # assumption is highly probable for the first two, and highly unlikely for
    # the last three. When the assumption is wrong, the ODBC driver will return
    # an error. However, if we were to throw a potentially more helpful error
    # here, we might be blocking a legitimate query without even trying it.
    if ((i <= num.delims)
        && (pat[i] %in% base::c("end.of.line", "end.expression", "comment.r",
                                "comment.end", "tag.endproc", "tag.endr")))
    {
      i <- i + 1L
    }
  }

  # Reaching this point means we are in a (possibly empty) SQL block, and there
  # are no delimiters (patterns) between the start-of-phrase index, k, and the
  # end of the script. It may even be that k is beyond the end of the script.

  # Unless within the block of an untrue condition, append all (any) remaining
  # (SQL) script to the current (SQL) statement.
  if (cond.current)
  {
    statement <- base::append(statement, base::substring(script, k))
  }

  # Prohibit query-submission in library mode.
  if (libmode
      && (base::any(base::grepl("[[:graph:]]", base::unlist(statement)))))
  {
    base::stop("Text outside of a procedure definition.")
  }

  # Submit the statement and pull the result. The statement might be blank or
  # empty, in which case the result will be NULL.
  dat <- base::withVisible(SqrlSubScript(datasource, statement))

  # If there was a result (if there was a query), replace the overall result.
  if (!base::is.null(dat$value))
  {
    result <- dat
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
  #   SqrlConfig(), SqrlHelp() (self), SqrlHelper(), SqrlPath(), SqrlTry(),
  #   srqlHelp.
  # tools Calls:
  #   Rd2HTML(), Rd2txt() (only if the tools package is installed).
  # utils Calls:
  #   browseURL(), help() (only if utils is attached).
  # SQRL Callers:
  #   SqrlDelegate(), SqrlHelp() (self), .onLoad(), .onUnload().
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
    base::return(base::cat("https://CRAN.R-project.org/package=SQRL\n"))
  }

  # If the tools package (which converts Rd files) is unavailable, display the
  # pre-built (static) interface-usage help page, and return.
  if (base::length(base::find.package("tools", quiet = TRUE)) == 0L)
  {
    base::return(utils::help("sqrlUsage", help_type = type))
  }

  # The tools package is available. We shall dynamically generate tailored help
  # for the invoking (data source's) interface function. This involves temp
  # files. Remove any existing SQRL temp files (from any prior SqrlHelp() call).
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
    driver <- "unknown or undefined"
  }

  # Extract and escape the data source's (SQRL) name.
  if (base::identical(config[["name"]], config[["interface"]]))
  {
    dsrc <- "of the same name"
  } else
  {
    dsrc <- base::paste0("\\file{", SqrlHelper(config[["name"]]), "}")
  }

  # Establish the channel status, and choose the appropriate phrase.
  if (base::is.null(config[["channel"]]))
  {
    ochan <- "closed"
  } else
  {
    ochan <- "open"
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
    base::paste0("\\title{ODBC Interface Function \\sQuote{", iface, "}}"),
    "\\description{",
    base::paste0("The function \\code{", iface, "} is"),
    base::paste0("the interface to the data source ", dsrc,"."),
    base::paste0("The \\acronym{ODBC} driver is ", driver, "."),
    base::paste0("Communications are ", ochan, "."),
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
    if (SqrlTry(tools::Rd2HTML(rdfile, htmlfile, package = "SQRL",
                                stylesheet = base::basename(cssfile)))$error)
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
    if (SqrlTry(tools::Rd2HTML(rdfile, htmlfile, package = "SQRL",
                                stylesheet = cssfile))$error)
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
  if (SqrlTry(tools::Rd2txt(rdfile, txtfile, package = "SQRL"))$error)
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
  # Any RODBC channel is reduced to its integer label.
  # Any library is truncated to its first element (plus an ellipsis).
  if (base::identical(base::class(value), base::class(base::list())))
  {
    if (!base::is.null(value[["channel"]]))
    {
      value[["channel"]] <- base::as.numeric(value[["channel"]])
    }
    if ("library" %in% base::names(value))
    {
      if (!base::is.null(value[["library"]])
          && (base::length(value[["library"]]) > 1L))
      {
        value[["library"]] <- base::paste0(
                                base::deparse(value[["library"]][1L]), ", ...")
      } else
      {
        value[["library"]] <- base::deparse(value[["library"]])
      }
      value[["library"]] <- base::gsub("%", "\\\\%", value[["library"]])
    }
    for (name in base::names(value)[base::names(value) != "library"])
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
  #   SqrlDelegate(), SqrlParam(), SqrlPing(), SqrlSubmit().
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

  # When the action is 'query', 'fetch', or 'ping', append a job-in-progress
  # marker ('*', '+', or '?', respectively) to the data source's window title
  # indicator, then return invisible NULL. This will work (as in, does nothing,
  # quietly) if the indicator isn't actually on.
  if (action %in% base::c("query", "fetch", "ping"))
  {
    glyph <- base::switch(action, "query" = "*", "fetch" = "+", "ping" = "?")
    if (do.title)
    {
      indic <- SqrlParam(datasource, "wintitle")
      glyphed <- base::paste0(indic, glyph)
      if (base::grepl("[[:graph:]]", indic))
      {
        windowtitle <- utils::getWindowTitle()
        for (unglyphed in base::paste0(indic, base::c(" ", "*", "+", "?", "")))
        {
          if (base::grepl(unglyphed, windowtitle, fixed = TRUE))
          {
            utils::setWindowTitle(title = base::sub("\\s+$", "",
                      base::sub(unglyphed, glyphed, windowtitle, fixed = TRUE)))
            break
          }
        }
      }
    }
    base::return(base::invisible(NULL))
  }

  # When the action is 'done', remove a job-in-progress marker ('*', '+', '?')
  # from the data source's window title indicator, then return invisible NULL.
  # This will work (does nothing, quietly) if no marker is actually present.
  if (action == "done")
  {
    if (do.title)
    {
      indic <- SqrlParam(datasource, "wintitle")
      unglyphed <- base::paste0(indic, " ")
      if (base::grepl("[[:graph:]]", indic))
      {
        windowtitle <- utils::getWindowTitle()
        for (glyphed in base::paste0(indic, base::c("*", "+", "?")))
        {
          if (base::grepl(glyphed, windowtitle, fixed = TRUE))
          {
            utils::setWindowTitle(title = base::sub("\\s+$", "",
                      base::sub(glyphed, unglyphed, windowtitle, fixed = TRUE)))
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
  uibody <- base::paste0("function(...) {SqrlShell(\"", datasource,
                          "\", base::parent.frame(), base::list(...))}")

  # Abort on invalid interface (name). Allowed values are NULL or a character
  # string. The requested name may, or may not, be available and assignable.
  if (!base::is.null(interface)
      && (!base::identical(base::class(interface),
                            base::class(base::character()))
          || (base::length(interface) != 1L)
          || !base::nzchar(base::trimws(interface))))
  {
    if (!vital)
    {
      base::return(base::invisible(NULL))
    }
    base::stop("Invalid interface name.")
  }

  # Remove any name and leading or trailing whitespace from the interface
  # argument. Applying trimws() to NULL would produce character(0). The
  # as.character() function removes any name attribute the string may have.
  if (!base::is.null(interface))
  {
    interface <- base::trimws(base::as.character(interface))
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
  #   SqrlClose(), SqrlParam(), SqrlPing(), SqrlTry().
  # RODBC Calls:
  #   odbcGetInfo().
  # SQRL Callers:
  #   SqrlDelegate(), SqrlOpen(), SqrlParam(), SqrlSource(), SqrlSources(),
  #   SqrlSubmit().
  # User:
  #   Has no direct access, and is unable to indirectly supply either argument.
  #   Argument validity checks are not required.

  # Attempt to obtain the channel parameter value for the specified data source.
  channel <- SqrlTry(SqrlParam(datasource, "channel"), warn = FALSE)

  # Return FALSE when the datasource is invalid (does not exist => is not open).
  if (channel$error)
  {
    base::return(FALSE)
  }

  # Isolate the value of the channel parameter (strip the SqrlTty() error flag).
  channel <- channel$value

  # Return FALSE when the channel is closed (and we knew that).
  if (base::is.null(channel))
  {
    base::return(FALSE)
  }

  # Return FALSE when the channel is not an RODBC handle (in which case we may
  # have mistakenly thought the channel was open, since it was non-null valued).
  if (!base::identical(base::class(channel), "RODBC"))
  {
    SqrlClose(datasource)
    base::return(FALSE)
  }

  # Attempt to obtain channel information. This will fail if the channel has
  # been closed from our end, or is not of RODBC class (repeating, in effect,
  # the test above), but will succeed if the channel has been closed from the
  # other end (and we were previously unaware of that).
  info <- SqrlTry(RODBC::odbcGetInfo(channel), warn = FALSE)

  # Return FALSE when the channel is closed (but we thought it was open).
  if (info$error)
  {
    SqrlClose(datasource)
    base::return(FALSE)
  }

  # When besure is FALSE (the default), we only check openness from our end.
  # That being the case, return TRUE if we get to this point (the channel
  # appears to be open from our end) and we're not going to be more thorough.
  if (!besure)
  {
    base::return(TRUE)
  }

  # Otherwise (when we want to be thorough), ping the source to make sure.
  # If the ping succeeds, the connection must be open.
  if (SqrlPing(datasource))
  {
    base::return(TRUE)
  }

  # The ping failed; the connection is not open after all (has been dropped at
  # the source's end). Formally close it at this end, before returning the
  # openness status (FALSE).
  SqrlClose(datasource)
  base::return(FALSE)
}

SqrlOff <- function()
{
  # Close SQRL channels, deactivate SQRL.
  # Args:
  #   None.
  # Returns:
  #   Invisible NULL, after closing channels and detaching SQRL.
  # SQRL Calls:
  #   SqrlCache(), SqrlClose(), SqrlInterface(), SqrlTry().
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
    SqrlTry(SqrlClose(datasource), warn = FALSE)
    SqrlTry(SqrlInterface(datasource, "remove"), warn = FALSE)
    cache <- SqrlCache(datasource)
    SqrlTry(base::remove(list = base::objects(pos = cache, all.names = TRUE),
                          pos = cache), warn = FALSE)
  }

  # Detach the public SQRL:Face (interfaces) environment. The garbage collector
  # should handle the rest. Again, wrapped in try() so that a failure here won't
  # have knock-on effects.
  SqrlTry(base::detach("SQRL:Face"), warn = FALSE)

  # Detach and unload the SQRL package. The .onUnload() function attempts to
  # detach SQRL:Face once again (but this doesn't matter).
  SqrlTry(base::detach("package:SQRL", unload = TRUE), warn = FALSE)

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
  #   SqrlIsOpen(), SqrlParam(), SqrlParams(), SqrlPing(), SqrlTry().
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
    channel <- SqrlTry(
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
    channel <- SqrlTry(
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
  # When RODBC::odbcConnect or RODBC::odbcDriverConnect encounter a failure to
  # connect, they do not stop with an error message, but instead return -1 and
  # throw warning messages with the details.
  if (channel$error
      || !base::identical(base::class(channel$value), "RODBC"))
  {
    base::stop("Connection attempt failed.")
  }

  # Looks like a valid connection channel was established. Record handle.
  channel <- SqrlParam(datasource, "channel", channel$value)

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
    SqrlPing(datasource, set = TRUE)
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
  #   SqrlCache(), SqrlClose(), SqrlConfig(), SqrlDefault(), SqrlDelegate(),
  #   SqrlDSNs(), SqrlFile(), SqrlIndicator(), SqrlInterface(), SqrlIsOpen(),
  #   SqrlOpen(), SqrlParam() (self), SqrlPing(), SqrlProc(), SqrlShell(),
  #   SqrlStatement(), SqrlSource(), SqrlSubmit(), SqrlSubScript(),
  #   SqrlStatement(), SqrlValue(), sqrlInterface().
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
    # Coerce parameters to a character vector (sort() doesn't handle lists).
    set <- base::as.character(base::unlist(set))

    # Retain only those (unique) parameter names that are in the official list.
    params <- base::sort(base::unique(set[set %in% SqrlParams("all")]))

    # If we are left with no parameters to reset, return invisible NULL.
    if (base::length(params) < 1L)
    {
      base::return(base::invisible(NULL))
    }

    # Construct a named list of the default values for those parameters.
    news <- base::vector(base::mode(base::list()), base::length(params))
    base::names(news) <- params
    for (param in params)
    {
      news[param] <- base::list(SqrlDefault(datasource, param))
    }

    # Retain only those parameters for which a value has been set. Any others
    # must necessarily already be at their defaults (resetting does nothing).
    params <- params[params %in% SqrlParam(datasource, "*")]

    # When all parameters are at their defaults, invisibly return them.
    if (base::length(params) < 1L)
    {
      base::return(base::invisible(news))
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
    # This is a bit of a kludge; here we set any visible indicators to values
    # that are identical to their defaults, then (later, below) we remove those
    # set values, leaving the actual defaults in place.
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
      news["interface"] <- base::list(SqrlParam(datasource, "interface"))
      params <- params[params != "interface"]
      if (base::length(params) < 1L)
      {
        base::return(base::invisible(news))
      }
    }

    # Remove the parameter-value definitions (restores default values).
    base::remove(list = params, pos = cacheenvir)

    # Invisibly return the new parameter-values (i.e., their defaults). Default
    # values are never secret or semi-secret, so these can go back to the user.
    base::return(base::invisible(news))
  }

  # When no value is supplied for the set argument, act as a getter.
  if (base::missing(set))
  {
    # Obtain the temporary parameter-values (environments) stack (a list).
    pstack <- if (base::exists("pstack", cacheenvir, inherits = FALSE))
              {
                base::get("pstack", cacheenvir, inherits = FALSE)
              } else
              {
                SqrlDefault(datasource, "pstack")
              }

    # If the stack itself was sought, return it.
    if (parameter == "pstack")
    {
      base::return(pstack)
    }

    # Otherwise, extract the last environment from the temporary-values stack.
    # This inherits from the previous environment (and so on, to the first).
    pstack <- pstack[[base::length(pstack)]]

    # When there is no set value (temporary or cached) for the parameter,
    # return its default. Default values are never secret or semi-secret.
    if (!base::exists(parameter, pstack, inherits = TRUE))
    {
      base::return(SqrlDefault(datasource, parameter))
    }

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
        value <- base::get(parameter, pstack, inherits = TRUE)
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
    # Return the current (temporary or cached) value.
    base::return(base::get(parameter, pstack, inherits = TRUE))
  }

  # The set argument has been supplied; act as a setter (cache and return).
  # First, we coerce the raw value to the expected type for the parameter.

  # Normal action is to set the permanent value of the named parameter.
  istemp <- FALSE
  targetenvir <- cacheenvir

  # However, if the parameter is 'pstack' and the set value is named, then this
  # is reinterpreted as a request to assign a temporary value (value-of-set) for
  # the named parameter (name-of-set) into the most recent environment of the
  # temporary values stack (pstack).
  if ((parameter == "pstack")
      && !base::is.null(base::names(set)))
  {
    parameter <- base::names(set)
    set <- set[[1L]]
    istemp <- TRUE
    pstack <- SqrlParam(datasource, "pstack")
    targetenvir <- pstack[[base::length(pstack)]]
    if ((base::length(parameter) != 1L)
        || !(parameter %in% SqrlParams("all")))
    {
      base::stop("Unrecognised parameter for temporary assignment.")
    }
    if (parameter %in% SqrlParams("no-temp-allowed"))
    {
      base::stop("Parameter does not support temporary values.")
    }
  }

  # In the special case where the parameter is (still) 'pstack', we have a
  # request to expand or contract the temporary-values environments stack.
  if (parameter == "pstack")
  {
    pstack <- SqrlParam(datasource, parameter)
    ptop <- base::length(pstack)
    if (set == "expand")
    {
      pstack <- base::append(pstack, base::new.env(parent = pstack[[ptop]]))
    } else if (base::length(pstack) >= 2L)
    {
      tpars <- base::objects(pstack[[ptop]])
      verbose <- base::interactive() && SqrlParam(datasource, "verbose")
      base::remove(list = tpars, pos = pstack[[ptop]])
      pstack <- pstack[-ptop]
      if ((base::length(tpars) > 0L)
          && verbose)
      {
        base::cat("\n")
        for (tpar in tpars)
        {
          rval <- base::deparse(SqrlParam(datasource, tpar))
          base::cat("Reverting:", tpar, "=", rval, "\n")
        }
        base::cat("\n")
      }
    }
    base::assign(parameter, pstack, cacheenvir)
    base::return(base::invisible(set))
  }

  # In the special case where the connection parameter has been specified as a
  # character vector of named and/or unnamed elements, we collapse that vector
  # to a single (connection) string. Where present, the vector element names
  # become the connection-parameter names within the string.
  if ((parameter == "connection")
      && base::identical(base::class(set), base::class(base::character()))
      && (base::length(set) > 0L)
      && !base::any(base::is.na(set)))
  {
    if (base::is.null(base::names(set))
        || !base::any(base::nzchar(base::names(set))))
    {
      set <- base::paste0(set, collapse = ";")
    } else
    {
      set <- base::paste0(base::names(set),
                          base::c("", "=")[base::nzchar(base::names(set)) + 1L],
                          set, collapse = ";")
    }
  }

  # Nullable-string parameters are string-types which accept a set value of NULL
  # as an alias for the empty string.
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
        && !base::identical(base::class(set), "RODBC"))
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
    # The integer and numeric classes are both of numeric mode.
    if (!(base::is.logical(set)
          || base::is.numeric(set)
          || base::is.character(set))
        || base::any(base::is.na(set)))
    {
      base::stop("Parameter must be of logical, numeric, or character type.")
    }

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
    if (base::length(set) != 1L)
    {
      base::stop("New parameter value is not a character string.")
    }

  # Values of the result and library parameters cannot be directly set by the
  # user, but NULL is taken to mean remove the current value, which is allowed.
  } else if (parameter %in% SqrlParams("nullable-internal"))
  {
    if (!(override
          || base::is.null(set)))
    {
      base::stop("New parameter value is not NULL.")
    }

  # Prevent the user from assigning to any name that is not on SqrlParams()'s
  # 'all' list. Internal functions may do so, provided the override flag is set.
  } else if (!override)
  {
    base::stop("Unrecognised parameter.")
  }

  # We have an acceptable value of set; so now act as a setter (below).
  # No further modification of the value occurs, other than whitespace trimming
  # for the prompt and wintitle parameters.

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
    # This shouldn't ever happen, but just in case.
    if (istemp)
    {
      base::stop(base::paste0("Cannot set a temporary value for the '",
                              parameter, "' parameter."))
    }
    # Throw an error on an attempt to change the parameter value. Must also
    # throw an error when attempting to set a secret or semi-secret parameter to
    # the value it already has, or else the value could be discovered by trial
    # and error.
    if ((parameter %in% base::c(SqrlParams("secret"),
                                  SqrlParams("semi-secret")))
        || !base::identical(set, SqrlParam(datasource, parameter)))
    {
      base::stop("Parameter is locked while a connection is open.")
    }
    # Otherwise, if the current value is a default (when no static value is
    # defined), set the (identical) new value as an equivalent static
    # replacement (for the default).
    if (!base::exists(parameter, cacheenvir, inherits = FALSE))
    {
      base::assign(parameter, set, targetenvir)
    }
    # Return the (unchanged) value.
    base::return(base::invisible(set))
  }

  # The channel parameter is a special case, because we want to toggle the
  # indicator state along with a change of channel existence (null/not).
  if (parameter == "channel")
  {
    # This shouldn't ever happen, but just in case.
    if (istemp)
    {
      base::stop("Cannot set a temporary value for the 'channel' parameter.")
    }
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
    base::assign(parameter, set, targetenvir)
    base::return(base::invisible(set))
  }

  # The connection parameter is a special case, since we want to extract further
  # parameter values from it, if we can. This may fail if any of the parameter
  # values contain = or ;, but none of the test systems allow these characters
  # in DSNs, passwords, etc. Does any system? See related 'scrape' comments
  # within SqrlOpen().
  if (parameter == "connection")
  {
    # This shouldn't ever happen, but just in case.
    if (istemp)
    {
      base::stop("Cannot set a temporary value for the 'connection' parameter.")
    }
    # Unless the connection string contains a DSN placeholder ('<dsn>'),
    # delete any dsn definition.
    if (!base::grepl("<dsn>", set))
    {
      SqrlParam(datasource, "reset", "dsn", override)
    }
    # RODBC::odbcConnect() likes to know the driver (from which it determines
    # whether or not it's dealing with MySQL). While we're doing that, we may as
    # well attempt to extract some other parameter values, first. We make sure
    # the driver parameter is done last, because setting a value for dsn sets
    # driver as a side effect, and we may want to override that.
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
        # SQRL accepts <uid> (etc.) as connection string template place holders
        # (to be replaced with current values at connection time). We don't want
        # to override default or previous values with these.
        if (value != base::paste0("<", param, ">"))
        {
          SqrlParam(datasource, param, value, override)
        }
      }
    }
    # Set the (unaltered) connection string, return invisibly.
    base::assign(parameter, set, targetenvir)
    base::return(base::invisible(set))
  }

  # Setting the dsn parameter is a special case, because we simultaneously
  # reset the connection parameter unless the connection string contains a
  # '<dsn>' placeholder. If the DSN is defined on the local system, then we
  # also set the driver parameter to the DSN's value, as obtained from
  # RODBC::odbcDataSources().
  if (parameter == "dsn")
  {
    if (istemp)
    {
      base::stop("Cannot set a temporary value for the 'dsn' parameter.")
    }
    if (!base::grepl("<dsn>", SqrlParam(datasource, "connection")))
    {
      SqrlParam(datasource, "reset", "connection", override)
    }
    base::assign(parameter, set, targetenvir)
    sources <- RODBC::odbcDataSources("all")
    if ((base::nzchar(set))
          && (set %in% base::names(sources)))
    {
      SqrlParam(datasource, "driver", sources[set], override)
    }
    base::return(base::invisible(set))
  }

  # The prompt and wintitle parameters are special cases, because, if the old
  # prompt or wintitle is currently visible, it must be removed before changing
  # the parameter value, and then the new value must be applied.
  if (parameter %in% base::c("prompt", "wintitle"))
  {
    if (istemp)
    {
      base::stop(base::paste0("Cannot set a temporary value for the '",
                              parameter, "' parameter."))
    }
    set <- base::trimws(set)
    if (set != SqrlParam(datasource, parameter))
    {
      isopen <- SqrlIsOpen(datasource)
      if (isopen)
      {
        SqrlIndicator(datasource, "hide", parameter)
      }
      base::assign(parameter, set, targetenvir)
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
    if (istemp)
    {
      base::stop("Cannot set a temporary value for the 'visible' parameter.")
    }
    if (set != SqrlParam(datasource, "visible"))
    {
      isopen <- SqrlIsOpen(datasource)
      if (isopen
          && !set)
      {
        SqrlIndicator(datasource, "hide")
      }
      base::assign(parameter, set, targetenvir)
      if (isopen
          && set)
      {
        SqrlIndicator(datasource, "show")
      }
    }
    base::return(base::invisible(set))
  }

  # The libstack parameter is a special case, because the set value is appended
  # to the top layer of the existing stack (rather than replacing the stack).
  if (parameter == "libstack")
  {
    # This shouldn't ever happen (unless the libstack storage mechanism is
    # changed to having a library within each layer of the pstack stack), but
    # we check here anyway, just in case.
    if (istemp)
    {
      base::stop("Cannot alter procedures via <with>.")
    }
    # A NULL value is interpreted as a request to remove the stack.
    if (base::is.null(set))
    {
      if (base::exists(parameter, cacheenvir, inherits = FALSE))
      {
        base::remove(list = parameter, pos = cacheenvir)
      }
      base::return(base::invisible())
    }
    # It is not possible to directly assign the lib[n][name] element within the
    # cache environment, so we have to pull the stack pointer back here, modify
    # the local copy, and then point the cache environment at this new copy.
    lib <- SqrlParam(datasource, parameter)
    # Unnamed strings are used as special stack-control values.
    if (base::is.null(base::names(set)))
    {
      # When set is 'expand', add a new layer to the top of the stack (list).
      if (set == "expand")
      {
        lib[[base::length(lib) + 1L]] <- base::character()
      # Otherwise, set will be 'contract'; remove the top layer of the stack.
      } else
      {
        lib[[base::length(lib)]] <- NULL
      }
    # Named strings are procedure definitions, to be added to the topmost layer
    # of the stack.
    } else
    {
      lib <- SqrlParam(datasource, parameter)
      lib[[base::length(lib)]][base::names(set)] <- base::as.character(set)
    }
    base::assign(parameter, lib, cacheenvir)
    base::return(base::invisible())
  }

  # The library parameter is a special case, because the set value is appended
  # to the existing library (rather than replacing it).
  if (parameter == "library")
  {
    # This shouldn't ever happen, but just in case.
    if (istemp)
    {
      base::stop("Cannot alter library via <with>.")
    }
    # A NULL value is interpreted as a request to reset (empty) the library.
    if (base::is.null(set))
    {
      base::return(SqrlParam(datasource, "reset", parameter))
    }
    # Otherwise, the value can only have come from SqrlFile(), and will be a
    # named string (procedure definition). Add that definition to the library.
    # It is not possible to directly assign the lib[name] element within the
    # cache environment.
    lib <- SqrlParam(datasource, parameter)
    lib[base::names(set)] <- base::as.character(set)
    lib <- lib[base::order(base::names(lib))]
    base::assign(parameter, lib, cacheenvir)
    base::return(base::invisible(set))
  }

  # For all other cases, set and (invisibly) return the new parameter value.
  base::assign(parameter, set, targetenvir)
  base::return(base::invisible(set))
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
  #   SqrlFile(), SqrlOpen(), SqrlParam(), SqrlSource(), SqrlSources(),
  #   SqrlValue(), sqrlAll().
  # User:
  #   Has no direct access, and is unable to supply the argument. Validity
  #   checks are not required.

  # Parameter-group definitions (find and return).
  base::return(base::switch(group,

    # All public (user-visible) parameter names, whether RODBC or SQRL.
    "all"                   = base::c("aCollapse",
                                      "as.is",
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
                                      "lCollapse",
                                      "library",
                                      "max",
                                      "na.strings",
                                      "name",
                                      "nullstring",
                                      "ping",
                                      "prompt",
                                      "pwd",
                                      "readOnlyOptimize",
                                      "result",
                                      "retry",
                                      "rows_at_time",
                                      "scdo",
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
                                      "retry",
                                      "scdo",
                                      "stringsAsFactors",
                                      "verbose",
                                      "visible"),

    # Parameters of character-vector type (any length, including zero).
    "character-type"        = base::c("na.strings"),

    # Parameters not to copy when duplicating an existing SQRL data source.
    "don't-copy"            = base::c("channel",
                                      "interface",
                                      "libstack",
                                      "name",
                                      "prompt",
                                      "result",
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
                                      "readOnlyOptimize",
                                      "rows_at_time",
                                      "tabQuote",
                                      "uid"),

    # Parameters whose values are lists of named values.
    "named-values"          = base::c("library"),

    # Parameters for which temporary working values cannot be assigned.
    "no-temp-allowed"       = base::c("autoclose",
                                      "channel",
                                      "interface",
                                      "library",
                                      "name",
                                      "prompt",
                                      "result",
                                      "visible",
                                      "wintitle"),

    # Parameters the user can make NULL, but whose values are otherwise only
    # settable by private SQRL functions.
    "nullable-internal"     = base::c("library",
                                      "result"),

    # String-type parameters that accept NULL as an aliaas for the empty string.
    "nullable-string"       = base::c("connection",
                                      "DBMSencoding",
                                      "driver",
                                      "dsn",
                                      "prompt",
                                      "pwd",
                                      "uid",
                                      "wintitle"),

    # Parameters that are omitted from the SqrlConfig() configuration list.
    "omit-from-config"      = base::c("result"),

    # Parameters to omit from the 'settings' subset of the configuration list.
    "omit-from-settings"    = base::c("channel",
                                      "connection",
                                      "driver",
                                      "dsn",
                                      "library",
                                      "name",
                                      "ping",
                                      "pwd",
                                      "result",
                                      "uid"),

    # Parameters that can be file-path valued (excluded from SqrlDefile()).
    "path-valued"           = base::c("driver",
                                      "dsn",
                                      "library"),

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
    "string-type"           = base::c("aCollapse",
                                      "case",
                                      "connection",
                                      "DBMSencoding",
                                      "dec",
                                      "driver",
                                      "dsn",
                                      "lCollapse",
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

SqrlPath <- function(path)
{
  # Determines whether or not the argument is a path to an existing file.
  # Args:
  #   path : A possible file path, perhaps given as a list of components.
  # Returns:
  #   The normalised file path, when ... appears to specify an existing file,
  #   or NULL, when ... does not appear to specify an existing file.
  # SQRL Calls:
  #   SqrlTry().
  # SQRL Callers:
  #   SqrlConfig(), SqrlDefile(), SqrlDelegate(), SqrlHelp(), SqrlSource().
  # User:
  #   Has no direct access, but is able to supply arguments(s) indirectly, via
  #   SqrlDelegate(). Unexpected input is silently caught.

  # Paste all arguments together.
  path <- SqrlTry(base::paste0(base::unlist(path), collapse = ""), warn = FALSE)

  # If pasting failed, the arguments must not specify a file (return NULL).
  if (path$error)
  {
    base::return(NULL)
  }

  # If the path isn't a single string, it cannot specify a file (return NULL).
  path <- path$value
  if ((base::length(path) != 1L)
      || (base::nchar(path) < 1L))
  {
    base::return(NULL)
  }

  # If path actually does point to a readable file, return the (normalised)
  # path. Note that files '.' and '..' exist as directories, and that file '"'
  # exists but is not read accessible (the '4' tests for read access).
  if (base::file.exists(path)
      && (base::file.access(path, 4L) == 0L)
      && !(base::file.info(path)$isdir))
  {
    base::return(base::normalizePath(path))
  }

  # The arguments do not appear to specify a file path. Return NULL.
  base::return(NULL)
}

SqrlPing <- function(datasource,
                      set = FALSE)
{
  # Sets and submits 'ping' queries for testing source connection channels.
  # Args:
  #   datasource : The name of a known data source.
  #   set        : Whether to set a ping query, or to ping the source with one.
  # Returns:
  #   In set mode, the resulting ping query, as a character string. In ping
  #   mode, TRUE if the source responded (is connected to), FALSE otherwise.
  # SQRL Calls:
  #   SqrlIndicator(), SqrlParam(), SqrlPing() (self), SqrlTry().
  # RODBC Calls:
  #   odbcQuery(), sqlQuery().
  # SQRL Callers:
  #   SqrlIsOpen(), SqrlOpen(), SqrlPing() (self).
  # User:
  #   Has no direct access, and is unable to supply the arguments. No argument
  #   validity checking is required. The user can define the ping query itself.
  # Restriction:
  #   This function assumes the existence of an RODBC channel handle for the
  #   data source. That is, the value of the 'channel' parameter must be an
  #   RODBC channel handle, rather than NULL. In set mode, the channel should be
  #   open, in ping mode it need not be (the connection could have been dropped
  #   from the other end). This function should only be called immediately after
  #   the existence of such a handle has been established (as SqrlisOpen() and
  #   SqrlOpen() both do).

  # In set mode, attempt to find a 'ping' query that works with the data source.
  if (set)
  {
    # Here, we define some 'pings', being very simple SQL statements. These are
    # used to ping the data source; confirming we're still connected when we get
    # the expected result back (or telling us we've lost the connection when we
    # don't). These, alas, are vendor dependent, so we have to guess, trial, and
    # see what works. Some vendor-independent method would be vastly preferable.

    # Ping for MySQL, PostgreSQL, SQL Server, SQLite, Teradata.
    p1 <- "select 1"

    # Ping for Oracle, MySQL, DB2.
    p2 <- "select 1 from dual"

    # Ping for Oracle, DB2.
    p3 <- "begin null; end;"

    # Arrange the pings into best-guess-first order, according to the driver.
    pings <- base::c(p1, p2, p3)
    driver <- base::tolower(SqrlParam(datasource, "driver"))
    if (base::grepl("oracle", driver, fixed = TRUE)
        || base::grepl("db2", driver, fixed = TRUE))
    {
      pings <- pings[base::c(3L, 2L, 1L)]
    }

    # Try each ping, in (driver-dependent) order of decreasing preference, until
    # we find a ping that works (is valid SQL for the data source). These could
    # also fail if the connection has been unexpectedly closed.
    for (ping in pings)
    {
      SqrlParam(datasource, "ping", ping)
      if (SqrlPing(datasource))
      {
        base::return(SqrlParam(datasource, "ping"))
      }
    }

    # Did not find a ping that works. Set and return the empty string. This
    # causes the pinging system to submit a junk query and scan the response for
    # error terms that suggest a lost connection (not completely reliable).
    base::return(SqrlParam(datasource, "ping", ""))
  }

  # In normal operation (not set mode), submit this query and see what happens.
  ping <- SqrlParam(datasource, "ping")

  # When we have a ping query, submit it to the driver and look for an error.
  if (!base::is.null(ping)
      && base::nzchar(ping))
  {
    # If RODBC::odbcQuery() is available, that's our preferred method. According
    # to the RODBC manual, it returns 1L on success, and -1L on failure, but is
    # 'likely to be confined to the 'RODBC' namespace in the near future'.
    if ("odbcQuery" %in% base::getNamespaceExports("RODBC"))
    {
      # Append ping-in-progress marker to the window-title connection indicator.
      SqrlIndicator(datasource, "ping")

      # Submit the ping query, and retrieve the status code (which takes either
      # of two values: 1L for success, or -1L for failure).
      s <- SqrlTry(RODBC::odbcQuery(
                          channel = SqrlParam(datasource, "channel"),
                          query = ping,
                          rows_at_time = SqrlParam(datasource, "rows_at_time")),
                    warn = FALSE)

      # Remove ping-in-progress marker from the window-title indicator.
      SqrlIndicator(datasource, "done")

      # An error suggests the connection is closed; return FALSE.
      # No error implies the connection is open; return TRUE.
      base::return(!(s$error || (s$value == -1L)))
    }

    # Otherwise, use RODBC::sqlQuery(), with errors = FALSE, and as.is = TRUE.
    # The as.is setting makes the result indifferent to most of the other
    # parameter values. With errors = FALSE, the function returns integer -1 on
    # failure, and something else otherwise (a character vector or data frame).
    SqrlIndicator(datasource, "ping")
    s <- SqrlTry(RODBC::sqlQuery(
                          channel = SqrlParam(datasource, "channel"),
                          query = ping,
                          errors = FALSE,
                          as.is = TRUE,
                          max = SqrlParam(datasource, "max"),
                          buffsize = SqrlParam(datasource, "buffsize"),
                          nullstring = SqrlParam(datasource, "nullstring"),
                          na.strings = SqrlParam(datasource, "na.strings"),
                          believeNRows = SqrlParam(datasource, "believeNRows"),
                          dec = SqrlParam(datasource, "dec"),
                          stringsAsFactors = FALSE,
                          rows_at_time = SqrlParam(datasource, "rows_at_time")),
                  warn = FALSE)
    SqrlIndicator(datasource, "done")
    base::return(!(s$error || base::identical(s$value, -1L)))
  }

  # In the absence of a ping query, submit a junk statement in an attempt to
  # cause the driver to generate an error message that indicates whether or not
  # that query was received by the source. This will not be completely reliable.
  SqrlIndicator(datasource, "ping")
  s <- SqrlTry(RODBC::sqlQuery(
                          channel = SqrlParam(datasource, "channel"),
                          query = "junk",
                          errors = TRUE,
                          as.is = TRUE,
                          max = SqrlParam(datasource, "max"),
                          buffsize = SqrlParam(datasource, "buffsize"),
                          nullstring = SqrlParam(datasource, "nullstring"),
                          na.strings = SqrlParam(datasource, "na.strings"),
                          believeNRows = SqrlParam(datasource, "believeNRows"),
                          dec = SqrlParam(datasource, "dec"),
                          stringsAsFactors = FALSE,
                          rows_at_time = SqrlParam(datasource, "rows_at_time")),
                warn = FALSE)
  SqrlIndicator(datasource, "done")

  # The error message should be a character vector. If we got something else,
  # take the connection to be closed (it probably is, but might not be).
  if (s$error
      || !base::identical(base::class(s$value), base::class(base::character()))
      || (base::length(s$value) == 0L))
  {
    base::return(FALSE)
  }

  # If the error message appears to indicate a socket error or closed
  # connection, assume that's the case (although we might be mistaken).
  for (word in base::c("sock", "libc", "connection", "reset", "open", "closed"))
  {
    if (base::any(base::grepl(word, s$value, fixed = TRUE)))
    {
      base::return(FALSE)
    }
  }

  # Otherwise, the error message appears to arise from the junk query arriving
  # at the source, in which case we take the connection to be open.
  base::return(TRUE)
}

SqrlPL <- function(state = NULL,
                    phrase = "")
{
  # Detects procedural (PL) script and tracks parser progress through the same.
  # Args:
  #   state  : A list of named procedural-language (PL) marker counts.
  #   phrase : A SQL fragment to scan for PL markers.
  # Returns:
  #   An updated state list.
  # SQRL Calls:
  #   None.
  # SQRL Callers:
  #   SqrlFile().
  # User:
  #   Has no direct access. The user is able to supply phrase via their SQL
  #   script, but only by way of SqrlFile(), which will ensure that phrase is a
  #   single string that does not contain any SQL comment or quoted literal. The
  #   user is unable to supply the state argument, although its value will
  #   reflect the content of their supplied SQL script. No argument validity
  #   checking is required.

  # Thus function is fallible. Procedural language extensions appear in Oracle,
  # DB2, Transact, Teradata, MySQL, Postgres, and many others. The nestable
  # 'begin ... end;' syntax is common, but the different DMBSes have their own
  # optional phrases beforehand. What's a keyword in one, may be a valid column
  # or variable name in another. In the event that PL parsing fails, the scdo
  # parameter can be set to FALSE (to submit only upon a <do> or <result> tag).

  # On NULL input, initialise and return a new state list.
  if (base::is.null(state))
  {
    base::return(base::list(block = FALSE, begins = 0L, ends = 0L))
  }

  # Because gregexpr() only finds disjoint matches, 'end;end', for instance,
  # would count as only one end below (they share the semicolon). By doubling
  # all word-break characters, 'end;end' becomes 'end;;end' and gregexpr()
  # finds both ends. Fortunately, gsub() does not re-double characters.
  phrase <- base::gsub("([^[:alnum:]@#$_])", "\\1\\1", phrase)

  # Count instances of 'begin'. This keyword is mandatory within most PL blocks.
  state$begins <- state$begins + base::sum(base::gregexpr(
                      "(^|[^[:alnum:]@#$_])begin([^[:alnum:]@#$_]|$)",
                      phrase, ignore.case = TRUE)[[1L]] > 0L)

  # Count instances of 'end'. This keyword is mandatory within most PL blocks,
  # and must be followed by a semicolon (to distinguish it from 'end loop' and
  # 'end if'. This will fail if there's a comment in between the end and the ;.
  state$ends <- state$ends + base::sum(base::gregexpr(
                      "(^|[^[:alnum:]@#$_])end\\s*;",
                      phrase, ignore.case = TRUE)[[1L]] > 0L)

  # If we already believe we're inside a PL block, return the updated state.
  if (state$block)
  {
    base::return(state)
  }

  # If we've got any begins, then we now think we're in a PL block.
  # Return the updated state.
  if (state$begins > 0L)
  {
    state$block <- TRUE
    base::return(state)
  }

  # Search for optional PL key phrases that appear before a (mandatory) begin.
  # Detection of any of these causes us to believe we're inside a PL block.
  state$block <- base::grepl(base::paste0(
          "(^|[^[:alnum:]@#$_])(declare|((create|replace)\\s+",
          "(function|package|procedure|trigger|type)))([^[:alnum:]@#$_]|$)"),
          phrase, ignore.case = TRUE)

  # Return the updated state list.
  base::return(state)
}

SqrlProc <- function(datasource,
                      proc)
{
  # Retrieves a stored procedures by its name.
  # Args:
  #   datasource : The name of data source, as known to SQRL.
  #   proc       : A possible stored-procedure name, perhaps as components.
  # Returns:
  #    The definition of the named procedure, as a character string. When no
  #    procedure matches the supplied name, NULL is returned instead.
  # SQRL Calls:
  #   SqrlParam(), SqrlTry().
  # SQRL Callers:
  #   SqrlDelegate().
  # User:
  #   Has no direct access. Is able to supply (only) the proc argument, via
  #   SqrlDelegate(). Exceptions from unexpected input are silently caught.

  # Collapse proc, which could be supplied as components, to a single string.
  proc <- SqrlTry(base::paste0(base::unlist(proc), collapse = ""), warn = FALSE)

  # If pasting failed, proc can't name a procedure (retun NULL).
  if (proc$error)
  {
    base::return(NULL)
  }
  proc <- proc$value

  # If proc isn't a single string, it cannot name a procedure (return NULL).
  if ((base::length(proc) != 1L)
      || (base::nchar(proc) < 1L))
  {
    base::return(NULL)
  }

  # Work backward through the temporary stack (provided it exists), and return
  # the first procedure definition with a matching name.
  lib <- SqrlParam(datasource, "libstack")
  for (i in base::rev(base::seq_along(lib)))
  {
    if (proc %in% base::names(lib[[i]]))
    {
      base::return(lib[[i]][proc])
    }
  }

  # The name wasn't found on the stack; so now search the main library. Once
  # again, if a matching name appears, then return the corresponding definition.
  lib <- SqrlParam(datasource, "library")
  if (proc %in% base::names(lib))
  {
    base::return(lib[proc])
  }

  # The name does not refer to any stored procedure. Return NULL.
  base::return(NULL)
}

SqrlShell <- function(datasource = "",
                      envir = base::parent.frame(),
                      args.list)
{
  # Relays commands from public interface functions to the private interpreter.
  # Args:
  #   datasource : The name of a known data source.
  #   envir      : An R environment, from which variables are inherited.
  #   args.list  : A list of arguments, to be interpreted and actioned.
  # Returns:
  #   The result of the command (frequently a data frame, string or list).
  # SQRL Calls:
  #   SqrlCache(), SqrlClose(), SqrlDelegate(), SqrlParam(), SqrlTry().
  # SQRL Callers:
  #   SqrlAll() (and data source interfaces).
  # User:
  #   User has no direct access, but is able to supply (only) the args.list
  #   argument from sqrlAll() and/or any data source interface functions). Since
  #   args.list is unrestricted (it could be SQL), no argument validity checking
  #   is performed here.

  # When autoclose is TRUE, always close any open connection upon exiting this
  # function in any manner (including when an error has been thrown somewhere).
  base::on.exit(
    if (SqrlCache(datasource, exists = TRUE)
        && SqrlParam(datasource, "autoclose"))
    {
      SqrlClose(datasource)
    })

  # Relay the arguments to SqrlDelegate, for interpretation and evaluation,
  # while trapping any error that might occur.
  x <- SqrlTry(base::withVisible(SqrlDelegate(datasource, envir, args.list)))

  # If an error occurred, throw a concise error message, showing only the top-
  # level interface function (or data source name), rather than the originating
  # internal function with all its messy arguments.
  if (x$error)
  {
    f <- SqrlParam(datasource, "interface")
    if (!base::is.null(f))
    {
      k <- base::parse(text = base::paste0(f, "(...)"), keep.source = FALSE)
      base::stop(base::simpleError(x$value, k[[1L]]))
    }
    s <- base::parse(text = datasource, keep.source = FALSE)
    base::stop(base::simpleError(x$value, s[[1L]]))
  }

  # No error occurred. Return the result visibly or invisibly, as intended.
  x <- x$value
  if (!x$visible)
  {
    base::return(base::invisible(x$value))
  }
  base::return(x$value)
}

SqrlStatement <- function(datasource,
                          parts)
{
  # Constructs a SQL statement from the components supplied.
  # Args:
  #   datasource : The name of a SQRL data source.
  #   parts      : A list of components, constituting a SQL statement.
  # Returns:
  #   The corresponding SQL statement. Differs from paste() in that lists are
  #   rewritten in comma-separated form and vectors in newline-separated form.
  # SQRL Calls:
  #   SqrlParam().
  # SQRL Callers:
  #   SqrlDelegate(), SqrlFile().
  # User:
  #   Has no direct access. Can supply the only argument, via SqrlDelegate().
  #   In the event that the argument contains an object that cannot be pasted,
  #   all calls of this function are wrapped in try().

  # As above, this function is only (directly) called from SqrlDelegate() and
  # SqrlFile(). Both (only) supply objects wrapped inside of a list.

  # Recurse over the list, converting the ultimate (atomic) objects to single
  # strings (collapsing vectors with the aCollapse character).
  ac <- SqrlParam(datasource, "aCollapse")
  elements <- base::rapply(parts, base::paste0, how = "unlist", collapse = ac)

  # Collapse the resulting character vector to a single string (with the
  # lCollapse character), and return that string.
  rc <- SqrlParam(datasource, "lCollapse")
  base::return(base::paste0(elements, collapse = rc))
}

SqrlSource <- function(def)
{
  # Defines (or re-defines) a data source and its interface.
  # Args:
  #   def : A source name and definition (string or file), in that order.
  # Returns:
  #   The interface name, invisibly, after creating, or re-defining, the source
  #   and its interface.
  # SQRL Calls:
  #   SqrlCache(), SqrlConfig(), SqrlDefile(), SqrlInterface(), SqrlIsOpen(),
  #   SqrlParam(), SqrlParams(), SqrlPath(), SqrlTry().
  # SQRL Callers:
  #   sqrlSource().
  # User:
  #   Has no direct access. Can supply the argument via sqrlSource() (only).
  #   That function guarantees the existence of at least either two terms or
  #   one named term. Additional checks (assignability, conflict, etc.) are
  #   performed here.

  # Separate the name from the definition component(s). When there is only one
  # term, we use it's name. When there is more than one term, we use the first
  # term as the name if that term is not itself named. If it is named, we look
  # instead for a unique term named 'name', and use that if it exists.
  if (base::length(def) == 1L)
  {
    name <- base::trimws(base::names(def))
    base::names(def) <- NULL
  } else if (base::is.null(base::names(def))
              || !base::nzchar(base::names(def)[1L]))
  {
    name <- base::trimws(def[[1L]])
    def[[1L]] <- NULL
  } else if ("name" %in% base::names(def))
  {
    i <- base::which(base::names(def) == "name")
    if (base::length(i) > 1L)
    {
      base::stop("Multiple 'name' terms.")
    }
    name <- base::trimws(def[[i]])
    def[[i]] <- NULL
  } else
  {
    base::stop("Could not identify the intended source name.")
  }

  # Ensure either all terms are named, or that no term is named. When the terms
  # are named, ensure all names are different (unique).
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
    datasources <- base::unique(base::as.character(base::unlist(def)))
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
    path <- SqrlPath(def)
    if (!base::is.null(path))
    {
      def <- base::list(config = path)

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
      && !(def$copy %in% SqrlCache("*")))
  {
    base::stop("Copy source original not found.")
  }

  # Abort if a configuration file has been specified, but that file cannot be
  # read (including file does not exist). We will miss this here when the file
  # path has been specified in a list, but SqrlConfig() will pick that up later.
  if (("config" %in% base::names(def))
      && !base::identical(base::class(def["config"]), base::class(base::list()))
      && base::is.null(SqrlPath(def["config"])))
  {
    base::stop("Cannot read the config file.")
  }

  # When the defining terms do not include a 'copy', 'config', or 'connection',
  # there is no possibility of a single term specifying a connection string. If,
  # additionally, we do not have a 'dsn' term, or if one of the terms does not
  # correspond to a SQRL/RODBC parameter, then we interpret all of the terms as
  # connection-string components, and construct the string from them.
  if (!base::any(base::c("copy", "config", "connection")
                  %in% base::names(def))
      && (!("dsn" %in% base::names(def))
          || !base::all(base::names(def) %in% SqrlParams("all"))))
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
    # This only returns the set (non-default) parameters. Names are unique.
    params <- SqrlParam(def$copy, "*")

    # Don't copy parameters we shouldn't (name, interface, etc.).
    params <- params[!(params %in% SqrlParams("don't-copy"))]

    # If the original source has a library, copy it to the new source.
    if ("library" %in% params)
    {
      SqrlParam(name, "reset", "library")
      lib <- SqrlParam(def$copy, "library")
      for (proc in base::names(lib))
      {
        script <- lib[[proc]]
        base::names(script) <- proc
        SqrlParam(name, "library", script, override = TRUE)
      }
      params <- params[params != "library"]
    }

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
  # copied from another source. The incomplete source is deleted on error. Note
  # that def$config might be a file path (potentially in component form), or a
  # list of (named) parameter = value pairs. SqrlConfig() will identify which
  # (or neither) is the case, and handle appropriately.
  if ("config" %in% base::names(def))
  {
    result <- SqrlTry(SqrlConfig(name, def$config))
    if (result$error)
    {
      SqrlCache(name, delete = TRUE)
      base::stop(result$value)
    }
  }

  # If we have an 'interface' term, attempt to apply the specified name. This
  # overrides any value that may have been set via config file. The incomplete
  # source is deleted upon error.
  if ("interface" %in% base::names(def))
  {
    result <- SqrlTry(
                  SqrlInterface(name, SqrlDefile("interface", def$interface)))
    if (result$error)
    {
      SqrlCache(name, delete = TRUE)
      base::stop(result$value)
    }
  }

  # Iterate over all other terms (besides 'copy', 'config', and 'interface'),
  # treating each as a SQRL/RODBC parameter. The incomplete source is deleted
  # upon any error. The uniqueness of names has been asserted, above.
  params <- base::names(def)
  params <- params[!(params %in% base::c("copy", "config", "interface"))]
  params <- base::c(params[params != "driver"], params[params == "driver"])
  for (param in params)
  {
    result <- SqrlTry(SqrlParam(name, param, SqrlDefile(param, def[[param]])))
    if (result$error)
    {
      SqrlCache(name, delete = TRUE)
      base::stop(result$value)
    }
  }

  # If no interface has been defined, attempt to apply the source name. The
  # incomplete source will be deleted if this is not possible.
  if (SqrlParam(name, "interface", isdefined = FALSE))
  {
    result <- SqrlTry(SqrlInterface(name, name))
    if (result$error)
    {
      SqrlCache(name, delete = TRUE)
      base::stop(result$value)
    }
  }

  # Return the source's configuration, invisibly.
  base::return(base::invisible(SqrlConfig(name)))
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
    SqrlAll(base::list("remove"), envir = base::parent.frame())
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

SqrlSubmit <- function(datasource,
                        statement,
                        retry = TRUE)
{
  # Submit a SQL statement to a connected data source.
  # Args:
  #   datasource : The name of a known data source.
  #   statement  : A SQL statement (as a single character string).
  #   retry      : When set to FALSE, do not resubmit on failure.
  # Returns:
  #   Result of submitting the statement (typically a data frame).
  # SQRL Calls:
  #   SqrlIndicator(), SqrlIsOpen(), SqrlOpen(), SqrlParam(),
  #   SqrlSubmit() (self), SqrlTry().
  # RODBC Calls:
  #   odbcGetErrMsg, odbcQuery, sqlGetResults(), sqlQuery().
  # SQRL Callers:
  #   SqrlDelegate(), SqrlSubmit() (self), SqrlSubScript().
  # User:
  #   Has no direct access. Is able to supply (only) the statement argument (a
  #   string), via SqrlSubScript(). No further checks are required.

  # If the statement is empty, return NULL (emulates no-query in any SQL).
  # Now that all queries go via the parser, this should never happen (everything
  # comes in from SqrlSubscript(), which already performs this operation).
  if (!base::grepl("[[:graph:]]", statement))
  {
    base::return(NULL)
  }

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

  # Our preferred method is to submit the statement via RODBC::odbcQuery(), and
  # then fetch the results via RODBC::sqlGetResults(). However, the RODBC manual
  # states that odbcQuery() is 'likely to be confined to the "RODBC" namespace
  # in the near future'. The same issue applies to RODBC::odbcGetErrMsg(), so
  # first we check these functions are available.
  rodbc <- base::getNamespaceExports("RODBC")
  if (("odbcQuery" %in% rodbc)
      && (!SqrlParam(datasource, "errors")
          || ("odbcGetErrMsg" %in% rodbc)))
  {
    # Append query-in-progress marker to the window-title connection indicator.
    SqrlIndicator(datasource, "query")

    # Submit the query, and retrieve the exit code (+1 = success, -1 = failure).
    status <- SqrlTry(RODBC::odbcQuery(
                          channel = SqrlParam(datasource, "channel"),
                          query = statement,
                          rows_at_time = SqrlParam(datasource, "rows_at_time")))

    # Remove query-in-progress marker from the window-title indicator.
    SqrlIndicator(datasource, "done")

    # Two modes of failure exist; RODBC::odbcQuery() could throw an error, or
    # else it could cleanly return its failure code (-1L). Should one occur, we
    # either try again, throw the error, or return the error message.
    if (status$error
        || (status$value == -1L))
    {
      # If we might need the ODBC error message, we'd better retrieve it now,
      # because SqrlIsOpen() pings the source (below), destroying that message.
      # If RODBC::odbcGetErrMsg() should fail here, we get the error message
      # for that failure, instead of the original RODBC::odbcQuery() message.
      if (!status$error
          && SqrlParam(datasource, "errors"))
      {
        error <- SqrlTry(RODBC::odbcGetErrMsg(SqrlParam(datasource, "channel")))
        status$value <- base::paste0(error$value, collapse = "\n")
      }

      # If this was a first attempt (retry = TRUE), and second attempts are
      # enabled (the retry parameter is also TRUE), and a ping of the source
      # reveals the connection to have been dropped, then we infer that was the
      # cause of the error, and make one more attempt (only). That will involve
      # opening a new channel, which might prompt the user for authentication.
      # This mechanism provides a (very) limited ability to recover from network
      # drop-outs, but it cannot restore temporary tables.
      if (retry
          && SqrlParam(datasource, "retry")
          && !SqrlIsOpen(datasource, besure = TRUE))
      {
        base::return(SqrlSubmit(datasource, statement, retry = FALSE))
      }

      # Otherwise, we do not make another attempt. When RODBC::odbcQuery() threw
      # an error, or when the 'errors' parameter is TRUE, we throw the error. In
      # the latter case, this has the effect of promoting ODBC failure messages
      # to local R exceptions (unlike RODBC, which simply returns the messages
      # as character strings).
      if (status$error
          || SqrlParam(datasource, "errors"))
      {
        base::stop(status$value)
      }

      # Otherwise (the 'errors' parameter is FALSE), return the error message
      # (as a character string, without raising an exception).
      base::return(status$value)
    }

    # The query has succeeded, but we have not yet retrieved the result of it.
    # Append fetch-in-progress marker to the window-title connection indicator.
    SqrlIndicator(datasource, "fetch")

    # Retrieve the data. If a connection error occurs here, we cannot easily
    # recover without re-submitting the query, since pinging the source will
    # destroy the waiting rows.
    result <- SqrlTry(
                RODBC::sqlGetResults(channel = SqrlParam(datasource, "channel"),
                  as.is = SqrlParam(datasource, "as.is"),
                  errors = SqrlParam(datasource, "errors"),
                  max = SqrlParam(datasource, "max"),
                  buffsize = SqrlParam(datasource, "buffsize"),
                  nullstring = SqrlParam(datasource, "nullstring"),
                  na.strings = SqrlParam(datasource, "na.strings"),
                  believeNRows = SqrlParam(datasource, "believeNRows"),
                  dec = SqrlParam(datasource, "dec"),
                  stringsAsFactors = SqrlParam(datasource, "stringsAsFactors")))

    # Remove fetch-in-progress marker from the window-title indicator.
    SqrlIndicator(datasource, "done")

    # If RODBC::sqlGetResults() threw an error, or if it appears to have cleanly
    # returned an error message or code, either try again, throw the exception,
    # or return the result (potentially an error message or code).
    if (result$error
        || base::identical(base::class(result$value),
                            base::class(base::integer()))
        || (base::identical(base::class(result$value),
                            base::class(base::character()))
              && (base::length(result$value) > 1L)))
    {
      # If the failure appears to have been caused by a lost connection, and
      # this is our first attempt, and the retry parameter is TRUE (enabled),
      # then make one more. Because SqrlIsOpen() may have destroyed any waiting
      # rows, the original query must be resubmitted.
      if (retry
          && SqrlParam(datasource, "retry")
          && !SqrlIsOpen(datasource, besure = TRUE))
      {
        base::return(SqrlSubmit(datasource, statement, retry = FALSE))
      }

      # If RODBC::sqlGetResults() threw an error, or if the 'errors' parameter
      # is TRUE, throw the error. In the latter case, this promotes the ODBC
      # error message to a local R exception, and throws it (RODBC doesn't).
      if (result$error
          || SqrlParam(datasource, "errors"))
      {
        base::stop(base::paste(result$value, collapse = "\n"))
      }
    }

    # Return the result. This could be a data frame, a character string, an
    # empty character vector, or an integer code (-1 = failure, -2 = no data).
    base::return(result$value)
  }

  # The block above is our preferred method, used so long as RODBC::odbcQuery()
  # remains publicly available. Should that not be the case, the script below
  # implements our fallback method, which uses RODBC::sqlQuery() instead.

  # Append query-in-progress indicator to the window-title connection indicator.
  SqrlIndicator(datasource, "query")

  # A valid connection exists. Submit the statement, and retrieve only the first
  # row (the least amount of data we can). Uses stringsAsFactors = FALSE, to
  # simplify merging with any additional rows (discussed below).
  result <- SqrlTry(RODBC::sqlQuery(channel = SqrlParam(datasource, "channel"),
                          query = statement,
                          errors = SqrlParam(datasource, "errors"),
                          as.is = SqrlParam(datasource, "as.is"),
                          max = 1L,
                          buffsize = SqrlParam(datasource, "buffsize"),
                          nullstring = SqrlParam(datasource, "nullstring"),
                          na.strings = SqrlParam(datasource, "na.strings"),
                          believeNRows = SqrlParam(datasource, "believeNRows"),
                          dec = SqrlParam(datasource, "dec"),
                          stringsAsFactors = FALSE,
                          rows_at_time = SqrlParam(datasource, "rows_at_time")))

  # Remove query-in-progress indicator from the window title.
  SqrlIndicator(datasource, "done")

  # On success, RODBC::sqlQuery() returns a data frame or character string (both
  # possibly empty). On an ODBC error, it returns either a character vector, or
  # an integer (either -1, failure, or -2, no data). Refer to the RODBC manual.
  # In the character vector error case, the length of the vector is usually at
  # least two (the ODBC driver error message, plus the RODBC error message), but
  # in some cases the driver can flag an error without generating a message to
  # go with it, in which case the result is a single character string, being the
  # RODBC message (only). All RODBC error messages begin with '[RODBC] ERROR:'.
  if (result$error
      || base::identical(base::class(result$value),
                          base::class(base::integer()))
      || (base::identical(base::class(result$value),
                          base::class(base::character()))
          && base::any(base::grepl("^\\[RODBC\\] ERROR:", result$value))))
  {
    # If the failure appears to have been caused by a lost connection, and this
    # is our first attempt, then make one more (unless the retry parameter has
    # been set to FALSE).
    if (retry
        && SqrlParam(datasource, "retry")
        && !SqrlIsOpen(datasource, besure = TRUE))
    {
      base::return(SqrlSubmit(datasource, statement, retry = FALSE))
    }

    # If RODBC::sqlQuery() threw an error, or when the 'errors' parameter is
    # TRUE, throw the error. In the latter case, this promotes the ODBC error
    # message or code to an R exception (RODBC doesn't).
    if (result$error
        || SqrlParam(datasource, "errors"))
    {
      base::stop(base::paste(result$value, collapse = "\n"))
    }
  }

  # No error occurred. Remove the error flag, retain only the (non-error) value.
  result <- result$value

  # When the result is not a data frame, there won't be any more rows to fetch.
  # This could be a character string or integer error code.
  if (!base::identical(base::class(result), base::class(base::data.frame())))
  {
    base::return(result)
  }

  # The result is a data frame of, at most, one row. If it has zero rows, then
  # there can be no more to fetch, so return it now. For consistency with RODBC,
  # strings are not converted to factors in this special case.
  if (base::nrow(result) == 0L)
  {
    base::return(result)
  }

  # The result is a data frame of precisely one row. There could be others left
  # to fetch, but if only one row is sought, we do not want them. In that case,
  # return the data frame (after converting strings to factors, if instructed).
  if (SqrlParam(datasource, "max") == 1L)
  {
    if (SqrlParam(datasource, "stringsAsFactors"))
    {
      for (i in base::seq_along(result))
      {
        if (base::identical(base::class(result[, i]),
                            base::class(base::character())))
        {
          result[, i] <- base::as.factor(result[, i])
        }
      }
    }
    base::return(result)
  }

  # Otherwise, we need to fetch any remaining rows (up to the specified limit).
  # Append fetch-in-progress marker to the window-title connection indicator.
  SqrlIndicator(datasource, "fetch")

  # Retrieve all remaining rows (up to any specified maximum limit).
  restof <- SqrlTry(
                RODBC::sqlGetResults(channel = SqrlParam(datasource, "channel"),
                        as.is = SqrlParam(datasource, "as.is"),
                        errors = SqrlParam(datasource, "errors"),
                        max = base::max(SqrlParam(datasource, "max") - 1L, 0L),
                        buffsize = SqrlParam(datasource, "buffsize"),
                        nullstring = SqrlParam(datasource, "nullstring"),
                        na.strings = SqrlParam(datasource, "na.strings"),
                        believeNRows = SqrlParam(datasource, "believeNRows"),
                        dec = SqrlParam(datasource, "dec"),
                        stringsAsFactors = FALSE))

  # Remove the fetch-in-progress marker from the window title.
  SqrlIndicator(datasource, "done")

  # With the initial call of RODBC::sqlQuery() having returned a non-empty data
  # frame (above), RODBC::sqlGetResults() should also have returned a data frame
  # (although, possibly one with zero rows). Anything else is an error.
  if (restof$error
      || !base::identical(base::class(restof$value),
                          base::class(base::data.frame())))
  {
    # If the failure appears to have been caused by a lost connection, and this
    # is our first attempt, then make one more (unless the retry parameter has
    # been set to FALSE).
    if (retry
        && SqrlParam(datasource, "retry")
        && !SqrlIsOpen(datasource, besure = TRUE))
    {
      base::return(SqrlSubmit(datasource, statement, retry = FALSE))
    }

    # If RODBC::sqlGetResults() threw an error, or when the 'errors' parameter
    # is TRUE, throw the error. The latter case promotes ODBC error messages to
    # local R exceptions (RODBC doesn't).
    if (restof$error
        || SqrlParam(datasource, "errors"))
    {
      base::stop(base::paste(restof$value, collapse = "\n"))
    }

    # Otherwise (when 'errors' is FALSE), return the unexpected result.
    base::return(restof$value)
  }

  # Append the subsequent rows (from RODBC::sqlGetResults()) to the initial row
  # (from RODBC::sqlQuery()). It is this operation that requires pulling with
  # stringsAsFactors = FALSE (above), because the two frames need not contain
  # the same factor-level definitions.
  result <- base::rbind(result, restof$value)

  # Convert strings to factors, if so instructed.
  if (SqrlParam(datasource, "stringsAsFactors"))
  {
    for (i in base::seq_along(result))
    {
      if (base::identical(base::class(result[, i]),
                          base::class(base::character())))
      {
        result[, i] <- base::as.factor(result[, i])
      }
    }
  }

  # Return the (non-empty) data frame.
  base::return(result)
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
  # utils Calls:
  #   head() (only if utils is attached).
  # SQRL Callers:
  #   SqrlFile().
  # User:
  #   Has no direct access, but is able to supply (only) the statement, phrase,
  #   and intermediate arguments via a SQRL script. These arguments will have
  #   already been parsed and worked into the correct format, by SqrlFile() and
  #   SqrlStatement(), so no argument validity checks should be required here.

  # If the phrase is non-empty, append it to the statement.
  if (base::nchar(phrase) > 0L)
  {
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
  }

  # If the statement is non-empty, submit it and retrieve the result.
  if (base::length(statement) > 0L)
  {
    # Collapse the statement to a single string. Submit it if non-blank.
    statement <- base::trimws(base::paste(statement, collapse = ""))
    if (base::grepl("[[:graph:]]", statement))
    {
      # Boolean; whether or not to show verbose output. The value of the verbose
      # parameter cannot change while this function is executing.
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

      # If verbose, output (some of) the result. Coming from SqrlSubmit(), this
      # should be a data frame, a short character vector, an integer, or NULL.
      # Methods for head() and print() are defined on all of these.
      if (verbose)
      {
        printed <- FALSE
        if ("package:utils" %in% base::search())
        {
          top <- utils::head(result)
          base::print(top)
          if (!base::identical(top, result))
          {
            base::cat("(output truncated)\n")
          }
        } else
        {
          base::cat(base::paste0("(object of class '",
                            base::paste0(base::class(result), collapse = " "),
                            "')\n"))
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
      # SQL_NO_DATA from the driver, after executing, say, drop table), or -2L
      # (under the same conditions, but when the errors parameter is FALSE), or
      # a zero-length character vector (sometimes produced by similar
      # operations), then return it invisibly.
      if (base::identical(result, -2L)
          || base::identical(result, "No Data")
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

  # Return NULL, signifying an undefined result (because there was no query).
  # SqrlSubmit() does the same thing, if it receives a blank statement (which it
  # shouldn't). RODBC::sqlQuery() (to which SqrlSubmit() is a wrapper), is
  # incapable of returning NULL (or NA). It doesn't actually matter whether or
  # not this is visible, since NULL is a special value; signifying to SqrlFile()
  # that the current overall result should not be replaced by this one.
  base::return(base::invisible(NULL))
}

SqrlTry <- function(expr,
                    warn = TRUE)
{
  # Evaluation with silent error catching and optional warning suppression.
  # Args:
  #   expr : An arbitrary R expression, to be evaluated.
  #   warn : When set to FALSE, warning messages are suppressed.
  # Returns:
  #   A vector of two named elements; 'error' and 'value'. When evaluation
  #   produces an error, 'error' will be TRUE and 'value' will be the error
  #   message. Otherwise (when the expression evaluated normally), 'error' will
  #   be FALSE and 'value' will be the result of that evaluation.
  # SQRL Calls:
  #   None.
  # SQRL Callers:
  #   SqrlClose(), SqrlDefile(), SqrlDelegate(), SqrlFile(), SqrlHelp(),
  #   SqrlIsOpen(), SqrlOff(), SqrlOpen(), SqrlPath(), SqrlPing(), SqrlProc(),
  #   SqrlShell(), SqrlSource(), SqrlSubmit(), sqrlInterface(), sqrlSource(),
  #   .onUnload().
  # User:
  #   Has no direct access, but can supply the expression indirectly. Here,
  #   that expression is inherently wrapped in tryCatch(), so no other checks
  #   are required.

  # Error-handling function.
  efun <- function(e)
  {
    base::list(error = TRUE, value = base::conditionMessage(e))
  }

  # When warnings are not to be suppressed, attempt to evaluate the expression
  # while trapping errors but throwing any warning messages.
  if (warn)
  {
    base::return(
        base::tryCatch(base::list(error = FALSE, value = expr), error = efun))
  }

  # Otherwise, warnings are to be suppressed. Attempt to evaluate the expression
  # while trapping errors and also suppressing any warning messages.
  base::return(base::suppressWarnings(
      base::tryCatch(base::list(error = FALSE, value = expr), error = efun)))
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

  # If the parameter is 'reset', then the value is a list of (uniquely) named
  # defaults. While there can be no secrets contained within those defaults,
  # some of them might be of named-value type, for which we return only the
  # names, rather than the named-values (for brevity, not security).
  if (parameter == "reset")
  {
    for (param in SqrlParams("named-values"))
    {
      if (param %in% base::names(value))
      {
        value[param] <- base::list(base::names(value[[param]]))
      }
    }
    base::return(value)
  }

  # Return only the names of any library entries, rather than their complete
  # definitions. This is for brevity, not for security.
  if (parameter %in% SqrlParams("named-values"))
  {
    base::return(base::names(value))
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
        # values (blank, <pwd>, and so on). These are unique.
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
      && base::identical(base::class(arglist[[1L]]),
                          base::class(base::character()))
      && (base::nchar(arglist[[1L]]) > 0L)
      && ((arglist[[1L]] %in% base::c(SqrlParams("all"), "source"))
          || base::grepl("^is\\s*open$", arglist[[1L]])))
  {
    base::return(SqrlAll(arglist, envir = base::parent.frame()))
  }

  # Apply the commands, return the results invisibly.
  base::return(base::invisible(SqrlAll(arglist, envir = base::parent.frame())))
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
  #   SqrlCache(), SqrlDefile(), SqrlInterface(), SqrlParam(), SqrlTry().
  # User:
  #   Exported function. User has direct access. The datasource name is checked
  #   for validity, but it is left to SqrlInterface() to establish the validity
  #   and usability of the interface name.

  # Either one or two arguments are expected.
  arglist <- base::list(...)
  if ((base::length(arglist) < 1L)
      || (base::length(arglist) > 2L))
  {
    k <- base::parse(text = "sqrlInterface(...)", keep.source = FALSE)
    m <- "A source name and an interface name are expected."
    base::stop(base::simpleError(m, k[[1L]]))
  }

  # Identify the data-source name and also the interface name (if specified).
  getname <- FALSE
  if (base::length(arglist) == 1L)
  {
    if (!base::is.null(base::names(arglist)))
    {
      datasource <- base::names(arglist)
      interface <- SqrlDefile("interface", arglist[[datasource]])
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
  if (!base::identical(base::class(datasource), base::class(base::character()))
      || (base::length(datasource) != 1L)
      || (base::nchar(datasource) < 1L)
      || SqrlCache(datasource, exists = FALSE))
  {
    k <- base::parse(text = "sqrlInterface(...)", keep.source = FALSE)
    m <- "Unrecognised data source."
    base::stop(base::simpleError(m, k[[1L]]))
  }

  # In the absence of a specified interface name, get and return the name of the
  # current interface to the data source (returnsS NULL if none exists).
  if (getname)
  {
    base::return(SqrlParam(datasource, "interface"))
  }

  # Relay the arguments to SqrlInterface() (returns the new name).
  f <- SqrlTry(base::withVisible(SqrlInterface(datasource, interface)))

  # In the event of an error, throw the message.
  if (f$error)
  {
    k <- base::parse(text = "sqrlInterface(...)", keep.source = FALSE)
    base::stop(base::simpleError(f$value, k[[1L]]))
  }

  # Return the new interface name, either visibly or invisibly, as appropriate.
  f <- f$value
  if (!f$visible)
  {
    base::return(base::invisible(f$value))
  }
  base::return(f$value)
}

sqrlOff <- function()
{
  # Close SQRL channels, deactivate SQRL.
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
  #   SqrlSource(), SqrlTry().
  # User:
  #   Exported function. User has direct access. Here, we ensure the existence
  #   of name and definition terms (in the form of multiple arguments, or at
  #   least one named argument). Additional checks are left to SqrlSource().

  # Unpack any list arguments (to their first-level elements).
  def <- base::list(...)
  i <- base::length(def)
  while (i > 0L)
  {
    if (base::identical(base::class(def[[i]]), base::class(base::list())))
    {
      j <- base::seq_along(def)
      if ((i == 1L)
          && !base::is.null(base::names(def))
          && base::nzchar(base::names(def)[1L]))
      {
        def <- base::c(base::names(def)[1L], def[[1L]], def[j[j > 1L]])
      } else
      {
        def <- base::c(def[j[j < i]], def[[i]], def[j[j > i]])
      }
    }
    i <- i - 1L
  }

  # Abort unless we have at least a pair of terms (name, definition) or a single
  # named term (name = definition).
  if ((base::length(def) < 2L)
      && base::is.null(base::names(def)))
  {
    k <- base::parse(text = "sqrlSource(...)", keep.source = FALSE)
    m <- "A name and definition are expected."
    base::stop(base::simpleError(m, k[[1L]]))
  }

  # Pass the arguments to SqrlSource() (returns the interface name, invisibly).
  s <- SqrlTry(SqrlSource(def))

  # In the event of an error, throw the message.
  if (s$error)
  {
    k <- base::parse(text = "sqrlSource(...)", keep.source = FALSE)
    base::stop(base::simpleError(s$value, k[[1L]]))
  }

  # Invisibly return the new interface name.
  base::return(base::invisible(s$value))
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
    k <- base::parse(text = "sqrlSources(...)", keep.source = FALSE)
    m <- "Argument should be 'all', 'user', 'system', or 'remove'."
    base::stop(base::simpleError(m, k[[1L]]))
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
  #   SqrlHelp(), SqrlTry(), SQRL:Face.

  # Remove any SQRL temp files from the R-session temp directory.
  SqrlTry(SqrlHelp(clean = TRUE), warn = FALSE)

  # Attempt to detach the public SQRL:Face environment, if not already done.
  if ("SQRL:Face" %in% base::search())
  {
    SqrlTry(base::detach("SQRL:Face"), warn = FALSE)
  }

  # Return invisible NULL.
  base::return(base::invisible(NULL))
}



######################################################################## EOF ###
