####################################################################### SQRL ###

# Wrapper to RODBC (and, as such, purely a tool of convenience).
# Handles multi-statement SQL scripts, optionally containing embedded R.
# Enables parameterised SQL queries.
# Automatic generation of a like-named user-interface to each data source.
# Manages multiple connections, their interfaces, and their parameters.
# Supports data-source/connection configuration files.
# Protects connection handles from rm(list = ls(all.names = TRUE)).
# Provides visual indication of which connections/channels are open.
# Provides visual indication of queries-in-progress (Windows only).
# Performs implicit pasting of multiple expressions into single statements.
# Detects closure of connection channels by remote servers.
# Attempts recovery from (reconnection following) network outages.
# Promotes remote SQL errors to local R errors (stop()s).
# Works best in Rgui on Windows.

# Mike Lee, South Titirangi, 8 November 2017.



#################################################################### HISTORY ###

# 10 November 2017.
# Changed default from visible TRUE to visible FALSE. (Since visibility means
# altering the user's prompt, it'd be polite to get permission first.) Added
# support for na.strings being a vector of strings.

# 30 August 2017 -- 8 November 2017.
# R-3. Complete re-write. Same interface, different implementation, additional
# capability. Dropped SQL version (vendor) specific features. Avoided the need
# for ahead-of-time or build-time source knowledge.

# 15 April 2014 -- 12 June, 2014.
# Packaged prototypes. Added support for multi-statement files with embedded R,
# but inherited the SQL version and advanced knowledge requirements of the
# prototype. Introduced stripping of comments from SQL prior to submission (on
# the data base of the time, too many comments at the beginning of a query led
# to that query being rejected).

# 7 January 2014.
# R-2. Original (unpackaged script) prototype. Primary objectives were to
# protect RODBC handles from rm(list = ls(all = TRUE)), and to provide
# simultaneous visual indication of connection status across multiple sources.
# Secondary features required specific versions of SQL, and advanced (ahead of
# time) domain-specific knowledge of the available data sources.



################################################################### CONTENTS ###

# srqlHaus            Private. Environment. Stores data source parameters.

# SqrlCache()         Private. Interfaces with srqlHaus (only point of contact).
# SqrlClose()         Private. Closes data source connection channels.
# SqrlConfig()        Private. Sets SQRL/RODBC parameters from a config file.
# SqrlCountArgs()     Private. Counts the number of arguments in a ... list.
# SqrlDelegate()      Private. Relays data between interfaces and functions.
# SqrlFace()          Private. Interfaces with the SQRL:Face environment.
# SqrlFile()          Private. Sources SQL (and/or R) statements from a file.
# SqrlIndicator()     Private. Toggles display of open-connection indicators.
# SqrlInterface()     Private. Defines and/or deletes data source interfaces.
# SqrlIsOpen()        Private. Tests whether or not source channels are open.
# SqrlOff()           Private. Closes all channels, detaches and unloads SQRL.
# SqrlOpen()          Private. Opens connection channels to data sources.
# SqrlParam()         Private. Gets and sets data source SQRL/RODBC parameters.
# SqrlPath()          Private. Checks if args are the path to an existing file.
# SqrlPing()          Private. Defines simple queries for pinging data sources.
# SqrlStatement()     Private. Assembles SQL statements from listed components.
# SqrlSource()        Private. Registers/defines new data sources with SQRL.
# SqrlSources()       Private. Look for, and summarise, known data sources.
# SqrlSubmit()        Private. Submits SQL, retrieves results, handles errors.
# SqrlSubScript()     Private. Relays data between SqrlFile() and SqrlSubmit().

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

# There will also exist a public environment, attached to the R search path as
# 'SQRL:Face', by the .onLoad() function, when the package is loaded.



########################################################## PRIVATE FUNCTIONS ###

SqrlCache <- function(datasource = "",
                      exists = NULL,
                      create = FALSE)
{
  # Checks, creates, lists and gets data source cache environments.
  # Args:
  #   datasource : The name of a data source, or '*' for all known data sources.
  #   exists     : If set to TRUE or FALSE, test if a cache exists or doesn't.
  #   create     : If set to TRUE, create a cache for the data source.
  # Returns:
  #   Either an environment handle, a logical (when performing an existence
  #   check), or a character vector (when listing all known data sources).
  # SQRL Calls:
  #   srqlHaus, SqrlParam().
  # SQRL Callers:
  #   SqrlOff(), SqrlParam(), SqrlSource(), SqrlSources(), sqrlInterface().
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
      base::stop("Cache already exists.")
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
  #   SqrlDelegate(), SqrlIsOpen(), SqrlOff().
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
                        confile = "",
                        parameter = "")
{
  # Assigns SQRL/RODBC parameter values, for a data source, from a file.
  # Args:
  #   datasource : The name of a known (to SQRL) data source.
  #   confile    : The path to a SQRL configuration file (optional).
  #   parameter  : The name of a single specific parameter (optional).
  # Returns:
  #   The imported configuration, as an invisible value (when a single parameter
  #   was specified) or as an invisible list of parameter (name, value) pairs
  #   (when no individual parameter was specified). When no configuration file
  #   is specified, this function acts as a getter, and returns a list of all
  #   SQRL/RODBC parameters and their current values (besides the password).
  # SQRL Calls:
  #   SqrlIsOpen(), SqrlParam(), SqrlPath().
  # SQRL Callers:
  #   SqrlDelegate(), SqrlSource().
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
    params <- base::c("dsn", "uid", "connection", "case", "believeNRows",
                "colQuote", "tabQuote", "interpretDot", "DBMSencoding",
                "rows_at_time", "readOnlyOptimize", "channel", "errors",
                "as.is", "max", "dec", "buffsize", "nullstring", "na.strings",
                "stringsAsFactors", "driver", "interface", "name", "ping",
                "verbose", "visible", "prompt", "wintitle", "dbcname")
    config <- base::list()
    for (param in params)
    {
      value <- SqrlParam(datasource, param)
      if (base::is.null(value))
      {
        value <- "NULL"
      }
      config[[param]] <- value
    }
    config[["pwd"]] <- base::character(0)
    base::return(config[base::order(base::names(config))])
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
        || base::grepl("^[[:space:]]#", lyne))
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

    # Allow multiple na.string entries. Append all to a vector. Note that x[[y]]
    # returns NULL when y is not among the names of x (i.e., the first time).
    if (param == "na.string")
    {
      value <- base::c(config[[param]], value)
    }
    
    # Retain the (name, value) pair, provided the name is not blank, and not
    # 'channel' (blocks setting/overwriting of this parameter).
    if ((base::nchar(param) > 0)
        && (param != "channel"))
    {
      config[[param]] <- value
    }
  }

  # When configuration was requested for a specific named parameter (only), set
  # only that one parameter's value.
  if (base::nchar(parameter) > 0)
  {
    # Assign the specified parameter's value, preferably from the matching
    # (name, value) pair in the file, or the first line (when pair not found).
    if (parameter %in% base::names(config))
    {
      if (parameter == "na.string")
      {
        for (string in config[[parameter]])
        {
          SqrlParam(datasource, parameter, string)
        }
      } else
      {
        SqrlParam(datasource, parameter, config[[parameter]])
      }
    } else {
      SqrlParam(datasource, parameter, first)
    }

    # Return the newly-set parameter value invisibly (except in the case of
    # parameter 'pwd'; obscure the actual password by returning character(0)).
    if (parameter == "pwd")
    {
      base::return(base::invisible(base::character(0)))
    }
    base::return(base::invisible(config[[parameter]]))
  }

  # When general configuration was requested, assign all values found.
  # This does allow setting of user-invented parameter names.
  for (parameter in base::names(config))
  {
    if (parameter == "na.string")
    {
      for (string in config[[parameter]])
      {
        SqrlParam(datasource, parameter, string)
      }
    } else
    {
      SqrlParam(datasource, parameter, config[[parameter]])
    }
  }

  # Return the configuration, invisibly, with any password obscured.
  if ("pwd" %in% base::names(config))
  {
    config[["pwd"]] <- base::character(0)
  }
  base::return(base::invisible(config))
}

SqrlCountArgs <- function(...)
{
  # Counts the number of items in the ... argument list.
  # Args:
  #   ... : A list of items (comma separated expressions).
  # Returns:
  #   The (integer) number of items in the ... list.
  # SQRL Calls:
  #   None.
  # SQRL Called By:
  #   SqrlDelegate().
  # User:
  #   Has no direct access, but can pass ... from SqrlDelegate().
  #   No argument checking is required, however.

  # Count and return the number of items passed in.
  base::return(base::nargs())
}

SqrlDelegate <- function(datasource = "",
                          ...,
                          envir = base::parent.frame())
{
  # Interpret the command, and forward to the appropriate handler.
  # Args:
  #   datasource : The name of a known database.
  #   ...        : A list of strings, forming a command or file path.
  #   envir      : An R environment (only used when sourcing files).
  # Returns:
  #   The result of the command (normally a data frame, sometimes a string).
  # SQRL Calls:
  #   SqrlClose(), SqrlConfig(), SqrlCountArgs(), SqrlFile(), SqrlInterface(),
  #   SqrlIsOpen(), SqrlOff(), SqrlOpen(), SqrlParam(), SqrlPath(),
  #   SqrlSources(), SqrlSubmit().
  # RODBC Calls:
  #   sqlColumns(), sqlTables(), sqlTypeInfo().
  # SQRL Callers:
  #   None (besides data source interfaces).
  # User:
  #   User has no direct access, but is able to supply (only) the ... arguments
  #   from any data source interface. Since ... is unrestricted, no argument
  #   validity checking is required.

  # Paste the command items together (unseparated).
  command <- base::paste(..., sep = "")

  # If no command was given, open a channel to the data source. If no channel
  # exists, a new channel is opened (and SqrlOpen() returns invisible NULL).
  # If a channel exists, but wasn't open after all (after besure = TRUE pings
  # the data source to check), we reaplce the dead channel with a new one
  # (SqrlOpen(), again). If a channel exists and is open, we do nothing else.
  if (base::length(command) == 0)
  {
    if (!SqrlIsOpen(datasource, besure = TRUE))
    {
      base::return(SqrlOpen(datasource))
    }
    base::return(base::invisible(NULL))
  }

  # If the command contains a list, vector, or other non-zero dimensional
  # object, then send it straight to SqrlSubmit().
  if (base::length(command) != 1)
  {
    base::return(SqrlSubmit(datasource, ...))
  }

  # If the command specifies a file path, try sourcing SQL from that file.
  file.path <- SqrlPath(...)
  if (!base::is.null(file.path))
  {
    base::return(SqrlFile(datasource, file.path, envir = envir))
  }

  # If the first word looks like standard SQL, submit the unaltered command.
  first.word <- base::tolower(
                    base::sub("^[[:space:]]*([[:graph:]]+).*$", "\\1", command))
  if (first.word %in% base::c("select", "create", "drop", "update", "insert"))
  {
    base::return(SqrlSubmit(datasource, command))
  }

  # Extract the first whole word from the first command item (we require these
  # special words to be explicit) and remove it from the remaining command.
  # Counting the arguments distinguishes fun('param') (return parameter value)
  # from fun('param', '') (set parameter value to the empty string).
  first.word <- base::sub("^[[:space:]]*([[:graph:]]+).*$", "\\1", ..1)
  other.words <- base::trimws(base::sub(first.word, "", command, fixed = TRUE))
  only.word <- base::ifelse((base::nchar(other.words) == 0)
                              && (SqrlCountArgs(...) == 1),
                              first.word, "")                           
                              
  # If the only word is 'close', close the data source channel.
  if ("close" == only.word)
  {
    if (!SqrlIsOpen(datasource))
    {
      base::warning("Channel not open.")
    }
    base::return(SqrlClose(datasource))
  }

  # If the first word is 'columns', call RODBC::sqlColumns() on the remainder.
  if ("columns" == first.word)
  {
    base::return(RODBC::sqlColumns(channel = SqrlParam(datasource, "channel"),
                                    sqltable = other.words,
                                    errors = SqrlParam(datasource, "errors"),
                                    as.is = TRUE))
  }

  # If the first word is 'config', get or set the configuration.
  if ("config" == first.word)
  {
    base::return(SqrlConfig(datasource, other.words))
  }

  # If the first word is 'interface', change the interface function.
  if ("interface" == first.word)
  {
    base::return(SqrlInterface(datasource, other.words))
  }

  # If the only word is 'isopen' (or if words one and two are 'is open'), return
  # the channel's open status (TRUE for open, FALSE otherwise). This calls with
  # besure = TRUE, to ping the source and be certain of the openness status.
  if (("isopen" == only.word)
      || (("is" == first.word)
          && ("open" == other.words)))
  {
    base::return(SqrlIsOpen(datasource, besure = TRUE))
  }

  # If the only word is 'off', close SQRL channels, terminate functionality.
  if ("off" == only.word)
  {
    base::return(SqrlOff())
  }

  # If the only word is 'off*', close all (SQRL and other) RODBC channels.
  if ("off*" == only.word)
  {
    base::return(SqrlOff(hard = TRUE))
  }

  # If the only word is 'open', open a channel to the specified data source.
  if ("open" == only.word)
  {
    base::return(SqrlOpen(datasource))
  }

  # If the command is 'sources', return the data source summary table.
  if ("sources" == only.word)
  {
    base::return(SqrlSources())
  }

  # If the only word is 'tables', call RODBC::sqlTables() on the data source.
  if ("tables" == only.word)
  {
    base::return(RODBC::sqlTables(channel = SqrlParam(datasource, "channel"),
                                    errors = SqrlParam(datasource, "errors"),
                                    as.is = TRUE))
  }

  # If the first word is 'typeinfo', call RODBC::sqlTypeInfo() on the remainder.
  if ("typeinfo" == first.word)
  {
    type <- base::ifelse(first.word == only.word, "all", other.words)
    base::return(RODBC::sqlTypeInfo(channel = SqrlParam(datasource, "channel"),
                                    type = type,
                                    errors = SqrlParam(datasource, "errors"),
                                    as.is = TRUE))
  }

  # When the first word is an SQRL/RODBC parameter, get or set that parameter.
  if (first.word %in% base::c("as.is", "believeNRows", "buffsize", "case",
              "channel", "colQuote", "connection", "dbcname", "DBMSencoding",
              "dec", "driver", "dsn", "errors", "interpretDot", "max",
              "na.string", "na.strings", "name", "nullstring", "ping", "prompt",
              "pwd", "readOnlyOptimize", "rows_at_time", "stringsAsFactors",
              "tabQuote", "uid", "verbose", "visible", "wintitle"))
  {

    # When getting, return the parameter's value (except for passwords, which
    # are obfuscated, and na.string, which isn't a real parameter).
    if (first.word == only.word)
    {
      if (first.word == "na.string")
      {
        base::stop("Parameter is write-only.")
      }
      if (first.word == "pwd")
      {
        base::return(base::character(0))
      }
      base::return(SqrlParam(datasource, first.word))
    }

    # Allow getting, but not setting, of the channel parameter from here.
    if (first.word == "channel")
    {
      base::stop("Parameter is read-only.")
    }
    
    # If the other words specify a file; set the parameter's value from that.
    if (!base::is.null(SqrlPath(other.words)))
    {
      base::return(SqrlConfig(datasource, other.words, first.word))
    }

    # Set the parameter's value to the supplied other words.
    base::return(SqrlParam(datasource, first.word, other.words))
  }

  # Otherwise, submit the original unaltered command.
  base::return(SqrlSubmit(datasource, ...))
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
                      envir = base::parent.frame())
{
  # Read a SQRL-script file and submit its content to a data source.
  # Args:
  #   datasource  : The name of a known data source.
  #   script.file : The file name (or path), as a string.
  #   envir       : An R environment (in which embedded R will be executed).
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

  # Sort the delimiters into ascending (script) positional order.
  ord <- base::order(pos)
  pos <- pos[ord]
  pat <- pat[ord]
  len <- len[ord]

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
  pp.envir <- base::new.env(parent = envir)

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
        # is to be assigned within the R post-processing environment.
        intermediate <- base::gsub("^<result\\s*->\\s*|>$", "",
                          base::substring(script, pos[i], pos[i] + len[i] - 1))

        # Submit the statement (with phrase) and pull the result.
        dat <- SqrlSubScript(datasource, statement, phrase,
                                intermediate, pp.envir)

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
      tag.end <- base::switch(r.type,
                              tag.r.begin = "tag.r.end",
                              tag.result = "tag.do")
      rscript <- base::list()
      i <- i + 1
      while ((i <= num.delims)
              && (pat[i] != tag.end))
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
        if ((r.type == "tag.result")
            && (i <= num.delims)
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
      if (r.type == "tag.r.begin")
      {
        rvalue <- base::eval(base::parse(text = rscript), pp.envir)
        statement <- base::append(statement, SqrlStatement(rvalue))

      # Otherwise (R post-processing), evaluate and retain the result.
      } else
      {
        # If verbose, output the script (prior to evaluation).
        if (verbose)
        {
          base::cat("\n")
          base::cat(rscript)
          base::cat("\n")
        }

        # Evaluate the script, retain the result.
        result <- base::eval(base::parse(text = rscript), pp.envir)

        # If verbose, output (some of) the result.
        if (verbose)
        {
          if (base::length(base::dim(result)) == 2)
          {
            base::print(result[1:base::min(base::nrow(result), 10), ])
          } else if (base::length(result) > 0)
          {
            base::print(result[1:base::min(base::length(result), 10)])
          } else if (base::length(result) == 0)
          {
            base::print(result)
          }
          base::cat("\n")
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
  # util Calls:
  #   getWindowTitle(), setWindowTitle() (only if the utils package is attached,
  #   and these two functions exist within it on the current OS/platform).
  # SQRL Callers:
  #   SqrlDelegate().
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
      utils::setWindowTitle(title =
            base::paste(base::sub("\\s+$", "", utils::getWindowTitle()), indic))
    }
    # Prepend command-prompt open-channel indicator.
    if (do.prompt)
    {
      indic <- SqrlParam(datasource, "prompt")
      base::options(prompt =
                        base::paste(indic, base::getOption("prompt"), sep = ""))
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
      if (base::nchar(indic) > 0)
      {
        windowtitle <- utils::getWindowTitle()
        if (base::grepl(indic, windowtitle, fixed = TRUE))
        {
          position <- base::max(
                          base::gregexpr(indic, windowtitle, fixed = TRUE)[[1]])
          before <- base::sub("\\s+$", "",
                                base::substring(windowtitle, 1, position - 1))
          after <- base::substring(windowtitle, position + base::nchar(indic))
          utils::setWindowTitle(title = base::paste(before, after, sep = ""))
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

  # When the action is 'busy', append a remote-job-in-progress marker ('*') to
  # the data source's window title indicator, then return invisible NULL. This
  # will work (as in, does nothing, quietly) if the indicator isn't actually on.
  if (action == "busy")
  {
    if (do.title)
    {
      indic <- SqrlParam(datasource, "wintitle")
      utils::setWindowTitle(title =
                            base::sub(indic, base::paste(indic, "*", sep = ""),
                                      utils::getWindowTitle(), fixed = TRUE))
    }
    base::return(base::invisible(NULL))
  }

  # When the action is 'done', remove a remote-job-in-progress marker ('*') from
  # the data source's window title indicator, then return invisible NULL. This
  # will work (as in, does nothing, quietly) if no marker is actually present.
  if (action == "done")
  {
    if (do.title)
    {
      indic <- SqrlParam(datasource, "wintitle")
      utils::setWindowTitle(title =
                            base::sub(base::paste(indic, "*", sep = ""), indic,
                                      utils::getWindowTitle(), fixed = TRUE))
    }
    base::return(base::invisible(NULL))
  }

  # This should be unreachable, but if we were to arrive here, return NULL.
  base::return(base::invisible(NULL))
}

SqrlInterface <- function(datasource = "",
                          interface = "",
                          vital = TRUE,
                          delete = FALSE)
{
  # Constructs a user-interface to a specified data source.
  # Args:
  #   datasource : The name of a known data source.
  #   interface  : The name to use for that data source's interface.
  #   vital      : When set to FALSE, name conflicts are non-fatal.
  #   delete     : When set to TRUE, the data source's interface is deleted.
  # Returns:
  #   A function (named <interface>) for interacting with the data source.
  #   Any pre-existing interface to that data source will be deleted.
  #   If interface is not specified, the interface name defaults to the data
  #   source name (sans whitespace). When delete == TRUE, no new interface is
  #   created (interface ignored), but any existing interface will be deleted
  #   (the interface argument is ignored in this case).
  # SQRL Calls:
  #   SqrlFace(), SqrlInterface() (self), SqrlParam().
  # SQRL Callers:
  #    SqrlDelegate(), SqrlInterface(), SqrlOff(), SqrlSource(), SqrlSources(),
  #    sqrlInterface().
  # User:
  #   Has no direct access, but is able to indirectly supply the datasource
  #   argument via sqrlInterface(), and through SqrlSources() by editing the
  #   registered data source names (DSNs) prior to loading SQRL. The user can
  #   indirectly supply the interface argument via sqrlInterface(),
  #   SqrlDelegate(), and through SqrlSources() by editing the DSNs prior to
  #   loading SQRL. The user can indirectly supply the delete argument only via
  #   sqrlInterface(). The user cannot indirectly supply the vital argument. In
  #   all cases, existence of the datasource is established before calling this
  #   function. The interface parameter is guaranteed to be a character string
  #   (singleton), but it is not assured to be usable (that is checked here).
  #   The delete parameter is guaranteed to be either TRUE or FALSE (not NA).

  # This is the user-interface function-body definition for the data source.
  uibody <- base::paste("function(...) {base::return(SqrlDelegate(\"",
              datasource, "\", ..., envir = base::parent.frame()))}", sep = "")

  # Isolate the previous interface (NULL when no interface was defined).
  preface <- SqrlParam(datasource, "interface")

  # On a request to delete the data source's interface, if we can confirm the
  # interface object retains its original SQRL definition, then we delete that
  # object. Either way, the interface is de-registered in the data source's
  # cache, and an invisible NULL is returned.
  if (delete)
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
  interface <- base::trimws(interface)
  if (base::nchar(interface) < 1)
  {
    interface <- base::gsub("[[:space:]]+", "", datasource)
  }

  # If the interface already exists (under the same name), return it (silently).
  # The above chack on preface guarantees existence within envir when not NULL.
  if (!base::is.null(preface)
      && (preface == interface))
  {
    base::return(base::invisible(SqrlFace(interface)))
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
    SqrlInterface(datasource, delete = TRUE)
  }

  # Assign the interface function to the chosen name. Note that changing the
  # interface (name) does not change the wintitle or prompt strings. Those are
  # both based upon the (invariant) data source name.
  SqrlFace(interface, uibody)

  # Register that assignment within the data source's cache. Again, this does
  # not alter the (data source name based) wintitle or prompt strings.
  SqrlParam(datasource, "interface", interface)

  # Return the new user-interface function (invisibly).
  base::return(base::invisible(SqrlFace(interface)))
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
    response <- base::try(
                      SqrlSubmit(datasource, ping, throw = TRUE, retry = FALSE),
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

SqrlOff <- function(hard = FALSE)
{
  # Close SQRL channels, deactivate SQRL. Optionally close other RODBC channels.
  # Args:
  #   hard : If TRUE, all RODBC channels will be closed (both SQRL and non-
  #           SQRL), via RODBC::odbcCloseAll(). Default is FALSE.
  # Returns:
  #   Invisible NULL, after closing channels and detaching SQRL.
  # SQRL Calls:
  #   SqrlCache(), SqrlClose(), SqrlInterface().
  # RODBC Calls:
  #   odbcCloseAll().
  # SQRL Callers:
  #   SqrlDelegate(), sqrlOff().
  # User:
  #   User has no direct access, but is able to supply the only argument via
  #   SqrlDelegate() and/or sqrlOff(). Both of these coerce to TRUE/FALSE,
  #   as does this function. No further argument validity checks are required.

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
            base::try(SqrlInterface(datasource, delete = TRUE), silent = TRUE))
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

  # In the optional case of a hard-off, close all RODBC channels (SQRL or
  # otherwise). This is thorough, but also affects non-SQRL channels.
  if (base::identical(hard, TRUE))
  {
    base::try(RODBC::odbcCloseAll())
  }

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
  #   SqrlIsOpen(), SqrlParam(), SqrlPing().
  # RODBC Calls:
  #   odbcConnect(), odbcDriverConnect().
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
  # the connection string, and/or the pwd parameter must be set, prior to making
  # a connection attempt. Otherwise, a (connection failure) error will result.

  # If a connection string has been defined for this source, connect using that.
  connection <- base::as.character(SqrlParam(datasource, "connection"))
  if (base::nchar(connection) > 0)
  {
    for (param in base::c("dsn", "dbcname", "driver", "uid", "pwd"))
    {
      connection <- base::gsub(base::paste("<", param, ">", sep = ""),
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
    channel <- base::try(
                RODBC::odbcConnect(
                dsn = SqrlParam(datasource, "dsn"),
                uid = SqrlParam(datasource, "uid"),
                pwd = SqrlParam(datasource, "pwd"),
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

  # Scrape uid, dsn, driver, and dbcname from the channel's connection attribute
  # (in case the user should have entered something new). The systems available
  # for testing (this) on do not permit use of '=', ';', '{', or '}' within any
  # of these parameters. The cstrings splitting operation (below) is likely to
  # yield incorrect results on any system that does allow these characters (and
  # when such characters are used). Do such systems exist? It doesn't matter if
  # these characters appear in a password, since that is neither present in the
  # connection attribute, nor scraped from it. Mis-scraping will not kill the
  # open channel, but it will produce an incorrect view in SqrlConfig(), and
  # will prevent network drop-out recovery in SqrlSubmit().
  cstring <- base::attr(channel, "connection.string")
  cstrings <- base::unlist(base::strsplit(cstring, ';'))
  for (param in base::c("uid", "dsn", "driver", "dbcname"))
  {
    pattern <- base::paste("^", param, "=", sep = "")
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
                      override = FALSE)
{
  # Gets and sets named SQRL/RODBC control parameters for a data source.
  # Args:
  #   datasource : The name of a known (to SQRL) data source.
  #   parameter  : The name of a SQRL or RODBC control parameter.
  #   set        : The value to assign to that parameter (optional).
  #   override   : If set to TRUE, open status does not block value changes.
  # Returns:
  #   The value of the named parameter for the named data source. If the set
  #   argument is specified, then the new value is returned (invisibly) after
  #   its assignment to the parameter (new passwords are not returned).
  # SQRL Calls:
  #   SqrlCache(), SqrlIndicator(), SqrlIsOpen(), SqrlParam() (self).
  # SQRL Callers:
  #   SqrlCache(), SqrlClose(), SqrlConfig(), SqrlDelegate(), SqrlFile(),
  #   SqrlIndicator(), SqrlInterface(), SqrlIsOpen(), SqrlOpen(), SqrlParam()
  #   (self), SqrlPing(), SqrlSource(), SqrlSources(), SqrlSubmit(),
  #   SqrlSubScript().
  # User:
  #   Has no direct access, but is able to supply (only) parameter and set via
  #   SqrlDelegate() and/or SqrlConfig(). The former vets parameter while the
  #   latter does not (although it will restrict parameter to being a string,
  #   and is write-only). Neither vets set, and that must be performed here.

  # Obtain a handle to the data source's SQRL cache.
  cacheenvir <- SqrlCache(datasource)

  # When the set argument is supplied, act as a setter (cache and return).
  if (!base::missing(set))
  {
    # Checks to move to user end functions once this fun hidden from user.
    if ((parameter == "name")
        && base::exists(parameter, cacheenvir, inherits = FALSE))
    {
      base::stop("Parameter is write-protected.")
    }

    # Prevent changing RODBC::odbcConnect() parameters while connection is open.
    # (Because those changes would only take effect on opening a new channel.)
    # The override condition allows SqrlOpen() to alter some of these (to values
    # the user may have entered) when the connection channel is first opened.
    if (!override
        && (parameter %in% base::c("dsn", "uid", "pwd", "connection", "case",
                                "believeNRows", "colQuote", "tabQuote",
                                "interpretDot", "DBMSencoding", "rows_at_time",
                                "readOnlyOptimize", "dbcname"))
        && SqrlIsOpen(datasource))
    {
      base::stop("Parameter is locked while connection is open.")
    }

    # Prvent overwriting (changing) the channel while it is open, with the
    # exception that a channel can be nullified (forced closed) at any time.
    if ((parameter == "channel")
        && !base::is.null(set)
        && SqrlIsOpen(datasource))
    {
      base::stop("Channel cannot be changed while open.")
    }

    # Coerce set to the appropriate data type for the specified parameter.
    # Firstly, parameters that are logically-valued.
    if (parameter %in% base::c("believeNRows", "readOnlyOptimize", "visible",
              "stringsAsFactors", "interpretDot", "errors", "as.is", "verbose"))
    {
      set <- base::suppressWarnings(base::as.logical(set))
      if ((base::length(set) != 1)
          || base::is.na(set))
      {
        base::stop("New parameter value not a logical singleton.")
      }

    # Parameters that are integer-valued.
    } else if (parameter %in% base::c("rows_at_time", "max", "buffsize"))
    {
      set <- base::suppressWarnings(base::as.integer(set))
      if ((base::length(set) != 1)
          || base::is.na(set))
      {
        base::stop("New parameter value is not an integer.")
      }

    # The interface parameter can be character-valued or null-valued.
    # Changing the parameter does not change the interface.
    } else if (parameter == "interface")
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
    } else if (parameter == "channel")
    {
      if (!base::is.null(set)
          && (base::class(set) != "RODBC"))
      {
        base::stop("New parameter value is not a connection handle.")
      }

    # The colQuote and tabQuote parameters can be either NULL or strings.
    } else if (parameter %in% base::c("colQuote", "tabQuote"))
    {
      if (!base::is.null(set))
      {
        set <- base::suppressWarnings(base::as.character(set))
        if ((base::length(set) != 1)
            || base::is.na(set))
        {
          base::stop("New parameter value is not a character string.")
        }
        if (base::tolower(set) == "null")
        {
          set <- NULL
        }
      }

    # The nullstring parameter is a character string, possibly NA_character_.
    } else if (parameter == "nullstring")
    {
      set <- base::suppressWarnings(base::as.character(set))
      if ((base::length(set) != 1))
      {
        base::stop("New parameter value is not a character string.")
      }
      if (base::is.na(set)
          || (set %in% base::c("NA", "NA_character_")))
      {
        set <- NA_character_
      }

    # All other parameters are (non-NA) character-valued (strings).
    } else
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
      # RODBC::odbcConnect() likes to know the driver (from which it determines
      # whether or not it's dealing with MySQL). While we're doing that, we may
      # as well attempt to extract some other parameter values, too.
      for (param in base::c("dsn", "dbcname", "driver", "pwd",
                            "password", "uid", "username"))
      {
        if (base::grepl(base::paste(param, "\\s*=", sep = ""),
                        set, ignore.case = TRUE))
        {
          assignee <- base::paste("^.*", param, "\\s*=", sep = "")
          value <- base::sub(assignee, "", set, ignore.case = TRUE)
          value <- base::trimws(base::sub(";.*$", "", value))
          # 'username' is a connection string alias for 'uid'.
          if (param == "username")
          {
            param <- "uid"
          # 'password' is a connection string alias for 'pwd'.
          } else if (param == "password")
          {
            param <- "pwd"
          }
          # SQRL accepts <uid> (etc.) as connection string template place
          # holders (to be replaced with current values at connection time).
          # We don't want to override default or previous values with these.
          if (value != base::paste("<", param, ">", sep = ""))
          {
            SqrlParam(datasource, param, value)
          }
        }
      }
      # Set the (unaltered) connection string, return it invisibly.
      base::assign(parameter, set, cacheenvir)
      base::return(base::invisible(set))
    }

    # The na.string parameter is a special case, used to append another string
    # to the na.strings (vector) parameter value.
    if (parameter == "na.string")
    {
      parameter <- "na.strings"
      if (base::exists(parameter, cacheenvir, inherits = FALSE))
      {
        set <- base::unique(base::c(SqrlParam(datasource, parameter), set))
      }
      base::assign(parameter, set, cacheenvir)
      base::return(base::invisible(set))
    }
    
    # The pwd parameter is a special case, because we don't want to return the
    # actual password (someone might be looking).
    if (parameter == "pwd")
    {
      base::assign(parameter, set, cacheenvir)
      base::return(base::invisible(base::character(0)))
    }

    # The prompt and wintitle parameters are special cases, because, if the
    # old prompt or wintitle is currently visible, it must be removed before
    # changing the parameter value, and then the new value must be applied.
    if (parameter %in% base::c("prompt", "wintitle"))
    {
      if (set != SqrlParam(datasource, "prompt"))
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
  base::return(base::switch(parameter,

    # Parameters for RODBC::odbcConnect() and/or RODBC::odbcDriverConnect().
    "dsn"                 = "",
    "uid"                 = base::Sys.getenv("username"),
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
    "dbcname"             = "",

    # Parameters for RODBC::sqlQuery().
    # Also uses believeNRows and rows_at_time, as above.
    "channel"             = NULL,
    "errors"              = TRUE,
    "as.is"               = FALSE,
    "max"                 = 0,
    "buffsize"            = 1000,
    "nullstring"          = NA_character_,
    "na.strings"          = "NA",
    "dec"                 = base::getOption("dec"),
    "stringsAsFactors"    = base::default.stringsAsFactors(),

    # Parameters for SQRL.
    "*"                   = base::objects(cacheenvir, all.names = TRUE),
    "driver"              = "",
    "interface"           = NULL,
    "name"                = datasource,
    "ping"                = NULL,
    "verbose"             = FALSE,
    "visible"             = FALSE,

    # Further SQRL parameters, for which the default values require additional
    # computation (and so are assigned to the cache for future reference).
    "prompt"    = {
                    marker <- base::substr(SqrlParam(datasource, "name"), 1, 1)
                    base::assign("prompt", marker, cacheenvir)
                    marker
                  },
    "wintitle"  = {
                    marker <- base::paste("(", SqrlParam(datasource, "name"),
                                          ")", sep = "")
                    base::assign("wintitle", marker, cacheenvir)
                    marker
                  },

    # No other default parameter values are defined (abort and notify).
    base::stop("Unknown parameter.")))
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
  #   SqrlConfig(), SqrlDelegate(), SqrlSource().
  # User:
  #   Has no direct access, but is able to supply arguments(s) indirectly, via
  #   SqrlDelegate(). There are no restrictions on these (no checks required).

  # Paste all arguments together.
  filepath <- base::paste(..., sep = "")

  # If the filepath does not have length one, then it cannot specify a file.
  if ((base::length(filepath) != 1)
      || (base::nchar(filepath) < 1))
  {
    base::return(NULL)
  }

  # If filepath actually does point to a file, return the (normalised) path.
  if (base::file.exists(filepath)
      && !(base::file.info(filepath)$isdir))
  {
    base::return(base::normalizePath(filepath))
  }

  # If filepath contains precisely one occurrence of '<<', start with the path
  # component preceding this (or the current directory, '.', if no preceding
  # component was specified) and climb back up the file tree (toward root, or
  # the drive letter) until the 'climbed' component (that is, the preceding
  # component plus some number of '../'s) plus the trailing component points to
  # a file (or give up after reaching root or the drive letter). If the
  # preceding component (as originally specified) does not exist as a file
  # system path, we return NULL right away (to avoid SqrlDelegate(param, path)
  # thinking that param is part of the path). Before testing before, trailing
  # path separators (\, /) must be trimmed (on Windows, file.exists() returns
  # FALSE whenever the path ends in a separator).
  pos <- base::unlist(base::gregexpr("<<", filepath, fixed = TRUE))
  if ((base::length(pos) == 1)
      && (pos > 0))
  {
    before <- base::trimws(base::substring(filepath, 0, pos - 1))
    after <- base::trimws(base::substring(filepath, pos + base::nchar("<<")))
    if (base::nchar(before) < 1)
    {
      before <- "."
    }
    before <- base::normalizePath(before, mustWork = FALSE)
    before <- base::sub("[\\/]+$", "", before)
    if (!base::file.exists(before))
    {
      base::return(NULL)
    }
    oldbefore <- ""
    while (before != oldbefore)
    {
      filepath <- base::file.path(before, after)
      if (base::file.exists(filepath))
      {
        base::return(base::normalizePath(filepath))
      }
      oldbefore <- before
      before <- base::normalizePath(base::file.path(before, ".."),
                                    mustWork = FALSE)
    }
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

SqrlSource <- function(interface = "",
                        ...)
{
  # Defines (or re-defines) a data source and its interface.
  # Args:
  #   interface : The name to use for the data source and its interface.
  #   ...       : A connection string, the path to a config file, or a source.
  # Returns:
  #   Invisible NULL, after creating, or re-defining, the source and interface.
  # SQRL Calls:
  #   SqrlCache(), SqrlConfig(), SqrlFace(), SqrlInterface(), SqrlParam(),
  #   SqrlPath().
  # SQRL Callers:
  #   sqrlSource().
  # User:
  #   Has no direct access. Can supply all arguments via sqrlSource() (only).
  #   The interface argument is guaranteed to be a non-blank, non-empty, string.
  #   The ... arguments are guaranteed to concatenate to a non-blank string.
  #   Additonal checks (assignability, conflict, etc.) are performed here.

  # Abort if the name (used for both source and interface) is unassignable.
  # (So that successfully adding a new source guarantees a named interface.)
  if (interface != base::make.names(interface))
  {
    base::stop("Unassignable interface name.")
  }

  # Abort if the name clashes with that of some object other than the interface
  # of a pre-existing data source of the same name (see comment below).
  if (SqrlFace(interface, clashes = TRUE)
      && (SqrlCache(interface, exists = FALSE)
          || SqrlParam(interface, "interface") != interface))
  {
    base::stop("Interface name conflict.")
  }

  # Create a cache for the data source, unless it already exists. (If it does
  # exist (see comment above), then we are simply adding or replacing some
  # parameter values and, possibly, the interface.)
  if (SqrlCache(interface, exists = FALSE))
  {
    SqrlCache(interface, create = TRUE)
  }

  # If ... names an existing data source, make our new source a duplicate of it.
  # (Copy (almost) all non-default parameter values from the original (<...>)
  # data source's cache, to the new (<interface>) data source's cache.)
  datasource <- base::paste(..., sep = "")
  if (SqrlCache(datasource, exists = TRUE))
  {
    params <- SqrlParam(datasource, "*")
    params <- params[!(params %in% base::c("name", "interface", "channel",
                                            "prompt", "wintitle"))]
    for (param in params)
    {
      SqrlParam(interface, param, SqrlParam(datasource, param))
    }

  # If, instead, ... names an existing file, configure <interface> from that.
  } else if (!base::is.null(SqrlPath(...)))
  {
    SqrlConfig(interface, SqrlPath(...))

  # Otherwise, assume ... specifies a connection string. Concatenate ... into a
  # semi-colon delimited string (each of the components supplied in ...  may or
  # may not end in a semi-colon) and store the result in <interface>'s cache.
  } else
  {
    # Concatenate ... into a ; delimited connection string.
    # (Each of the supplied components may or may not end in a semi-colon.)
    bits <- base::sub(";$", "", base::list(...))
    bits <- base::sub("^(\\s*[[:alpha:]]+\\s*=)", ";\\1", bits)
    string <- base::paste(bits, collapse = "")
    string <- base::sub("^;", "", string)
    SqrlParam(interface, "connection", string)
  }

  # Generate the interface for the connection string. This ought to always
  # succeed, since we've verified the name is assignable and doesn't clash.
  SqrlInterface(interface)

  # Return invisible NULL.
  base::return(base::invisible(NULL))
}

SqrlSources <- function(silent = FALSE)
{
  # Imports registered DSNs, and returns a summary table of defined sources.
  # Args:
  #   silent : Whether or not to return the summary table. Default is FALSE.
  # Returns:
  #   Either invisible NULL (when silent = TRUE), or (otherwise) a data frame
  #   summarising locally defined data sources. There is no guarantee that any
  #   of these suorces are presently available, or even that they exist.
  # SQRL Calls:
  #   SqrlCache(), SqrlInterface(), sqrlIsOpen(), SqrlParam().
  # RODBC Calls:
  #   odbcDataSources().
  # SQRL Callers:
  #   SqrlDelegate(), sqrlSources(), .onLoad().
  # User:
  #   User has no direct access, and is unable to supply the argument.
  #   Argument validity checking is not required.

  # Import a list of registered data sources (DSNs).
  sources <- RODBC::odbcDataSources()

  # Filter out Microsoft Access, dBASE, and Excel sources.
  sources <- sources[
                !base::grepl("Access|dBASE|Excel", sources, ignore.case = TRUE)]

  # If any of the sources was previously unknown (has no associated cache), then
  # create a new cache for it. Store some valuables in the cache, then attempt
  # to generate an interface for the source (failure to do so is non-fatal).
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

  # In silent mode, return invisible NULL.
  if (base::identical(silent, TRUE))
  {
    base::return(base::invisible(NULL))
  }

  # Otherwise, retrieve and return a summary of sources (data frame).
  params <- base::c("name", "interface", "open", "uid", "driver")
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
        value <- SqrlParam(datasource, param)
        if (base::is.null(value))
        {
          value <- NA
        }
      }
      sumlist[[param]] <- base::append(sumlist[[param]], value)
    }
  }
  sumtab <- NULL
  for (param in params)
  {
    sumtab <- base::cbind(sumtab, base::unlist(sumlist[[param]]))
  }
  sumframe <- base::as.data.frame(sumtab, stringsAsFactors = FALSE)
  sumframe <- sumframe[base::order(sumframe[, 1]), ]
  base::colnames(sumframe) <- params
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
  #   sqlQuery().
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

  # Append job-in-progress indicator to the window-title connection indicator.
  SqrlIndicator(datasource, "busy")

  # A valid connection exists, submit the statement.
  result <- RODBC::sqlQuery(channel = SqrlParam(datasource, "channel"),
                  query = statement,
                  errors = SqrlParam(datasource, "errors"),
                  as.is = SqrlParam(datasource, "as.is"),
                  max = SqrlParam(datasource, "max"),
                  buffsize = SqrlParam(datasource, "buffsize"),
                  nullstring = SqrlParam(datasource, "nullstring"),
                  na.strings = SqrlParam(datasource, "na.strings"),
                  believeNRows = SqrlParam(datasource, "believeNRows"),
                  dec = SqrlParam(datasource, "dec"),
                  stringsAsFactors = SqrlParam(datasource, "stringsAsFactors"),
                  rows_at_time = SqrlParam(datasource, "rows_at_time"))

  # Remove job-in-progress indicator from the window title.
  SqrlIndicator(datasource, "done")

  # On success, RODBC::sqlQuery() returns a data frame or character string (both
  # possibly empty). On error, RODBC::sqlQuery() returns a character vector of
  # error messages (always with at least two elements, it seems) or an integer
  # (either -1 or -2). From R, see ?RODBC::sqlQuery. This detects such errors.
  srcerr <- ((base::class(result) == base::class(base::integer()))
              || ((base::class(result) == base::class(base::character()))
                    && (base::length(result) > 1)))

  # If there was an error, and the retry flag is set (to TRUE), and we have a
  # stored password (with which to reconnect to the data source), and a double
  # check of the connection channel's open status (this time, with a ping of
  # the data source) reveals that it's actually closed, then we infer that the
  # channel was likely closed by the source and submit the statement one more
  # time (only). This call of SqrlSubmit() will attempt to reopen the channel.
  # This provides a (very) limited ability to recover from network drop-outs.
  if (srcerr
      && retry
      && (base::nchar(SqrlParam(datasource, "pwd")) > 0)
      && !SqrlIsOpen(datasource, besure = TRUE))
  {
    base::return(SqrlSubmit(datasource, ..., throw = throw, retry = FALSE))
  }

  # RODBC::sqlQuery() doesn't (often) throw proper errors (when these occur on
  # the data source side). Instead, it returns the error messages (as sent from
  # the data source). Trap and throw these (as fatal errors), unless the throw
  # argument and the error parameter are both FALSE.
  if (srcerr
      && (throw
          || SqrlParam(datasource, "errors")))
  {
    base::stop(base::paste(result, collapse = "\n"))
  }

  # Everything seems to have gone smoothly; return the result.
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
          base::print(result[1:base::min(base::nrow(result), 10), ])
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



########################################################### PUBLIC FUNCTIONS ###

sqrlInterface <- function(datasource = "",
                          interface = "",
                          delete = FALSE)
{
  # Constructs a user-interface to a specified data source.
  # Args:
  #   datasource : The name of a known data source.
  #   interface  : The name to use for that data source's interface.
  #   delete     : When set to TRUE, the data source's interface is deleted.
  # Returns:
  #   A function (named <interface>) for interacting with the data source.
  #   Any pre-existing interface to that data source will be deleted.
  #   If interface is not specified, the interface name defaults to the data
  #   source name (sans whitespace). When delete == TRUE, no new interface is
  #   created (interface ignored), but any existing interface will be deleted
  #   (the interface argument is ignored in this case).
  # SQRL Calls:
  #   SqrlCache(), SqrlInterface().
  # User:
  #   Exported function. User has direct access. All arguments are checked for
  #   validity (although this does not guarantee usability of the interface,
  #   which is left for SqrlInterface() to establish).

  # Abort on non-existence of the specified data source.
  if ((base::class(datasource) != base::class(base::character()))
      || (base::length(datasource) != 1)
      || (base::nchar(datasource) < 1)
      || SqrlCache(datasource, exists = FALSE))
  {
    base::stop("Unrecognised data source.")
  }

  # Abort on invalid (non-string) interface. The empty string is allowed.
  # The name may, or may not, be available and assignable.
  if ((base::class(interface) != base::class(base::character()))
      || (base::length(interface) != 1))
  {
    base::stop("Invalid interface name.")
  }

  # Abort on an invalid delete value (can be TRUE or FALSE, only).
  if (!base::identical(delete, FALSE)
      && !base::identical(delete, TRUE))
  {
    base::stop("Invalid delete-parameter value.")
  }

  # Relay the arguments to SqrlInterface().
  base::return(SqrlInterface(datasource, interface, delete = delete))
}

sqrlOff <- function(ensure = FALSE)
{
  # Close SQRL (optionally also all other RODBC) channels, deactivate SQRL.
  # Args:
  #   ensure : If TRUE, all RODBC channels will be closed. Default is FALSE.
  # Returns:
  #   Invisible NULL, after closing channels and detaching SQRL.
  # SQRL Calls:
  #   SqrlOff().
  # User:
  #   Exported function. User has direct access. Their only input argument is
  #   coerced to a Boolean singleton (either TRUE or FALSE, not NA) before
  #   passing on.

  # Relay the command-option to SqrlOff() (returns invisible NULL).
  base::return(SqrlOff(base::identical(ensure, TRUE)))
}

sqrlSource <- function(name = "",
                        ...)
{
  # Defines (or re-defines) a data source and its interface.
  # Args:
  #   name : The name to use for the data source and its interface.
  #   ...  : A connection string, the path to a config file, or a source.
  # Returns:
  #   Invisible NULL, after creating, or re-defining, the source and interface.
  # SQRL Calls:
  #   SqrlSource().
  # User:
  #   Exported function. User has direct access. We require name to be a
  #   non-empty, non-blank, string, and ... to be at least one non-empty,
  #   non-blank, string. Those checks are performed here.

  # Abort if name is blank or non-singleton.
  if ((base::class(name) != base::class(base::character()))
      || (base::length(name) != 1)
      || (base::nchar(base::trimws(name)) < 1))
  {
    base::stop("Invalid or missing name.")
  }

  # Abort if ... is blank or does not concatenate to a singleton.
  concat <- base::paste(..., sep = "")
  if ((base::length(concat) != 1)
      || (base::nchar(base::trimws(concat)) < 1))
  {
    base::stop("Invalid or missing connection details.")
  }

  # Pass the arguments to SqrlSource() (returns invisible NULL).
  base::return(SqrlSource(base::trimws(name), ...))
}

sqrlSources <- function()
{
  # Re-scans registered DSNs, and returns a summary table of defined sources.
  # Args:
  #   None.
  # Returns:
  #   A data frame summarising locally defined data sources. There is no
  #   guarantee that any of them are presently available, or even if they exist.
  # SQRL Calls:
  #   SqrlSources().
  # User:
  #   Exported function. User has direct access, but the function has no args.

  # Call SqrlSources(), return the summary.
  base::return(SqrlSources())
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

  # Look for registered data sources (DSNs). Create an interface for each one.
  SqrlSources(silent = TRUE)

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

  # Attempt to detach the public SQRL:Face environment, if it not already done.
  if ("SQRL:Face" %in% base::search())
  {
    base::try(base::detach("SQRL:Face"))
  }

  # Return invisible NULL.
  base::return(base::invisible(NULL))
}



######################################################################## EOF ###
