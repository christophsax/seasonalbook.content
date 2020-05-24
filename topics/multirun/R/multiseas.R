multiseas <- function(x, xreg = NULL, xtrans = NULL,
                 seats.noadmiss = "yes", transform.function = "auto",
                 regression.aictest = c("td", "easter"), outlier = "",
                 automdl = "", na.action = na.omit,
                 out = FALSE, dir = NULL, ..., list = NULL){


  # dir = "~/Desktop/x13"
  z <- list()  # output object
  z$call <- match.call()

  # intial checks
  checkX13(fail = TRUE, fullcheck = FALSE, htmlcheck = FALSE)

  # lookup table for output specification
  SPECS <- NULL
  data(specs, envir = environment(), package = "seasonal")  # avoid side effects
  SERIES_SUFFIX <- SPECS$short[SPECS$is.series]

  # save series name
  series.name <- names(x)

  # remove quotes and : in series.names
  series.name <- gsub('[\'\\"]', '', series.name)
  series.name <- gsub(':', '_', series.name)

  # parent.frame() <- sys.frame(-1)  # environment where seas was called

  # using the list argument instead of '...''
  if (is.null(list)){
    list <- list(...)

    # save call as list with evaluated arguments
    cl <- match.call(seas, z$call)
    z$list <- lapply(as.list(cl)[-1], eval, envir = parent.frame())

  } else {
    # save list with evaluated arguments
    z$list <- lapply(list, eval, envir = parent.frame())

    if (!inherits(list, "list")){
      stop("the 'list' argument mus be of class 'list'")
    }
    if (length(names(list)) != length(list)){
      stop("all spec.argument combinations in 'list' must be named")
    }
    # overwrite defaults if specified in the list
    dl <- names(list)[names(list) %in% names(formals(seas))]
    for (dli in dl){
      assign(dli, list[[dli]])
    }
    if ("list" %in% dl){
      stop("no 'list' argument inside the 'list' argument allowed")
    }
    if (length(list(...) > 0)){
      warning("if 'list' is specified, spec.argument combinations delivered to '...' are ignored.")
    }
    if ("x" %in% dl){
      series.name <- "ser"
    }
    # remove defaults from list
    list <- list[!(names(list) %in% dl)]
  }

  # check series
  if (!all(sapply(x, inherits, "ts"))) {
    stop("'x' argument is not a time series.")
  }

  if (any(sapply(x, function(x){start(x)[1]}) <= 1000)) {
    stop("start year of 'x' must be > 999.")
  }

  if(length(unique(sapply(x, frequency))) > 1) {
    stop("all series must have the same frequency.")
  }

  freq <- frequency(x[[1]])

  # na action
  x.na <- lapply(x, na.action)

  # temporary working dir and filenames
  pat <- if (out) "x13out" else "x13"
  wdir <- tempfile(pattern = pat)
  while (file.exists(wdir)) {
    wdir <- tempfile(pattern = pat)
  }

  dir.create(wdir)

  # file names for
  iofile <- file.path(wdir, "iofile")      # inputs and outputs (w/o suffix)
  datafile <- file.path(wdir, sprintf("data%d.dta", seq_along(x.na)))  # series to adjust
  metafile <- file.path(wdir, "metafile.dta")
  # user defined variables
  xreg.file <- file.path(wdir, "xreg.dta")
  xtrans.file <- file.path(wdir, "xtrans.dta")

  ### write data
  for(i in seq_along(x.na)) {
    seasonal:::write_ts_dat(x.na[[i]], file = datafile[i])
  }

  writeLines(datafile, metafile)

  ### construct spclist (spclist fully describes the .spc file)
  spc <- list()
  class(spc) <- c("spclist", "list")

  # add data series
  spc$series$format <- "\"datevalue\""
  spc$series$period <- freq

  # add the default options
  spc$transform$`function` <- transform.function
  spc$regression$aictest <- regression.aictest
  spc$seats$noadmiss <- seats.noadmiss

  spc <- seasonal:::mod_spclist(spc, list = list(outlier = outlier, automdl = automdl))

  # add user defined options
  spc <- seasonal:::mod_spclist(spc, list = list)

  # remove double entries, adjust outputs
  spc <- seasonal:::consist_spclist(spc)

  # TODO: Does this make sense with multiple series at all? I really don't know
  ### user defined regressors
  if (!is.null(xreg)){
    if (frequency(xreg) != freq){
      stop('xreg and x must be of the same frequency.')
    }
    seasonal:::write_ts_dat(na.action(xreg), file = xreg.file)
    # user names either from input (single "ts"), or from colnames ("mts)
    if (is.null(dim(xreg))){
      if (inherits(substitute(xreg), "name")){
        user <- deparse(substitute(xreg))
      } else {
        user <- "xreg"
      }
    } else {
      user <- paste0("xreg", 1:NCOL(xreg))
      # user <- gsub("[\\(\\)]", "", colnames(xreg))
    }

    if (!is.null(spc$x11regression)){
      spc$x11regression$user <- user
      spc$x11regression$file <- paste0("\"", xreg.file, "\"")
      spc$x11regression$format <- "\"datevalue\""
    } else {
      spc$regression$user <- user
      spc$regression$file <- paste0("\"", xreg.file, "\"")
      spc$regression$format <- "\"datevalue\""
    }
  }

  if (!is.null(xtrans)){
    if (frequency(xtrans) != freq){
      stop('xtrans and x must be of the same frequency.')
    }
    write_ts_dat(na.action(xtrans), file = xtrans.file)
    # user names either from input (single "ts"), or from colnames ("mts)
    if (is.null(dim(xtrans))){
      if (inherits(substitute(xtrans), "name")){
        name <- deparse(substitute(xtrans))
      } else {
        name <- "xtrans"
      }
    } else {
      name <- paste0("xtrans", 1:NCOL(xtrans))
      # name <- gsub("[\\(\\)]", "", colnames(xtrans))
    }
    spc$transform$name = name
    spc$transform$file <- paste0("\"", xtrans.file, "\"")
    spc$transform$format <- "\"datevalue\""
  }

  ### write spc
  spctxt <- seasonal:::deparse_spclist(spc)
  writeLines(spctxt, con = paste0(iofile, ".spc"))

  ### Run X13, either with full output or not
  multirun_x13(iofile, out)

  flist <- list.files(wdir) # all files produced by X-13

  ### Save output files if 'dir' is specified
  if (!is.null(dir)){
    if (!file.exists(dir)){
      dir.create(dir)
    }
    file.copy(file.path(wdir, flist), dir, overwrite = TRUE)
    message("All X-13ARIMA-SEATS output files have been copied to '", dir, "'.")
  }

  ### Import from X13

  # check wether there is output at all.
  outfile <- if (getOption("htmlmode") == 1){
    sub("\\.dta", ".html", datafile)
  } else {
    sub("\\.dta", ".out", datafile)
  }

  # add all series that have been produced and are specified in SERIES_SUFFIX
  file.suffix <- unlist(lapply(strsplit(flist, "\\."), function(x) x[[length(x)]]))
  is.series <- file.suffix %in% SERIES_SUFFIX

  # data tables (names depend on method, thus a separate call is needed)
  iodatafile <- sub("\\.dta", "", datafile)

  method <- ""
  if(!is.null(spc$seats)) {
    method <- "seats"
  } else if(!is.null(spc$x11)) {
    method <- "x11"
  }

  input_names <- names(x)

  # TODO: Could move this into an independent functions with paths as input
  read_per_series_input <- function(i) {
    filepath <- iodatafile[i]
    filename <- paste0("data", i)

    out_series <- list()

    if(file.exists(file.path(wdir, paste0(filename, ".html")))) {
      series_files <- is.series & grepl(paste0("^", filename, "\\..+$"), flist)
      out_series$series <- lapply(file.path(wdir, flist[series_files]), seasonal:::read_series, frequency = freq)
      names(out_series$series) <- file.suffix[series_files]
    }

    if(method != "") {
      # TODO: Why read data twice tho? I think this can be skipped/made optional for poweruse
      out_series$data <- seasonal:::read_data(method = method, file = filepath, freq)
    }

    out_series$err <- seasonal:::read_err(filepath)

    seasonal:::drop_x13messages(z$err, sprintf("%s: Series has been generated, but X-13 returned an error\n\n", input_names[i]),
                     msgfun = warning)

    if (is.null(out_series$data) && any(c("x11", "seats") %in% names(spc))){
      seasonal:::drop_x13messages(out_series$err, msg = "X-13 has run but produced no data\n\n", ontype = "all", msgfun = warning)
    }

    # read .udg file
    out_series$udg <- seasonal:::read_udg(filepath)

    # read .log file
    if (getOption("htmlmode") != 1){
      out_series$log <- readLines(paste0(filepath, ".log"), readLines, encoding = "UTF-8")
    }

    # read .est file
    out_series$est <- seasonal:::read_est(filepath)

    # read .mdl file
    mdl <- readLines(paste0(filepath, ".mdl"))

    # Workaround: in the .mdl output, full regime changes are returned weiredly.
    # E.g.
    # variables=(
    #  td/ for before 1955.Jan/
    # )
    is.r.change <- grepl("//?[ A-Za-z]", mdl)
    rch0 <- mdl[is.r.change]
    rch <- gsub("//[ A-Za-z].+ ", "//", rch0)
    rch <- gsub("/[ A-Za-z].+ ", "/", rch0)
    mdl[is.r.change] <- rch
    out_series$model <- try(seasonal:::parse_spc(mdl), silent = TRUE)

    # fails for very complicated models, but is needed only for static()
    if (inherits(out_series$model, "try-error")){
      out_series$model <- NULL
    }

    # read .out file (will be returned only if out = TRUE)
    outtxt <-  readLines(outfile[i], encoding = "UTF-8")

    # always keep fivebestmdl
    out_series$fivebestmdl <- seasonal:::detect_fivebestmdl(outtxt)

    ### Checks

    # check if model choosen by seats is identical
    if (any(grepl("Model used for SEATS decomposition is different", out_series$err))){
      message(paste("Model used in SEATS is different:", out_series$udg['seatsmdl']))
    }

    # check if freq detection in read_series has worked
    if (!is.null(out_series$data)){
      ff <- frequency(out_series$data)
    } else if (length(out_series$series) > 0){
      ff <- unique(sapply(out_series$series[sapply(out_series$series, is.ts)], frequency))
    } else {
      ff <- NULL
    }

    if (!is.null(ff)){
      if (!as.numeric(out_series$udg['freq']) %in% ff){
        msg <- paste0("Frequency of imported data (", ff, ") is not equal to frequency of detected by X-13 (", as.numeric(out_series$udg['freq']), "). X-13 retured the addital messages: \n\n")
        seasonal:::drop_x13messages(out_series$err, msg = msg, ontype = "all")
      }
    }

    ### final additions to output
    if (!is.null(attr(x.na[[i]], "na.action"))){
      out_series$na.action <- attr(x.na[[i]], "na.action")
    }

    if (out){
      out_series$out <- outtxt
    }

    out_series
  }

  per_series_output <- lapply(seq_along(datafile), read_per_series_input)

  names(per_series_output) <- names(x)

  z$results <- per_series_output

  # Todo: store this as per series?
  z$x <- x
  z$spc <- spc
  z$wdir <- wdir

  # clean up
  if (!out){
    unlink(wdir, recursive = TRUE)
  }

  class(z) <- "multiseas"
  z
}

# TODO: multirun is really not necessary. just add a "metafile = NULL" param to run_x13
multirun_x13 <- function(file, out){
  # run X-13ARIMA-SEATS platform dependently
  #
  # file  character, full filename w/o suffix
  #
  # run X-13 as a side effect
  #
  # required by seas

  env.path <- Sys.getenv("X13_PATH")
  # -n no tables
  # -s store additional output (.udg file)
  flags <- if (out) {"-s"} else {"-n -s"}
  if (.Platform$OS.type == "windows"){
    if (getOption("htmlmode") == 1){
      x13.bin <- paste0("\"", file.path(env.path, "x13ashtml.exe"), "\"")
    } else {
      x13.bin <- paste0("\"", file.path(env.path, "x13as.exe"), "\"")
    }
    # change wd on win as X-13 writes `fort.6` to it
    owd <- getwd()
    on.exit(setwd(owd))
    setwd(dirname(file))

    msg <- shell(paste(x13.bin, file, flags, "-d metafile"), intern = TRUE)
  } else {
    if (getOption("htmlmode") == 1){
      # ignore case on unix to avoid problems with different binary names
      fl <- list.files(env.path)
      x13.bin <- file.path(env.path, fl[grepl("^x13ashtml$", fl, ignore.case = TRUE)])
    } else {
      x13.bin <- file.path(env.path, "x13as")
    }
    # CHS: either this or full path to metafile
    owd <- getwd()
    on.exit(setwd(owd))
    setwd(dirname(file))

    msg <- system(paste(x13.bin, file, flags, "-d metafile"), intern = TRUE, ignore.stderr = TRUE)

  }
  # error message if output contains the word ERROR
  if (inherits(msg, "character")){
    if (any(grepl("ERROR", msg))){
      if (file.exists(paste0(file, ".err"))){
        if (any(grepl("iofile_err", msg))){
          # read from separate file
          err <- read_err(file)
          drop_x13messages(err)
        } else {
          # fall back: parse message
          err <- detect_error(msg, htmlmode = 0)
          drop_x13messages(err)
        }
      }
    }
  }

  # error message on non-zero failing
  if (!is.null(attr(msg, "status"))){
    if (attr(msg, "status") > 0){
      msg <- system(paste(x13.bin, file, flags), intern = TRUE, ignore.stderr = FALSE)
      stop("X-13 has returned a non-zero exist status, which means that the current spec file cannot be processed for an unknown reason.", call. = FALSE)
    }
  }

}
