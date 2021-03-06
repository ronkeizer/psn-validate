## Purpose: validation script for PsN cdd tool
## RJK 2013

## General functions
as.num <- function (x) {
  filt <- grep("[a-df-zA-DF-Z\"]", x)
  y <- x
  y[filt] <- NA
  as.numeric(y)
}
comp <- function (test, ref, tol, abs=FALSE) {
  dif = test - ref
  res <- FALSE
  if (!abs) {
      dif[ref!=0] <- dif[ref!=0] / ref[ref!=0]
  }
  return(max(dif) < tol)
}
decommafy <- function (str) {
  res <- strsplit (str, ",")[[1]]
  for (i in seq(res)) {
      res[i] <- as.character(res[i])
  }
  return(res)
}
read_cdd_info <- function (filename) {
  dat <- readLines(filename)[-c(1,2)]
  dat <- gsub("\"","",dat)
  bias_line <- grep("bias,", dat)[1]
  dat[bias_line] <- gsub("\"","", dat[bias_line])
  vals <- as.num(strsplit(dat[bias_line],",")[[1]][-1])
  info <- list()
  info$ofv  <- vals[1]
  info$pars <- vals[-1]
  info
}

## Specify all tests
if (length(args$test_all) > 0) {
    if (args$test_all == "1") {
        args$test_ci <- "1"
        args$test_ofv <- "1"
   }
}
tests <- c("ci", "ofv")
if (length(args$test_all) > 0) {
    if (args$test_all == "1") {
      for (i in seq(tests)) {
        args[[paste("test_",tests[i], sep="")]] <- 1
      }
  }
}
# if test not specified, it is not run
for (i in seq(tests)) {
  if (! paste("test_",tests[i], sep="") %in% names(args)) {
      args[[paste("test_",tests[i], sep="")]] <- 0
  }
}

## Start testing
all_res <- TRUE
tool <- "cdd"
psn_folder <- paste(tool, "_test_dir", sep="")
## parsed results files
csv_t <- paste(psn_folder,"/",tool,"_results.csv",sep="")
csv_r <- paste(tool,"_results_ref.csv",sep="")

## First check if all files have been created / copied
files_exist <- TRUE
if (!file.exists(csv_t)) {
  cat (paste(toupper(tool),"RESULTS FILE NOT FOUND!"))
  files_exist <- FALSE
}
if (!file.exists(csv_r)) {
  cat (paste(toupper(tool),"REFERENCE RESULTS FILE NOT FOUND!"))
  files_exist <- FALSE
}

if (files_exist) {
  info_t <- read_cdd_info (csv_t)
  info_r <- read_cdd_info (csv_r)
  ## Implement tests
  ## Implement tests
  tests <- data.frame(matrix (c("test_jackknife_par_bias",  "pars",
                                "test_jackknife_ofv_bias",  "ofv" ),
                                ncol=2, byrow=TRUE))
  colnames(tests) <- c("t_name", "xp_name")
  for (i in seq(tests$t_name)) {
    test_name <- as.character(tests$t_name[i])
    xp_name <- as.character(tests$xp_name[i])
    if (!is.null(args[[test_name]])) {
      if ((args[[test_name]] == "TRUE")|(args[[test_name]] == "1")) {
        tol <- args$tolerance
        if (xp_name == "ofv") {
          tol <- args$ofv_abs_tol
        }
        test_res <- comp(info_t[[xp_name]], info_r[[xp_name]], tol=tol)
        if (test_res) {
          cat (paste("TEST SUCCESS: ",test_name," COMPARISON\n", sep=""))
        } else {
          all_res <- FALSE
          cat (paste("TEST FAIL: ",test_name," COMPARISON! \n",sep=""))
          cat (paste("   Test data:      ", info_t[[xp_name]], "\n",sep=""))
          cat (paste("   Reference data: ", info_r[[xp_name]], "\n",sep=""))
        }
      }
    }
  }
}

## Write overall test succes
if (all_res) {
    cat ("OVERALL TEST RESULT: SUCCESS\n")
} else {
    cat ("OVERALL TEST RESULT: FAILED!\n")
}

