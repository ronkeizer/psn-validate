
## Purpose: validation script for PsN llp tool
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
read_llp_info <- function (filename) {
  dat <- read.csv(filename, skip=1)
  colnames(dat)[grep("lower",colnames(dat))] <- "lower"
  colnames(dat)[grep("upper",colnames(dat))] <- "upper"
  dat
}

## Specify all tests
if (length(args$test_all) > 0) {
    if (args$test_all == "1") {
        args$test_ci <- "1"
        args$test_ofv <- "1"
   }
}

## Start testing
all_res <- TRUE
tool <- "llp"
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
  info_t <- read_llp_info (csv_t)
  info_r <- read_llp_info (csv_r)
  ## Implement tests
  tests <- data.frame(matrix (c("test_ci",  "lower",
                                "test_ci",  "upper",
                                "test_ofv",  "maximum.likelihood.estimate"
                                ),
                                ncol=2, byrow=TRUE))
  colnames(tests) <- c("t_name", "xp_name")
  for (i in seq(tests$t_name)) {
    test_name <- as.character(tests$t_name[i])
    xp_name <- as.character(tests$xp_name[i])
    if (!is.null(args[[test_name]])) {
      if ((args[[test_name]] == "TRUE")|(args[[test_name]] == "1")) {
        test_res <- comp(info_t[[xp_name]], info_r[[xp_name]], tol=args$tolerance)
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

