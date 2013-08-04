## Purpose: validation script for PsN bootstrap tool
## RJK 2013

## General functions
as.num <- function (x) {
  filt <- grep("[a-df-zA-DF-Z]", x)
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
  if (max(dif) < tol) {
    res <- TRUE
  }
  return(res)
}
decommafy <- function (str) {
  res <- strsplit (str, ",")[[1]]
  for (i in seq(res)) {
      res[i] <- as.character(res[i])
  }
  return(res)
}
read_psn_csv <- function (csv_file) {
  csv <- readLines(csv_file)
  keys <- c()
  for (i in seq(csv)) {
    csv[i] <- gsub ('"', '', csv[i])
    csv[i] <- gsub ("\\((\\d*?),(\\d*?)\\)", "\\(\\1;\\2\\)", csv[i], perl=TRUE)
    if(length(grep(",", csv[i])) == 0) {
        keys <- c(keys, csv[i])
    }
  }
  info <- list()
  for (i in seq(keys)) {
    line_no <- match(keys[i],csv)+1
    info[[keys[i]]] <- cbind(as.num(decommafy(csv[line_no+1])[-1]))
    rownames(info[[keys[i]]]) <- decommafy(csv[line_no])[-1]
  }
  return(info)
}

## Specify all tests
tests <- c("ofv_mean", "ofv_sd", "parameter_mean", "parameter_sd", "bootstrap_ofv", "bootstrap_theta")
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
tool <- "bootstrap"
psn_folder <- paste(tool, "_test_dir", sep="")
raw_csv <- dir(psn_folder, "raw_results_(.)*.csv")[1]
## Raw results csv files
csv_test <- paste(psn_folder,"/",raw_csv,sep="")
csv_ref <- paste("raw_results_ref.csv",sep="")
## parsed results files
csv_t <- paste(psn_folder,"/",tool,"_results.csv",sep="")
csv_r <- paste(tool,"_results_ref.csv",sep="")

## First check if all files have been created / copied
files_exist <- TRUE
if (!file.exists(csv_test)) {
  cat ("BOOTSTRAP RAW RESULTS FILE NOT FOUND!")
  files_exist <- FALSE
}
if (!file.exists(csv_ref)) {
  cat ("BOOTSTRAP REFERENCE RAW RESULTS FILE NOT FOUND!")
  files_exist <- FALSE
}
if (!file.exists(csv_t)) {
  cat ("BOOTSTRAP RESULTS FILE NOT FOUND!")
  files_exist <- FALSE
}
if (!file.exists(csv_r)) {
  cat ("BOOTSTRAP REFERENCE RESULTS FILE NOT FOUND!")
  files_exist <- FALSE
}

if (files_exist) {
  info_test <- read.csv(csv_test)
  info_ref <- read.csv(csv_ref)
  info_t <- read_psn_csv(csv_t)
  info_r <- read_psn_csv(csv_r)

  ## OFV tests
  tol <- as.num(args$tolerance)
  abs <- FALSE
  tol_abs <- tol
  if (!is.null(args$ofv_abs_tol)) {
      tol_abs <- as.num(args$ofv_abs_tol)
      abs <- TRUE
  }
  ## Test OFV difference (pairwise between all bootstrap samples for test and reference)
  if ((args$test_bootstrap_ofv == "TRUE")|(args$test_bootstrap_ofv == "1")) {
      test_res_ofv <- comp(
        info_test$ofv
        ,
        info_ref$ofv
        , tol_abs, abs)
      if (!test_res_ofv) {
          all_res <- FALSE
          cat ("TEST FAIL: PAIRWISE OFV COMPARISON!\n")
      } else {
          cat ("TEST SUCCESS: PAIRWISE OFV COMPARISON\n")
      }
  }
  ## Test parameter differences (pairwise between all bootstrap samples for test and reference)
  if ((args$test_bootstrap_theta == "TRUE")|(args$test_bootstrap_theta == "1")) {
    nams <- names(info_ref)
    param_id <- c( (grep("ofv", nams)[1]+1) : (grep("OMEGA", nams)[1]-1) )
    #param_id <- c( (grep("ofv", nams)[1]+1) : (grep("seTHETA", nams)[1]-1) )
    res <- c()
    for (i in seq(info_ref[,1])) {
      res <- c(res, comp(info_test[i, param_id], info_ref[i, param_id], tol=tol))
    }
    if (sum(res) == length(info_ref[,1])) {
      cat ("TEST SUCCESS: PAIRWISE PARAMETER COMPARISON")
    } else {
      all_res <- FALSE
      cat ("TEST FAIL: PAIRWISE PARAMETER COMPARISON! ")
      cat (paste("(Fail in bootstrap sample: ", (1:length(info_ref[,1]))[!res], ")", "\n",sep=""))
    }
  }

  ## Test mean of OFV difference between test and ref
  if ((args$test_ofv_mean == "TRUE")|(args$test_ofv_mean == "1")) {
      test_res_ofv_mean <- comp(info_t$means[1] - info_r$means[1], tol_abs, abs)
      if (!test_res_ofv_mean) {
          all_res <- FALSE
            cat ("TEST FAIL: OFV MEAN COMPARISON!\n")
      } else {
          cat ("TEST SUCCESS: OFV MEAN COMPARISON\n")
      }
  }
  ## Test sd of OFV difference between test and ref
  if ((args$test_ofv_sd == "TRUE")|(args$test_ofv_sd == "1")) {
      test_res_ofv_sd <- comp(info_t$standard.errors[1], info_r$standard.errors[1], tol_abs, abs)
      if (!test_res_ofv_sd) {
          all_res <- FALSE
          cat ("TEST FAIL: OFV SD COMPARISON!\n")
      } else {
          cat ("TEST SUCCESS: OFV SD COMPARISON\n")
      }
  }
  ## Test parameter differences
  if ((args$test_parameter_mean == "TRUE")|(args$test_parameter_mean == "1")) {
      filt <- !is.na(info_r$means)&(info_r$means!=0)  ## test only on non-zero and non-NA values!!
      test_res_par_mean <- comp(info_t$means[filt], info_r$means[filt], tol=tol)
      if (!test_res_par_mean) {
         all_res <- FALSE
         cat ("TEST FAIL: PARAMETER MEAN COMPARISON!\n")
      } else {
         cat ("TEST SUCCESS: PARAMETER MEAN COMPARISON\n")
      }
  }
  if ((args$test_parameter_sd == "TRUE")|(args$test_parameter_sd == "1")) {
    filt <- !is.na(info_r$standard.errors)&(info_r$standard.errors!=0)  ## test only on non-zero and non-NA values!!
    filt[grep("se",row.names(info_r$standard.errors))[1]:length(filt),1] <- FALSE # only on parameters
    test_res_par_sd <- comp(info_t$standard.errors[filt][-1], info_r$standard.errors[filt][-1], tol=tol)
    if (!test_res_par_sd) {
      all_res <- FALSE
      cat ("TEST FAIL: PARAMETER SD COMPARISON!\n")
    } else {
      cat ("TEST SUCCESS: PARAMETER SD COMPARISON\n")
    }
  }
} else {
    all_res <- FALSE
}

## Write overall test succes
if (all_res) {
    cat ("OVERALL TEST RESULT: SUCCESS\n")
} else {
    cat ("OVERALL TEST RESULT: FAILED!\n")
}
