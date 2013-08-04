## Purpose: validation script for PsN scm tool
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

## Specify all tests
tests <- c("final_model_same", "ofv_final_model")
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
tool <- "scm"
psn_folder <- paste(tool, "_test_dir", sep="")
raw <- dir(psn_folder, "raw_results_(.)*.csv")[1]
## Raw results csv files
csv_test <- paste(psn_folder,"/",raw,sep="")
csv_ref <- paste("raw_results_ref.csv",sep="")
## parsed results files
log_t <- paste(psn_folder,"/short_scmlog.txt",sep="")
log_r <- paste("short_scmlog_ref.txt",sep="")

## First check if all files have been created / copied
files_exist <- TRUE
if (!file.exists(log_t)) {
  cat (paste(toupper(tool),"LOG FILE FOR TEST NOT FOUND! (short_scmlog.txt)"))
  files_exist <- FALSE
}
if (!file.exists(log_r)) {
  cat (paste(toupper(tool),"LOG FILE FOR REFERENCE NOT FOUND! (short_scmlog.txt)"))
  files_exist <- FALSE
}

if (files_exist) {
  info_test <- read.csv(csv_test)
  info_ref <- read.csv(csv_ref)
  info_t <- readLines(log_t)
  info_r <- readLines(log_r)

  ## Implement tests
  ## Test if the final scm model is the same as reference
  if(as.num(args$test_final_model_same)) {
    test_res <- TRUE
    cov_rels_t <- unlist(strsplit(info_t[(tail(grep("Relations included", info_t), 1)+1):(length(info_t)-2)]," "))
    cov_rels_r <- unlist(strsplit(info_r[(tail(grep("Relations included", info_r), 1)+1):(length(info_r)-2)], " "))
    cov_rels_t1 <- cov_rels_t[cov_rels_t != ""]
    cov_rels_r1 <- cov_rels_r[cov_rels_r != ""]
    if (length(cov_rels_t1) != length(cov_rels_r1)) { # same number of covariates
        test_res <- FALSE
    }
    if (sum(cov_rels_t1 %in% cov_rels_r1) != length(cov_rels_r1)) { # all covariates the same?
        test_res <- FALSE
    }
    if (test_res) {
      cat (paste("TEST SUCCESS: FINAL MODEL CCOMPARISON\n", sep=""))
    } else {
      all_res <- FALSE
      cat (paste("TEST FAIL: FINAL MODEL COMPARISON! \n",sep=""))
    }
  }
  ## Test if the OFV for the final model is the same as reference
  if(as.num(args$test_ofv_final_model)) {
    cov_rels_t <- strsplit(info_t[1:(head(grep("Relations included", info_t), 1)-1)]," ")
    cov_rels_r <- strsplit(info_r[1:(head(grep("Relations included", info_r), 1)-1)]," ")
    ofv_final_t <- 0
    ofv_final_r <- 0
    for (i in seq(cov_rels_r)) {
        if (!is.na(match ("YES!", cov_rels_r[[i]]))) {
            ofv_final_t <- as.num(cov_rels_t[[i]][cov_rels_t[[i]] != ""][4])
            ofv_final_r <- as.num(cov_rels_r[[i]][cov_rels_r[[i]] != ""][4])
        }
    }
    test_res <- TRUE
    if (!comp (ofv_final_t, ofv_final_r, 1, abs=TRUE)) {
      test_res <- FALSE
    }
    if (test_res) {
      cat (paste("TEST SUCCESS: FINAL MODEL OFV COMPARISON\n", sep=""))
    } else {
      all_res <- FALSE
      cat (paste("TEST FAIL: FINAL MODEL OFV COMPARISON! \n",sep=""))
    }
  }
} else {
  all_res <- FALSE
  cat (paste ("TEST FAIL:",test_name, "SOME OF THE REQUIRED FILES WERE NOT FOUND! \n"))
}

## Write overall test succes
if (all_res) {
    cat ("OVERALL TEST RESULT: SUCCESS\n")
} else {
    cat ("OVERALL TEST RESULT: FAILED!\n")
}

