## Purpose: validation script for PsN lasso tool
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
read_lasso_info <- function (filename) {
  dat <- readLines(filename)
  csv <- c()
  for (i in seq(dat)) {
    tmp <- strsplit(dat[i], " ")[[1]]
    tmp <- tmp[tmp != ""]
    csv <- rbind(csv, as.num(tmp[-3]))
  }
  colnames(csv) <- c("t", "group", "coeff", "seed")
  csv <- data.frame(csv[-1,])
  # read the ofv log file
#   for (i in seq(dat)) {
#     tmp <- strsplit(dat[i], ":")[[1]]
#     if ((length(tmp)==2)&&(length(grep("OFV in validation group", tmp))>0)) {
#       csv <- rbind(csv, tmp)
#     }
#   }
#   csv_ofv <- data.frame(csv_ofv, row.names=paste("row", 1:length(csv_ofv[,1])))
  csv
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
tool <- "lasso"
psn_folder <- paste(tool, "_test_dir", sep="")
## parsed results files
csv_t <- paste(psn_folder,"/coeff_table.log",sep="")
csv_r <- "coeff_table_ref.log"

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
  info_t <- read_lasso_info (csv_t)
  info_r <- read_lasso_info (csv_r)
  ## Implement tests
  tests <- data.frame(matrix (c("test_coeff",  "coeff" ),
                                ncol=2, byrow=TRUE))
  colnames(tests) <- c("t_name", "xp_name")
  for (i in seq(tests$t_name)) {
    test_name <- as.character(tests$t_name[i])
    xp_name <- as.character(tests$xp_name[i])
    if (!is.null(args[[test_name]])) {
      if ((args[[test_name]] == "TRUE")|(args[[test_name]] == "1")) {
        test_res <-
          comp(info_t[[xp_name]], info_r[[xp_name]], tol=1)
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

