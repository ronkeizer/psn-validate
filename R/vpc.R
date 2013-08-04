## Purpose: validation script for PsN vpc tool
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
  res <- max(dif) < tol
  return(res)
}
decommafy <- function (str) {
  res <- strsplit (str, ",")[[1]]
  for (i in seq(res)) {
      res[i] <- as.character(res[i])
  }
  return(res)
}
read_vpc_csv <- function (csv_file) {
  csv <- readLines(csv_file)
  vpc_no <- c((grep("VPC results",csv)+4) : (grep("Diagnostics VPC", csv)[1]-2))
  npc_no <- grep("NPC results",csv)
  vpc_info <- csv[vpc_no]
  vpc_csv <- c()
  for (i in seq(vpc_info)) {
      vpc_csv <- rbind(vpc_csv, as.num(decommafy(vpc_info[i])))
  }
  as.num(decommafy(vpc_info[i]))
  colnam <- strsplit(csv[(grep("VPC results",csv)+3)], ",")
  colnames(vpc_csv) <- gsub("\\s","",colnam[[1]])
  colnames(vpc_csv) <- gsub('\\"',"", colnames(vpc_csv))
  return(data.frame(vpc_csv))
}

## Specify all tests
tests <- c("pi_media", "pi_median_ci", "pi_5", "pi_95", "obs_median", "obs_5", "obs_95")
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
tool <- "vpc"
psn_folder <- paste(tool, "_test_dir", sep="")
raw <- dir(psn_folder, "raw_results_(.)*.csv")[1]
## Raw results csv files
csv_test <- paste(psn_folder,"/",raw,sep="")
csv_ref <- paste("raw_results_ref.csv",sep="")
## parsed results files
csv_t <- paste(psn_folder,"/",tool,"_results.csv",sep="")
csv_r <- paste(tool,"_results_ref.csv",sep="")

## First check if all files have been created / copied
files_exist <- TRUE
if (!file.exists(csv_test)) {
  cat (paste(toupper(tool),"RAW RESULTS FILE NOT FOUND!"))
  files_exist <- FALSE
}
if (!file.exists(csv_ref)) {
  cat (paste(toupper(tool),"REFERENCE RAW RESULTS FILE NOT FOUND!"))
  files_exist <- FALSE
}
if (!file.exists(csv_t)) {
  cat (paste(toupper(tool),"RESULTS FILE NOT FOUND!"))
  files_exist <- FALSE
}
if (!file.exists(csv_r)) {
  cat (paste(toupper(tool),"REFERENCE RESULTS FILE NOT FOUND!"))
  files_exist <- FALSE
}

if (files_exist) {
  info_test <- read.csv(csv_test)
  info_ref <- read.csv(csv_ref)
  info_t <- read_vpc_csv(csv_t)
  info_r <- read_vpc_csv(csv_r)

  ## Implement tests
  tests <- data.frame(matrix (c("test_pi_median",     "X50.sim",
                                "test_pi_median_ci",  "X95.CIfor50.from",
                                "test_pi_median_ci",  "X95.CIfor50.to",
                                "test_obs_median",    "X50.real",
                                "test_obs_5",         "X5.real",
                                "test_obs_95",        "X95.real",
                                "test_pi_5",          "X5.sim",
                                "test_pi_95",         "X95.sim"),
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
