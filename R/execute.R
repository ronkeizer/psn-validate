## Purpose: validation script for PsN execute tool
## RJK 2013

## General functions
as.num <- function (x) { as.numeric(as.character(x)) }
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
get_info_from_csv <- function (csv_file) {
  csv <- readLines(csv_file)
  ofv_lineno <- grep ("Objective function value", csv)
  ofv_line <- csv[ofv_lineno[1]]
  info <- list()
  info$ofv <- as.num (strsplit ( csv[ofv_lineno[1]], ": ",)[[1]][2])
  csv_dat <- strsplit(csv[(ofv_lineno+3):(length(csv)-1)], ",")
  info$thetas <- c()
  info$omegas <- c()
  info$sigmas <- c()
  for (i in seq(csv_dat)) {
    csv_line <- csv_dat[[i]]
    if (length(grep("THETA",csv_line[1])) > 0) {
      info$thetas <- c(info$thetas, as.num(csv_line[2]))
    }
  }
  for (i in seq(csv_dat)) {
    csv_line <- csv_dat[[i]]
    if (length(grep("OMEGA",csv_line[4])) > 0) {
      info$omegas <- c(info$omegas, as.num(csv_line[6]))
    }
  }
  for (i in seq(csv_dat)) {
    csv_line <- csv_dat[[i]]
    if (length(grep("SIGMA",csv_line[8])) > 0) {
      info$sigmas <- c(info$sigmas, as.num(csv_line[10]))
    }
  }
  return (info)
}

## Specify all tests
tests <- c("ofv", "parameters")
# if test not specified, it will not be run not run
if (length(args$test_all) > 0) {
    if (args$test_all == "1") {
      for (i in seq(tests)) {
        args[[paste("test_",tests[i], sep="")]] <- 1
      }
  }
}
# if test not specified, it will not be run
for (i in seq(tests)) {
  if (! paste("test_",tests[i], sep="") %in% names(args)) {
      args[[paste("test_",tests[i], sep="")]] <- 0
  }
}

## Start testing
all_res <- TRUE
csv_test <- paste(args$nm_output, ".csv", sep="")
csv_ref <- paste(args$reference, "_ref.csv", sep="")
files_exist <- TRUE
if (!file.exists(csv_test)) {
  cat ("NONMEM OUTPUT FILE NOT FOUND!")
  files_exist <- FALSE
}
if (!file.exists(csv_ref)) {
  cat ("REFERENCE FILE NOT FOUND!")
  files_exist <- FALSE
}

if (files_exist) {
  info_test <- get_info_from_csv(csv_test)
  info_ref <- get_info_from_csv(csv_ref)

  if (args$tolerance == "") {
    cat ("WARNING: TOLERANCE NOT SPECIFIED! ASSUMING TOLERANCE = 0.01 FOR THIS STEP!")
    tol <- 0.01
  } else {
    tol <- as.num(args$tolerance)
  }
  if (args$ofv_abs_tol == "") {
    cat ("WARNING: ABSOLUTE TOLERANCE NOT SPECIFIED! ASSUMING ABSOLUTE TOLERANCE FOR OFV COMPARISON = 1 FOR THIS STEP!")
    tol_abs <- 1
  } else {
    tol_abs <- as.num(args$ofv_abs_tol)
    abs <- TRUE
  }

  ## Test OFV difference
  if ((args$test_ofv == "TRUE")|(args$test_ofv == "1")) {
    abs <- TRUE
    test_res_ofv <- comp(info_test$ofv, info_ref$ofv, tol_abs, abs)
    if (!test_res_ofv) {
      all_res <- FALSE
      cat ("OFV COMPARISON FAILED!\n")
    }
  }

  ## Test parameter differences
  if ((args$test_parameters == "TRUE")|(args$test_parameters == "1")) {
    test_res_thetas <- comp(info_test$thetas, info_ref$thetas, tol)
    test_res_omegas <- comp(info_test$omegas, info_ref$omegas, tol)
    test_res_sigmas <- comp(info_test$sigmas, info_ref$sigmas, tol)
    if (!test_res_thetas) {
      all_res <- FALSE
      cat ("TEST FAIL: THETA COMPARISON!\n")
    } else {
        cat ("TEST SUCCES: THETA COMPARISON!\n")
    }
    if (!test_res_omegas) {
      all_res <- FALSE
      cat ("TEST FAIL: OMEGA COMPARISON!\n")
    } else {
        cat ("TEST SUCCES: OMEGA COMPARISON!\n")
    }
    if (!test_res_sigmas) {
      all_res <- FALSE
      cat ("TEST FAIL: SIGMA COMPARISON!\n")
    } else {
        cat ("TEST SUCCES: SIGMA COMPARISON!\n")
    }
  }
} else {
  all_res <- FALSE
}
## Write overall test succes
if (all_res) {
  cat ("Overall test result: SUCCESS\n")
} else {
  cat ("Overall test result: FAILED!\n")
}
