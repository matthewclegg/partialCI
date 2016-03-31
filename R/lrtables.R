# lrtables.R -- functions for building up tables of likelihood ratios
# Copyright (C) 2016 Matthew Clegg

#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

# 1.  Start R
# 2.  Load the partialCI package
# 3.  Create the list of samples and likelihood tables by entering the 
#     following command:
#       partialCI:::build_lrtables()
#     On my Macbook Pro, this takes about a week to run.
# 4.  Reload the set of likelihood tables:
#       partialCI:::load_lrtables()
# 5.  Regenerate the lrdata.R file
#       partialCI:::dump_lrdata()
# 6.  Locate the new lrdata.R file on disk and move it to the partialCI/R
#     source code directory.
# 7.  Rebuild the partialCI package
#       R CMD build partialCI
# 8.  Reinstall the partialCI package
#       R CMD install partialCI.tar.gz
# 9.  Delete the temporary directories samples/ and tables/

# if(getRversion() >= "2.15.1")  utils::globalVariables(c("PAR.SAMPLES",
#  "PAR.JOINT.CRITICAL.VALUES.DT",
#  "PAR.JOINT.CRITICAL.VALUES.KPSS.DT",
#  "PAR.SAMPLES.DT"))

# if (!exists("PAR.SAMPLES")) PAR.SAMPLES <- NULL
# if (!exists("PAR.SAMPLES.DT")) PAR.SAMPLES.DT <- NULL
# if (!exists("PAR.POWER.SAMPLES")) PAR.POWER.SAMPLES <- NULL
# if (!exists("PAR.POWER.PVMR.SAMPLES")) PAR.POWER.PVMR.SAMPLES <- NULL
# if (!exists("PAR.JOINT.CRITICAL.VALUES.DT")) PAR.JOINT.CRITICAL.VALUES.DT <- NULL
# if (!exists("PAR.JOINT.CRITICAL.VALUES.KPSS.DT")) PAR.JOINT.CRITICAL.VALUES.KPSS.DT <- NULL

LR_TABLES_LIST <- c("PCI.RWNULL.JP.LRQT", 
                    "PCI.MRNULL.JP.LRQT",
                    "PCI.RWNULL.ROB.JP.LRQT", 
                    "PCI.MRNULL.ROB.JP.LRQT",
                    "PCI.RWNULL.TWOSTEP.LRQT", 
                    "PCI.MRNULL.TWOSTEP.LRQT",
                    "PCI.RWNULL.ROB.TWOSTEP.LRQT", 
                    "PCI.MRNULL.ROB.TWOSTEP.LRQT",
                    "PCI.JOINT.CRITICAL.VALUES")

for (t in LR_TABLES_LIST) {
  if (!exists(t)) assign(t, NULL)
}

build_lrtables <- function (dir="tables", debug=FALSE, nrep=10000, rebuild_samples=TRUE) { 
  # Rebuilds all of the tables that are contained in this file.
  # This function takes several days to a week to run to completion.
  # The tables are written to files in the specified directory,
  # where they can then be loaded back into R and used to update
  # this file.
  
  if (debug) {
    nr <- 1
  } else {
    nr <- nrep
  }
  
  if (rebuild_samples) {
    cat("Rebuilding PCI likelihood ratio samples ...\n")
    pci.generate.likelihood_ratio.samples(nrep=nrep)
  }
  pci.load.likelihood_ratio.samples()
  SAMPLES <- PCI.SAMPLES
  
  dir.create(dir, recursive=TRUE, showWarnings=FALSE)
 
  pci.rwnull.jp.lrqt <- quantile.table.from.samples(
    "rw_lrt", 
    SAMPLES[SAMPLES$sigma_M==0.0 & SAMPLES$robust == FALSE & SAMPLES$pci_opt == "jp",])
  pci.rwnull.twostep.lrqt <- quantile.table.from.samples(
     "rw_lrt", 
      SAMPLES[SAMPLES$sigma_M==0.0 & SAMPLES$robust == FALSE & SAMPLES$pci_opt == "twostep",])
  pci.rwnull.rob.jp.lrqt <- quantile.table.from.samples(
    "rw_lrt", 
    SAMPLES[SAMPLES$sigma_M==0.0 & SAMPLES$robust == TRUE & SAMPLES$pci_opt == "jp",])
  pci.rwnull.rob.twostep.lrqt <- quantile.table.from.samples(
    "rw_lrt", 
    SAMPLES[SAMPLES$sigma_M==0.0 & SAMPLES$robust == TRUE & SAMPLES$pci_opt == "twostep",])
  
  pci.mrnull.jp.lrqt <- quantile.table.from.samples(
    "mr_lrt", 
    SAMPLES[SAMPLES$sigma_R==0.0 & SAMPLES$robust == FALSE & SAMPLES$pci_opt == "jp",])
  pci.mrnull.twostep.lrqt <- quantile.table.from.samples(
    "mr_lrt", 
    SAMPLES[SAMPLES$sigma_R==0.0 & SAMPLES$robust == FALSE & SAMPLES$pci_opt == "twostep",])
  pci.mrnull.rob.jp.lrqt <- quantile.table.from.samples(
    "mr_lrt", 
    SAMPLES[SAMPLES$sigma_R==0.0 & SAMPLES$robust == TRUE & SAMPLES$pci_opt == "jp",])
  pci.mrnull.rob.twostep.lrqt <- quantile.table.from.samples(
    "mr_lrt", 
    SAMPLES[SAMPLES$sigma_R==0.0 & SAMPLES$robust == TRUE & SAMPLES$pci_opt == "twostep",])  
  
  dput (pci.rwnull.jp.lrqt, sprintf("%s/%s", dir, "PCI.RWNULL.JP.LRQT"))  
  dput (pci.rwnull.twostep.lrqt, sprintf("%s/%s", dir, "PCI.RWNULL.TWOSTEP.LRQT"))  
  dput (pci.rwnull.rob.jp.lrqt, sprintf("%s/%s", dir, "PCI.RWNULL.ROB.JP.LRQT"))  
  dput (pci.rwnull.rob.twostep.lrqt, sprintf("%s/%s", dir, "PCI.RWNULL.ROB.TWOSTEP.LRQT"))  
  
  dput (pci.mrnull.jp.lrqt, sprintf("%s/%s", dir, "PCI.MRNULL.JP.LRQT"))  
  dput (pci.mrnull.twostep.lrqt, sprintf("%s/%s", dir, "PCI.MRNULL.TWOSTEP.LRQT"))  
  dput (pci.mrnull.rob.jp.lrqt, sprintf("%s/%s", dir, "PCI.MRNULL.ROB.JP.LRQT"))  
  dput (pci.mrnull.rob.twostep.lrqt, sprintf("%s/%s", dir, "PCI.MRNULL.ROB.TWOSTEP.LRQT"))  
    
  pci.joint.critical.values <- pci.findall.joint.critical.values()
  dput (pci.joint.critical.values, sprintf("%s/%s", dir, "PCI.JOINT.CRITICAL.VALUES"))
  
  printf ("%s done\n", Sys.time())  
}

load_table <- function (..., dir="tables") {
  # Loads a table and stores it in a global variable
  for (table_name in list(...)) {
    printf("Loading %s\n", table_name)
    tab <- dget(sprintf("%s/%s", dir, table_name))
    if (exists(table_name, envir=asNamespace("partialCI"))) {
      unlockBinding(table_name, asNamespace("partialCI"))
    }
    assign(table_name, tab, envir = asNamespace("partialCI"))
  }
}

load_lrtables <- function () {  
  sapply(LR_TABLES_LIST, function (t) load_table(t))
  0
}

dump_lrdata <- function(filename="lrdata.R") {
  
  dump(LR_TABLES_LIST, filename)  
  cat("\n\n", file=filename, append=TRUE)
}

