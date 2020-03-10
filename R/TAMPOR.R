#' TAMPOR
#'
#' performs TAMPOR normalization.
#'
#' @param dat - input data
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references none
#'
#' @keywords none
#'
#' @examples
#' TAMPOR(dat, traits)
TAMPOR <- function(dat,
                   traits,
                   noGIS = FALSE,
                   useAllNonGIS = FALSE,
                   batchPrefixInSampleNames = FALSE,
                   GISchannels = "GIS",
                   iterations = 250,
                   samplesToIgnore = FALSE,
                   meanOrMedian = "median",
                   removeGISafter = FALSE,
                   minimumBatchSize = 5,
                   parallelThreads = 2,
                   outputSuffix = "TAMPOR",
                   path = getwd()) {
  # Input data
  cleanDat <- dat
  outputfigs <- outputtabs <- path
  # Imports.
  suppressPackageStartupMessages({
    require(doParallel, quietly = TRUE)
    require(vsn, quietly = TRUE)
    require(limma, quietly = TRUE)
  })
  # Parallel processing.
  clusterLocal <- makeCluster(c(rep("localhost", parallelThreads)),
    type = "SOCK"
  )
  registerDoParallel(clusterLocal)
  # Standardize "Batch" column in traits
  if ("batch" %in% colnames(traits) | "Batch" %in% colnames(traits) | "BATCH" %in% colnames(traits)) {
    if ("batch" %in% colnames(traits)) colnames(traits)[which(colnames(traits) == "batch")] <- "Batch"
    if ("BATCH" %in% colnames(traits)) colnames(traits)[which(colnames(traits) == "BATCH")] <- "Batch"
  } else {
    cat("WARNING: no 'Batch' column found in traits file. Failing over to batchPrefixInSampleNames=TRUE.\n")
    batchPrefixInSampleNames <- TRUE
  }

  # Check that all sample names for abundance are found in traits
  if (!identical(colnames(cleanDat), na.omit(rownames(traits)[na.omit(match(colnames(cleanDat), rownames(traits)))]))) {
    missingSampleTraits <- sort(setdiff(colnames(cleanDat), na.omit(rownames(traits)[na.omit(match(colnames(cleanDat), rownames(traits)))])))
    cat(paste0("WARNING: Missing traits for the following sample(s) in provided abundance data:\n", paste(missingSampleTraits, collapse = ", "), "\n"))
    cat("         These samples have been removed.\n")
    cleanDat <- cleanDat[, -match(missingSampleTraits, colnames(cleanDat))]
  }

  # Check for and remove extra traits not in abundance data
  if (length(intersect(rownames(traits), colnames(cleanDat))) < length(rownames(traits))) {
    extraSampleTraits <- sort(setdiff(rownames(traits), na.omit(colnames(cleanDat)[na.omit(match(rownames(traits), colnames(cleanDat)))])))
    cat(paste0("WARNING: Sample(s) in traits not found in provided abundance data:\n", paste(extraSampleTraits, collapse = ", "), "\n"))
    cat("         These samples have been removed from traits.\n")
    traits <- traits[-match(extraSampleTraits, rownames(traits)), ]
  }

  # Annotate which samples go with a batch, and identifiers for each sample, which can be checked against GISchannels list
  if (batchPrefixInSampleNames) {
    if (!length(which(grepl("\\.", rownames(traits)))) == nrow(traits)) {
      stop("ERROR: sample names in both input files expected to have at least one '.' separating batch.channel! (Not found in traits sample/row names.)\n")
    } else {
      sampleIndex <- as.data.frame(do.call(rbind, strsplit(rownames(traits), "[.]")))[, 1:2]
      colnames(sampleIndex) <- c("batch", "channel")
      traits$Batch <- sampleIndex$batch
    }
  }

  # Check for possible GIS samples in other situations
  if (!length(which(grepl("\\.", rownames(traits)))) == nrow(traits)) {
    channelCandidates <- rownames(traits)
  } else {
    channelCandidates <- as.data.frame(do.call(rbind, strsplit(rownames(traits), "[.]")))[, 2]
  }

  if ((!"GIS" %in% colnames(traits) & length(na.omit(match(GISchannels, channelCandidates))) == 0 & length(na.omit(match(GISchannels, rownames(traits)))) == 0)
  | ("GIS" %in% colnames(traits) & (length(which(names(table(traits$GIS)) == "GIS")) + length(which(as.logical(names(table(traits$GIS)))))) == 0)) {
    if (!noGIS) cat("WARNING: no GIS samples specified or not found if specified. Failing over to noGIS=TRUE!\n")
    noGIS <- TRUE
    traits$GIS <- rep("GIS", nrow(traits))
    GISchannels <- "GIS"
  }
  if (length(which(as.logical(names(table(traits$GIS))))) >= 1) traits$GIS[which(as.logical(traits$GIS))] <- "GIS"
  if (!exists("sampleIndex")) {
    if (!"GIS" %in% colnames(traits)) {
      sampleIndex <- data.frame(batch = traits$Batch, channel = rownames(traits))
      cat("NOTE: Sample names from traits will be checked for exact matching to GISchannels specified.\n")
    } else {
      sampleIndex <- data.frame(batch = traits$Batch, channel = traits$GIS)
    }
  }

  # Match abundance (cleanDat) columns to batch-ordered traits
  sampleIndex <- sampleIndex[order(traits$Batch), ]
  rownames(sampleIndex) <- NULL # renumber after ordering.
  traits <- traits[order(traits$Batch), ]
  cleanDat <- cleanDat[, na.omit(match(rownames(traits), colnames(cleanDat)))]
  cleanDat.original <- cleanDat

  batchIndex <- as.character(unique(sampleIndex$batch))
  if (!"Batch" %in% traits) traits$Batch <- sampleIndex$batch


  # Ignore samples in samplesToIgnore vector (set all their values to NA)
  if (length(na.omit(match(samplesToIgnore, colnames(cleanDat)))) == length(samplesToIgnore)) {
    for (i in 1:length(samplesToIgnore)) {
      cleanDat[, samplesToIgnore[i]] <- as.vector(rep(NA, nrow(cleanDat)))
    }
  } else {
    cat("NOTE: one or more samplesToIgnore do not match sample names (colnames) in input abundance data.\n      Not ignoring any samples.\n")
  }


  # Set parameters for runs with no GIS
  if (noGIS) {
    if (useAllNonGIS) cat("WARNING: noGIS and useAllNonGIS options were both enabled. Keeping with noGIS enabled, useAllNonGIS disabled.\n")
    useAllNonGIS <- FALSE
    GISchannels <- rownames(traits) # all channels are specified in all batches for all denominators in equation 1.
    sampleIndex$channel <- rep("GIS", nrow(sampleIndex))

    traits$GIS <- sampleIndex$channel
  }


  # Check sample names, channels in all batches to confirm at least one of GISchannels exists in each batch
  minimumBatchSize <- as.integer(minimumBatchSize)
  batchwiseSampleCounts <- sapply(as.character(batchIndex), function(batch) length(which(sampleIndex$batch == batch)))
  batchwiseGIScandidateCounts <- sapply(as.character(batchIndex), function(batch) length(which(any(c(GISchannels, "GIS") %in% sampleIndex$channel[sampleIndex$batch == batch]))) + length(which(any(c(GISchannels, "GIS") %in% rownames(traits)[sampleIndex$batch == batch]))) + length(which(traits$GIS[sampleIndex$batch == batch] == "GIS")))
  names(batchwiseGIScandidateCounts) <- names(batchwiseSampleCounts) <- as.character(batchIndex)
  if (!any(batchwiseGIScandidateCounts == 0) & !any(batchwiseSampleCounts < minimumBatchSize)) {
    if (!noGIS) cat("NOTE: Successfully checked that each batch has at least one sample designated as GIS control replicate.\n")
  } else {
    badBatches <- as.character(batchIndex)[which(batchwiseGIScandidateCounts == 0)]
    badBatches <- sort(unique(c(badBatches, as.character(batchIndex)[which(batchwiseSampleCounts < minimumBatchSize)])))
    cat(paste0("WARNING: Removing ", length(badBatches), " batches with <", minimumBatchSize, " samples and/or no designated control (GIS) replicates found:\n", paste(badBatches, collapse = ", "), "\n"))
    for (batch in badBatches) {
      traits <- traits[-which(sampleIndex$batch == batch), ]
      sampleIndex <- sampleIndex[-which(sampleIndex$batch == batch), ]
      cleanDat <- cleanDat[, match(rownames(traits), colnames(cleanDat))]
      batchIndex <- as.character(unique(sampleIndex$batch))
    }
  }


  ## Specify Normalization 'channels' as batch-specific indices, for mean or median initial denominator
  # *Now allows setting GISindices even if your GIS is on different channels in different batches, essentially if channel order is not the same in each batch.
  # This enables using TAMPOR for batched LFQ or LFQ of different cohorts.
  if ("GIS" %in% colnames(traits)) GISchannels <- unique(c(GISchannels, rownames(traits)[which(traits$GIS == "GIS")])) # fixes bug where GIS channels specified in traits$GIS not recognized after splitting channel names when batchPrefixInSampleNames = TRUE
  GISindices <- list()
  i.prev <- 0
  offset <- 0
  iter <- 0
  for (i in as.character(batchIndex)) {
    GISindices[[i]] <- vector()
    iter <- iter + 1
    for (denomChannel in GISchannels) {
      GISindices[[i]] <- c(GISindices[[i]], which(sampleIndex$batch == unique(sampleIndex$batch)[iter] & (sampleIndex$channel == denomChannel | rownames(traits) == denomChannel | sampleIndex$channel == "GIS")))
    }
    if (!i.prev == 0) {
      offset <- offset + length(which(sampleIndex$batch == i.prev))
    }
    GISindices[[i]] <- unique(GISindices[[i]]) - offset
    i.prev <- i
  }
  # names(GISindices)<-batchIndex  #in case batchIndexes are numeric


  ## Finalize traits$GIS column and throw error message if one or more batch has no GIS
  if (!sum(table(unlist(lapply(GISindices, length)))) == length(batchIndex)) stop("ERROR: ONE OR MORE BATCH IS MISSING DENOMINATOR SAMPLE DESIGNATIONS.")

  globalExperimentGISsamples <- as.vector(unlist(sapply(names(GISindices), function(x) rownames(traits)[which(traits$Batch == x)[ GISindices[[x]] ]])))
  traits$GIS <- rep(NA, nrow(traits))
  traits$GIS[match(globalExperimentGISsamples, rownames(traits))] <- "GIS"

  if (!identical(which(traits$GIS == "GIS"), match(globalExperimentGISsamples, rownames(traits)))) stop("ERROR: GIS samples now specified in traits do not match those being used.")


  # Check for 0 and negative values, and tell the user to address this in their data themselves, or choose to replace with NA
  badValues <- as.vector(na.omit(cleanDat[cleanDat <= 0]))
  badRows <- rownames(cleanDat[which(apply(cleanDat, 1, function(x) min(c(x, 1), na.rm = TRUE)) <= 0), ]) # c(x,1) suppresses warnings for data with rows all NA
  if (length(badValues) > 0) {
    it <- 0
    cat("ABUNDANCES FORMAT WARNING/CHOICE:  you have ", length(badValues), " values <=0 in your input Abundances across ", length(badRows), " rows.\n\nNOTE: First bad rows (up to 10):\n")
    while (it <= min(10, length(badRows))) {
      cat(badRows[it + 1])
      cat(paste(cleanDat[badRows, ][(it * ncol(cleanDat) + 1):((it + 1) * ncol(cleanDat))], collapse = ", "), "\n")
      it <- it + 1
    }
    fix <- readline(prompt = paste0("What do you want to do? [ENTER]=Exit function to fix; OR type any value to set these to NA: "))
    if (!fix == "") stop("\n")
    # replace with <=0 values with NA; otherwise +/- Inf values propagate later!
    cleanDat <- apply(cleanDat, 2, function(x) {
      x[x <= 0] <- NA
      x
    })

    cat("__________________________________\n")
    it <- 0
    cat("First fixed rows (up to 10):\n")
    while (it <= min(10, length(badRows))) {
      cat(badRows[it + 1])
      cat(paste(cleanDat[badRows, ][(it * ncol(cleanDat) + 1):((it + 1) * ncol(cleanDat))], collapse = ", "), "\n")
      it <- it + 1
    }
    # Show user rows that had 0 or negative values fixed (to NA).
    cat(paste0(length(badValues), " bad values (<=0) in matrix replaced with NA.\nFirst fixed rows (up to 100) with at least one bad value are listed here:\n"))
    if (!is.null(badRows)) head(badRows, 100)
  }

  cleanDat.original <- cleanDat


  #####################################################################################
  iterationTrackingDF <- data.frame(Iteration = 1:iterations, FrobeniusNorm = NA, FrobenPrev = NA, FrobenDiff = NA, FrobenOverFirstFroben = NA)
  cat(paste0("Starting # of Rows in data: ", nrow(cleanDat), ".\n"))
  #####################################################################################
  for (repeats in 1:iterations) {
    if (repeats == 1 & exists("cleanDatNorm2")) rm(cleanDatNorm2)
    timer.start <- Sys.time()
    ###################################################################################################################################
    # STEP 1a. Ratio data and prepare to row-normalize
    ratioedBatches <- batchGISavgs <- list()
    ratioCleanDatUnnorm <- data.frame(row.names = rownames(cleanDat))
    withinBatchGISgeomeans <- withinBatchRowGeomeans <- data.frame(row.names = rownames(cleanDat))

    #  comb <- function(x, ...) lapply(x, function(i) do.call(list,lapply(x, function(y) x[[y]])))
    #  step1a <- foreach(batch=batchIndex, .combine='comb', .multicombine=TRUE, .init=list(list(), list(), list())) %dopar% {
    step1a <- foreach(batch = as.character(batchIndex)) %dopar% {
      tempForAvg <- matrix()
      tempForAvg <- as.data.frame(as.matrix(cleanDat[, which(sampleIndex$batch == batch)][, GISindices[[batch]] ], nrow = nrow(cleanDat), ncol = dim(cleanDat[, which(sampleIndex$batch == batch)][, GISindices[[batch]] ])[2]))
      batchGISavgs <- apply(tempForAvg, 1, function(x) eval(parse(text = paste0(meanOrMedian, "(x,na.rm=TRUE)")))) # ADDED na.rm v04 ##MEAN/MEDIAN FUNCTION CHOICE***
      ratioedBatches <- cleanDat[, which(sampleIndex$batch == batch)] / batchGISavgs

      ## Below unnormed ratio data are only assembled for graphing purposes, for comparison to step 1b and final step 2 output
      ## If batches are randomized channels distributing cases and controls evenly across all batches, useAllNonGIS==TRUE
      if (useAllNonGIS) {
        df3 <- as.data.frame(as.matrix(apply(ratioedBatches[, -GISindices[[batch]] ], 1, function(x) eval(parse(text = paste0("2^", meanOrMedian, "(log2(na.omit(x)))")))), ncol = dim(ratioedBatches)[2], nrow = dim(ratioedBatches)[1])) ## MEAN/MEDIAN FUNCTION CHOICE***
        # as.matrix(), NOT matrix()
      } else {
        ## If we cannot rely on the robust assumption of batch-to-batch biological equivalence (with randomized sample order across all avalable channels in all batches), then use robust mean of GIS samples only
        df3 <- as.data.frame(as.matrix(apply(ratioedBatches, 1, function(x) eval(parse(text = paste0("2^", meanOrMedian, "(log2(na.omit(x[GISindices[[batch]] ])))")))), ncol = dim(ratioedBatches)[2], nrow = dim(ratioedBatches)[1]))
        # as.matrix(), NOT matrix()
      } ## MEAN/MEDIAN FUNCTION CHOICE***
      return(list(batchGISavgs, ratioedBatches, df3))
    }

    # re-combine list elements from three outputs, over all batches
    batchGISavgs <- do.call(list, lapply(step1a, function(x) {
      x[[1]]
    }))
    ratioedBatches <- do.call(list, lapply(step1a, function(x) {
      x[[2]]
    }))
    withinBatchRowGeomeans <- as.data.frame(do.call(cbind, lapply(step1a, function(x) {
      x[[3]]
    }))) # was also set to "withinBatchGISgeomeans" (not used below)
    names(batchGISavgs) <- names(ratioedBatches) <- colnames(withinBatchRowGeomeans) <- batchIndex

    ratioCleanDatUnnorm <- do.call(cbind, ratioedBatches)


    ###################################################################################################################################
    ## Step 1b. Complete row-normalization. This step normalizes rows within batch by batchCorrFactors; ratioCleanDatUnnorm does not go through this step
    meanBatchGeomeans <- apply(withinBatchRowGeomeans, 1, function(x) eval(parse(text = paste0(meanOrMedian, "(na.omit(x))")))) ## MEAN/MEDIAN FUNCTION CHOICE***

    ## Rowwise (RW) relative abundances from GIS (or representative samples),
    # if we want to take the whole protein row (across batches) back to abundance after normalization step2 is complete**
    RW.relAbunFactors <- RW.GISavgs <- data.frame(row.names = rownames(cleanDat))
    RW.GISavgs <- cbind(apply(data.frame(column = as.character(batchIndex)), 1, function(x) batchGISavgs[[x]]))
    colnames(RW.GISavgs) <- as.character(batchIndex)
    RW.relAbunFactors <- apply(RW.GISavgs, 1, function(x) eval(parse(text = paste0("2^", meanOrMedian, "(log2(na.omit(x)))")))) #** relative abundance multipliers for recovery of relative abundance (all rows from input cleanDat) ##MEAN/MEDIAN FUNCTION CHOICE***
    rownames(RW.GISavgs) <- names(RW.relAbunFactors) <- rownames(cleanDat)

    ## Calculate Step 1b multipliers to complete RW normalization
    batchCorrFactors <- meanBatchGeomeans / withinBatchRowGeomeans
    colnames(batchCorrFactors) <- colnames(withinBatchRowGeomeans) <- as.character(batchIndex)

    # rowwise correct by multiplers (batchCorrFactors)
    ratioCleanDatNorm <- foreach(batch = as.character(batchIndex), .combine = "cbind", .multicombine = TRUE) %dopar% as.data.frame(ratioedBatches[[batch]] * batchCorrFactors[, batch])

    ###################################################################################################################################

    ## log2 transform ratios output from step 1a, and 1b
    cleanDat.log2.ratioUnnorm <- log2(ratioCleanDatUnnorm)
    cleanDatNormNoColScaling <- log2(ratioCleanDatNorm)


    ## For cleanDat.log2.ratioUnnorm:
    ## Enforce <50% missingness (1 less than half of columns (or round down half if odd number of columns))
    LThalfSamples <- length(colnames(cleanDat.log2.ratioUnnorm)) / 2
    LThalfSamples <- LThalfSamples - if ((length(colnames(cleanDat.log2.ratioUnnorm)) %% 2) == 1) {
      0.5
    } else {
      1.0
    }

    removedRownames1 <- rownames(cleanDat.log2.ratioUnnorm[which(rowSums(as.matrix(is.na(cleanDat.log2.ratioUnnorm))) > LThalfSamples), ]) # list rows to be removed
    removedRownames1

    # remove rows with >=50% missing values (only if there are some rows to be removed)
    if (length(na.omit(match(removedRownames1, rownames(cleanDat.log2.ratioUnnorm)))) == length(removedRownames1) & length(removedRownames1) > 0) {
      cleanDat.log2.ratioUnnorm <- cleanDat.log2.ratioUnnorm[-match(removedRownames1, rownames(cleanDat.log2.ratioUnnorm)), ]
    } else {
      cat("")
    } # "no rows removed.\n"); }
    dim(cleanDat.log2.ratioUnnorm)


    ## For cleanDatNormNoColScaling and companion relative abundance factors:
    ## Enforce <50% missingness (1 less than half of cleanDatNormNoColScaling columns (or round down half if odd number of columns))
    LThalfSamples <- length(colnames(cleanDatNormNoColScaling)) / 2
    LThalfSamples <- LThalfSamples - if ((length(colnames(cleanDatNormNoColScaling)) %% 2) == 1) {
      0.5
    } else {
      1.0
    }

    removedRownames <- rownames(cleanDatNormNoColScaling[which(rowSums(as.matrix(is.na(cleanDatNormNoColScaling))) > LThalfSamples), ]) # list rows to be removed
    #  removedRownames

    # remove rows with >=50% missing values (only if there are some rows to be removed)
    if (length(as.vector(na.omit(match(removedRownames, rownames(cleanDatNormNoColScaling))))) == length(removedRownames) & length(removedRownames) > 0) {
      cleanDatNormNoColScaling <- cleanDatNormNoColScaling[-match(removedRownames, rownames(cleanDatNormNoColScaling)), ]
      RW.relAbunFactors.HiMissRmvd <- RW.relAbunFactors[-match(removedRownames, names(RW.relAbunFactors))]
      # nrow(cleanDatNormNoColScaling)-length(removedRownames1) #, on iteration 1 is the final row count for the matrix; for this CSF data it should be 2875.
      cat(paste0("[iter_", repeats, "] Removed ", length(removedRownames), " high missingness rows. ", nrow(cleanDatNormNoColScaling), " rows remaining."))
    } else {
      cat(paste0("[iter_", repeats, "] No rows removed with >=50% missing values."))
      RW.relAbunFactors.HiMissRmvd <- RW.relAbunFactors
    }
    dim(cleanDatNormNoColScaling)


    if (repeats == 1) {
      ratioCleanDatUnnorm.iter1 <- 2^cleanDat.log2.ratioUnnorm
      relAbundanceUnnorm.iter1 <- as.matrix(RW.relAbunFactors.HiMissRmvd * ratioCleanDatUnnorm.iter1)
      colnames(ratioCleanDatUnnorm.iter1) <- colnames(relAbundanceUnnorm.iter1) <- colnames(ratioCleanDatNorm)
    }


    prevIterCleanDatNorm2 <- data.frame(matrix(0, nrow = nrow(cleanDatNormNoColScaling), ncol = ncol(cleanDatNormNoColScaling)))
    if (exists("cleanDatNorm2")) prevIterCleanDatNorm2 <- cleanDatNorm2


    ###################################################################################################################################
    ## Step 2, Enforce equal loading assumption on output of step 1b (all well-quantified proteins equally considered/weighted)
    colMeans(ratioCleanDatUnnorm, na.rm = TRUE)
    # should be zero, but they are usually not.

    ## Set all column means to ~0 (10^-16 or less), essentially 0=log2(ratio/GIS) for all column means or an average ratio/GIS=1
    #  cleanDatNorm <- scale(cleanDatNormNoColScaling, scale = FALSE)
    #  colMeans(cleanDatNorm, na.rm = TRUE)

    ## alternative columnwise normalization operation using mean or median [equivalent to scale() function above, if colAvg=mean(x,na.rm=TRUE) ]
    cleanDatNorm2 <- apply(cleanDatNormNoColScaling, 2, function(x) {
      colAvg <- eval(parse(text = paste0(meanOrMedian, "(x,na.rm=TRUE)"))) ## MEAN/MEDIAN FUNCTION CHOICE***
      outputCol <- x - colAvg # rep(colAvg,length(x));
      outputCol
    })

    ## show columnwise normalization/scaling methods are equivalent (with rounding to a few decimals, at least)
    #  colMeans(cleanDatNorm, na.rm = TRUE) == colMeans(cleanDatNorm2, na.rm = TRUE) # TRUE if meanOrMedian="mean"

    ## GIS-derived relative RW abundances can be applied back to the Step2 RW & CW-normalized data's rows.
    ## values of cleanDatNorm2 are log2(measurement/batch-stabilized GIS abundance).
    if (paste(names(RW.relAbunFactors.HiMissRmvd), collapse = ",") == paste(rownames(cleanDatNorm2), collapse = ",")) {
      relAbundanceNorm2 <- RW.relAbunFactors.HiMissRmvd * 2^cleanDatNorm2
    } else {
      stop(paste0("\n[iter_", repeats, "] ERROR: step 2 data with removed high missingness rows does not match relative abundance factors after trying to remove same rows.\n"))
    }


    ###################################################################################################################################
    ## Prepare for convergence check and next iteration

    cleanDat <- relAbundanceNorm2
    DFforFrobCurrent <- apply(cleanDatNorm2, 2, as.numeric)
    DFforFrobPrev <- apply(prevIterCleanDatNorm2, 2, as.numeric)
    removeColumnsCurrent <- which(apply(DFforFrobCurrent, 2, function(x) sum(is.na(x))) == nrow(DFforFrobCurrent))
    removeColumnsPrev <- which(apply(DFforFrobPrev, 2, function(x) sum(is.na(x))) == nrow(DFforFrobPrev))

    if (length(removeColumnsCurrent) > 0) {
      frobeniusNormCurrent <- norm(na.omit(DFforFrobCurrent[, -removeColumnsCurrent]), type = "F")
    } else {
      frobeniusNormCurrent <- norm(matrix((na.omit(DFforFrobCurrent))), type = "F")
    }
    if (length(removeColumnsPrev) > 0) {
      frobeniusNormPrev <- norm(na.omit(DFforFrobPrev[, -removeColumnsPrev]), type = "F")
    } else {
      frobeniusNormPrev <- norm(na.omit(DFforFrobPrev), type = "F")
    }
    initialFrobeniusNorm <- if (repeats == 1) {
      frobeniusNormCurrent
    } else {
      initialFrobeniusNorm
    }
    iterationTrackingDF[repeats, ] <- c(repeats, frobeniusNormCurrent, frobeniusNormPrev, frobeniusNormPrev - frobeniusNormCurrent, frobeniusNormCurrent / initialFrobeniusNorm)
    timer <- difftime(Sys.time(), timer.start, units = "secs")
    time.sec <- round(as.numeric(timer), 2)
    cat(paste0("     ", time.sec, " sec     iteration convergence tracking (Frobenius Norm Difference):  ", signif(iterationTrackingDF[repeats, 4], 3), "\n"))
    if (abs(iterationTrackingDF[repeats, 4]) < 0.00000001) {
      cat("...Reached convergence criterion (Frobenius Norm Difference)<1e-8!\n")
      break
    }
  } # closes "for (repeats in 1:iterations)"
  iterations.intended <- iterations
  iterations <- repeats
  converged <- as.logical(abs(iterationTrackingDF[repeats, 4]) < 0.00000001)

  rm(step1a)

  # END NORMALIZATION
  ###################################################################################################################################

  ## Below unnormed ratio (from iteration 1) data are only assembled for graphing purposes, for comparison to step 1b and final step 2 output
  ratioCleanDatUnnorm <- ratioCleanDatUnnorm.iter1 # as.matrix(cbind(ratioCleanDatUnnorm, ratioedBatches[[batch]]))
  relAbundanceUnnorm <- relAbundanceUnnorm.iter1 # as.matrix(RW.relAbunFactors.HiMissRmvd*ratioCleanDatUnnorm)


  # saveRDS(cleanDatNorm2,paste0(outputtabs,"cleanDat.TAMPOR_",iterations,"iter.RDS"))
  # saveRDS(relAbundanceNorm2,paste0(outputtabs,"relAbundanceNorm2.alternateTAMPOR.output_",iterations,"iter.RDS"))
  # saveRDS(ratioCleanDatUnnorm,paste0(outputtabs,"ratioCleanDatUnnorm.naiveRatioOverGIS_",iterations,"iter.RDS"))
  # saveRDS(relAbundanceUnnorm,paste0(outputtabs,"relAbundanceUnnorm.naiveRelAbun.AfterRatioOverGIS_",iterations,"iter.RDS"))

  ###################################################################################################################################
  ## Prepare sample traits

  # library(WGCNA) #only used for labels2colors() function; fn reproduced instead of requiring install of all WGCNA dependencies.

  ## Insure that traits column order are correct.
  traits <- traits[match(colnames(cleanDatNorm2), rownames(traits)), ]

  traits$BatchColor <- WGCNA::labels2colors(traits$Batch)

  numericMeta <- traitsWithGIS <- traits

  ##############################################################################
  ## Sample Removal and finalization of cleanDat as log2(ratio).

  ## One can remove GIS here, depending on whether these are (not) biologically meaningful for comparisons downstream
  if (removeGISafter) numericMeta <- traits <- traits[!traits$GIS == "GIS", ] #***
  cleanDat <- cleanDatNorm2[, match(rownames(traits), colnames(cleanDatNorm2))] # NOTE: overwrites cleanDat (intermediate structure during cleanup)

  ratioCleanDatUnnorm <- ratioCleanDatUnnorm[, match(rownames(traits), colnames(ratioCleanDatUnnorm))]
  relAbundanceNorm2 <- relAbundanceNorm2[, match(rownames(traits), colnames(relAbundanceNorm2))]


  # remove any ignored columns (all NA)
  removeColumns.cleanDat <- samplesToIgnore[as.vector(na.omit(match(colnames(cleanDat), samplesToIgnore)))] # which(apply(cleanDat, 2, function(x) sum(is.na(x))) == nrow(cleanDat))
  if (length(removeColumns.cleanDat) > 0) {
    cleanDat <- cleanDat[, -match(removeColumns.cleanDat, colnames(cleanDat))]
    numericMeta <- traits <- traits[match(colnames(cleanDat), rownames(traits)), ]

    ratioCleanDatUnnorm <- ratioCleanDatUnnorm[, -match(removeColumns.cleanDat, colnames(ratioCleanDatUnnorm))]
    relAbundanceNorm2 <- relAbundanceNorm2[, -match(removeColumns.cleanDat, colnames(relAbundanceNorm2))]
  }
  cleanDat.orig <- as.data.frame(cleanDat.original)[, match(rownames(traits), colnames(cleanDat.original))]

  # Only keep rows in original cleanDat (cleanDat.orig) matching final normed data
  #  dim(cleanDat.orig)
  cleanDat.orig <- cleanDat.orig[match(rownames(cleanDat), rownames(cleanDat.orig)), ]

  if (!length(which(traits$GIS == "GIS")) == nrow(traits) & !removeGISafter) { # single return statement now ok if only outputting recorded PDF pages...
    # Close parallel connections.
    suppressWarnings(stopCluster(clusterLocal))
    return(list(
      cleanDat = cleanDatNorm2,
      cleanRelAbun = relAbundanceNorm2,
      traits = traits,
      cleanDat.oneIter = ratioCleanDatUnnorm,
      cleanRelAbun.oneIter = relAbundanceUnnorm,
      iterations = iterations,
      converged = converged
    ))
  } else {
    # Close parallel connections.
    suppressWarnings(stopCluster(clusterLocal))
    return(list(
      cleanDat = cleanDatNorm2,
      cleanRelAbun = relAbundanceNorm2,
      traits = traits,
      cleanDat.oneIter = ratioCleanDatUnnorm,
      cleanRelAbun.oneIter = relAbundanceUnnorm,
      iterations = iterations,
      converged = converged
    ))
  }
} # close TAMPOR function
