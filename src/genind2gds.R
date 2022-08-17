genind2gds <- function(gi, gds.fn = "genind.gds", verbose = TRUE) {
        
        ## Author: Simon Crameri, sfcrameri@gmail.com, Aug. 2022
        
        ## Usage
        # Convert a genind object (adegenet package) to a valid input file for SNPRelate package
        
        ## Arguments
        # gi       genind object (can have multi-allelic variants, which will be removed)
        # gds.fn   name of written output file, as in SNPRelate::snpgdsCreateGeno
        # verbose  if TRUE, writes info and progress to screen
        
        ## Details
        # - Needed R packages (adegenet, SNPRelate) will be automatically installed
        # - A user prompt appears if the chosen output file name already exists
        # - locNames(gi) need to have the following format:
        #   <CHROM>_<POS>
        # - colnames(gi@tab) need to have the following fomat:
        #   <CHROM>_<POS>.<ALLELE>
        
        library(adegenet)
        if (!"SNPRelate" %in% installed.packages()) {
          if (!"BiocManager" %in% installed.packages()) {
            install.packages("BiocManager")
          }
          BiocManager::install("SNPRelate")
        }
        
        # Check input
        stopifnot(inherits(gi, "genind"), is.character(gds.fn))
        
        if (file.exists(gds.fn)) {
          ANSWER <- readline(prompt = paste0("<", gds.fn, "> already exists. Do you wish to replace it? y[es] / n[o] "))
          if (ANSWER %in% c("Y","y","YES","Yes","yes")) {
            cat("\n")
          } else {
            stop("Please choose a different <gds.fn> file name")
          }
        }
        
        # Subset to biallelic SNPs only
        nloc <- nLoc(gi)
        locbi <- which(gi@loc.n.all == 2)
        if (!all(locNames(gi) %in% names(locbi))) {
          gi <- gi[loc = locbi, drop = TRUE]
          if (verbose) {
            cat(paste0("excluded ", nloc-length(locbi), " (", round(100*(nloc-length(locbi))/nloc,2), "%) non-biallelic variants, ", nLoc(gi), " remain\n"))
          }
        } else {
          if (verbose) {
            cat(paste0("all ", nloc, " variants are biallelic\n"))
          }
        }
        
        # Set SNPRelate::snpgdsCreateGeno arguments
        # this assumes a locus naming as <CHROM_POS.ALLELE>
        genmat <- gi@tab[,seq(1,ncol(gi@tab),by=2)]
        genmat[is.na(genmat)] <- 9
        sample.id <- indNames(gi)
        snp.id <- gi@loc.fac[seq(1,ncol(gi@tab),by=2)]
        snp.chromosome <- sub("_[0-9]+$", "", gi@loc.fac[seq(1,ncol(gi@tab),by=2)])
        snp.position <- as.numeric(sub("^.*_", "", gi@loc.fac[seq(1,ncol(gi@tab),by=2)]))
        snp.allele <- unlist(lapply(gi@all.names, paste, collapse = "/"))
        other.vars <- gi@other
        
        if (verbose) {cat(paste0("writing SNP genotype dataset to <", gds.fn, ">...\n"))}
        SNPRelate::snpgdsCreateGeno(gds.fn = gds.fn, genmat = genmat, snpfirstdim = FALSE,
                                    sample.id = sample.id, snp.id = snp.id,
                                    snp.chromosome = snp.chromosome,
                                    snp.position = snp.position,
                                    snp.allele = snp.allele,
                                    other.vars = other.vars)
        
        if (verbose) {cat(paste0("done!\n"))}
      }
      
