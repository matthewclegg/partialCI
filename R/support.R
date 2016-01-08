# support.R -- a miscellaneous collection of supporting functions

ctext <- function (str, n) {
    # Centers the string str in a field of n blanks
    if (nchar(str) == n) return(str)
    if (nchar(str) > n) return(substr(str, 1, n))
    
    nleft <- floor((n - nchar(str))/2)
    nright <- n - (nchar(str) + nleft)
    left_pad <- paste(rep(" ", nleft), collapse="")
    right_pad <- paste(rep(" ", nright), collapse="")
    pstr <- paste(left_pad, str, right_pad, sep="")
    pstr
}

printf <- function (...) { cat(sprintf(...)); }
println <- function (...) { cat(sprintf(...)); cat("\n"); }

quantile.table.from.samples <- function (colname, samples,
                                         quantiles = seq(0.01, 0.99, by=0.01)) {
  # Builds a table of quantile values from the data.frame samples
  
  sample_sizes <- sort(unique(samples$n))
  
  nq <- length(quantiles)
  ns <- length(sample_sizes)
  
  qtab <- matrix(NA, ncol=ns+1, nrow=nq+1)
  qtab[2:(nq+1), 1] <- quantiles
  qtab[1, 2:(ns+1)] <- sample_sizes
  
  for (i in 2:(ns+1)) {
    n <- sample_sizes[i-1]
    qtab[2:(nq+1), i] <- quantile(samples[samples$n == n, colname], quantiles)
  }
  
  qtab
}

quantile_table_interpolate <- function (qtab, sample_size, stat, stop.on.na=FALSE) {
	# On input, qtab is a dataframe of quantiles.  Each column corresponds to
	# a sample size, and each row corresponds to a quantile value.  The sample
	# sizes are given in the first row, and the quantiles are given in the
	# first column.  
	n <- nrow(qtab)
	i <- findInterval(sample_size, qtab[1,2:ncol(qtab)])+1
	if (i == 1) {
		parent_name <- as.character(sys.call(-1)[[1]])
		if (stop.on.na) {
			stop (parent_name," requires a minimum of ", qtab[1,2], " observations.")
		} else {
			warning (parent_name, " requires a minimum of ", qtab[1,2], " observations.")
			return(NA)
		}
	}
	y1 <- approx(qtab[2:n, i], qtab[2:n, 1], stat, rule=2)$y
	if (i < ncol(qtab)) {
		y2 <- approx(qtab[2:n, i+1], qtab[2:n, 1], stat, rule=2)$y
		n1 <- qtab[1,i]
		n2 <- qtab[1,i+1]
		y <- y1 * (n2 - sample_size) / (n2 - n1) + y2 * (sample_size - n1)/(n2 - n1)
	} else {
		y <- y1
	}
	y
}


