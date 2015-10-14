## placeholder for functions from mbie-specific packages
# 2015-06-24


##
## 1. Rename levels
##

   rename.levels <- function (x, orig, new) 
      {
         if (!is.factor(x)) 
            stop("x must be a factor")
        if (length(orig) != length(new)) 
            stop("Number of new labels must equal number of old labels.")
        for (i in 1:length(orig)) {
            levels(x)[levels(x) == orig[i]] <- new[i]
             }
        return(x)
       }


##
## 2. Colour scheme for MBIE
##

   mbie.cols <- function (x = 1:7) 
        {
            if (x[1] == "Duo1") 
                x <- 1:2
            if (x[1] == "Trio1") 
                x <- 1:3
            if (x[1] == "Duo2") 
                x <- 2:3
            if (x[1] == "Trio2") 
                x <- 3:5
            if (x[1] == "Duo3") 
                x <- 4:5
            if (x[1] == "Trio3") 
                x <- c(4, 6:7)
            if (x[1] == "Duo4") 
                x <- 6:7
            if (x[1] == "Duo5") 
                x <- c(4, 7)
            as.vector(MBIE.cols[x])
        }

##
## 3. Wrap function to help with text in ggplot2
##

   wrap <- function (s, n = 10) 
      {
          tmp <- gsub(paste0("(.{1,", n, "})(\\s|$)"), "\\1\n", s)
          nc <- nchar(tmp)
          tmp <- substring(tmp, 1, nc - 1)
          return(tmp)
      }

##
## 4. Format Dollars & "compound annual growth rate" (CAGR) from {mbie} package

   FormatDollars <- function (x, endmark = "", ...) 
       {
           x <- paste0("$", format(round(x, ...), big.mark = ","), endmark)
           x <- gsub(" ", "", x)
           return(x)
       }

   CAGR <- function (ratio, period, digits = 1) 
	{
	    round((exp(log(ratio)/period) - 1) * 100, digits)
	}

