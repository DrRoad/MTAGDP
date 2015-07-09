
# MBIE colours & theme set
MBIE.cols <- c(
               Teal=rgb(0,98,114, maxColorValue=255),
               Green=rgb(151,215,0, maxColorValue=255),
               Blue=rgb(0,181,226, maxColorValue=255),
               Purple=rgb(117,59,189, maxColorValue=255),
               Pink=rgb(223,25,149, maxColorValue=255),
               Orange=rgb(255,105,0, maxColorValue=255),
               Yellow=rgb(251,225,34, maxColorValue=255)
              ) 


theme_set <- function(new)  {
    missing <- setdiff(names(theme_gray()), names(new))
    if (length(missing) > 0) {
        warning("New theme missing the following elements: ", 
            paste(missing, collapse = ", "), call. = FALSE)
    }
    old <- theme
    theme <<- new
    invisible(old)
}




