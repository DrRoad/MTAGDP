
#--------- set the fonts ----------#

sme_font        <- "Gustan-Book"
annotation_font <- sme_font
dashboard_font  <- sme_font

sme_title       <- "Gustan-Bold"

#---------- set sizes for plots ---------#
# half horizontal (shape 1)
  w1 <- 176 / 25.4
  h1 <-  82 / 25.4

# half vertical (shape 2)
  w2 <-  82 / 25.4
  h2 <- 172 / 25.4

# quarter (shape 3)
  w3 <- 85 / 25.4
  h3 <- 82 / 25.4

# whole (snapshots only)
  w4 <- 176 / 25.4
  h4 <- 172 / 25.4

# half full page (no text), vertical
  w5 <- 130 / 25.4
  h5 <- 172 / 25.4

# half full page (no text), horizontal
  w6 <- 267 / 25.4
  h6 <-  83 / 25.4

#------------some commonly used scales------

# MBIE colours & theme set
mbie.cols <- function (x = 1:7) {
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

MBIE.cols <- c(                                                ## replaced by the mbie.cols function
               Teal   = rgb(  0, 98,114, maxColorValue=255),
               Green  = rgb(151,215,  0, maxColorValue=255),
               Blue   = rgb(  0,181,226, maxColorValue=255),
               Purple = rgb(117, 59,189, maxColorValue=255),
               Pink   = rgb(223, 25,149, maxColorValue=255),
               Orange = rgb(255,105,  0, maxColorValue=255),
               Yellow = rgb(251,225, 34, maxColorValue=255)
              )
              
firmsize_cols <- c("Zero (0)"             = "black",
                   "Micro (1-5)"          = mbie.cols(1),
                   "Small (6-19)"         = mbie.cols(3),
                   "Small-medium (20-49)" = mbie.cols(4),
                   "Medium (50-99)"       = mbie.cols(2),
                   "Large (100+)"         = mbie.cols(5),
                   "Micro-small (1-19)"   = mbie.cols(1),
                   "Medium-large (50+)"   = mbie.cols(5),
                   "Small (6–9)"          = mbie.cols(3),
                   "Small (10–19)"        = mbie.cols(6))

# Basic MBIE colours
  smeC1 <-  scale_color_manual(values=firmsize_cols)
  smeF1 <-  scale_fill_manual(values=firmsize_cols)

# set themes
# theme_set <- function(new)  {
    # missing <- setdiff(names(theme_gray()), names(new))
    # if (length(missing) > 0) {
        # warning("New theme missing the following elements: ", 
            # paste(missing, collapse = ", "), call. = FALSE)
    # }
    # old <- theme
    # theme <<- new
    # invisible(old)
# }


# plot(1:6, 1:6, pch=19, col=mbie.cols(), cex=10)
# Gradated colours
  low         <- mbie.cols(2)
  high        <- mbie.cols(1)
  sme_palette <- colorRampPalette(c(low, high))

  smeCgrad1 <- scale_color_manual(values=sme_palette(8)[3:8])
  smeFgrad1 <- scale_fill_manual(values=sme_palette(8)[3:8])

  smeCgrad2 <- scale_color_brewer(palette = "Spectral")
  smeFgrad2 <- scale_fill_brewer(palette = "Spectral")


#--------------themes-------------
# theme_sme1 - preferred basis for the one-off plots that need an X axis
theme_sme1 <- function(base_size = 7, base_family=sme_font, ...){
  theme_bw(base_size=base_size, base_family = base_family) %+replace%
    theme(legend.title.align=0.5,
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          plot.title = element_text(size = 11, family = sme_title),
          legend.title = element_text(face = "plain"),
          legend.key = element_blank())
}

# theme_sme2 is the same as theme_sme1 except with the x axis suppressed - quite a common
# configuration
theme_sme2 <- function(base_size = 7, base_family=sme_font, ...){
  theme_sme1(base_size=base_size, base_family = base_family, ...) %+replace%
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 11, family = sme_title),
          legend.title = element_text(face = "plain"),
          legend.key = element_blank()) 
}

theme_sme3 <- function(base_size = 7, base_family=sme_font, ...){
  theme_bw(base_size=base_size, base_family = base_family) %+replace%
    theme(legend.title.align=0.5)
}


# theme_set(theme_sme1())

# for maps
theme_sme4  <- function(base_size = 7, base_family=sme_font, ...){
    theme_bw(base_size=base_size, base_family = base_family) %+replace%
    theme(legend.title.align=0.5,
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(size = 11, family = sme_title),
          legend.title = element_text(face = "plain"),
          legend.key = element_blank())
}



#==============snapshot stuff======================

theme_sme_snapshot <- function(base_size = 7, base_family=sme_font, ...){
  theme_bw(base_size=base_size, base_family = base_family) %+replace%
    theme(legend.title.align=0.5,
          panel.border = element_blank(),
          legend.key = element_rect(colour = "white"))
}


## update to theme_nothing
theme_nothing <- function (base_size = 12, base_family = "sans") 
{
    theme_bw(base_size = base_size, base_family = base_family) %+replace% 
        theme(rect              = element_blank(), 
              line              = element_blank(), 
              axis.ticks.length = unit(0.01, "cm"), 
              # axis.ticks.margin = unit(0, "lines"),   ## depreciated
              axis.text         = element_blank(), 
              axis.ticks        = element_blank(), 
              axis.title        = element_blank())
}


