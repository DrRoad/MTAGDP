## This code is generated to allow users to enter a time period they would like to analyze

## This code is adapted from https://gist.github.com/jbryer/3342915

##  @param vars character list of variable names which will be used for the returned list
##  @params labels the labels of the variables. 
##  @params fun the list of functions that will be applied to the user entries
##  @params title title of the dialog box

getTimePeriod <- function (vars, 
                           labels = vars,
                           fun = rep(list(as.character), length(vars)),
                           title = 'Specify Time Period for Analysis',
                           prompt = NULL) {
  
  
  stopifnot(length(vars) == length(labels), length(labels) == length(fun))
  
  # Create a variable to keep track of the state of the dialog window:
  # done = 0; If the window is active
  # done = 1; If the window has been closed using the OK button
  # done = 2; If the window has been closed using the Cancel button or destroyed
  done <- tclVar(0)
  
  tt <- tktoplevel()
  tkwm.title(tt, title)  
  entries <- list()
  tclvars <- list()
  
  # Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens, 
  # assign 2 to done.
  tkbind(tt,"<Destroy>",function() tclvalue(done)<-2)
  
  for(i in seq_along(vars)) {
    tclvars[[i]] <- tclVar("")
    entries[[i]] <- tkentry(tt, textvariable=tclvars[[i]])
  }
  
  doneVal <- as.integer(tclvalue(done))
  results <- list()
  
  
  reset <- function() {
    for(i in seq_along(entries)) {
      tclvalue(tclvars[[i]]) <<- ""
    }
  }
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  
  cancel <- function() {
    tclvalue(done) <- 2
  }
  cancel.but <- tkbutton(tt, text='Cancel', command=cancel)
  
  submit <- function() {
    for(i in seq_along(vars)) {
      tryCatch( {
        results[[vars[[i]]]] <<- fun[[i]](tclvalue(tclvars[[i]]))
        tclvalue(done) <- 1
      },
      error = function(e) { tkmessageBox(message=geterrmessage()) },
      finally = { }
      )
    }
    
    tryCatch( {
      checkAllValues(results)
    }, 
    error = function(e){ tkmessageBox(message=geterrmessage()) },
    finally = { }
    )
    
  }
  submit.but <- tkbutton(tt, text="Submit", command=submit)
  
  if(!is.null(prompt)) {
    tkgrid(tklabel(tt,text=prompt), columnspan=3, pady=10)
  }
  
  for(i in seq_along(vars)) {
    tkgrid(tklabel(tt, text=labels[i]), entries[[i]], pady=10, padx=10, columnspan=4)
  }
  
  tkgrid(submit.but, cancel.but, reset.but, pady=10, padx=10, columnspan=3)
  tkfocus(tt)
  
  # Do not proceed with the following code until the variable done is non-zero.
  #   (But other processes can still run, i.e. the system is not frozen.)
  tkwait.variable(done)
  
  if(tclvalue(done) != 1) {
    results <- NULL
  }
  
  tkdestroy(tt)
  
  return(results)
  
} ## End of dialog box.

#  startYear <- as.numeric(readline(prompt="Enter the start year for the analysis after 1999 (inclusive): "))
#  endYear <- as.numeric(readline(prompt= "Enter the end year for the analysis (inclusive): "))
#  deflationYear <- as.numeric(readline(prompt= "End the reference for deflation calculation: "))
checkStart <- function(startYear){
  
  startYear <- as.integer(startYear)
  
  if(is.na(startYear)){
    startYear<-2000 ## Default value for the start of the time period for the analysis.
    paste("The default value for the start of the time period for analysis is set to be ", startYear)
  }
  
  if(startYear < 2000){
    stop("Beginning period for  the analysis should be after 1999")
  }  
  
  return(startYear)
} ## End of checkStart function

checkEnd <- function(endYear){
  
  endYear <- as.integer(endYear)
  if(is.na(endYear)){
    endYear <- year(Sys.Date()) - 3 ## The latest data on how GDP is distributed by territory and industry is available is for three years prior
    paste("The default value for the end of the time period for analysis is set to be", endYear)
  }
  return(endYear)
  
}

# checkDeflation <- function(deflationYear){
#   
#   deflationYear <- as.integer(deflationYear)
#   
#   if(is.na(deflationYear)){
#     deflationYear <- 2013 ## Should it be the end year?
#     paste("The default value for the reference year for deflation calculation is set to be", deflationYear)
#   }
#   return(deflationYear)
#   
# }

checkAllValues <-function(userInput){
  startYear <- userInput$startYear
  endYear <- userInput$endYear
  #deflationYear <- userInput$deflationYear
  
  if(endYear <= startYear){
    stop("Specified time period is not valid.")
  }
  
#   if(deflationYear < startYear || deflationYear > endYear){
#     stop("The year specified as reference for deflation calculation is out of the time period specified for analysis")
#   }
  
}
