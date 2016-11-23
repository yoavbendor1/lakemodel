#' day length simulator
#' 
#' this function simulates a day length in a specified location the user is asked to provide a minimum and maximum day length the values are arranged from JAN 1st to DEC 31st
#'
#' @param     mindaylength    (hr) minimum day length
#' @param     maxdaylength    (hr) maximum day length
#' @param     hemisphere     (days) define the hemisphere in which the calculation takes place. default value is N, corresponding to 356 (corresponding to DEC 21st)

#' @author Yoav BD
#' @return daily wind for one year (arbitrary units)

# internal parameters
# maxdays (-) number of days in a year. default is 366

daylengthsimulator <-
function(mindaylength=10, maxdaylength=14,hemisphere="N"){
    
    #define internal parameters
    # number of days in a year is 366
    maxdays=366
    
    #define the phase which fits the hemisphere
    if (as.character(hemisphere)=="N") {phase=356}
    if (as.character(hemisphere)=="S") {phase=204}
    
    #arrange the day index according to the hemisphere which defines the phase
    days=seq(from=1, to=366 ,by=1)
    days=days+phase
    for (ii in 1:length(days)) {
      if (days[ii]>366) days[ii]=days[ii]-maxdays
    }
    
    #calculate both halves of the yearly daylength cycle
    daylength1=seq(from = mindaylength, to = maxdaylength, length.out =((366)/2))
    daylength2=seq(from = maxdaylength, to = mindaylength, length.out =((366)/2))
    daylength3=c(daylength1,daylength2)
    
    daylengthbyday=rep(mindaylength,maxdays)
    # match the appropriate day length to the right place in the result vector
    for (ii in 1:length(days)) {
      daylengthbyday[ii]=daylength3[days[ii]]}

    # return the daylength vector
    return(daylengthbyday)
    
  }
