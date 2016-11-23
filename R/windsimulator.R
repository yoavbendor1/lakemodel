#' wind simulator
#' 
#' this function simulates a daily wind for the daily_lake model the user is asked to provide a factor defining the amplitude of the sine wave, as well as factors defining the jitter (noise) of the wind
#' 
#'
#' @param     cycles    (-) number of cycles used to model wind (i.e. # of windy seasons per year). default value is 1
#' @param     amp       (-) amplitude of the wind factor (the wind will start from 0 to amp). default value is 1
#' @param     jitfactor (-) factor of jit, default value is 0.1 of amp
#' @param     jitamount (-) amount of jit, default value is 0.1 of amp
#' @param     phase     (days) define when the yearly wind cycle starts in days. default value is 0 (corresponding to Jan 1st)
#' 
#' @author Yoav BD
#' @return daily wind for one year (arbitrary units)

# internal parameters
# maxdays (-) number of days in a year. default is 366

windsimulator <-
function(cycles=1, amp=1, jitfactor=0.1, jitamount=0.1, phase=0){
    #define internal parameters
    # number of days in a year is 366
    maxdays=366
    
    #inititate variable to simulate wind
    days=seq(from=1 , to=maxdays, by=1)
    wind=seq(from=1 , to=maxdays, by=1)
    
    #calculate initial sin wind simulation
    for (ii in 1:maxdays){
    wind[ii]=abs(sin((cycles*pi)*(ii/maxdays)+phase*pi/maxdays))
    }
    
    wind=unlist(wind)
      
    #add jitter to wind
    windjit=jitter(wind, factor = jitfactor, amount = jitamount)
    
    #zero negative wind values resulting from jitter
    for (ii in 1:length(windjit)){
      if (windjit[ii]<0) windjit[ii]=0
    }
    
    return(list(wind, windjit))
  }
