#' daily lake
#' 
#' this function calculates the daily nurient budget and growth of algae populations in the lake the model assumes a one dimensional stratified lake the process takes place in the epilimnion, where growing algae consume the nutrient whan a certain wind threshold is reached, the lake mixes and the nutrients are redistributed between the epi- and the hypolimnion
#'
#' @param     hypoP    (arbitrary units) P concentrations in hypoliminion
#' @param     epiP     (arbitrary units) P concentrations in epiliminion
#' @param     hypoN    (arbitrary units) N concentrations in hypoliminion
#' @param     epiN     (arbitrary units) N concentrations in epiliminion
#' @param     hypoC    (arbitrary units) C concentrations in hypoliminion
#' @param     epiC     (arbitrary units) C concentrations in epiliminion
#' @param     windbydayjitter     (arbitrary units) daily wind
#' @param     daylengthbynum      (hours) daily hours of light
#' @param     windthreh           (arbitrary units) limit of threshold to mix the lake
#'
#' @author Yoav BD
#' @return daily nutrients and algae populations (arbitrary units)

daily_lake <-
function(hypoP=100, epiP=100, hypoN=100, epiN=100, hypoC=100, epiC=100, windday, daylength,cyanopop,greenpop ,rgreen, rcyano, windthresh){
  
  new_cyanopop=cyanopop*exp(rcyano*epiP*epiN*epiC*daylength)
  new_greenpop=greenpop*exp(rgreen*epiP*epiN*epiC*daylength)

  Cdeficit=1.06*(new_cyanopop-cyanopop+new_greenpop-greenpop)
  Ndeficit=0.16*(new_cyanopop-cyanopop+new_greenpop-greenpop)
  Pdeficit=0.01*(new_cyanopop-cyanopop+new_greenpop-greenpop)
  
  tempepiC=epiC-Cdeficit
  tempepiN=epiN-Ndeficit
  tempepiP=epiP-Pdeficit
  
  temphypoC=hypoC
  temphypoN=hypoN
  temphypoP=hypoP
  

  if(tempepiC < 0) tempepiC=0
  if(temphypoC < 0) temphypoC=0
  
  if(tempepiN < 0) tempepiN=0
  if(temphypoN < 0) temphypoN=0
  
  if(tempepiP < 0) tempepiP=0
  if(temphypoP < 0) temphypoP=0
  
  if(windday > windthresh){
    tempepiC=mean(tempepiC, temphypoC)
    temphypoC=tempepiC
    tempepiN=mean(tempepiN, temphypoN)
    temphypoN=tempepiN
    tempepiP=mean(tempepiP, temphypoP)
    temphypoP=tempepiP}
  
lake_cond=c(temphypoP, tempepiP, temphypoN, tempepiN, temphypoC, tempepiC,new_cyanopop, new_greenpop)
}
