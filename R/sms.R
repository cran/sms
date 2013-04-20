#====================== Class ========================

##' A microsimulation object
##' 
##' It holds all microsimulation details and objects such as data, results etc.
##' @param census: A census data.frame where each row contains census information about a geographical area
##' @param panel: A data.frame containing the individual based records from a panel survey. those data will be fitted to small area contrains and will populate each vrtual area.
##' @param lexicon: A data.frame containing the association of columns between census data and panel data. Each row contain a conection between census and panel data.frame.
##' @param resuls: A list of results from the fitting process.
##' @param iterations: The number of itertions until th end of the fitting process.
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @name microsimulation-class
##' @exportClass microsimulation
setClass("microsimulation",
         representation(
           census="data.frame",panel="data.frame",lexicon="data.frame",results="ANY",iterations="numeric"),
         prototype(
           census=data.frame(),panel=data.frame(),lexicon=data.frame(),results=list(),iterations=0)
)

##' getInfo Generic
##' 
##' getInfo Generic
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @param object A microsimulation object to get its information.
##' @aliases getInfo,microsimulation-generic
##' @usage library(sms)
##' lesvos= sms::getSampleMicrosimulation(10,50,30)
##' getInfo(lesvos)
setGeneric("getInfo", function(object) {
  standardGeneric("getInfo")
  #cat("Generic getInfo \n")
})

##' getInfo Method
##' 
##' Get information from a microsimulation object
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @param object A microsimulation object to get its information.
##' @exportMethod getInfo
##' @aliases getInfo,microsimulation-method
##' @usage library(sms)
##' lesvos= sms::getSampleMicrosimulation(10,50,30)
##' getInfo(lesvos)
setMethod("getInfo", signature(object = "microsimulation"), function(object) {
  cat(paste0("A microsimulation object with ",length(object@panel)," panel records and ",length(object@census)," census areas\n"))
})



##' getTAEs Generic
##' 
##' Get the TAE from a microsimulation object.
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @param object A microsimulation object to get its information.
##' @aliases getTAEs,microsimulation-generic
##' @usage library(sms)
##' lesvos= sms::getSampleMicrosimulation(10,50,30)
##' taes=getTAEs(lesvos)
##' print(taes)
setGeneric("getTAEs", function(object) {
  standardGeneric("getTAEs")
})

##' getTAEs Method
##' 
##' getTAEs Method
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @param object A microsimulation object to get its information.
##' @return taes A list of numbers indicating the Total Absolute Error of the fitting process for each of the census areas.
##' @exportMethod getTAEs
##' @aliases getTAEs,microsimulation-method
##' @usage library(sms)
##' lesvos= sms::getSampleMicrosimulation(10,50,30)
##' taes=getTAEs(lesvos)
##' print(taes)
setMethod("getTAEs", signature(object = "microsimulation"), function(object) {
  #cat(paste0("A microsimulation object with ",length(object@panel)," panel records and ",length(object@census)," census areas\n"))
  taes=sapply(object@results, '[[', "tae")
  return(taes)
})

#====================== Simulation Methods ========================

##' Generate sample census and panel (survey) datasets 
##'
##' Generate random census and panel (survey) data for testing purpose
##' @title getSampleData
##' @param census_n A The census data in dataframe format
##' @param bhps_n The size of the panel data
##' @return A list with 2 dataframes. a "census" and "panel" dataframe
##' @author Dimitris Kavroudakis
##' @export
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @examples library(sms)
##' gen=getSampleData(census_n=10, bhps_n=55) #Generate a sample census dataset of 10 areas and a individual based survey dataset of 55 individual records.
##' print(gen) #print the generated data
getSampleData = function(census_n=3,bhps_n=20){
  # Census Data
  areaid_init=8300
  areaid=(areaid_init:(areaid_init+census_n-1))
  population=sample(1:20,census_n,replace=T)
  he=sample(1:15,census_n,replace=T)
  females=sample(1:12,census_n,replace=T)
  census=data.frame(areaid=areaid,population=population,he=he,females=females)
  # Panel Data
  pid_init=6501
  pid=(pid_init:(pid_init+bhps_n-1))
  he=sample(0:1,bhps_n,replace=T)
  female=sample(0:1,bhps_n,replace=T)
  agemature=sample(0:1,bhps_n,replace=T)
  car_owner=sample(0:1,bhps_n,replace=T)
  house_owner=sample(0:1,bhps_n,replace=T)
  working=sample(0:1,bhps_n,replace=T)
  bhps=data.frame(pid, he,female, agemature,car_owner,house_owner, working)
  return(list(census=census, panel=bhps))
}

##' Generate a sample Microsimulation object
##'
##' Generate a sample Microsimulation object with sample data for testing purpose.
##' @title getSampleMicrosimulation
##' @param census_n number of areas to generate
##' @param bhps_n number of individual records to generate
##' @param iterations Number of iterations to use when simulating 
##' @return a microsimulation object with data
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @export
##' @examples library(sms)
##' insms=getSampleMicrosimulation(census_n=10,bhps_n=50)
##' print(insms)
getSampleMicrosimulation = function(census_n=3,bhps_n=20,iterations=5){
  gen=sms::getSampleData(census_n=census_n,bhps_n=bhps_n)
  #  Lexicon
  in.lexicon=data.frame(c("he","he" ) , c("females","female" ) )
  names(in.lexicon)=c("education","sex")
  row.names(in.lexicon)=c("census_row","panel_row")
  insms= new("microsimulation",
              census=gen$census,
              panel=gen$panel, 
              lexicon=in.lexicon, 
              iterations=iterations)
  return(insms)
}

##' Select n random rows from a dataframe
##'
##' Select n random rows from a dataframe
##' @title random_panel_selection
##' @param indf The initial dataframe from wich a selection will be made.
##' @param n The number of random rows
##' @return a selection of rows as a dataframe
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @export
##' @examples library(sms)
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
##' some.individuals=random_panel_selection(panel,4)
##' print(some.individuals) #print the selection of individuals
random_panel_selection=function(indf,n){
  return(indf[sample(nrow(indf), n, replace=TRUE), ]) 
}


##' Calculate the error of a selection.
##'
##' Calculates the Total Absolute Error (TAE) of a selection for a census area.
##' @title Calculate error of a selection
##' @param selection A population selection, to evaluate its error
##' @param area_census An area from census (a row)
##' @param lexicon A data.frame with details about data connections
##' @return TAE Total Absolute Error of this selection against the census description of this area.
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com} 
##' @export
##' @examples library(sms)
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' this_area=as.data.frame(census[1,]) #Select the first area from the census table
##' selection=random_panel_selection(panel,this_area$population) #make a random selection of individuals for this area.
##' error=calculate_error(selection,this_area,in.lexicon) #evaluate the Total Absolute Error (TAE) for this selection
##' print(error) #print the error of the selection
calculate_error=function(selection,area_census,lexicon){
  nvar=length(lexicon)
  myNewColumnNames=c()
  for (i in 1:nvar){# for every variable in the lexicon
    column_name=colnames( lexicon )[i]# con_01, con_02
    myNewColumnNames=c(myNewColumnNames,as.character(lexicon[column_name]["census_row",]))
  }
  area.errors=c(areaid="")#Add one column
  area.errors[myNewColumnNames]="" #add the other columns: cars, mature
  this_area_id=area_census$areaid
  area.error=data.frame(areaid=this_area_id)# prepare the Dataframe with the errors for this area.
  tae=0
  for (i in 1:nvar){# for every variable in the lexicon
    column_name=colnames( lexicon )[i]# con_01, con_02
    at.census= area_census[[ as.character(lexicon[column_name]["census_row",]) ]]# cars 5, mature 6
    panel.column=as.character(lexicon[column_name]["panel_row",])
    at.selection= sum(selection[panel.column])
    this.var.error=abs(at.census - at.selection)
    tae=tae+this.var.error
    area.errors[panel.column]=this.var.error
  }  
  return(tae)
}


##' Make a single selection of individual records for a census area.
##'
##' Select a number of individual records from panel dataset, to represent a census description of an area.
##' @title selection_for_area
##' @param inpanel The panel dataset
##' @param area_census A census area
##' @param inlexicon A data lexicon showing the variable associations.
##' @return list A list of results (#areaid, #selection, #error)
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @export
##' @examples library(sms)
##' n=20
##' panel=data.frame(pid=6001:(6000+n), he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' this_area=as.data.frame(census[1,]) #Select the first area from the census table
##' sel=selection_for_area(panel,this_area, in.lexicon) #make a representation for this area.
##' print(sel) #print the representation
selection_for_area=function(inpanel, area_census, inlexicon){
  this_area_id=area_census$areaid
  this_area_pop=area_census$population
  selection=random_panel_selection(inpanel,this_area_pop)
  error=calculate_error(selection,area_census,inlexicon)
  return(list(areaid=this_area_id,selection=selection,error=abs(error)))
}

##' Plot the selection process of an area from a microsimulation object.
##'
##' Plot errors during selection process for an area.
##' @title Plot selection results
##' @param insms The input results
##' @param number the number of the area to plot
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @examples library(sms)
##' n=20
##' panel=data.frame(pid=6001:(6000+n), he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' ansms= new("microsimulation",census=census,panel=panel, lexicon=in.lexicon, iterations=5)
##' sa=run_parallel_SA(ansms)
##' plotTries( sa, 1 )
##' @export
plotTries <- function(insms, number){
  if (!is(insms, "microsimulation")){
    stop("You gave me a non-microsimulation object to plot.\n\t\tPlease give me a proper microsimulation object.")
  }
  if ((length(insms@results)<1)){
    stop("There are no results to plot. Run the simulation first and then plot the results.")
  }
  #print("Inresult")
  #print(nrow(inresult$selection))
  inresult=insms@results[[number]]
  states=inresult$error_states
  tries=inresult$tries
  lim=range(states,tries)
  plot(1:length(states),ylim=lim,states, type="b", pch=20, xlab="Iterations", ylab="Total Absolute Error")
  points(1:length(tries), tries, col="red")
  title(main = list(paste("Area",inresult$areaid), cex=0.9, font=4))
  mtext(cex=0.8,paste("Improvements:",length(unique(states))-1," ","TAE:",inresult$tae, 
                      "\nPopulation:",nrow(inresult$selection) )
  )#the error improvents towards 0 error.
}

##' Find the best selection of individual records for a census area.
##'
##' Calculate the best area representation, after a series of selection tries.
##' @title find_best_selection
##' @param area A census area
##' @param insms A microsimulation object which holds the data and details of the simulation such as iterations, lexicon.
##' @return list A list with results (#areaid, #selection, #tae, #tries, #error_states).
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @export
##' @examples library(sms)
##' n=20
##' panel=data.frame(pid=6001:(6000+n), he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' this_area=as.data.frame(census[1,]) #Select the first area from the census table
##' insms= new("microsimulation",census=census,panel=panel, lexicon=in.lexicon, iterations=10)
##' best=find_best_selection(this_area, insms)
##' print(best)
find_best_selection<- function(area,insms){
  area_census=as.data.frame(area)
  it=insms@iterations
  bhps=insms@panel
  my.lexicon=insms@lexicon
  old_error=NULL
  errors_list=c()
  current_error_list=c()
  result_selection=NULL
  for (i in 1:it){
    current_selection=selection_for_area(bhps,area_census, my.lexicon)
    if (i==1){# At first time
      result_selection=current_selection #Just in case we got the best selection.
      old_error=abs(current_selection$error) #Keep the initial error
    }
    current_error_list=c(current_error_list,current_selection$error)# Append the current selection error.
    
    if (old_error > abs(current_selection$error) ) {#If this new error is better
      old_error = abs(current_selection$error) #keep the error
      result_selection=current_selection #keep the selection
    }
    errors_list=c(errors_list,old_error) #append the error to the errors_list
  }
  return(list(areaid=result_selection$areaid,
              selection=result_selection$selection,
              tae=result_selection$error,
              tries = current_error_list, 
              error_states = errors_list)
  )
}



##' Run a simulation in serial mode with Hill Climbing
##'
##' Run a simulation in serial mode with Hill Climbing
##' @title run_parallel_HC
##' @param insms A microsimulation object which holds the data and details of the simulation such as iterations, lexicon.
##' @return msm_results An object with the results of the simulation, for each area.
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @examples library(sms)
##' library(sms)
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' in.lexicon=data.frame(c("he","he" ),c("fem","females" ) )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' insms= new("microsimulation",census=census,panel=panel, lexicon=in.lexicon, iterations=10)
##' re=run_parallel_HC(insms)
##' print(re)
##' 
##' @export
#run_parallel <- function(census,iterations,bhps, the.lexicon){
run_parallel_HC <- function(insms){
  #options(cores=2)
  library(parallel)
  library(doParallel)
  library(foreach)
  #library(doSNOW)
  cores=2
  cl <- makePSOCKcluster(cores ) 
  registerDoParallel(cl)
  #cores=parallel::detectCores()
  #cl<-parallel::makeCluster(cores,type="PSOCK")
  #doSNOW::registerDoSNOW(cl)
  #doParallel::registerDoParallel(cl)
  myFunctions=c("find_best_selection",  "calculate_error", "selection_for_area", "random_panel_selection")
  msm_results=
    foreach(i=iter(insms@census, by='row'), .export=myFunctions, combine=c) %dopar% {
      find_best_selection(i,insms)
    }
  stopCluster(cl)
  insms@results= msm_results
  i <- NULL 
  rm(i)
  return(insms)
}



##' Run a simulation in serial mode
##'
##' Run a simulation in serial mode.
##' @title Run_serial
##' @param insms A microsimulation object which holds the data and details of the simulation such as iterations, lexicon.
##' @return msm_results An object with the results of the simulation, for each area.
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @examples library(sms)
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' insms= new("microsimulation",census=census, panel=panel, lexicon=in.lexicon, iterations=5)
##' results= run_serial( insms)
##' print(results)
##' @export
run_serial <- function(insms){
  library(parallel)
  library(foreach)
  #library(doSNOW)
  library(doParallel)
  myFunctions=c("find_best_selection_SA", "calculate_error", "selection_for_area", "random_panel_selection")
  msm_results=
    foreach(i=iter(insms@census, by='row'), .export=myFunctions, combine=c) %do% {
      sms::find_best_selection(i,insms)
    }
  i <- NULL 
  rm(i)
  return(msm_results)
}

##' Run a simulation in parallel mode with Simulated Annealing
##'
##' @title find_best_selection_SA
##' @param area_census A census dataset consisting of various areas rows.
##' @param insms A microsimulation object which holds the data and details of the simulation such as iterations, lexicon.
##' @return msm_results An object with the results of the simulation, of this area.
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @examples library(sms)
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' this_area=as.data.frame(census[1,]) #Select the first area from the census table
##' insms= new("microsimulation",census=census, panel=panel, lexicon=in.lexicon, iterations=5)
##' myselection= find_best_selection_SA( this_area, insms)
##' print(myselection)
##' @export
find_best_selection_SA <- function (area_census, insms) {
  iterations=insms@iterations
  bhps=insms@panel
  in.lexicon=insms@lexicon
  #area_census=as.data.frame(area_census)
  old_error=NULL
  errors_list=c()
  current_error_list=c()
  result_selection=NULL
  tolerance=iterations/2
  for (i in 1:iterations){
    current_selection=selection_for_area(bhps,area_census, in.lexicon)
    if (i==1){# At first time
      old_error=abs(current_selection$error) #Keep the initial error
      result_selection=current_selection #Just in case we got the best selection.
    }
    current_error_list=c(current_error_list,current_selection$error)# Append the current selection error.
    
    if (old_error > abs(current_selection$error) ) {#If this new error is better
      old_error = abs(current_selection$error) #keep the error
      result_selection = current_selection #keep the selection
    }
    else if( tolerance > 0 ){
      dice=sample(c(F,T),1, prob=c((iterations/2) , (tolerance/(iterations/2)) )) #roll a boolean dice
      if(dice){ #if the dice gives TRUE
        old_error = abs(current_selection$error) #keep the error
        result_selection = current_selection #keep the selection
      }
    }
    tolerance=tolerance-1
    errors_list=c(errors_list,old_error) #append the error to the errors_list
  }
  return(list(areaid=result_selection$areaid,selection=result_selection$selection,tae=result_selection$error,
              tries = current_error_list,error_states = errors_list))
}


##' Run a simulation in parallel mode with Simulated Annealing
##'
##' @title run_parallel_SA
##' @param insms A microsimulation object which holds the data and details of the simulation such as iterations, lexicon.
##' @return msm_results An object with the results of the simulation, for each area.
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @examples library(sms)
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' insms= new("microsimulation",census=census, panel=panel, lexicon=in.lexicon, iterations=5)
##' results= run_parallel_SA(insms)
##' print(results)
##' @export
run_parallel_SA <- function(insms){
  census=insms@census 
  iterations=insms@iterations 
  bhps=insms@panel 
  in.lexicon=insms@lexicon 
  #options(cores=2)
  library(parallel)
  library(foreach)
  library(doParallel)
  
  #library(Rmpi)
  #library(doSNOW)
  #cores=parallel::detectCores()
  cores=2
  cl <- makePSOCKcluster(cores ) 
  registerDoParallel(cl)
  #cl<-parallel::makeCluster(parallel::detectCores(),type="SOCK")
  #registerDoSNOW(cl)
  
  doParallel::registerDoParallel(cl)
  myFunctions=c("find_best_selection_SA", "calculate_error", "selection_for_area", "random_panel_selection")
  msm_results=
    foreach(i=iter(census, by='row'), .export=myFunctions, combine=c) %dopar% {
      sms::find_best_selection_SA(i,insms)
    }
  stopCluster(cl)
  i <- NULL 
  rm(i) 
  insms@results= msm_results
  return(insms)
}





##' Generate small area population microdata from census and survey datasets. 
##' Fit the survey data to census area descriptions and export the population of small areas.
##' 
##' Please feel free to contact me for error, problems or other features of the library.
##' \email{dimitris123@@gmail.com}
##' 
##' Generate small area population microdata from census and panel datasets. 
##' Fit the survey data to census area descriptions and export the popultion of small areas.
##' 
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @name sms-package
##' @docType package
##' @title Spatial Microsimulation Library
NULL


