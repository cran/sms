#setwd("~/working/Publications/sms")


#====================== Simulation Methods ========================


##' Select random rows
##'
##' Select a specific number of random rows from a data.frame
##' @title random_panel_selection
##' @param indf A dataframe
##' @param n Number of random rows
##' @return a selection of rows.
##' @author Dimitris Kavroudakis
##' @export
##' @examples library(sms)
##' #========================= Panel Data ============================
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
##' some.individuals=random_panel_selection(panel,4)
##' print(some.individuals) #print the selection of individuals
random_panel_selection=function(indf,n){
  return(indf[sample(nrow(indf), n, replace=TRUE), ]) 
}


##' Calculate error of a selection
##'
##' Calculates the Total Absolute Error of an area selection.
##' @title Calculate error of a selection
##' @param selection A population selection, to evaluate its error
##' @param area_census An area from census (a row)
##' @param lexicon A data.frame with details about data connections
##' @return TAE Total Absolute Error of this selection against the census description of this area.
##' @author Dimitris Kavroudakis
##' @export
##' @examples library(sms)
##' #========================= Panel Data ============================
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
##' #========================= Census Data ============================
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' #========================= Data linkage ===========================
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
  #myNewColumnNames=c(as.character(lexicon["con_01"]["census_row",]),# Write loop to get data from lexicon.
  #as.character(lexicon["con_02"]["census_row",])
  #)
  area.errors=c(areaid="")#Add one column
  area.errors[myNewColumnNames]="" #add the other columns: cars, mature
  
  this_area_id=area_census$areaid
  #this_area_pop=area_census$population # TODO: "population" should exist in area_census
  
  area.error=data.frame(areaid=this_area_id)# prepare the Dataframe with the errors for this area.
  #cat("\n")
  #print (area_census)
  #print(selection)
  tae=0
  for (i in 1:nvar){# for every variable in the lexicon
    column_name=colnames( lexicon )[i]# con_01, con_02
    at.census= area_census[[ as.character(lexicon[column_name]["census_row",]) ]]# cars 5, mature 6
    
    
    panel.column=as.character(lexicon[column_name]["panel_row",])
    at.selection= sum(selection[panel.column])
    #print((at.selection))
    
    this.var.error=abs(at.census - at.selection)
    #print(this.var.error)
    tae=tae+this.var.error
    area.errors[panel.column]=this.var.error
    
    #print(paste( panel.column, "Census=",at.census, "Selection=",at.selection, "Error=",this.var.error))
  }
  #print(paste("TAE:",tae))
  
  #============Delete the folowing ===============
  #print(selection)
  #con_01_error= abs( area_census$cars - sum(selection$n_cars) )
  #print(con_01_error)
  
  #matures.here=selection[selection$age=="mature",]
  #print(matures.here)
  #print(nrow(matures.here))
  #mature_error= abs( area_census$mature - nrow(matures.here) )
  
  #area.error=data.frame(areaid=this_area_id,mature_error,con_01_error)
  #errors=rbind(errors,area.error)
  #print(errors)
  
  #errors$tae=errors$mature_error + errors$con_01_error
  #tae=sum(area.errors)
  #print(tae)
  #print(total_tae)
  #print(total_tae)
  #==================================================
  return(tae)
}


##' Make a selection
##'
##' Select a number of population from panel data, to represent an area.
##' @title selection_for_area
##' @param inpanel The panel dataset
##' @param area_census A census area
##' @param inlexicon A data lexicon showing the variable associations.
##' @return list A list of results (#areaid, #selection, #error)
##' @author Dimitris Kavroudakis
##' @export
##' @examples library(sms)
##' #========================= Panel Data ============================
##' n=20
##' panel=data.frame(pid=6001:(6000+n), he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
##' #========================= Census Data ============================
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' #========================= Data linkage ===========================
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' this_area=as.data.frame(census[1,]) #Select the first area from the census table
##' sel=selection_for_area(panel,this_area, in.lexicon) #make a representation for this area.
##' print(sel) #print the representation
selection_for_area=function(inpanel,area_census, inlexicon){
  this_area_id=area_census$areaid
  this_area_pop=area_census$population
  selection=random_panel_selection(inpanel,this_area_pop)
  error=calculate_error(selection,area_census,inlexicon)
  return(list(areaid=this_area_id,selection=selection,error=abs(error)))
}

##' Plot selections
##'
##' Plot errors during selection process for an area.
##' @title Plot selection results
##' @param inresult The input selection process to plot
##' @author Dimitris Kavroudakis
##' @examples library(sms)
##' #========================= Panel Data ============================
##' n=20
##' panel=data.frame(pid=6001:(6000+n), he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
##' #========================= Census Data ============================
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' #========================= Data linkage ===========================
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' iterations=60
##' sa=run_parallel_SA(census,iterations,panel,in.lexicon)
##' plotTries(sa[[2]])
##' @export
plotTries=function(inresult){
  #print("Inresult")
  #print(nrow(inresult$selection))
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

##' Find the best selection for an area
##'
##' Calculate the best area representation, after a series of selection tries.
##' @title find_best_selection
##' @param area A census area
##' @param it Number of iterations
##' @param bhps A panel dataset.
##' @param my.lexicon The data lexicon
##' @return list A list with results (#areaid, #selection, #tae, #tries, #error_states).
##' @author Dimitris Kavroudakis
##' @export
##' @examples library(sms)
##' #========================= Panel Data ============================
##' n=20
##' panel=data.frame(pid=6001:(6000+n), he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
##' #========================= Census Data ============================
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' #========================= Data linkage ===========================
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' iterations=60
##' this_area=as.data.frame(census[1,]) #Select the first area from the census table
##' best=find_best_selection(this_area, iterations, panel, in.lexicon)
##' print(best)
find_best_selection<- function(area,it,bhps, my.lexicon){
  area_census=as.data.frame(area)
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



##' Run a simulation in serial mode
##'
##' Run a simulation in serial mode
##' @title run_parallel
##' @param census A census dataset consisting of various areas rows.
##' @param iterations The number of iterations
##' @param bhps the panel dataset
##' @param the.lexicon The data lexicon.
##' @return msm_results An object with the results of the simulation, for each area.
##' @author Dimitris Kavroudakis
##' @examples library(sms)
##' #========================= Panel Data ============================
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
##' #========================= Census Data ============================
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' #========================= Data linkage ===========================
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' iterations=70
##' results= run_parallel( census, iterations, panel, in.lexicon)
##' print(results)
##' @export
run_parallel <- function(census,iterations,bhps, the.lexicon){
  #options(cores=2)
  library(parallel)
  library(foreach)
  library(doSNOW)
  cl<-makeCluster(parallel::detectCores(),type="SOCK")
  doSNOW::registerDoSNOW(cl)
  myFunctions=c("find_best_selection",  "calculate_error", "selection_for_area", "random_panel_selection")
  msm_results=
    foreach(i=iter(census, by='row'), .export=myFunctions, combine=c) %dopar% {
      find_best_selection(i,iterations,bhps, the.lexicon)
    }
  stopCluster(cl)
  i <- NULL 
  rm(i)
  return(msm_results)
}

##' Run a simulation in serial mode
##'
##' Run a simulation in serial mode.
##' @title Run_serial
##' @param census A census dataset consisting of various areas (rows)
##' @param iterations The number of iterations
##' @param panel the panel dataset
##' @param this.lexicon The data lexicon.
##' @return msm_results An object with the results of the simulation, for each area.
##' @author Dimitris Kavroudakis
##' @examples library(sms)
##' #========================= Panel Data ============================
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
##' #========================= Census Data ============================
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' #========================= Data linkage ===========================
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' iterations=70
##' results= run_serial( census, iterations, panel, in.lexicon)
##' print(results)
##' @export
run_serial <- function(census,iterations,panel, this.lexicon){
  library(parallel)
  library(foreach)
  library(doSNOW)
  myFunctions=c("find_best_selection_SA", "calculate_error", "selection_for_area", "random_panel_selection")
  msm_results=
    foreach(i=iter(census, by='row'), .export=myFunctions, combine=c) %do% {
      find_best_selection(i,iterations,panel, this.lexicon)
    }
  i <- NULL 
  rm(i)
  return(msm_results)
}

##' Run a simulation in parallel mode with Simulated annealing
##'
##' @title find_best_selection_SA
##' @param area_census A census dataset consisting of various areas rows.
##' @param iterations The number of iterations
##' @param bhps the panel dataset
##' @param in.lexicon The data lexicon.
##' @return msm_results An object with the results of the simulation, of this area.
##' @author Dimitris Kavroudakis
##' @examples library(sms)
##' #========================= Panel Data ============================
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
##' #========================= Census Data ============================
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' #========================= Data linkage ===========================
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' this_area=as.data.frame(census[1,]) #Select the first area from the census table
##' iterations=70
##' myselection= find_best_selection_SA( iterations, panel, this_area, in.lexicon)
##' print(myselection)
##' @export
find_best_selection_SA <- function (iterations, bhps, area_census, in.lexicon) {
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


##' Run a simulation in parallel mode with Simulated annealing
##'
##' @title run_parallel_SA
##' @param census A census dataset consisting of various areas rows.
##' @param iterations The number of iterations
##' @param bhps the panel dataset
##' @param in.lexicon The data lexicon.
##' @return msm_results An object with the results of the simulation, for each area.
##' @author Dimitris Kavroudakis
##' @examples library(sms)
##' #========================= Panel Data ============================
##' n=20
##' panel=data.frame(pid=6001:6020, he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
##' #========================= Census Data ============================
##' census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
##' #========================= Data linkage ===========================
##' con_01=c("he","he" )
##' con_02=c("fem","females" )
##' in.lexicon=data.frame(con_01,con_02 )
##' row.names(in.lexicon)=c("census_row","panel_row")
##' 
##' iterations=70
##' results= run_parallel_SA( census, iterations, panel, in.lexicon)
##' print(results)
##' @export
run_parallel_SA <- function(census,iterations,bhps, in.lexicon){
  #options(cores=2)
  library(parallel)
  library(foreach)
  library(doSNOW)
  cl<-makeCluster(parallel::detectCores(),type="SOCK")
  doSNOW::registerDoSNOW(cl)
  myFunctions=c("find_best_selection_SA", "calculate_error", "selection_for_area", "random_panel_selection")
  msm_results=
    foreach(i=iter(census, by='row'), .export=myFunctions, combine=c) %dopar% {
      find_best_selection_SA(iterations,bhps,i,in.lexicon)
    }
  stopCluster(cl)
  i <- NULL 
  rm(i) 
  return(msm_results)
}





##' Generate small area population microdata from census and panel datasets. Fit the panel data to census area descriptions.
##' 
##' Small area population estimates.
##' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
##' @name sms-package
##' @docType package
##' @title Spatial Microsimulation Library
NULL
