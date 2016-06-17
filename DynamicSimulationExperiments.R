rm(list = ls())
dirname = getSrcDirectory(function(x) {x})
setwd(dirname)
require('testthat')
source('Datasets.R')
source('DynamicSimulation.R')
error.file = paste(getwd(),"/Errors.dat",sep="")


msgDebug = T
pdatainitial = 0.5

nseeds = 100
seedspool = sample(1:1e+05,nseeds,replace=F)


for(seed in seedspool){
  set.seed(seed)
  name = sample(names.datasets,1)
  
  data = prepare.data(name)
  classes = as.matrix(data[[2]])
  data = as.matrix(data[[1]])
  
  nclasses = length(unique(classes))
  ndata = nrow(data)
  if(msgDebug){
    cat("\nSeed:",seed)
    cat("\nDataset:",name)
    cat("\nndata:",ndata)
    cat("\nnclasses:",nclasses)
  }
  
  result.tests = test_file("testDynamicSimulation.R",reporter = "summary")
  
  ntests = length(result.tests)
  for(i in 1:ntests){
    nresults = length(result.tests[[i]]$results)
    
    for (j in 1:nresults){
      txt = as.character(result.tests[[i]]$results[[j]])
      if (grepl("failure",txt)){
        st = result.tests[[i]]$results[[j]]$message
        st = str_replace_all(st,"\n",". ")
        aux = c(seed,st)
        write(aux,error.file,ncolumns=2,append=T,sep="\t")
        if(msgDebug){
          cat("\n",aux)
        }
      }
    }
  }
  
}

