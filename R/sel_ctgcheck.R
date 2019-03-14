# test putting all selected las in one folder and process with cores
# this another way to check selected las
# and much faster

library(lidR)

rm(list=ls())
ctg <- catalog("PROCESSED_DATA/SEL_PREPROCESS/")
opt_cores(ctg) <- 1
opt_chunk_size(ctg) <- 0
opt_chunk_buffer(ctg) <- 0

ctgCheck <- function(x)
{
  
  fname <- strsplit(x@files, "/")[[1]][3] 
  print(str(x))
  invisible(readline(prompt = "Press [enter] to continue"))
  
  las <- readLAS(x, filter = "-drop_z_below 0")
  
  if(is.empty(las)) return(NULL)
  
  writeLines("\n ============================== \n")
  cat(fname)
  # lascheck
  lascheck(las)
  
  # print las info
  writeLines("\n")
  print(fname)
  print(las)
  
  # classification
  writeLines("\n")
  cat("Unique class: ",unique(las@data$Classification))
  print(table(las@data$Classification))
  
  # height
  writeLines("\n")
  cat("MinZ:", min(las@data$Z), " MaxZ:", max(las@data$Z))
  graphics::hist(las@data$Z, main=fname, breaks=15)
  writeLines("\n")
  
  invisible(readline(prompt = "Press [enter] to continue"))
}

flgCheck <- catalog_apply(ctg, ctgCheck)


