
ctlg <- catalog("/DATA/LIDAR GIZ/LASOUTLIERS/line_1/")
opt_chunk_buffer(ctlg) <- 0
opt_chunk_size(ctlg)   <- 0 
opt_cores(ctlg) <- 2

print_ctg <- function(chunk)
{
  # read chunk ----
  las <- readLAS(chunk)
  if(is.empty(las)) 
  {
    print(chunk@files)
    return(NULL)
  }
  
  print(paste("Class:\n", class(chunk)))
  print(paste("Structure:\n", str(chunk)))
  print(paste("Input (chunk) name:\n", chunk@files))
  print(paste("Output (chunk) save:\n", chunk@save))
}

catalog_apply(ctlg, print_ctg)
