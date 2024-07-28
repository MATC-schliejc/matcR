#may need this if you cannot remove app data path
myPaths <- .libPaths()   # get the paths
myPaths <- c("C:/R/R-4.3.1", myPaths)  # switch them  AND UPDATE R VERSION
.libPaths(myPaths)  # reassign them


install.packages('C:\\local\\matcR_0.2.3.tar.gz', repos = NULL, type="source")
