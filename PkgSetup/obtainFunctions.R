#script to setup package build

#url link
url <- ""

#destination folder
destfile <- file.path(getwd(),"R","functions.R")

#download file
download.file(url,destfile)
