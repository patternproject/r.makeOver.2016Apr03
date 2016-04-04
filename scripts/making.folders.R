## This file sets the state ####

my.f <- function(fname)
{
  my.path = paste0("/mnt/r.makeOver.2016Apr03/",fname)
  dir.create(my.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  
} # end my.f

my.f("data")
my.f("write_up")
my.f("scripts")
my.f("figures")