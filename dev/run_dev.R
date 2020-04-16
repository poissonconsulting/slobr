
rm(list=ls(all.names = TRUE))

all_attached <- paste("package:", names(sessionInfo()$otherPkgs),
                      sep = "")
try(suppressWarnings(lapply(all_attached, detach, character.only = TRUE,
                            unload = TRUE)), silent = TRUE)

slobr::run_slobr("demo")
