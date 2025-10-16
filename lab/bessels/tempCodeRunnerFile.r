- sub("^--file=", "", args[grep("--file=", args)])
this_dir <- if (length(script_path)) dirname(normalizePath(script_path)) else getwd()
setwd(this_dir)
cat("Saving files to:", this_dir, "\n")