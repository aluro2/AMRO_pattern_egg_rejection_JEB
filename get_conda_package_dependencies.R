deps <- renv::dependencies()

pkg_names <- paste("r", 
                   sort(unique(deps$Package)),
                   sep = "-") 

pkg_names <- as.data.frame(paste("- ", pkg_names))

write.table(
  pkg_names,
  file = "package_dependencies.tsv",
  quote=FALSE,
  sep='\t',
  col.names = FALSE,
  row.names = FALSE
)
