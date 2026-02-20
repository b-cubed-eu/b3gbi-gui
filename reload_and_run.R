# Force reload script
rm(list = ls())
files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for(f in files) {
  message("Loading: ", basename(f))
  source(f, local = FALSE)
}
shiny::runApp(".", port = 4260)
