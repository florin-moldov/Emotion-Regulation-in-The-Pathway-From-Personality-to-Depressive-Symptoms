source("renv/activate.R")

if (interactive() && Sys.getenv("TERM_PROGRAM") == "vscode") {
  # Safe check for httpgd
  if (requireNamespace("httpgd", quietly = TRUE)) {
    options(device = function(...) httpgd::httpgd())
  }
  
  # Trigger the VS Code extension to watch this session
  if (exists(".vsc.attach")) .vsc.attach()
}