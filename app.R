# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
library(ZetaAnalyticsTB)
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
ZetaDashboard::run_app() # add parameters here (if any)
# ZetaDashboard::run_app()
