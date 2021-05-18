################################################################################
## @Title: zzz.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 12.05.2020
## @Last updated: 01.27.2020
## @Status: Active
## ------------------------------------------------------------------------------
## Notes:
## Replace 'httr' by 'curl' or viceversa to get only 1 dependency check "connect"
##  family of fx 
## Clean list of packages and review to use "Suggest" category when appropriate
## ------------------------------------------------------------------------------

## NAMESPACE defintion
#' @import curl
#' @import dplyr
#' @import forcats
#' @import ggforce
#' @import ggplot2
#' @import ggrepel
#' @importFrom graphics title
#' @importFrom grDevices dev.off pdf
#' @import haven
#' @importFrom httr authenticate GET content
#' @importFrom  htmlwidgets saveWidget
#' @importFrom irr kappam.fleiss
#' @importFrom jsonlite fromJSON
#' @import labelled
#' @import lubridate
#' @importFrom magrittr "%>%"
#' @importFrom MASS fractions
#' @importFrom matrixStats weightedMedian
#' @import openxlsx
#' @rawNamespace import(plotly, except = last_plot)
#' @importFrom pps ppss
#' @importFrom psych ICC alpha
#' @import purrr
#' @import readxl
#' @import rlang
#' @importFrom rlist list.subset
#' @importFrom robustbase adjboxStats
#' @import scales
#' @import srvyr
#' @rawNamespace import(stats, except = c(lag, filter))
#' @import stringr
#' @import tibble
#' @import tidyr
#' @import utils
#' @importFrom webshot2 webshot


.onLoad <- function(...) {
  ##  Create directories
  dir.create(paste0(getwd(), "/output_ddbb"), showWarnings = FALSE)
  dir.create(paste0(getwd(), "/output_plots"), showWarnings = FALSE)
  dir.create(paste0(getwd(), "/output_tables"), showWarnings = FALSE)
}

.onAttach <- function(...) {
  # print(utils::packageDescription("R.HNAP", fields = c("Package", "Version",
  #                                                      "Title",   "Imports")))

  packageStartupMessage(
    "Package: ", utils::packageDescription("R.HNAP", fields = "Package"), "\n",
    "Version: ", utils::packageDescription("R.HNAP", fields = "Version"), "\n",
    "Description: ", utils::packageDescription("R.HNAP", fields = "Title"), "\n",
    "Imports: ", utils::packageDescription("R.HNAP",
      fields = "Imports",
      drop = TRUE
    ), "\n",
    "\nYour working directory is: ", getwd(), "\n",
    "\nThe following folder have been added to store outputs:\n",
    "~/output_ddbb\n",
    "~/output_tables\n",
    "~/output_plots\n"
  )
}
# 
# .onLoad(pkgname = "R.HNAP")
# .onAttach(pkgname = "R.HNAP")
# rm(.onLoad)
# rm(.onAttach)
