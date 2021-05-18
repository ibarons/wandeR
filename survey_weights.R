################################################################################
## @Title: survey_weights.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 23.02.2020
## @Last updated:
## @Status: Maturing
## ------------------------------------------------------------------------------
##
## Notes:
## Include a char. vector or dots to define the strata variables as argument
## 
## `poststrata` argument turn it into a character vector to define multiple  
## variable by which postratify
##
## admin4Pcode tunr into PSU argument with symbol (same as strata)
##
## Pending to add "k" as cluster size argument for stage 2 prob unequal. 
## Furthermore instead of arg `psu.as.clust` you can create arg cluster.size and
## if psu.as.clust == TRUE then state cluster.size = "psu.var.name" (remove psu.as.clust)
##
## ------------------------------------------------------------------------------
################################################################################
## ----------------------------------------------------------------------------
#' @name SurveyWeight
#' @title Calculate sample weights.
#'
#' @description Calculate sample design weights (basic + response) and 
#' post-stratification for multiple sample designs. It offer basic check to 
#' explore the resulting weights.
#'
#' @usage
#' SurveyWeight(db, strata, stages = 2, probs = "equal", fix.p = FALSE, 
#' response = TRUE, poststrata = NULL, psu.as.clust = TRUE,
#' checks = FALSE, export.log = FALSE)
#'
#' @param db a df containing with as many rows as sampled SSU. It should contain
#' at least the following variables: `Nh`, `nh`, `Mi`, `Mh`, `mi`, `mh` and the 
#' `strata` variables
#' @param strata String with the name of the variable for the strata. if multiple
#' variables define the strata, paste them into one string.
#' @param stages integer defining numebr of stages
#' @param probs string defining the "equal" or "unequal" probabilities
#' @param psu.as.clust logical should PSU be considered as cluster, or a subgroup
#' of SSU will be taken frome the PSU 
#' @param response Logical shoudl response weights need ot be calculated. `db` should
#' then include the variable `mi.r`
#' @param poststrata String with the name of the variables by which post-stratify
#' @param fix.p fix probabilities > 1 and turn the to 1.
#' @param checks display checks to explore the reliability of weights
#' @param export.log Logical should db of weights and all its intermediate
#' steps be exported
#'
#' @details
#' Notation for variable names in `db`:
#' 
#' j = jth SSU
#' i = ith PSU
#' d = dth Domain
#' h = hth Strata
#' 0 = Total Population/Sample
#' 
#' Nd = number of PSU in the d th domain
#' Nh = number of PSUs in the h th strata
#' N0 = number of PSUs in the population
#' 
#' Mi = number of SSUs in i th PSU
#' Md = number of SSU in the d th domain
#' Mh = number of SSUs in the h th strata
#' M0 = numebr of SSUs in the population
#' Miw = number fo SSU in the ith PSU after weighting
#
#' nd = number of PSU in the sample of the d th domain
#' nh = number of PSUs in the sample of the h th strata
#' n0 = number of PSUs in the sample
#' 
#' mi = number of SSUs in the sample of i th PSU
#' md = number of SSU in the sample of the d th domain
#' mh = number of SSUs in the sample of the h th strata
#' m0 = numebr of SSUs in the sample of the population
#' 
#' mi.r = Responded (sampled) number of SSUs in the sample of i th PSU
#' 
#' #' W = weight
#' Wij.bas = Basic weight for the j SSU in the ith PSU
#' Wij.res = Response weight for the j SSU in the ith PSU
#' Wij.des = Design (Basic + response) weights for j SSU in the ith PSU
#' Wij.cal = Calibration weight for the j SSU in the ith PSU
#' Wij.pos = Post-stratification weight for the j SSU in the ith PSU
#' Wijh.des = Design weight for the j SSU in the ith PSU in the hth strata
#' 
#' P = probability
#'
#' @references
#' 
#' Lohr, S. L. (2010). Sampling: Design and Analysis. Brook/Cole. 
#' http://www.ru.ac.bd/stat/wp-content/uploads/sites/25/2019/03/407_07_Lohr-Sampling_-Design-and-Analysis.pdf
#' 
#' United Nations Statistics Division (Ed.). (2008). Designing household survey 
#' samples: Practical guidelines. United Nations. 
#' https://unstats.un.org/unsd/demographic/sources/surveys/Series_F98en.pdf
#' 
#' ICF International. (2012). Demographic and Health Survey Sampling and 
#' Household Listing Manual. ICF International; DHS Toolkit. 
#' https://dhsprogram.com/publications/publication-dhsm4-dhs-questionnaires-and-manuals.cfm
#'
#' @return a list containing the input data frame with all weights calculation 
#' added as columns. If `checks = TRUE` it add to the list: 
#'  1) comparision between weighted population and reference population at the 
#'  lowest strata level; 
#'  2) descriptive summary of the weights; 
#'  3) analysis of correlation between weight and reference population. 
#' It add two plot: 
#'  1) to visualize linearity between weights and reference population; 
#'  2) Distribution fo PSU size of the reference population
#'
#' @export
## ----------------------------------------------------------------------------

SurveyWeight <- function(db, strata, stages = 2, probs = "equal", fix.p = FALSE, 
                         response = TRUE, poststrata = NULL, psu.as.clust = TRUE,
                         checks = FALSE, export.log = FALSE) {
  
  out <- c() # Output storage
  strata.var <- rlang::sym(strata)
  
  ###### CHECK FOR MAJOR IRREGULARITIES
  
  ### PENDING ERROR THE MINIMUM VAIRBLES NEEDED ARE NTO FOUDN FOR YOU MODEL. CHECK THE NAMES
  
  
  if ("Nh" %in% colnames(db) & any(is.na(db$Nh), is.na(db$nh))) {
    stop("There are missing values in your PSU")
  }
  if ("Nh" %in% colnames(db) & any(is.na(db$Mi), is.na(db$mi))) {
    stop("There are missing values in your SSU")
  }
  if ("Nh" %in% colnames(db) & any(db$Nh == 0)) {
    stop("Number of PSU = 0")
  }
  if ("Nh" %in% colnames(db) & any(db$Mi == 0)) {
    stop("Number of SSU = 0")
  }

  if ("Nh" %in% colnames(db) & any(db$Nh < db$nh) & fix.p == FALSE) {
    warning(
      "Some strata have Nh < nh, thus probability of inclusion > 1.
  Please, fix these cases or turn `fix.p = TRUE` to equal these probabilities to 1\n
  The affected PSU are: ",
      paste(
        unlist(unique(db[which(db$Nh < db$nh), grep("^admin3", colnames(db))])),
        collapse = "; "
      )
    )
  }
  
  if ("Mi" %in% colnames(db) & any(db$Mi < db$mi) & fix.p == FALSE) {
    warning(
      "Some communities have Mi < mi, thus probability of inclusion > 1.
  Please, fix these cases or turn `fix.p = TRUE` to equal these probabilities to 1\n
  The affected PSU are: ",
      paste(
        unlist(unique(db[which(db$Mi < db$mi), grep("^admin4", colnames(db))])),
        collapse = "; "
      )
    )
  }
  
  ################################# BASIC WEIGHTS ################################
  
  db <- db %>%
    {
      if (stages == 1 & probs == "equal") {
        
        # SRS / 1-Stage cluster equal probabilities (equal or unequal sizes) (Lohr (2010) pp. 171, 179)
        dplyr::mutate(., 
          Pi.bas = nh / Nh, # PSU
          Wij.bas = 1 / Pi.bas
        ) 
      } else if (stages == 1 & probs == "unequal") {
        
        # 1-stage sample with unequal probabilities (e.g. PPS) with replacement (Lohr (2010) p.231; 234)
        dplyr::mutate(.,
          Pi.bas = Mi / Mh, # SSU - probability of PSU is defined by the size Mi
          Wij.bas = 1 / Pi.bas
        )
      } else if (stages == 2 & probs == "equal") {
        
        # 2-stages with equal probabilities with replacement (Lohr (2010) p.184)
        dplyr::mutate(., 
          Pi.bas = nh / Nh, # PSU
          Pj.bas = mi / Mi, # SSU
          Pij.bas = Pi.bas * Pj.bas,
          
          Wij.bas = 1 / Pij.bas
        ) 
      } else if (stages == 2 & probs == "unequal" & psu.as.clust == TRUE) {
        
        # 2-stages with unequal probabilities (e.g. PPS) with replacement (Lohr (2010) p.236)
        dplyr::mutate(., 
          Pi.bas = (nh * Mi) / Mh, # n = nh number of PSU to be selected in each strata (no sub-groups defined for the PSU: Clusters = PSU)
          Pj.bas = 1 / Mi, # SSU | for this case 1 refer to the number of SSU unit (mi) that will be selected every time the PSU is selected
          Pij.bas = Pi.bas * Pj.bas,
          Wij.bas = 1 / Pij.bas,
        ) 
      } else if (stages == 2 & probs == "unequal" & psu.as.clust == FALSE) {
        
        # 2-stages with unequal probabilities (e.g. PPS) with replacement (Lohr (2010) p.236; see REACH note on HNAP HHS June 2006)
        dplyr::mutate(., 
          Pi.bas = (k * Mi) / Mh, # k = number of groups of HH (cluster) of k.size to be selected in each PSU (location) (k subgroups of k.size defined for each PSU: Clusters != PSU)
          Pj.bas = k.size / Mi, # SSU | k.size =  the size of the groups of HH (cluster) to be selected every time the PSU is selected 
          Pij.bas = Pi.bas * Pj.bas,
          Wij.bas = 1 / Pij.bas,
        ) 
      }
    }
  
  ###### FIX P > 1 = 1
  
  if (fix.p == TRUE) {
    
    db <- db %>%
      dplyr::mutate(
        #Mi = dplyr::if_else(mi.r > Mi, as.numeric(mi.r), Mi), # when mi > Mi... correct the sample frame or correct the wight..??
        dplyr::across(dplyr::matches("^(Pi.bas|Pj.bas)$"), ~ dplyr::if_else(. > 1, 1, .))
      ) %>%
      {
        if ("Pij.bas" %in% colnames(db)) {
          dplyr::mutate(.,
            Pij.bas = Pi.bas * Pj.bas,
            Wij.bas = 1 / Pij.bas, 
          )
        } else
          dplyr::mutate(., Wij.bas = 1 / Pi.bas)
      }
  }
  
  ############################# RESPONSE WEIGHTS #################################
  
  if (response == TRUE) {
    db <- db %>% 
      dplyr::mutate(
        Pij.res = mi.r / mi,
        Wij.res = 1 / Pij.res,
        Wij.des = Wij.bas * Wij.res,
      
        prop.extra = Mi / Wij.des, # each SSU proportion extra of their PSU population to cover not sample SSU
      )   
  }
  
  ############################  POST-STRATIFICATION ##############################
  
  if (!is.null(poststrata)) {
    
    postrata.var <- rlang::sym(poststrata)
    
    db <- db %>%
    dplyr::group_by(., !!strata.var) %>%
      {
        if ("Wij.des" %in% colnames(db)) {
          dplyr::mutate(.,
            Pij.adj = sum(Wij.des, na.rm = TRUE) / !!postrata.var ,
            Wij.adj = 1 / Pij.adj,
            Wij = Wij.des * Wij.adj,
            
            prop.extra = Mi / Wij, # each SSU proportion extra of their PSU population to cover not sample SSU
          )
        } else
          dplyr::mutate(.,
            Pij.adj = sum(Wij.bas, na.rm = TRUE) / !!postrata.var ,
            Wij.adj = 1 / Pij.adj,
            Wij = Wij.bas * Wij.adj,
            
            prop.extra = Mi / Wij, # each SSU proportion extra of their PSU population to cover not sample SSU
          )
      } %>%
      dplyr::ungroup()
  }
  
  out[["weights"]] <- db
  
  ########## CHECK WEIGHTS COHERENCY
  
  w.bas <- rlang::sym("Wij.bas")
  
  if ("Wij" %in% colnames(db)) {
    w <- rlang::sym("Wij")
  } else if ("Wij.des" %in% colnames(db)) {
    w <- rlang::sym("Wij.des")
  } else {
    w <- w.bas
  }
  
  # Check match with reference population
  
  # !!! NEED TO FIX THIS. YOU HAVE THE PSU LEVEL (ADMIN4) WITH THIS YOU CNA GET ADMIN0 OUT OF CHECK = TRUE
  # CONDITION AND MOVE ADMIN3 AND (ADMIN1 - SHALL WE KEEP IT?) INTO CHECK = TRUE CONDITION
  
  
  wdist.admin4 <- db %>%
    dplyr::group_by(admin1Pcode, !!strata.var, admin4Pcode) %>%
    dplyr::summarise(
      Wij.bas = sum(Wij.bas),
      !!w := sum(!!w),
      Mi = Mi[1],
      .groups = "drop"
    ) 
  
  wdist.admin3 <- db %>%
    dplyr::group_by(admin1Pcode, !!strata.var) %>%
    dplyr::summarise(
      Mhw.bas = sum(!!w.bas),
      Mhw = sum(!!w),
      Mh = Mh[1],
      .groups = "drop_last"
    ) %>%
    dplyr::mutate(abs.diff = abs(Mhw - Mh), perc.diff = abs.diff / Mh) %>%
    dplyr::arrange(dplyr::desc(abs.diff))
  
  wdist.admin1 <- wdist.admin3 %>%
    dplyr::summarise(
      Mw.bas = sum(Mhw.bas),
      Mw = sum(Mhw),
      M = sum(Mh),
      .groups = "drop_last"
    ) %>%
    dplyr::mutate(abs.diff = abs(Mw - M), perc.diff = abs.diff / M) %>%
    dplyr::arrange(dplyr::desc(abs.diff))
  
  wdist.admin0 <- wdist.admin1 %>%
    dplyr::summarise(
      M0w.bas = sum(Mw.bas),
      M0w = sum(Mw),
      M0 = sum(M),
      .groups = "drop_last"
    ) %>%
    dplyr::mutate(abs.diff = abs(M0w - M0), perc.diff = abs.diff / M0) %>%
    dplyr::arrange(dplyr::desc(abs.diff))


  if (checks == TRUE) {
    
    # comparision frame VS weight at lowest level
    out[["compare"]] <- wdist.admin3
    
    # Weights descriptive summary
    sample <- unique(db[, c(strata, "nh", "mh")])
    wsumm <- data.frame(
      sample,
      sd = sapply(sample[,1][[1]], function(x) 
        sd(unlist(db[which(db[, strata][[1]] == x), as.character(w)]))
      ),
      t(sapply(sample[,1][[1]], function(x)
        summary(unlist(db[which(db[, strata][[1]] == x), as.character(w)])))
      ),
      row.names = NULL
    ) %>%
      dplyr::relocate(Mean, .before = sd)

    out[["summary"]] <- wsumm 
    
    # Check linearity with reference population.
    # 2 stage equal prob: the greater the population, the greater the weight (diagonal stick)
    # 2 stage unequal prob: same weight for all (straight stick)
    n.admin <- as.character(sample(db[, strata][[1]], 25))
    
    p <- wdist.admin4 %>%
      dplyr::filter(!!strata.var %in% n.admin) %>%
      tidyr::pivot_longer(c(Wij.bas, !!w), names_to = "Type", values_to = "Weights") %>%
      ggplot2::ggplot(ggplot2::aes(Weights, Mi, color = Type)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(. ~ eval(strata.var), scales = "free")
    
    print(p)
    
    cor <- sapply(unique(wdist.admin4[, strata][[1]]), function (x) cor(
      unlist(wdist.admin4[which(wdist.admin4[, strata][[1]] %in% x), as.character(w)]),
      unlist(wdist.admin4[which(wdist.admin4[, strata][[1]] %in% x), "Mi"]),
    ))
    
    out[["linearity"]] <- cor 
    
    
    # Lohr 2010 p.187
    # Note the wide variation in the means from clutch to clutch. This
    # indicates that eggs within the same clutch tend to be more similar than two randomly selected
    # eggs from different clutches, and that clustering does not provide as much information per egg
    # as would an SRS of eggs.
    # ----
    # Here is use with population size. Better to use a variable of interest (for 
    # Lohr case was egg volume, for me might be income or type of shelter) 

   p1 <- wdist.admin4[which(wdist.admin4[, strata][[1]] %in% n.admin), ] %>%
      ggplot2::ggplot(ggplot2::aes(admin4Pcode, Mi)) +
      ggplot2::geom_point() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7)) +
      ggplot2::facet_wrap(. ~ admin1Pcode, scales = "free")
    
    print(p1) ## FIND A WAY TO ORDER THE X AXIS FROM LOWEST TO HIGHEST... TO SEE LINEAR PATTER...
    
  }

  ####################################### OUTPUT ###############################
  
  cat("\nWeight type:", as.character(w), "\n")
  
  cat("\nWeight Summary:\n")
  print(db %>% dplyr::pull(!!w) %>% summary(.))
  
  cat("\nCompare weighted to frame population:\n")
  print(as.data.frame(wdist.admin0))
  
  cat(
    "\nCorrelation weight to frame:", 
    cor(dplyr::pull(wdist.admin4, !!w), wdist.admin4$Mi)
  ) 
  
  if (export.log == TRUE) {
    
    wb <- R.HNAP::ExportXLSX(list(weights.log = list(db = db)))
    openxlsx::saveWorkbook(
      wb,
      paste0("output_tables/", R.HNAP::DateStamp("_weights.xlsx")),
      overwrite = TRUE
    )
    cat(
      "\nWeights log exported to:\n",
      paste0("~/output_tables/", R.HNAP::DateStamp("_weights.xlsx")), "\n"
    )
    
  }
  
  return(out)
}

 




# 
# sframe <- R.HNAP::CleanIt(readxl::read_excel(
#   "../db/Survey_WoS_2101_frame.and.sample_final.xlsx", guess_max = 9000
# )) %>%
#   dplyr::filter(`Frame Base Dec2020` == 1) %>%
#   dplyr::rename(
#     "admin3" = `Sub-district Pcode`, "admin4" = `Location Pcode`,
#     "Mi" = `HH Dec2020`, "mi" = `nHH Frame Base Dec2020`
#   ) %>%
#   dplyr::add_count(admin3, wt = Mi, name = "Mh") %>%
#   dplyr::add_count(admin3, name = "Nh") %>%
#   dplyr::add_count(admin3, wt = mi, name = "mh") %>%
#   dplyr::group_by(admin3) %>%
#   dplyr::mutate(nh = sum(dplyr::if_else(mi > 1, 1, 0))) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(admin3, admin4, Nh, nh, Mh, Mi, mh, mi)
# 
# 
# db.hh <- haven::read_spss("../db/Survey_WoS_2101_hh.sav")
# n.psu <- dplyr::count(db.hh, admin1, admin3, admin4, name = "mi.r") %>%
#   dplyr::add_count(admin3, wt = mi.r, name = "mh.r") %>%
#   dplyr::add_count(admin3, name = "nh.r")
# 
# 
# slog <- dplyr::left_join(
#   n.psu, sframe[, -grep("^admin3$", colnames(sframe))], by = "admin4"
# )
# 
# # write.csv(slog, "sample.log.csv")
# 
# db <- dplyr::left_join(
#   db.hh[, grep("^admin\\d$|hh.uuid", colnames(db.hh))], slog,
#   by = c("admin1", "admin3", "admin4")
# ) %>%
#   dplyr::filter(admin4 != "CP001190")
# 
# 
# 
# x <- SurveyWeight(db, stages = 2, probs = "equal", psu.as.clust = TRUE,
# response = TRUE, poststrata = TRUE, fix.p = TRUE, checks = TRUE)
# 
# weights <- sample.we %>%
#   dplyr::mutate(
#     # Basic weights
#     Pi.bas = nh / Nh, # PSU
#     Pj.bas = mi / Mi, # SSU
#     Pij.bas = Pi.bas * Pj.bas,
#     
#     Wij.bas = 1 / Pij.bas
#   ) %>%
#   dplyr::mutate(
#     # Response weights
#     Pij.res = (Wij.bas * mi.r) / (Wij.bas * mi),
#     Wij.res = 1 / Pij.res,
#     
#     Wij.des = Wij.bas * Wij.res
#   ) %>%
#   
#   dplyr::group_by(., admin3) %>%  
#   dplyr::mutate(.,
#     Pij.adj = sum(Wij.des, na.rm = TRUE) / Mh ,
#     Wij.adj = 1 / Pij.adj,
#     Wij = Wij.des * Wij.adj,
#   )












