#' @title End questionnaires Analysis - end.quest
#' @description Perform a univariate ANOVA on each questionnaire scores. Also, print the tabulation of some questionnaires.
#' @param data data frame
#' @param quest Ending point questionnaires: BDI, STAIR, STAXI, MRS, SRS -> consider upper case.
#' @param tabulation Results of the tabulations, the default value is false. The results includes the qualitative result according the spanish sample.
#' @return univariate anova table for: variability in each experimental group. And if it is the case, the results of the tabulations.
#' @examples
#' df<-data.frame('your data')
#' end.quest(df, quest = 'BDI')
#' end.quest(df, quest = 'BDI', tabulation = T)
#' end.quest(df, 'BDI', T)
#'


end.quest <- function(data, quest = NULL, tabulation = F){
  ifelse(require(magrittr) == T, 'Loaded', 'Not Loaded')
  data <- dplyr::transmute(data, id = id, Gender = Gender, induction = induction,
                           BDI = BDI, STAXIR = STAXIRASGO, STAIR = STAIR, MRS = MRS, SRS = SRS)
  data <- dplyr::arrange(data, induction, id) %>% na.omit(data)

  if(!is.null(quest)){
    if(quest == 'BDI' || quest == 'STAIR' || quest == 'STAXIR' || quest == 'MRS' || quest == 'SRS') {
      aov <- data %>%
        rstatix::anova_test(
          dv = all_of(quest), between = induction,
          effect.size = 'pes'
        ) %>%
        rstatix::get_anova_table()
    } else {
      stop('quest only can be BDI, STAIR, STAXIR, MRS, SRS')
    }
  } else {
    stop('quest is a mandatory parameter.')
  }
  cat('Univariate-ANOVA on', quest, 'scores.', sep = ' ', fill = T)
  print(aov)

  if(tabulation){
    if(quest == 'BDI'){
      bdi <- data %>% dplyr::select(id, Gender, induction, BDI)
      dep.rate <- rep(NA, nrow(bdi))

      for(i in 1:nrow(bdi)){
        if(bdi[i,3] >= 0 && bdi[i,4] <= 13){
          dep.rate[i] <- 'Minima'
        } else if(bdi[i,4] >= 14 && bdi[i,4]<=18) {
          dep.rate[i] <- 'Leve'
        } else if(bdi[i,4] >= 19 && bdi[i,4] <= 27) {
          dep.rate[i] <- 'Moderada'
        } else if(bdi[i,4] >= 28 && bdi[i,4] <= 63) {
          dep.rate[i] <- 'Grave'
        } else{
          dep.rate[i] <- 'ERROR'
          stop('Value out of range in BDI')
        }
      }
      bdi <- dplyr::mutate(bdi,
                           dep.rate = dep.rate)
      results <- bdi

    } else if(quest == 'STAIR'){
      stair <- data %>% dplyr::select(id, Gender, induction, STAIR)
      anx.trait <- rep(NA, nrow(stair))

      for(i in 1:nrow(stair)){
        if(stair[i,2] == 'M'){
          if(stair[i,4] >= 0 && stair[i,4] <= 13){
            anx.trait[i] <- 'Bajo'
          } else if(stair[i,4] >= 14 && stair[i,4] <= 18){
            anx.trait[i] <- 'Bajo Promedio'
          } else if(stair[i,4] == 19){
            anx.trait[i] <- 'Promedio'
          } else if(stair[i,4] >= 20 && stair[i,4] <= 25){
            anx.trait[i] <- 'Sobre Promedio'
          } else if(stair[i,4] >= 26 && stair[i,4] <= 60){
            anx.trait[i] <- 'Alto'
          } else {
            anx.trait[i] <- 'ERROR'
            stop('Value out of range in STAIR')
          }
        } else if(stair[i,2] == 'F'){
          if(stair[i,4] >= 0 && stair[i,4] <= 16){
            anx.trait[i] <- 'Bajo'
          } else if(stair[i,4] >= 17 && stair[i,4] <= 23){
            anx.trait[i] <- 'Bajo Promedio'
          } else if(stair[i,4] >= 24 && stair[i,4] <= 25){
            anx.trait[i] <- 'Promedio'
          } else if(stair[i,4] >= 26 && stair[i,4] <= 32){
            anx.trait[i] <- 'Sobre Promedio'
          } else if(stair[i,4] >= 33 && stair[i,4] <= 60){
            anx.trait[i] <- 'Alto'
          } else {
            anx.trait[i] <- 'ERROR'
            stop('Value out of range in STAIR')
          }
        } else {
          stop('Gender must be M or F.')
        }
      }
      stair <- dplyr::mutate(stair,
                             anx.trait = anx.trait)
      results <- stair

    } else if(quest == 'STAXIR'){
      staxir <- data %>% dplyr::select(id, Gender, induction, STAXIR)
      rage.trait <- rep(NA, nrow(staxir))

      for(i in 1:nrow(staxir)){
        if(staxir[i,2] == 'M'){
          if(staxir[i,4] >= 0 && staxir[i,4] <= 16){
            rage.trait[i] <- 'Nulo'
          } else if(staxir[i,4] >= 17 && staxir[i,4] <= 18){
            rage.trait[i] <- 'Bajo'
          } else if(staxir[i,4] >= 19 && staxir[i,4] <= 21){
            rage.trait[i] <- 'Moderado'
          } else if(staxir[i,4] >= 22 && staxir[i,4] <= 24){
            rage.trait[i] <- 'Alto'
          } else if(staxir[i,4] >= 25 && staxir[i,4] <= 40){
            rage.trait[i] <- 'Muy Alto'
          } else {
            rage.trait[i] <- 'ERROR'
            stop('Value out of range in STAXIR')
          }
        } else if(staxir[i,2] == 'F'){
          if(staxir[i,4] <= 17){
            rage.trait[i] <- 'Nulo'
          } else if(staxir[i,4] >= 18 && staxir[i,4] <= 20){
            rage.trait[i] <- 'Bajo'
          } else if(staxir[i,4] >= 21 && staxir[i,4] <= 23){
            rage.trait[i] <- 'Moderado'
          } else if(staxir[i,4] >= 24 && staxir[i,4] <= 25){
            rage.trait[i] <- 'Alto'
          } else if(staxir[i,4] >= 26 && staxir[i,4] <= 40){
            rage.trait[i] <- 'Muy Alto'
          } else {
            rage.trait[i] <- 'ERROR'
            stop('Value out of range in STAXIR')
          }
        } else {
          stop('Gender must be M or F')
        }
      }
      staxir <- dplyr::mutate(staxir,
                              rage.trait = rage.trait)
      results <- staxir
    } else if(quest == 'MRS'){
      message('Tabulation is not coded yet :)')
      results <- NULL
    } else if(quest == 'SRS'){
      message('Tabulation is not coded yet :)')
    } else {
      stop('Questionnaire non existent.')
      results <- NULL
    }

    cat('\n\nTABULATION:', quest, sep = ' ', fill = T)
    print(results)
  }
}
