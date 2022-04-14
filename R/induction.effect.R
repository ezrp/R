#' @title Induction Effect Anova - induction.effect
#' @description Effect of Induction at each time point -> Only in induction phase.
#' @param data data frame
#' @param quest Induction questionnaires: EVEA or SAM -> consider upper case.
#' @param score Dependent variable you'll use, depending on the induction phase questionnaire: anger, sadness, fear, happiness for the EVEA; arousal, valence for the SAM.
#' @param var.eq Equality of variance, the default value is true; when false perform whelch's t-test
#' @return anova table
#' @examples
#' df<-data.frame('your data')
#' twoWay.aov(df, quest = 'EVEA', score = 'sadness')
#' twoWay.aov(df, quest = 'EVEA', score = 'sadness', var.eq = F)
#' twoWay.aov(df, 'EVEA', sadness')
#' twoWay.aov(df, 'EVEA', sadness', F)
#'

induction.effect <- function(data, quest = NULL, score = NULL, var.eq = T){
  ifelse(require(magrittr) == T, 'Loaded', 'Not Loaded')
  data <- dplyr::arrange(data, induction, id)

  long <- rptfm::long.data(data, 'induction', 1, quest = quest, score = score)

  if(var.eq == T){
    aov <- long %>%
      dplyr::group_by(time) %>%
      rstatix::anova_test(score ~ induction, effect.size = 'pes') %>%
      rstatix::get_anova_table()
  } else {
    aov <- long %>%
      dplyr::group_by(time) %>%
      rstatix::welch_anova_test(score ~ induction) %>%
      rstatix::get_anova_table()
  }

  cat('ANOVA: Effect of Induction at each time point', fill = T)
  return(aov)
}
