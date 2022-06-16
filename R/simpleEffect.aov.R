#' @title Simple Effect - simpleEffect.aov
#' @description Perform the mixed anova for two levels: Time x Induction, in induction phase.
#' @param data data frame
#' @param phase analysis phases. Mandatory parameter, it could be: 'induction', 'learning', or 'transfer'.
#' @param quest Induction questionnaires: EVEA or SAM -> consider upper case.
#' @param score Dependent variable you'll use, depending on the induction phase questionnaire: anger, sadness, fear, happiness for the EVEA; arousal, valence for the SAM.
#' @param covariate Variable name to perform ANCOVA: EDS -> consider upper case.
#' @return anova table of the simple effect according each phase. Specially useful in induction phase
#' @examples
#' df<-data.frame('your data')
#' twoWay.aov(df, phase='induction', quest = 'EVEA', score = 'sadness')
#' twoWay.aov(df, phase='induction', quest = 'EVEA', score = 'sadness', covariate = 'EDS')
#' twoWay.aov(df, 'induction', 'EVEA', sadness')
#' twoWay.aov(df, 'induction', 'EVEA', sadness', 'EDS')
#'

simpleEffect.aov <- function(data, phase = NULL, quest = NULL, score = NULL, covariate = NULL){
  ifelse(require(magrittr) == T, 'Loaded', 'Not Loaded')
  data <- dplyr::arrange(data, induction, id)

  if(!is.null(phase)){
    if(phase == 'learning'){
      message('Not coded yet :)')

    } else if(phase == 'induction'){
      long <- rptfm::long.data(data, phase, 1, quest, score)

      aov <- rstatix::anova_test(
        long, dv = score, wid = id, between = induction, within = time,
        covariate = all_of(covariate), effect.size = 'pes') %>%
        rstatix::get_anova_table()

    } else if(phase == 'transfer'){
      long <- rptfm::long.data(data, phase)

      aov <- long %>%
        dplyr::group_by(induction) %>%
        rstatix::anova_test(dv = Cooperation, wid = id,
                            within = GroupBehavior, effect.size = 'pes') %>%
        rstatix::get_anova_table()

      aov <- data.frame(aov)

    } else {
      stop('phase only can be learning, induction or transfer.')

    }
  } else {
    stop('phase is mandatory.')
  }

  cat('SIMPLE MAIN EFFECT', phase, aov$Effect[nrow(aov)], sep = ' - ', fill = T)
  return(aov)
}
