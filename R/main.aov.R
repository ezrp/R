#' @title Mixed Main Anova - main.aov
#' @description Perform the mixed anova for each experimental phase.
#' @param data data frame
#' @param phase analysis phases. Mandatory parameter, it could be: 'baseline' 'induction', 'learning', or 'transfer'.
#' @return anova table
#' @examples
#' df<-data.frame('your data')
#' main.aov(df,phase='induction')
#' main.aov(df,'induction')
#'

main.aov <- function(data, phase = NULL){
  ifelse(require(magrittr) == T, 'Loaded', 'Not Loaded')
  data <- dplyr::arrange(data, induction, id)


  if(!is.null(phase)){
    if(phase == 'baseline'){
      long <- rptfm::long.data(data, phase)

      aov <- rstatix::anova_test(long, dv = Cooperation, wid = id,
                                 between = induction, within = PartnerEthnicity, effect.size = 'pes') %>%
        rstatix::get_anova_table()

    } else if(phase == 'induction'){
      long <- rptfm::long.data(data, phase, 2)

      aov <- rstatix::anova_test(long, dv = score, wid = id,
                                 between = induction, within = c(Time, Emotion), effect.size = 'pes') %>%
        rstatix::get_anova_table()

    } else if(phase == 'learning') {
      message('Not coded yet :)')

    } else if(phase == 'transfer') {
      long <- rptfm::long.data(data, phase)

      aov <- rstatix::anova_test(long, dv = Cooperation, wid = id,
                                 between = induction, within = GroupBehavior, effect.size = 'pes') %>%
        rstatix::get_anova_table()

    } else {
      stop('phase only could be baseline, induction, learning, or transfer.')
    }
  } else {
    stop('phase is a mandatory parameter.')
  }

  cat('MAIN ANOVA', phase, aov$Effect[nrow(aov)], sep = ' - ', fill = T)
  return(aov)
}
