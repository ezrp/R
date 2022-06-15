#' @title graph.phase
#' @description specific graphs for each phase
#' @param data data frame
#' @param phase analysis phases. Mandatory parameter, it could be: 'baseline' 'induction', 'learning', or 'transfer'.
#' @param quest Induction questionnaires: EVEA or SAM -> consider upper case
#' @param score Dependent variable you'll use, depending on the questionnaire: anger, sadness, fear, happiness for the EVEA; arousal, valence for the SAM.
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param ylim y-axis limit
#' @return specific graphic useful in each experimental phase
#' @examples
#' df<-data.frame('your data')
#' graph.phase(df,phase='baseline')
#' graph.phase(df,phase='induction', quest = 'EVEA', score = 'sadness')
#' graph.phase(df, 'induction', EVEA', sadness')
#'

graph.phase <- function(data, phase = NULL, quest = NULL, score = NULL, xlab = NULL, ylab = NULL, ylim = NULL){
  ifelse(require(magrittr) == T, 'Loaded', 'Not Loaded')

  if(!is.null(phase)){
    if(phase == 'baseline'){
      long <- rptfm::long.data(data, phase)

      graph <- ggpubr::ggbarplot(
        long, x = 'PartnerEthnicity', y = 'Cooperation', color = 'PartnerEthnicity',
        legend.title = 'Partner Ethnicity', width = 0.3, xlab = 'Partner Ethnicity', ylab = 'Cooperation Rate',
        ggtheme = ggplot2::theme_bw(), palette = c('black', 'blue'), add = 'mean'
      )

    } else if(phase == 'learning'){
      long <- rptfm::long.data(data, phase)

      graph <- ggpubr::ggline(
        long, x = 'Block', y = 'Cooperation', color = 'GroupBehavior', palette = 'uchicago',
        legend.title = 'Group Behavior', xlab = 'Blocks', ylab = 'Cooperation Rate',
        error.plot = 'errorbar', add = 'mean', facet.by = c('Consistency')
      )

    } else if(phase == 'induction' && !is.null(quest)) {
      long <- rptfm::long.data(data, phase, 1, quest, score)
      ylab <- paste(score, quest, 'score', sep = ' ')

      graph <- ggpubr::ggline(
        long, x = 'time', y = 'score', color = 'induction', legend.title = 'Induction Groups',
        xlab = xlab, ylab = ylab, ylim = ylim,
        add = 'mean', palette = c('red', 'grey', 'blue')
      )

    } else if(phase == 'transfer') {
      long <- rptfm::long.data(data, phase)

      graph <- ggpubr::ggbarplot(
        long, x = 'induction', y = 'Cooperation', color = 'GroupBehavior', legend.title = 'Group Behavior',
        add = 'mean', error.plot = 'upper_errorbar', palette = 'uchicago', fill = 'GroupBehavior', ylim=c(0.05,0.8), xlab = xlab, ylab = ylab,
        position = ggplot2::position_dodge(0.8), ggtheme = ggplot2::theme_bw(), width = 0.6, hide.ns = F
      )
    } else {
      stop('Incorrect phase, or, in induction phase quest and score are mandatory parameters.')
    }
  } else {
    stop('phase is a mandatory parameter.')
  }

  return(graph)
}
