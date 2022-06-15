#' @title graph.phase
#' @description specific graphs for each phase
#' @param data data frame
#' @param phase analysis phases. Mandatory parameter, it could be: 'baseline' 'induction', 'learning', or 'transfer'.
#' @param quest Induction questionnaires: EVEA or SAM -> consider upper case
#' @param score Dependent variable you'll use, depending on the questionnaire: anger, sadness, fear, happiness for the EVEA; arousal, valence for the SAM.
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param ylim y-axis limit
#' @return specific graphic useful in each experimental phase. We used mean and standard error bars.
#' @examples
#' df<-data.frame('your data')
#' graph.phase(df,phase='baseline')
#' graph.phase(df,phase='induction', quest = 'EVEA', score = 'sadness')
#' graph.phase(df, 'induction', EVEA', sadness')
#'

graph.phase <- function(data, phase = NULL, quest = NULL, score = NULL, xlab = NULL, ylab = NULL, ylim = NULL){
  ifelse(require(ggplot2) == T, 'Loaded', 'Not Loaded')
  ifelse(require(ggpubr) == T, 'Loaded', 'Not Loaded')

  if(!is.null(phase)){
    if(phase == 'baseline'){
      long <- rptfm::long.data(data, phase)

      graph <- ggbarplot(
        long, x = 'PartnerEthnicity', y = 'Cooperation', color = 'PartnerEthnicity',
        legend.title = 'Partner Ethnicity', width = 0.3, xlab = 'Partner Ethnicity', ylab = 'Cooperation Rate',
        ggtheme = theme_bw(), palette = c('black', 'blue'), add = 'mean'
      )

    } else if(phase == 'learning'){
      long <- rptfm::long.data(data, phase)

      graph <- ggline(
        long, x = 'Block', y = 'Cooperation', color = 'GroupBehavior', palette = 'uchicago',
        legend.title = 'Group Behavior', xlab = 'Blocks', ylab = 'Cooperation Rate',
        add = 'mean_se', error.plot = 'errorbar', facet.by = c('Consistency')
      )

    } else if(phase == 'induction' && !is.null(quest)) {
      long <- rptfm::long.data(data, phase, 1, quest, score)
      ylab <- paste(score, quest, 'score', sep = ' ')

      graph <- ggline(
        long, x = 'time', y = 'score', color = 'induction', legend.title = 'Induction Groups',
        xlab = xlab, ylab = ylab, ylim = c(0, 25),
        add = 'mean_se', error.plot = 'errorbar', palette = c('red', 'black', 'blue')
      )

    } else if(phase == 'transfer') {
      long <- rptfm::long.data(data, phase)

      graph <- ggbarplot(
        long, x = 'induction', y = 'Cooperation', color = 'GroupBehavior', legend.title = 'Group Behavior',
        add = 'mean_se', error.plot = 'upper_errorbar', palette = 'uchicago', fill = 'GroupBehavior', ylim=c(0.05,0.8), xlab = xlab, ylab = ylab,
        position = position_dodge(0.8), ggtheme = theme_bw(), width = 0.6, hide.ns = F
      )
    } else {
      stop('Incorrect phase, or, in induction phase quest and score are mandatory parameters.')
    }
  } else {
    stop('phase is a mandatory parameter.')
  }

  return(graph)
}
