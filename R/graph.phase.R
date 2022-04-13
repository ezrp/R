#' @title long.data
#' @description transform wide form data into long form. Important to do repeated measure ANOVA analysis
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
#' long.data(df,phase='baseline')
#' long.data(df,phase='induction', quest = 'EVEA', score = 'sadness')
#' long.data(df, 'induction', EVEA', sadness')
#'

graph.phase <- function(data, phase = NULL, quest = NULL, score = NULL, xlab = NULL, ylab = NULL, ylim = NULL){

  if(!is.null(phase)){
    if(phase == 'baseline'){
      long <- rptfm::long.data(data, 'baseline')

      graph <- ggpubr::ggbarplot(
        long, x = 'PartnerEthnicity', y = 'Cooperation', color = 'PartnerEthnicity',
        legend.title = 'Partner Ethnicity', width = 0.3, xlab = 'Partner Ethnicity', ylab = 'Cooperation Rate',
        ggtheme = ggplot2::theme_bw(), palette = c('black', 'blue'), add = c('mean', 'jitter')
      )

    } else if(phase == 'learning'){
      message('Not coded yet :)')

    } else if(phase == 'induction' && !is.null(quest)) {
      long <- rptfm::long.data(data, phase, 1, quest, score)
      ylab <- paste(score, quest, 'score', sep = ' ')

      graph <- ggpubr::ggline(
        long, x = 'time', y = 'score', color = 'induction',
        legend.title = 'Induction Groups', xlab = xlab, ylab = ylab, ylim = ylim,
        add = 'mean', palette = c('red', 'grey', 'blue')
      )

    } else if(phase == 'transfer') {
      message('Not coded yet :)')

    } else {
      stop('Incorrect phase. \nIn induction phase quest and score are mandatory parameters.')
    }
  } else {
    stop('phase is a mandatory parameter.')
  }

  return(graph)
}
