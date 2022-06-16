#' @title Summary statistics - summary.statistics
#' @description It returns descriptives for two phases: baseline and induction. You can also input the type to make a summary of the data.
#' @param data data frame
#' @param phase analysis phases. Mandatory parameter, it could be: 'baseline' or 'transfer'.
#' @param type Output type. It could be: mean_sd, mean, mean_se
#' @return a tibble with the statistics
#' @examples
#' df<-data.frame('your data')
#' summary.statistics(df,phase='baseline')
#' summary.statistics(type = 'mean_sd)
#' long.data(df, 'induction', '1, 'mean_sd')
#'

summary.statistics <- function(data, phase = NULL, type = NULL){
  ifelse(require(magrittr) == T, 'Loaded', 'Not Loaded')

  if(!is.null(phase)){
    if(phase == 'baseline'){
      long <- rptfm::long.data(data, phase)

      summary <- long %>% dplyr::group_by(PartnerEthnicity) %>%
        rstatix::get_summary_stats(Cooperation, type = type)
    } else if (phase == 'transfer'){
      long <- rptfm::long.data(data, phase)

      summary <- long %>% dplyr::group_by(induction, GroupBehavior) %>%
        rstatix::get_summary_stats(Cooperation, type = type)

    } else {
      stop('phase only can be baseline or transfer.')
    }
  } else {
    stop('phase is a mandatory parameter.')
  }

  return(summary)
}
