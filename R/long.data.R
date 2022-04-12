#' @title long.data
#' @description transform wide form data into long form. Important to do repeated measure ANOVA analysis
#' @param data data frame
#' @param phase analysis phases. Mandatory parameter, it could be: 'induction', 'learning', or 'transfer'.
#' @return long form data frame
#' @examples
#' df<-data.frame('your data')
#' long.data(df,phase='induction')
#' long.data(df,'induction')
#'

long.data <- function(data, phase = NULL){
  ifelse(require(magrittr) == T, "Loaded", "Not Loaded")

  if(!is.null(phase)){
    if(phase == 'induction'){
      df <- dplyr::transmute(data, id = id, induction = induction, pre_Anger = PRE_EVEA_anger, pre_Sadness = PRE_EVEA_sadness,
                             post_Anger = POST_EVEA_anger, post_Sadness = POST_EVEA_sadness, fin_Anger = FIN_EVEA_anger, fin_Sadness = FIN_EVEA_sadness)
      df <- dplyr::arrange(df,induction, id)

      long <- df %>%
        rstatix::gather(key = 'Time', value = 'score', pre_Anger, pre_Sadness, post_Anger, post_Sadness, fin_Anger, fin_Sadness) %>%
        rstatix::convert_as_factor(id,induction)

      n <- length(long$id)

      EmotionCol <- rep(c(rep('Anger', n/6), rep('Sadness', n/6)), length(levels(long$induction)))
      TimeCol <- c(rep('pre', n/3), rep('post', n/3), rep('fin', n/3))

      long <- dplyr::transmute(long, id = id, induction = induction, Time = TimeCol, Emotion = EmotionCol, score = score) %>%
        rstatix::convert_as_factor(Time, Emotion) %>%
        rstatix::reorder_levels('Time', order = c('pre', 'post', 'fin'))

      df <- long

    } else if(phase == 'learning'){
      message('This part is not coded yet for long.data function')
    } else if(phase == 'transfer') {
      print('This part is not coded yet for long.data function')
    } else {
      stop("phase must be 'induction', 'learning', or 'transfer'")
    }
  } else {
    stop("You have to specify 'phase' parameter")
  }

  return(df)
}
