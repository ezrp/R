#' @title long.data
#' @description transform wide form data into long form. Important to do repeated measure ANOVA analysis
#' @param data data frame
#' @param phase analysis phases. Mandatory parameter, it could be: 'baseline' 'induction', 'learning', or 'transfer'.
#' @param within Number of within-subject factors: 1 or 2
#' @param quest Induction questionnaires: EVEA or SAM -> consider upper case
#' @param score Dependent variable you'll use, depending on the induction phase questionnaire: anger, sadness, fear, happiness for the EVEA; arousal, valence for the SAM.
#' @return long form data frame
#' @examples
#' df<-data.frame('your data')
#' long.data(df,phase='baseline')
#' long.data(df,phase='induction', within = 1, quest = 'EVEA', score = 'sadness')
#' long.data(df, 'induction', 1, 'EVEA', sadness')
#'

long.data <- function(data, phase = NULL, within = NULL, quest = NULL, score = NULL){
  ifelse(require(magrittr) == T, "Loaded", "Not Loaded")
  data <- dplyr::arrange(data,induction,id)

  if(!is.null(phase)){
    if(phase == 'baseline'){
      df <- dplyr::transmute(data, id = id, induction = induction, Blacks = Blacks_B1, Whites = Whites_B1)

      long <- df %>%
        rstatix::gather(key = 'PartnerEthnicity', value = 'Cooperation', Blacks, Whites) %>%
        rstatix::convert_as_factor(id, induction, PartnerEthnicity)

      df <- long

    }else if(phase == 'induction'){
      if(within == 2){
        df <- dplyr::transmute(data, id = id, induction = induction, pre_Anger = PRE_EVEA_anger, pre_Sadness = PRE_EVEA_sadness,
                               post_Anger = POST_EVEA_anger, post_Sadness = POST_EVEA_sadness, fin_Anger = FIN_EVEA_anger, fin_Sadness = FIN_EVEA_sadness)

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
      } else if(within == 1){
        if(quest == 'EVEA' && score == 'anger'){
          df <- dplyr::transmute(data, id = id, induction = induction, pre = PRE_EVEA_anger,
                                 post = POST_EVEA_anger, fin = FIN_EVEA_anger, EDS = EDS)
        } else if(quest == 'EVEA' && score == 'sadness'){
          df <- dplyr::transmute(data, id = id, induction = induction, pre = PRE_EVEA_sadness,
                                 post = POST_EVEA_sadness, fin = FIN_EVEA_sadness, EDS = EDS)
        } else if(quest == 'EVEA' && score == 'fear'){
          df <- dplyr::transmute(data, id = id, induction = induction, pre = PRE_EVEA_fear,
                                 post = POST_EVEA_fear, fin = FIN_EVEA_fear, EDS = EDS)
        } else if(quest == 'EVEA' && score == 'happiness'){
          df <- dplyr::transmute(data, id = id, induction = induction, pre = PRE_EVEA_happiness,
                                 post = POST_EVEA_happiness, fin = FIN_EVEA_happiness, EDS = EDS)
        } else if(quest == 'SAM' && score == 'arousal'){
          df <- dplyr::transmute(data, id = id, induction = induction, pre = PRE_SAM_arousal,
                                 post = POST_SAM_arousal, fin = FIN_SAM_arousal, EDS = EDS)
        } else if(quest == 'SAM' && score == 'valence') {
          df <- dplyr::transmute(data, id = id, induction = induction, pre = PRE_SAM_valence,
                                 post = POST_SAM_valence, fin = FIN_SAM_valence, EDS = EDS)
        } else {
          stop('Error in paramaters quest or score.')
        }
        long <- df %>%
          rstatix::gather(key = 'time', value = 'score', pre, post, fin) %>%
          rstatix::convert_as_factor(id, time, induction) %>%
          rstatix:: reorder_levels('time', order = c('pre', 'post', 'fin'))

        df <- long

      } else {
        stop('num.within must be 1 or 2')
      }
    } else if(phase == 'learning'){
      message('Learning phase not coded yet :)')
    } else if(phase == 'transfer') {
      message('Learning phase not coded yet :)')
    } else {
      stop("phase must be 'baseline', 'induction', 'learning', or 'transfer'")
    }
  } else {
    stop("phase parameter is mandatory.")
  }

  return(df)
}
