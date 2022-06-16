#' @title Induction Effect Anova - induction.effect
#' @description Effect of Induction at each time point -> only in induction phase. Also, it can perform simple effect analysis in specific time point.
#' @param data data frame
#' @param quest Induction questionnaires: EVEA or SAM -> consider upper case.
#' @param score Dependent variable you'll use, depending on the induction phase questionnaire: anger, sadness, fear, happiness for the EVEA; arousal, valence for the SAM.
#' @param var.eq Equality of variance, the default value is true; when false perform whelch's t-test
#' @param simple.effect If it is true and if there was a statistical significant result, it will be performed a simple effect analysis of variance in the particular level of time factor.
#' @return anova table for: induction effect at each time point. And if it is the case, the results of the simple effect.
#' @examples
#' df<-data.frame('your data')
#' twoWay.aov(df, quest = 'EVEA', score = 'sadness')
#' twoWay.aov(df, quest = 'EVEA', score = 'sadness', var.eq = F, simple.effect = T)
#' twoWay.aov(df, 'EVEA', sadness', simple.effect = T)
#' twoWay.aov(df, 'EVEA', sadness', F, T)
#'

induction.effect <- function(data, quest = NULL, score = NULL, var.eq = T, simple.effect = F){
  ifelse(require(magrittr) == T, 'Loaded', 'Not Loaded')
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
  aov <- data.frame(aov)
  cat('ANOVA: Induction effect at each time point on', quest, score, sep = ' ', fill = T)
  print(aov)

  if(simple.effect == T){
    pVal <- F

    for(i in 1:nrow(aov)){
      if(aov[i,6] < 0.05){
        pVal <- T
        timeVar <- as.character(aov[i,1])

        post <- long[long$time == timeVar,]

        angVSneut.aov <- post[post$induction != 'Sadness',] %>%
          rstatix::anova_test(score ~ induction, effect.size = 'pes') %>%
          rstatix::get_anova_table()

        angVSsad.aov <- post[post$induction != 'Neutral',] %>%
          rstatix::anova_test(score ~ induction, effect.size = 'pes') %>%
          rstatix::get_anova_table()

        sadVSneut.aov <- post[post$induction != 'Anger',] %>%
          rstatix::anova_test(score ~ induction, effect.size = 'pes') %>%
          rstatix::get_anova_table()

        results <- merge(angVSneut.aov, merge(angVSsad.aov, sadVSneut.aov, all = T), all = T)
        results <- cbind(Groups = c('Sadness vs Neutral', 'Anger vs Sadness', 'Anger vs Neutral'), results)
        cat('\nPerformed in time -',timeVar,'factor.','\nComparissons on',quest, score, 'score:', sep = ' ', fill = T)
        print(results)
      }
    }
    if(!pVal){message('There were no statistical significant results.')}
  }
}
