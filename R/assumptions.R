#' @title Check assumptions for anova analysis
#' @description In this function you can analize for outliers, normality and homogeneity assumptions
#' @param data data frame
#' @param type assumption type to be checked
#' @return assumptions results
#' @examples
#' df<-data.frame('your data')
#' assumptions(df);
#' assumptions(df,type='outliers')
#'

assumptions <- function(data, type = NULL){
  colnames(data) <- c('id', 'induction', 'Blacks', 'Whites')

  long <- data %>%
    gather(key = 'PartnerEthnicity', value = 'Cooperation', Blacks, Whites) %>%
    convert_as_factor(id, induction, PartnerEthnicity)


  if(type == 'outliers'){
    outliers <- long %>%
      group_by(PartnerEthnicity, induction) %>%
      identify_outliers(Cooperation)
    print(outliers)
  }else if(type == 'normality'){
    normality <- long %>%
      group_by(PartnerEthnicity, induction) %>%
      shapiro_test(Cooperation)

    qplot <- ggqqplot(long, 'Cooperation', ggtheme = theme_bw()) +
      facet_grid(PartnerEthnicity ~ induction)
    print(normality)
    print(qplot)
  } else if (type == 'homogeneity'){
    homogeneity <- long %>%
      group_by(PartnerEthnicity) %>%
      levene_test(Cooperation ~ induction)

    print(homogeneity)
  }
}


#ANOVA AND SUMMARY STATISTICS
baseline.aov <- function(data, mean_sd = FALSE, graph = FALSE){
  colnames(data) <- c('id', 'induction', 'Blacks', 'Whites')

  long <- data %>%
    gather(key = 'PartnerEthnicity',value = 'Cooperation', Blacks, Whites) %>%
    convert_as_factor(id, induction, PartnerEthnicity)

  if(mean_sd == T){
    summary <- long %>% group_by(PartnerEthnicity) %>%
      get_summary_stats(Cooperation, type = 'mean_sd')
    print(summary)
  }

  if(graph == T){
    graph <- ggbarplot(
      long, x = 'PartnerEthnicity', y = 'Cooperation',
      color = 'PartnerEthnicity', width = 0.3, ylab = 'Cooperation Rate', ggtheme = theme_bw(),
      palette = c('black','blue'), add = c('mean', 'jitter')
    )
    print(graph)
  }

  aov.baseline <- anova_test(
    long, dv = Cooperation, wid = id,
    between = induction, within = PartnerEthnicity, effect.size = 'pes'
  ) %>% get_anova_table()

  return(aov.baseline)
}
