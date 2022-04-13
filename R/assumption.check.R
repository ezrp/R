#' @title Check assumptions for anova analysis
#' @description In this function you can analize for outliers, normality and homogeneity assumptions
#' @param data data frame
#' @param phase study phase. Mandatory: 'baseline', 'induction', 'learning', or 'transfer'.
#' @param type assumption type to be checked. Mandatory: 'outliers', 'normality', 'homogeneity'
#' @return assumptions results
#' @examples
#' df<-data.frame('your data')
#' assumptions(df, phase = 'baseline', type = 'outliers')
#' assumptions(df,'baseline', 'outliers')
#'

assumption.check <- function(data, phase = NULL, type = NULL){
  ifelse(require(magrittr) == T, "Loaded", "Not Loaded")
  data <- dplyr::arrange(data,induction,id)

  if(!is.null(phase)){
    if(phase == 'baseline'){
      df <- rptfm::long.data(data,'baseline')

      if(!is.null(type)){
        if(type == 'outliers'){
          assum.check <- df %>%
            dplyr::group_by(PartnerEthnicity, induction) %>%
            rstatix::identify_outliers(Cooperation)
        } else if(type == 'normality'){
          assum.check <- df %>%
            dplyr::group_by(PartnerEthnicity, induction) %>%
            rstatix::shapiro_test(Cooperation)

          qplot <- ggpubr::ggqqplot(df, 'Cooperation', ggtheme = ggplot2::theme_bw()) +
            ggplot2::facet_grid(PartnerEthnicity ~ induction)
          print(qplot)
        } else if(type == 'homogeneity'){
          assum.check <- df %>%
            dplyr::group_by(PartnerEthnicity) %>%
            rstatix::levene_test(Cooperation ~ induction)
        } else {
          stop("phase must be 'outliers', 'normality', or 'homogeneity'.")
        }
      } else {
        stop("You have to include 'type' parameter")
      }
    } else if(phase == 'induction'){
      df <- rptfm::long.data(data,'induction')
      if(!is.null(type)){
        if(type == 'outliers'){
          assum.check <- df %>%
            dplyr::group_by(Time, Emotion, induction) %>%
            rstatix::identify_outliers(score)
        } else if(type == 'normality'){
          assum.check <- df %>%
            dplyr::group_by(Time, Emotion, induction) %>%
            rstatix::shapiro_test(score)

          qplot <- ggpubr::ggqqplot(df, 'score', ggtheme = ggplot2::theme_bw()) +
            ggplot2::facet_grid(Emotion + induction ~ Time, labeller = 'label_both')
          print(qplot)
        } else if(type == 'homogeneity'){
          assum.check <- df %>%
            dplyr::group_by(Time, Emotion) %>%
            rstatix::levene_test(score ~ induction)
        } else {
          stop("phase must be 'outliers', 'normality', or 'homogeneity'.")
        }
      } else {
        stop("You have to include 'type' parameter")
      }
    } else if(phase == 'learning'){
      message('This part is not coded for this function yet')
    } else if(phase == 'transfer') {
      message('This part is not coded for this function yet')
    } else {
      stop("phase must be 'baseline', 'induction', 'learning', or 'transfer'.")
    }
  } else {
    stop("You have to include 'phase' parameter")
  }


  return(assum.check)
}
