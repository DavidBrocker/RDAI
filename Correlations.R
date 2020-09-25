library(ggplot2)
##########################
#     Correlation        #
##########################
p_cor <- function(x){
# Load Libaries
library(ggplot2)
library(latex2exp)
# Begin Function
  y <- x
  p_cor_r <- cor.test(x,y)
  p_cor_r2 <- p_cor_r$estimate^2
  p_cor_df <-
    data.frame(
      x,y
    )
 ggplot(p_cor_df,aes(x,y,color="green")) +
   geom_point()+
   labs(title="Perfect Positive Correlation",
        caption = TeX(sprintf("$R^{2}$=%s",p_cor_r2)))+
   theme_classic()
}

n_cor <- function(x){
  # Load Libaries
  library(ggplot2)
  library(latex2exp)
  # Begin Function
  y <- rev(x)
  p_cor_r <- cor.test(x,y)
  p_cor_r2 <- p_cor_r$estimate^2
  p_cor_df <-
    data.frame(
      x,y
    )
  ggplot(p_cor_df,aes(x,y,color="green")) +
    geom_point()+
    labs(title="Perfect Negative Correlation",
         caption = TeX(sprintf("$R^{2}$=%s",p_cor_r2)))+
    theme_classic()
}

curve_cor <- function(x){
  # Load Libaries
  library(ggplot2)
  library(latex2exp)
  # Begin Function
  y <- c(x[1:5],rev(x[1:5]))
  p_cor_r <- cor.test(x,y)
  p_cor_r2 <- p_cor_r$estimate^2
  p_cor_df <-
    data.frame(
      x,y
    )
  ggplot(p_cor_df,aes(x,y,color="green")) +
    geom_point()+
    labs(title="Curvilinear Correlation",
         caption = TeX(sprintf("$R^{2}$=%s",p_cor_r2)))+
    theme_classic()
}

no_cor <- function(x){
  # Load Libaries
  library(ggplot2)
  library(latex2exp)
  # Begin Function
  y <- rnorm(length(x))
  p_cor_r <- cor.test(x,y)
  p_cor_r2 <- round(p_cor_r$estimate^2,2)
  p_cor_df <-
    data.frame(
      x,y
    )
  ggplot(p_cor_df,aes(x,y,color="green")) +
    geom_point()+
    labs(title="No Correlation",
         caption = TeX(sprintf("$R^{2}$=%s",p_cor_r2)))+
    theme_classic()
}

##########################
#        t-test          #
##########################

single_tt <- function(x,hyp){
  t.test(x,mu=hyp)
}

paired_tt <- function(x,y){
  t.test(x,y,paired = T)
}

ind_tt <- function(x,y){
  t.test(x,y,var.equal = T,paired = F)
}

##########################
#        ANOVA           #
##########################


  df <- 
    data.frame(iv =rep(c("young","middle","old"),each=7),
               dv = c(rnorm(7,10),rnorm(7,20),rnorm(7,40))
    )
  df$iv <- factor(df$iv,
                  levels = c("young","middle","old"),
                  ordered = T)
  df %>% 
    group_by(iv) %>% 
    summarise(mn=mean(dv)) %>% 
    ggplot(aes(iv,mn,color=iv))+
    geom_line(group=1,color="black",linetype=1)+
    geom_point() +
    scale_color_discrete() +
    labs(x="\n Independent Variable",
         y="\n Dependent Variable",
         title="Mean differences in age groups on some dv")+
    theme_classic()+
    theme(legend.title = element_blank())


