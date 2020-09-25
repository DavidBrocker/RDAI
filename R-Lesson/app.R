library(shiny)
library(rtweet)
library(ggplot2)
library(tidytext)
library(dplyr)

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
        geom_point(show.legend = F)+
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
        geom_point(show.legend = F)+
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
        geom_point(show.legend = F)+
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
        geom_point(show.legend = F)+
        labs(title="No Correlation",
             caption = TeX(sprintf("$R^{2}$=%s",p_cor_r2)))+
        theme_classic()
}

df <- 
  data.frame(iv =rep(c("young","middle","old"),each=7),
             dv = c(rnorm(7,10),rnorm(7,20),rnorm(7,40))
  )
df$iv <- factor(df$iv,
                levels = c("young","middle","old"),
                ordered = T)
df_plot <- 
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

ui <- 
  navbarPage(
    "RDA Workshop",
    selected = "Correlations",
    inverse=T,
    tabPanel("Correlations",
             fluidPage(
               titlePanel(title="Correlations"),
               sidebarLayout(position = "left",
                             sidebarPanel(
                               radioButtons("cors","Correlation Type",
                                            c("Perfect Positive"="postv",
                                              "Perfect Negative"="negtv",
                                              "Curvilinear"="curve",
                                              "No Correlation"="none"),
                                            selected = "postv"),
                               tableOutput("cortab")
                               ),
                             mainPanel(
                               plotOutput("corplot")
                               )
                             )
               )
             ),
    tabPanel("ANOVA",
             fluidPage(
               titlePanel(title="One-Way ANOVA"),
               sidebarLayout(position = "left",
                             sidebarPanel(
                               tableOutput("aovtable")
                             ),
                             mainPanel(
                               plotOutput("aovplot")
                             )
                            )
               )
             )

)

server <- function(input, output) {
    output$corplot <- renderPlot({
        x <- 1:10
        typ <- switch(input$cors,
                      postv=p_cor,
                      negtv=n_cor,
                      curve=curve_cor,
                      none=no_cor)
        typ(x)
    })
    output$cortab <- renderTable({
      x <- 1:10
      tab <- switch(input$cors,
                    postv=y <- x,
                    negtv=y <- rev(x),
                    curve=y <- c(x[1:5],rev(x[1:5])),
                    none=y <- rnorm(length(x)))
      data.frame(x,y)
    })
    output$aovtable <- renderTable({
      df
    })
    output$aovplot <- renderPlot({
      df_plot
    })

}

shinyApp(ui, server)