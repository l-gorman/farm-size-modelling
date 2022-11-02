
library("gamlss")
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)

N <- 10000

set.seed(404)
X <- cbind(1,runif(N, 0, 100)) # Generate N uniform random numbers between 0-100



B_mu <- c(0.1, 0.5) # Intercept and coefficient
B_sigma <- c(-1.5, 0.002) # Intercept and coefficient
B_nu <- c(-1,-0.01) # Intercept and coefficient
B_tau <- c(0.01,0.01) # Intercept and coefficient


mu <- c(X %*% B_mu)
sigma <- c(exp(X %*% B_sigma))
nu <- c(X %*% B_nu)
tau <- c(exp(X %*% B_tau))


Y <- rBCT(N, mu=mu, sigma=sigma, nu=nu, tau=tau) %>% tibble::as_tibble()
# Y <- rBCT(n = N,mu=50, sigma=0.1, nu=0.1, tau=0.1) %>% tibble::as_tibble()

# Setting Distributions Params for Coefficients




all_data <- tibble::as_tibble(
  list(
    mu=mu,
    sigma=sigma,
    nu=nu,
    tau=tau,
    X=X[,2],
    y=Y$value
  )
)

sample <- sample(c(TRUE, FALSE), nrow(all_data), replace=TRUE, prob=c(0.8,0.2))

train_df <- all_data[sample,]
test_df <- all_data[!sample,]

ggplot(train_df, aes(x=X,y=y))+
  geom_point() +
  geom_line(aes(y=mu))

ggplot(train_df, aes(x=X,y=y))+
  geom_point() +
  geom_line(aes(y=mu))+
  ylim(c(0,200))

ggplot(Y, aes(x=value))+
  geom_histogram()






test_fit <- gamlss(y~X, 
                   sigma.formula =~X, 
                   nu.formula = ~X, 
                   tau.formula = ~X ,
                   data =train_df,  
                   family=BCT(),
                   control = gamlss.control(n.cyc = 200)
                  )

summary(test_fit)
plot(test_fit)


# Plot Centiles
centiles_result <- gamlss::centiles(test_fit,)

probs <- c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6)
centiles <- gamlss::centiles.pred(test_fit,
                                  xname = "X",
                                  cent = probs,
                                  xvalues = seq(1,100)) %>% as_tibble()

centiles <- centiles %>%
  tidyr::pivot_longer(cols = colnames(centiles)[colnames(centiles)!="x"])
centiles <- centiles %>%
  rename(centile=name)

quantiles <- paste0("Q",probs)

centiles$centile_name <- paste0("Q",centiles$centile)

colours <- RColorBrewer::brewer.pal(n=length(quantiles),name = "Paired")





prediction_plot <- ggplot()
prediction_plot <- prediction_plot+
  geom_point(data=train_df, aes(x=X,y=y, shape="Data Point"), color="dodgerblue4") +
  scale_shape_manual("",values=c(16))+
ylim(c(0,320))

prediction_plot <- prediction_plot + 
  geom_line(data=centiles[centiles$centile==50,],aes(x=x, y=value, color="Estimate"))+
  scale_colour_manual(c("",""),values=c("black","black"))


for (quantile in 1:length(quantiles)){
  if (quantile!=1){
    min_df <- centiles[centiles$centile_name==quantiles[quantile - 1],c("x","value")] %>% rename(min_value=value)
    
    max_df <- centiles[centiles$centile_name==quantiles[quantile],c("x","value")]%>% rename(max_value=value)
    
    temp_df <- min_df %>% merge(max_df, by="x", all.x = T, all.y = F)
    prediction_plot <- prediction_plot +
      geom_ribbon(data=temp_df,aes(x=x, ymax=max_value, ymin=min_value),  fill=colours[quantile],alpha=0.5)
    
    # blank_plot()+geom_ribbon(data=temp_df,aes(x=x, ymax=max_value, ymin=min_value),  fill=colours[quantile],alpha=0.5)

    
  }
}

# centiles$centile <- as.numeric(centiles$centile)
# prediction_plot + 
#   ggridges::geom_vridgeline(stat="ydensity",
#                           mapping=aes(x=50, 
#                                       y=value,width=..density..),
#                           data=centiles[centiles$centile<75 & centiles$centile>25,],inherit.aes = F,
#                           scale=-200,
#                           alpha=0.9,
#                           fill="black",
#                           trim=F
#                           )+
# ggridges::geom_vridgeline(stat="ydensity",
#                           mapping=aes(x=15, 
#                                       y=value,width=..density..),
#                           data=centiles[centiles$centile<25 & centiles$centile>0,],inherit.aes = F,
#                           scale=-200,
#                           alpha=0.9,
#                           fill="black",
#                           trim=F)+
#   ggridges::geom_vridgeline(stat="ydensity",
#                             mapping=aes(x=85, 
#                                         y=value,width=..density..),
#                             data=centiles[centiles$centile<100 & centiles$centile>74,],inherit.aes = F,
#                             scale=-200,
#                             alpha=0.9,
#                             fill="black",
#                             trim=F)














gamlss::centiles
