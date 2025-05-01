sample_parameter_space <- function() {

     params <- MOSAIC::default_config

     params$beta_j0_hum <- runif(1, 0.0000000001, 0.5)
     params$beta_j0_env <- runif(1, 0.0000000002, 1)

     params$alpha_1 <- rbeta(1, 9.5, 0.5)
     params$alpha_2 <- rbeta(1, 9.5, 0.5)

     return(params)

}
