d <- read.csv2("shared_reading_OSF.csv") %>%  mutate_all(~na_if(., ''))
data <-  d %>% filter(!is.na(study_ID))

ef <- escalc("SMD", data = data, m1i = m1i, n1i = N1i, sd1i = sd1i,
                       m2i = m2i, n2i = N2i, sd2i = sd2i)
prior <- c(prior(normal(0,1), class = "Intercept"),
    prior(normal(0,1), class =  "sd"))

ma <- brm(yi|se(vi) ~ 1 + (1|study_ID),
    data = ef,
    prior = prior,
    warmup = 100, iter = 200) 

post.samples <- ma %>% posterior::as_draws_df(., c("^b_", "^sd_"), regex = TRUE) %>% 
  rename("smd" = "b_Intercept", "tau" = "sd_study_ID__Intercept")
  #purrr::lmap(., rename_with(., grepl("b_"), "smd"))
  
  ggplot(aes(x = smd), data = post.samples) +
    geom_density(fill = "lightblue",                # set the color
                 color = "lightblue", alpha = 0.7) +  
    geom_point(y = 0,                               # add point at mean
               x = mean(post.samples$smd)) +
    labs(x = expression(italic(SMD)),
         y = element_blank()) +
    theme_minimal()
  
  ggplot(aes(x = tau), data = post.samples) +
    geom_density(fill = "lightgreen",               # set the color
                 color = "lightgreen", alpha = 0.7) +  
    geom_point(y = 0, 
               x = mean(post.samples$tau)) +        # add point at mean
    labs(x = expression(tau),
         y = element_blank()) +
    theme_minimal()
  #pp_check(ma())
