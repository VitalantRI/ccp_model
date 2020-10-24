setwd("~/dev/ccp_model/manuscript_factors_driving_availability/")
library(tidyverse)
library(arrow)

empiric_2nd <- read_feather("data/donor_return_2nd_cumprob.feather") %>%
  filter(time != 9999) %>%
  select(-prob_lb, -prob_ub)
empiric_3rd <- read_feather("data/donor_return_3rd_cumprob.feather") %>%
  filter(time != 9999) %>%
  select(-prob_lb, -prob_ub)
empiric_4th <- read_feather("data/donor_return_4th_cumprob.feather") %>%
  filter(time != 9999) %>%
  select(-prob_lb, -prob_ub)

pad <- empiric_2nd[1:3,]
pad$time <- rep(130, 3)
pad$prob <- c(max(empiric_2nd$prob),max(empiric_3rd$prob),max(empiric_4th$prob))
pad$return <- c("Return for 2nd donation (empirical)", "Return for 3rd donation (empirical)","Return for 4th+ donation (empirical)")

empiric_2nd %>%
  mutate(return = "Return for 2nd donation (empirical)") %>%
  bind_rows(
    empiric_3rd %>%
      mutate(return = "Return for 3rd donation (empirical)")
  ) %>%
  bind_rows(
    empiric_4th %>%
      mutate(return = "Return for 4th+ donation (empirical)")
  ) %>% 
  bind_rows(
    pad
  ) -> empiric

exp_model_func <- function(t, K = 0.44068065575293935, lambd = 0.02564293675387818, t0 = 7, scale=1.0) {
  prob <- ifelse(t<t0, 0, scale*K * (1.0 - exp(-lambd*(t-t0)*scale)))
  return(prob)
}

parametric_2nd_1.0 <- tibble(
  time = seq(0,130,1),
  prob = exp_model_func(t = seq(0,130,1), K = 0.44068065575293935, lambd = 0.02564293675387818, scale = 1.0),
  return = "Return for 2nd donation (parametric, scale=1.0)"
)

parametric_3rd_1.0 <- tibble(
  time = seq(0,130,1),
  prob = exp_model_func(t = seq(0,130,1), K = 0.7004050311562576, lambd = 0.03913344618984413, scale = 1.0),
  return = "Return for 3rd donation (parametric, scale=1.0)"
)

parametric_4th_1.0 <- tibble(
  time = seq(0,130,1),
  prob = exp_model_func(t = seq(0,130,1), K = 0.8076846503792819, lambd = 0.05034356712490996, scale = 1.0),
  return = "Return for 4th+ donation (parametric, scale=1.0)"
)

parametric_2nd_0.5 <- tibble(
  time = seq(0,130,1),
  prob = exp_model_func(t = seq(0,130,1), K = 0.44068065575293935, lambd = 0.02564293675387818, scale = 0.5),
  return = "Return for 2nd donation (parametric, scale=0.5)"
)

parametric_2nd_1.5 <- tibble(
  time = seq(0,130,1),
  prob = exp_model_func(t = seq(0,130,1), K = 0.44068065575293935, lambd = 0.02564293675387818, scale = 1.5),
  return = "Return for 2nd donation (parametric, scale=1.5)"
)

parametric_2nd_2.0 <- tibble(
  time = seq(0,130,1),
  prob = exp_model_func(t = seq(0,130,1), K = 0.44068065575293935, lambd = 0.02564293675387818, scale = 2.0),
  return = "Return for 2nd donation (parametric, scale=2.0)"
)

empiric %>%
  bind_rows(parametric_2nd_1.0) %>%
  bind_rows(parametric_3rd_1.0) %>%
  bind_rows(parametric_4th_1.0) %>%
  bind_rows(parametric_2nd_0.5) %>%
  bind_rows(parametric_2nd_1.5) %>%
  bind_rows(parametric_2nd_2.0) -> return_probs 



return_probs %>%
  ggplot(aes(x = time, y = prob)) + #colour = as.factor(return)
  geom_line() +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_x_continuous(breaks = seq(0,120,20)) +
  xlab("Days") + ylab("Cumulative prrobability of return") +
  facet_wrap(vars(factor(return, 
                         levels = c("Return for 2nd donation (empirical)",
                                    "Return for 3rd donation (empirical)",
                                    "Return for 4th+ donation (empirical)",
                                    "Return for 2nd donation (parametric, scale=1.0)",
                                    "Return for 3rd donation (parametric, scale=1.0)",
                                    "Return for 4th+ donation (parametric, scale=1.0)",
                                    "Return for 2nd donation (parametric, scale=0.5)",
                                    "Return for 2nd donation (parametric, scale=1.5)",
                                    "Return for 2nd donation (parametric, scale=2.0)"
                                    ))), ncol = 3) +
  theme_bw()
ggsave("figs/empiric_vs_parametric_returndists.pdf", w = 9, h = 7.5)

# never 2nd
1-0.42733566

# never 3rd
1-0.69357771

#never 4th
1-0.8914565
