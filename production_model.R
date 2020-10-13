# Copyright 2020 Vitalant and W. Alton Russell
# Authors: Eduard Grebe, W. Alton Russell, Brian Custer
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# The software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this software. If not, see <http://www.gnu.org/licenses/>.

simulate_production_model <- function(input) {
  #browser()
  demand <- dplyr::lag(
    # element-wise addition
    round(input$admissions*(input$perc_hosp_ccp/100)*input$doses_per_patient_hosp + input$icu_admissions*(input$perc_icu_ccp/100)*input$doses_per_patient_icu), 
    n = as.integer(input$demand_lag), #crazy workaround 
    default = 0)
  production_model(start_date = input$start_date,
                   end_date = input$end_date,
                   production_by_timestep = input$production_by_timestep,
                   p_release = input$p_release,
                   production_lag = as.integer(input$production_lag), #crazy workaround
                   demand_by_timestep = demand) %>%
    return()
}

production_model <- function(start_date = ymd("2020-04-01"), 
                             end_date = ymd("2020-05-31"), 
                             timestep = "days", 
                             production_by_timestep,
                             p_release,
                             production_lag,
                             demand_by_timestep) {
  n_steps <- length(seq(start_date, end_date, by = timestep))
  t <- seq(0,n_steps-1)
  
  tibble::tibble(
    date = seq(start_date, end_date, by = timestep),
    t = t,
    units_collected = production_by_timestep,
    units_released = round(p_release * dplyr::lag(units_collected, 
                                                  n = production_lag, 
                                                  default = 0)),
    demand = demand_by_timestep,
    utilized = rep(0, n_steps),
    remaining_stock = rep(0, n_steps),
    unmet_demand = rep(0, n_steps)
  ) -> sim
  
  for (i in 2:n_steps){
    sim[i,]$utilized <- min(sim[i-1,]$remaining_stock + sim[i,]$units_released, sim[i,]$demand)
    sim[i,]$remaining_stock <- sim[i-1,]$remaining_stock + sim[i,]$units_released - sim[i,]$utilized
    sim[i,]$unmet_demand <- sim[i,]$demand - sim[i,]$utilized
  }
  
  return(sim)
}

plot_production_model <- function(sim, 
                                  vars = c("units_collected", "units_released",
                                           "demand", "utilized", "remaining_stock", "unmet_demand"),
                                  limit_y = TRUE,
                                  smooth_lines = FALSE) {
  sim %>%
    select(-t) %>%
    tidyr::pivot_longer(units_collected:unmet_demand, 
                        names_to = "variable", 
                        values_to = "value") %>%
    dplyr::mutate(variable = factor(variable, levels = c("units_collected", 
                                                         "units_released",
                                                         "demand", 
                                                         "utilized", 
                                                         "remaining_stock", 
                                                         "unmet_demand"))) %>%
    dplyr::filter(variable %in% vars) -> sim_long
  
  if (limit_y) { 
    y_max <- max(dplyr::filter(sim_long, !(variable %in% c("remaining_stock")))$value)
  } else { y_max <- max(sim_long$value) }
  
  if (smooth_lines) {
    sim_long %>%
      ggplot2::ggplot(aes(x = date, y = value, colour = variable)) +
      #geom_line() +
      geom_smooth(se=FALSE, span = 0.1) +
      #geom_point() +
      ylab("CCP units") +
      xlab("Date") +
      coord_cartesian(ylim = c(0, y_max)) + 
      scale_color_brewer(palette = "Paired") +
      theme_bw() +
      theme(legend.position = "bottom") -> plot
  } else {
    sim_long %>%
      ggplot2::ggplot(aes(x = date, y = value, colour = variable)) +
      #geom_line() +
      geom_line() +
      geom_point() +
      ylab("CCP units") +
      xlab("Date") +
      coord_cartesian(ylim = c(0, y_max)) + 
      scale_color_brewer(palette = "Paired") +
      theme_bw() +
      theme(legend.position = "bottom") -> plot
  }
  
  return(plot)
}


change_input <- function(input, var_name, new_val, start_date = NA){
  #browser()
  if(is.na(start_date)){
    input[var_name] <- new_val
  } else {
    input[[var_name]] <- c(rep(input[[var_name]], start_date - input$sim_dates[1]),
                           rep(new_val, input$sim_dates[2] - start_date + 1))
  }
  
  return(input)
}

add_scenario <- function(scenarios = list(), 
                         scenario_name, 
                         var_name, 
                         new_val, 
                         input, 
                         start_date = NA){
  #browser()
  scenarios[[scenario_name]] <- simulate_production_model(change_input(input, var_name, new_val, start_date))
  return(scenarios)
}

plot_scenarios <- function(sim,
                           scenarios, 
                           vars = c("units_collected", "units_released",
                                    "demand", "utilized", "remaining_stock", "unmet_demand"),
                           limit_y = TRUE,
                           smooth_lines = TRUE) {
  
  sim %>%
    select(-t) %>%
    tidyr::pivot_longer(units_collected:unmet_demand, 
                        names_to = "variable", 
                        values_to = "value") %>%
    dplyr::mutate(variable = factor(variable, levels = c("units_collected", 
                                                         "units_released",
                                                         "demand", 
                                                         "utilized", 
                                                         "remaining_stock", 
                                                         "unmet_demand"))) %>%
    dplyr::filter(variable %in% vars) -> sim_long
  
  sim_long$scenario <- "Orig."
  
  for(i in 1:length(scenarios)){
    scenarios[[i]] %>%
      select(-t) %>%
      tidyr::pivot_longer(units_collected:unmet_demand, 
                          names_to = "variable", 
                          values_to = "value") %>%
      dplyr::mutate(variable = factor(variable, levels = c("units_collected", 
                                                           "units_released",
                                                           "demand", 
                                                           "utilized", 
                                                           "remaining_stock", 
                                                           "unmet_demand"))) %>%
      dplyr::filter(variable %in% vars) -> scenar_long
    
    scenar_long$scenario <- names(scenarios)[i]
    sim_long <- rbind(sim_long, scenar_long)
  }
  
  levels <- c("Orig.", names(scenarios))
  sim_long$scenario <- factor(sim_long$scenario, levels = levels, ordered = TRUE)
  
  if (limit_y) { 
    y_max <- max(dplyr::filter(sim_long, !(variable %in% c("remaining_stock")))$value)
  } else { y_max <- max(sim_long$value) }
  
  if (smooth_lines) {
    sim_long %>%
      ggplot2::ggplot(aes(x = date, y = value, colour = variable, linetype = scenario)) +
      #scale_linetype_manual(values = c(1, 2, 3, 4))
      #geom_line(size = 1) +
      geom_smooth(size = 1, se=FALSE, span = 0.1) +
      ylab("CCP units") +
      xlab("Date") +
      coord_cartesian(ylim = c(0, y_max)) + 
      scale_color_brewer(palette = "Paired") +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vertical",
            legend.box.just = "left",
            legend.margin = margin(0,0,0,0,unit="pt")
      ) -> plot
  } else {
    sim_long %>%
      ggplot2::ggplot(aes(x = date, y = value, colour = variable, linetype = scenario)) +
      #scale_linetype_manual(values = c(1, 2, 3, 4))
      geom_line(size = 1) +
      #geom_smooth(size = 1, se=FALSE, span = 0.1) +
      ylab("CCP units") +
      xlab("Date") +
      coord_cartesian(ylim = c(0, y_max)) + 
      scale_color_brewer(palette = "Paired") +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vertical",
            legend.box.just = "left",
            legend.margin = margin(0,0,0,0,unit="pt")
      ) -> plot
  }
  
  return(plot)
}
