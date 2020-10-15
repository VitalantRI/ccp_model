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

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reticulate)
#library(future)
#library(promises)
#library(ipc)
#plan(multicore)


# -------- Set up Python environment (compatible with shinyapps.io) -------- #
PYTHON_DEPENDENCIES = c('pip','numpy','scipy','pandas','matplotlib','pyarrow')

VIRTUALENV_NAME = "ccp"

if  (Sys.info()[['user']] == 'shiny') {
  virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
  python_path = Sys.getenv('PYTHON_PATH')
  reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
  reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES)
  reticulate::use_virtualenv(virtualenv_dir, required = T)
} else {
  options(shiny.port = 7450)
  if (!(VIRTUALENV_NAME %in% reticulate::conda_list()$name))  {
    reticulate::conda_create(envname = VIRTUALENV_NAME, packages = "python", conda = "auto")
    reticulate::conda_install(
      envname = VIRTUALENV_NAME,
      packages = PYTHON_DEPENDENCIES,
      forge = FALSE,
      conda = "auto",
      python_version = "3.8.5"
    )
  }
  reticulate::use_condaenv(condaenv = VIRTUALENV_NAME, conda = "auto", required = T)
}
# -------- Set up Python environment (compatible with shinyapps.io) -------- #

# Load requirements

reticulate::source_python("collection_model.py")
source("utility_functions.R")
source("production_model.R")
#import::from(foreach, "%dopar%")

shinyServer(function(input, output, session) {
  
  scenarios <<- list()
  
  #counter <<- 0
  
  observe({
    #counter <<- counter + 1
    #print(counter)
    if (input$epidemic_est_type == "covidactnow") {
      maxdate <- lubridate::today() + lubridate::days(89)
    } else if (input$epidemic_est_type == "ihme") {
      maxdate <- lubridate::as_date(read_epi_maxdate(region = input$region)) - lubridate::days(1)
    } else if (input$epidemic_est_type == "file") {
      maxdate <- lubridate::ymd("2021-12-31")
    }
    if (input$sim_dates[2] <= maxdate) {
      new_end <- input$sim_dates[2]
    } else if (input$sim_dates[2] > maxdate) {
      new_end <- maxdate
    }
    updateDateRangeInput(session, "sim_dates",
                         label = "Simulation period",
                         end = new_end,
                         min = lubridate::ymd("2020-02-01"),
                         max = maxdate
    )
    
    
    # FOR SOME REASON THIS DOES NOT WORK LIKE THE UPDATE ABOVE
    # THE input$recruitment_start_date VARIABLE GETS LENGTH OF ZERO MAKING THE
    # CHECKS FALL OVER
    min_recruit <- input$sim_dates[1]
    max_recruit <- input$sim_dates[2]
    # if (input$recruitment_start_date >= min_recruit & input$recruitment_start_date <= max_recruit) {
    #   new_recruit <- input$recruitment_start_date
    # } else if (input$recruitment_start_date < min_recruit) {
    #   new_recruit <- min_recruit
    # } else if (input$recruitment_start_date > max_recruit) {
    #   new_recruit <- max_recruit
    # }
    updateDateInput(session, "recruitment_start_date",
                    label = "CCP donor recruitment start:",
    #                value = new_recruit,
                    min = min_recruit,
                    max = max_recruit
    )
    
    min_collect <- input$recruitment_start_date
    max_collect <- input$sim_dates[2]
    updateDateInput(session, "collection_start_date",
                    label = "CCP collection start:", 
                    #                value = new_recruit,
                    min = min_collect,
                    max = max_collect
    )
    
  })
  
  read_epi_file <- reactive({
    req(input$epidemic_est_input_file)
    epidata <- readr::read_csv(input$epidemic_est_input_file$datapath)
    validate(
      need("date" %in% colnames(epidata), "File must contain column 'date'"),
      need("discharges" %in% colnames(epidata), "File must contain column 'discharges'"),
      need("admissions" %in% colnames(epidata), "File must contain column 'admissions'"),
      need("icu_admissions" %in% colnames(epidata), "File must contain column 'icu_admissions'")
    )
    epidata %>%
      mutate(date = lubridate::as_date(date)) %>%
      filter(date >= input$sim_dates[1], date <= input$sim_dates[2]) %>%
      mutate(t = seq(from = 0, to = nrow(.)-1, by = 1)) %>%
      select(date, t, discharges, admissions, icu_admissions) %>%
      return()
  })
  
  read_historical_collections_file <- reactive({
    req(input$historical_collections_file)
    readr::read_csv(input$historical_collections_file$datapath) %>%
      mutate(date = lubridate::as_date(date)) %>%
      filter(date >= input$sim_dates[1], date <= input$sim_dates[2]) -> historical_collections
    dates <- seq(input$sim_dates[1], input$sim_dates[2], by = "days")
    collection_start_time <- match(input$collection_start_date, dates)-1
    
    validate(
      need(min(historical_collections$date) == input$collection_start_date, "Collection data file must start on collection start date.")
    )
    
    tibble::tibble(
      date = seq(min(historical_collections$date), max(historical_collections$date), by = "days")
    ) %>%
      mutate(t = seq(from = collection_start_time, to = collection_start_time+nrow(.)-1, by = 1)) %>%
      left_join(historical_collections, by = c("date" = "date")) %>%
      mutate(
        collections = case_when(
          !is.na(collections) ~ as.integer(collections),
          is.na(collections) ~ as.integer(0)
        ),
        units = case_when(
          !is.na(units) ~ as.integer(units),
          is.na(units) ~ as.integer(0)
        ),
        ma_collections = ma(collections, n = 14, sides = 1)
      ) %>%
      return()
  })
  
  read_historical_collections <- reactive({
    if (input$historical_collections == "upload") {
      read_historical_collections_file() %>%
        return()
    }
  })
  
  extract_collections <- reactive({
    read_historical_collections() %>%
      pull(collections) %>%
      return()
  })
  
  extract_units <- reactive({
    read_historical_collections() %>%
      pull(units) %>%
      return()
  })
  
  extract_ma <- reactive({
    read_historical_collections() %>%
      pull(ma_collections) %>%
      return()
  })
  
  read_epi_can <- reactive({
    if (input$can_recovered_type == "discharged" & !(input$compute_prop_hospitalized)) {
      read_epi_can_fordates(
        start_date = input$sim_dates[1],
        end_date = input$sim_dates[2],
        region = input$region,
        prop_hospitalized = input$percent_hospitalized/100,
        prop_direct_icu = input$percent_direct_icu/100,
        prop_stepped_up_icu = input$percent_stepped_up_icu/100,
        delay_step_up = input$delay_admission_icu
      ) %>%
        return()
    } else {
      read_epi_can_fordates(
        start_date = input$sim_dates[1],
        end_date = input$sim_dates[2],
        region = input$region,
        prop_hospitalized = "calculate",
        prop_direct_icu = input$percent_direct_icu/100,
        prop_stepped_up_icu = input$percent_stepped_up_icu/100,
        delay_step_up = input$delay_admission_icu
      ) %>%
        return()
    }
  })
  
  read_epi_ihme <- reactive({
    # temporary workaround, remove column selection option for discharges
    read_epi_ihme_fordates(
      start_date = input$sim_dates[1],
      end_date = input$sim_dates[2],
      region = input$region,
      pathprefix = "data"
    ) %>%
      select_columns_ihme(
        #discharges = "mean", #input$discharges_est_type,
        admissions = input$hosp_est_type,
        icu_admissions = input$icu_est_type
      ) %>%
      return()
  })
  
  load_epidata <- reactive({
    if (input$epidemic_est_type == "file") {
      epidata <- read_epi_file()
    } else if (input$epidemic_est_type == "ihme") {
      epidata <- read_epi_ihme()
    } else if (input$epidemic_est_type == "covidactnow") {
      showModal(modalDialog(
        title = "Covid Act Now projections",
        "The Covid Act Now project has deprecated the API providing access to 
        epidemic projections in the form needed by this tool. We are attempring 
        to arrange access to the relevant estimates and hope to re-enable the 
        use of Covid Act Now epidemic estimates in the near future. Please 
        select 'IHME model' or 'Upload estimates'.",
        easyClose = FALSE
      ))
      # if (input$can_recovered_type == "discharged") {
      #   epidata <- read_epi_can() %>%
      #     select(date, t, discharges, admissions, icu_admissions)
      # } else if (input$can_recovered_type == "recovered") {
      #   epidata <- read_epi_can() %>%
      #     select(date, t, recoveries, admissions, icu_admissions) %>%
      #     rename(discharges = recoveries) # temporary hack, we shouldn't have discharges hardcoded
      # }
    }
    if(min(epidata$date) > input$sim_dates[1]) {
      dates <- seq(input$sim_dates[1], min(epidata$date)-lubridate::ddays(1), by = "days")
      tibble::tibble(
        date = dates,
        t = seq(0, length(dates)-1),
        discharges = rep(0, length(dates)),
        admissions = rep(0, length(dates)),
        icu_admissions = rep(0, length(dates))
      ) %>%
        bind_rows(
          epidata %>%
            mutate(t = seq(from = length(dates), to = nrow(.)+length(dates)-1, by = 1))
        ) %>%
        arrange(date) -> epidata
    }
    
    if (max(epidata$date) < input$sim_dates[2]) {
      dates <- seq(max(epidata$date)+lubridate::ddays(1), input$sim_dates[2], by = "days")
      t0 <- max(epidata$t)+1
      tibble::tibble(
        date = dates,
        t = seq(t0, t0-1+length(dates)),
        discharges = rep(0, length(dates)),
        admissions = rep(0, length(dates)),
        icu_admissions = rep(0, length(dates))
      ) %>%
        bind_rows(epidata) %>%
        arrange(date) -> epidata
    }
    return(epidata)
  })
  
  simulate_collections <- reactive({
    return(NULL)
  })
  
  output$epidata_table <- renderTable({
    load_epidata() %>%
      mutate(date = as.character(date)) %>%
      select(date, recoveries = discharges, admissions, icu_admissions) %>%
      return()
  })
  
  output$discharges_plot <- renderPlot({
    load_epidata() %>%
      pivot_longer(discharges, names_to = "type", values_to = "n") %>%
      filter(type == "discharges") %>%
      ggplot(aes(x = date, y = n, colour = type)) +
      geom_line() + 
      theme_bw() +
      theme(legend.position = "none") %>%
      return()
  })
  
  output$hosp_admissions_plot <- renderPlot({
    load_epidata() %>%
      pivot_longer(admissions, names_to = "type", values_to = "n") %>%
      filter(type == "admissions") %>%
      ggplot(aes(x = date, y = n, colour = type)) +
      geom_line() + 
      theme_bw() +
      theme(legend.position = "none") %>%
      return()
  })
  
  output$icu_admissions_plot <- renderPlot({
    load_epidata() %>%
      pivot_longer(icu_admissions, names_to = "type", values_to = "n") %>%
      filter(type == "icu_admissions") %>%
      ggplot(aes(x = date, y = n, colour = type)) +
      geom_line() + 
      theme_bw() +
      theme(legend.position = "none") %>%
      return()
  })
  
  extract_recovered <- reactive({
    load_epidata() %>%
      pull(discharges) %>%
      return()
  })
  
  extract_hosp_admissions <- reactive({
    load_epidata() %>%
      pull(admissions) %>%
      return()
  })
  
  extract_icu_admissions <- reactive({
    load_epidata() %>%
      pull(icu_admissions) %>%
      return()
  })
  
  compute_collection_capacity <- reactive({
    # Should we account for the actual lengths of the months in the simulation?
    average_days_per_month <- 30.42
    
    # SIMPLIFIED CALCULATION BASED ON TURNS/DAY
    total_procedures_pm_fixed <- input$n_fixed * input$turns_per_day_fixed * input$staffed_days_pm_fixed
    total_procedures_pm_mobile <- input$n_mobile * input$turns_per_day_mobile * input$staffed_days_pm_mobile
    
    available_procedures_pm <- (input$perc_machine_time_fixed/100)*total_procedures_pm_fixed + (input$perc_machine_time_mobile/100)*total_procedures_pm_mobile
    available_procedures_pd <- available_procedures_pm/average_days_per_month
    
    # We are expecting users to capture realistic capacity using the capacity parameters
    if (input$constrain_collections == "no") {
      expected_collections_pd <- floor(available_procedures_pd)
    } else if (input$constrain_collections == "prop_max") {
      expected_collections_pd <- floor(available_procedures_pd * (input$collection_perc_of_capacity/100))
    } else if (input$constrain_collections == "mult_hist_ma" & input$historical_collections %in% c("vitalant","upload")) {
      final_ma <- extract_ma()[length(extract_ma())]
      expected_collections_pd <- floor(pmin(available_procedures_pd, final_ma*input$collection_mult_ma))
    } else { # Handle inconsistent inputs, e.g. constrain by MA and no historical collections - do not constrain
      expected_collections_pd <- floor(available_procedures_pd)
    }
    
    return(expected_collections_pd)
  })
  
  output$collection_capacity_procedures <- renderText({
    prettyNum(compute_collection_capacity(), big.mark=",")
  })
  
  output$collection_capacity_units <- renderText({
    prettyNum(round(compute_collection_capacity() * input$ccp_units_per_collection), big.mark=",")
  })
  
  observeEvent(input$return_info, {
    showModal(modalDialog(
      title = "Donor return time distributions",
      withMathJax("Donor return times are drawn from cumulative distribution functions (for the 2nd, 3rd or 4th and later donations) fit to Vitalant data. If you wish to assume more or less optimistic scenarios, modify the scaling factor.\n
      The maximum scaling factor is set to avoid the probability of return exceeding 1 for any times.\n
      You can also choose to modify the asymptote and growth rate parameters. The functional form is:\n
      $$p_{return}(t) = 0 \\text{ if } t<t_0 \\text{ and}$$\n
      $$p_{return}(t) = s K\\left(1.0 - e^{-s \\lambda(t-t_0)}\\right) \\text{ if } t≥t_0$$\n
      $$\\text{with } s \\text{ the scale factor, } K \\text{ the asymptote, } \\lambda \\text{ the growth rate.}$$\n"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$view_returndists, {
    showModal(modalDialog(
      title = "Donor return time distributions",
      plotOutput("returndist_plot", height = "250px"),
      easyClose = TRUE
    ))
  })
  
  output$returndist_plot <- renderPlot({
    dists <- compute_returndists()
    dists[[1]] %>%
      filter(time < 9999) %>%
      bind_rows(
        dists[[2]] %>%
          filter(time < 9999)
      ) %>%
      bind_rows(
        dists[[3]] %>%
          filter(time < 9999)
      ) %>%
      ggplot(aes(x = time, y = prob)) + #colour = as.factor(return)
      geom_line() +
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
      scale_x_continuous(breaks = seq(0,120,20)) +
      xlab("Days") + ylab("Cumulative prrobability of return") +
      facet_wrap(vars(factor(return, 
                             levels = c("Return for 2nd donation",
                                        "Return for 3rd donation",
                                        "Return for 4th+ donations"
                             ))), ncol = 3) +
      theme_bw() %>%
      return()
  })
  
  compute_returndists <- reactive({
    if (input$customize_returndist) {
      K_2nd = input$K_2nd
      lambda_2nd = input$lambda_2nd
      K_3rd =  input$K_3rd
      lambda_3rd = input$lambda_3rd
      K_4th =  input$K_4th
      lambda_4th = input$lambda_4th
    } else {
      K_2nd = 0.4407
      lambda_2nd = 0.02564
      K_3rd =  0.7004
      lambda_3rd = 0.03913
      K_4th =  0.8077
      lambda_4th = 0.05034
    }
    
    donret_2_dist <- tibble(
      time = c(seq(0,130,1), 9999),
      prob = c(exp_model_func(t = seq(0,130,1), K = K_2nd, lambd = lambda_2nd, scale = input$scaling_factor_2nd, t0 = input$period_temp_ineligible), 1.0),
      return = "Return for 2nd donation"
    )
    donret_3_dist <- tibble(
      time = c(seq(0,130,1), 9999),
      prob = c(exp_model_func(t = seq(0,130,1), K = K_3rd, lambd = lambda_3rd, scale = input$scaling_factor_3rd, t0 = input$period_temp_ineligible), 1.0),
      return = "Return for 3rd donation"
    )
    donret_4_dist <- tibble(
      time = c(seq(0,130,1), 9999),
      prob = c(exp_model_func(t = seq(0,130,1), K = K_4th, lambd = lambda_4th, scale = input$scaling_factor_4th, t0 = input$period_temp_ineligible), 1.0),
      return = "Return for 4th+ donations"
    )
    return(list(donret_2_dist, donret_3_dist, donret_4_dist))
  })
  
  simulate_collection <- reactive({
    dates <- seq(input$sim_dates[1], input$sim_dates[2], by = "days")
    collection_start_time <- match(input$collection_start_date, dates)-1 # correct 1-indexing of R
    recruitment_start_time <- match(input$recruitment_start_date, dates)-1 # correct 1-indexing of R
    n_steps <- length(dates)
    recovered <- extract_recovered()
    capacity <- rep(compute_collection_capacity(), n_steps)
    
    if (input$historical_collections == "sim_all") {
      historical_collections <- NULL
    } else {
      historical_collections <- extract_collections()
    }
    
    donret_2_dist = compute_returndists()[[1]]
    donret_3_dist = compute_returndists()[[2]]
    donret_4_dist = compute_returndists()[[3]]
    
    scale_factor <- input$sim_reduced_perc/100
    
    parameters <- list(
      run_id = 1,
      iterations = as.integer(n_steps),
      report_level = as.integer(3),
      num_agents_init = as.integer(0),
      recovered = as.integer(round(recovered * scale_factor)),
      historical_collections = as.integer(round(historical_collections * scale_factor)),
      prob_eligible = input$perc_eligible/100,
      delay_eligibility = input$delay_eligibility,
      duration_eligibility = input$duration_eligibility,
      temp_ineligibility_period = input$period_temp_ineligible,
      donor_return_dist_type = "parametric",
      donor_return_second = donret_2_dist,
      donor_return_second_scale = input$scaling_factor_2nd,
      donor_return_third = donret_3_dist,
      donor_return_third_scale = input$scaling_factor_3rd,
      donor_return_later = donret_4_dist,
      donor_return_later_scale = input$scaling_factor_4th,
      donor_return_prob_col = "prob",
      delay_recruitment = input$delay_recruitment,
      prob_willing = input$perc_willing/100,
      prob_male = 1-(input$perc_female/100),
      male_relative_propensity = input$male_relative_propensity_perc/100,
      max_recruitment = as.integer(round(input$max_recruitment * scale_factor)),
      prob_other_deferral = input$perc_other_deferral/100,
      prob_failed_donation = input$perc_failed/100,
      prob_hla_female = input$perc_female_HLA_pos/100,#(input$perc_female_everpreg/100)*(input$perc_HLAAb_pos/100),
      prob_tti = input$perc_inf_pos/100,
      prob_Ab_pos = input$perc_Ab_release/100,
      additional_qualification_criterion = FALSE,
      prob_meet_qualification_criterion = NULL,
      max_collections = as.integer(round(capacity * scale_factor)),
      mean_units_per_collection = input$ccp_units_per_collection,
      max_collection_growth = ifelse(input$limit_collection_growth,  1+(input$max_collection_growth_perc/100), 9999),
      recruitment_start = recruitment_start_time,
      collection_start = collection_start_time,
      n_sims = as.integer(input$n_simulations)
    )
    
    if (input$n_simulations == 1) {
      parameters <- reticulate::dict(parameters)
      withProgress(message = 'Running collection simulation', value = 0, {
        py_incProgress <- py_func(
          function(value) {
            incProgress(value)
          })
        
        start <- Sys.time()
        sim <- run_simulation(parameters,
                              seed = as.integer(6570),
                              progress_callback = py_incProgress)
      })
      sim$get_report() %>%
        rename(t = iteration) %>%
        mutate(date = seq(input$sim_dates[1], input$sim_dates[2], by = "days")) %>%
        return()
    } else {
      parameters <- reticulate::dict(parameters)
      id <- showNotification(paste("Running", input$n_simulations, "simulations."), duration = NULL)
      sims <- multi_threaded_run(parameters,
                                 simulations = as.integer(input$n_simulations),
                                 processes = as.integer(ifelse(Sys.info()[['user']] == 'shiny',min(count_cores(),8), count_cores())),
                                 root_seed = as.integer(6570),
                                 return_results = TRUE,
                                 progress_callback = NULL)
      removeNotification(id)
      sum_func = eval(as.name(input$summary_func))
      sims[[1]]$get_report() %>%
        rename(t = iteration) %>%
        mutate(date = seq(input$sim_dates[1], input$sim_dates[2], by = "days")) -> collection_sims
      for (i in 2:length(sims)) {
        sims[[i]]$get_report() %>%
          rename(t = iteration) %>%
          mutate(date = seq(input$sim_dates[1], input$sim_dates[2], by = "days")) %>%
          bind_rows(collection_sims) -> collection_sims
      }
      collection_sims %>%
        group_by(t) %>%
        summarise(
          date = first(date),
          num_agents = round(sum_func(num_agents)),
          ineligible = round(sum_func(ineligible)),
          eligible = round(sum_func(eligible)),
          donors = round(sum_func(donors)),
          eligible_donors = round(sum_func(eligible_donors)),
          active_donors = round(sum_func(active_donors)),
          temp_ineligible = round(sum_func(temp_ineligible)),
          deferred = round(sum_func(deferred)),
          disqualified = round(sum_func(disqualified)),
          unavailable = round(sum_func(unavailable)),
          collections = round(sum_func(collections)),
          units = round(sum_func(units))
        ) %>%
        return()
    }
  })
  
  output$collection_sim_table <- renderTable({
    simulate_collection() %>%
      mutate(date = as.character(date)) %>% #
      select(date, t, donors, active_donors, collections, units) %>%
      return()
  })
  
  output$collection_sim_plot <- renderPlot({
    sim <- simulate_collection()
    ymax <- ifelse(input$scale_collection_plot_donors, max(sim$donors), max(sim$active_donors))
    if (input$smooth_collection_plot) {
      sim %>%
        select(t, date, donors, eligible_donors, active_donors, unavailable, deferred, disqualified, collections, units) %>%
        pivot_longer(donors:units, names_to = "Compartment", values_to = "value") %>%
        mutate(Compartment = factor(Compartment, levels = c("donors","eligible_donors","active_donors", "unavailable", "deferred", "disqualified", "collections","units"))) %>%
        ggplot(aes(x = date, y = value, colour = Compartment)) + 
        geom_vline(xintercept = input$recruitment_start_date, linetype = 2) +
        geom_vline(xintercept = input$collection_start_date, linetype = 2) +
        geom_smooth(se=FALSE, size=1, span=0.1) +
        coord_cartesian(ylim = c(0, ymax)) + # max(sim$active_donors)*1.5
        theme_bw() +
        theme(legend.position="bottom", legend.title = element_blank()) %>%
        return()
    } else {
      sim %>%
        select(t, date, donors, eligible_donors, active_donors, unavailable, deferred, disqualified, collections, units) %>%
        pivot_longer(donors:units, names_to = "Compartment", values_to = "value") %>%
        mutate(Compartment = factor(Compartment, levels = c("donors","eligible_donors","active_donors", "unavailable", "deferred", "disqualified", "collections","units"))) %>%
        ggplot(aes(x = date, y = value, colour = Compartment)) + 
        geom_vline(xintercept = input$recruitment_start_date, linetype = 2) +
        geom_vline(xintercept = input$collection_start_date, linetype = 2) +
        geom_line(size = 1) +
        coord_cartesian(ylim = c(0, ymax)) +
        theme_bw() +
        theme(legend.position="bottom", legend.title = element_blank()) %>%
        return()
    }
    
  })
  
  calculate_prop_released <- reactive({
    # Placeholder for production model
    prop_female <- input$perc_female/100
    prop_female_HLAAb_neg <- 1 - input$perc_female_HLA_pos/100
    prop_HLA_release <- (1 - prop_female) + prop_female*prop_female_HLAAb_neg
    prop_inf_neg <- 1 - input$perc_inf_pos/100
    prop_Ab_release <- input$perc_Ab_release/100
    prop_release <- prop_HLA_release*prop_inf_neg*prop_Ab_release
    return(prop_release)
  })
  
  simulate_production <- reactive({
    
    units_by_timestep <- simulate_collection() %>%
      pull(units)
    r <- input$sim_reduced_perc/100
    
    input_production <- list(
      units_by_timstep = units_by_timestep/r,
      admissions = extract_hosp_admissions(),
      icu_admissions = extract_icu_admissions(),
      perc_hosp_ccp = input$perc_hosp_ccp,
      doses_per_patient_hosp = input$doses_per_patient_hosp,
      perc_icu_ccp = input$perc_icu_ccp,
      doses_per_patient_icu = input$doses_per_patient_icu,
      demand_lag = input$demand_lag,
      start_date = input$sim_dates[1],
      end_date = input$sim_dates[2],
      production_by_timestep = units_by_timestep,
      p_release = 1.0,
      production_lag = input$production_lag
    )
    
    simulate_production_model(input_production) %>%
      return()
  })
  
  output$production_sim_table <- renderTable({
    simulate_production() %>%
      mutate(date = as.character(date)) %>%
      return()
  })
  
  output$production_sim_plot <- renderPlot({
    plot_production_model(simulate_production(), 
                          limit_y = input$limit_y, 
                          smooth_lines = input$smooth_production_plot, 
                          vars = input$plot_variables)
  })
  
  output$prop_units_released <- renderText({
    paste0(round(calculate_prop_released()*100, 1), "%")
  })
  
  output$n_units_released <- renderText({
    prettyNum(round(compute_collection_capacity() * input$ccp_units_per_collection * calculate_prop_released()), big.mark=",")
  })
  
  output$total_units_released <- renderText({
    prettyNum(sum(simulate_production()$units_released), big.mark=",")
  })
  
  output$total_units_instock <- renderText({
    simulate_production() %>%
      pull(remaining_stock) %>%
      last() %>%
      prettyNum(x=., big.mark=",")
  })
  
  output$n_cores <- renderText({
    paste0(as.integer(ifelse(Sys.info()[['user']] == 'shiny',min(count_cores(),8), count_cores())), " processor cores available.")
  })
  
  observeEvent(input$clear_scenarios, {
    scenarios <<- list()
    output$scenario_plot <- NULL
  })
  
  observeEvent(input$add_scenario, {
    #browser()
    units_by_timestep <- simulate_collection() %>%
      pull(units)
    r <- input$sim_reduced_perc/100
    input_production <- list(
      units_by_timstep = units_by_timestep/r,
      admissions = extract_hosp_admissions(),
      icu_admissions = extract_icu_admissions(),
      perc_hosp_ccp = input$perc_hosp_ccp,
      doses_per_patient_hosp = as.numeric(input$doses_per_patient_hosp),
      perc_icu_ccp = input$perc_icu_ccp,
      doses_per_patient_icu = as.numeric(input$doses_per_patient_icu),
      demand_lag = input$demand_lag,
      start_date = input$sim_dates[1],
      end_date = input$sim_dates[2],
      production_by_timestep = units_by_timestep,
      p_release = 1.0,
      production_lag = input$production_lag
    )
    
    scenarios %>%
      add_scenario(
        scenario_name = input$scenario_name,
        var_name = input$scenario_variable,
        new_val = case_when(
          input$scenario_variable == "perc_hosp_ccp" ~ as.numeric(input$s_perc_hosp_ccp),
          input$scenario_variable == "doses_per_patient_hosp" ~ as.numeric(input$s_doses_per_patient_hosp),
          input$scenario_variable == "perc_icu_ccp" ~ as.numeric(input$s_perc_icu_ccp),
          input$scenario_variable == "doses_per_patient_icu" ~ as.numeric(input$s_doses_per_patient_icu),
          input$scenario_variable == "production_lag" ~ as.numeric(input$s_production_lag), #crazy workaround
          input$scenario_variable == "demand_lag" ~ as.numeric(input$s_demand_lag) #crazy workaround
        ),
        input = input_production,
        start_date = NA #input$scenario_start_date
      ) ->> scenarios
    
    output$scenario_plot <- renderPlot({
      plot_scenarios(sim = simulate_production(), 
                     scenarios = scenarios,
                     limit_y = input$limit_y_scenarios, 
                     smooth_lines = input$smooth_scenario_plot, 
                     vars = input$plot_variables_scenarios)
    })
    
  })
  
})
