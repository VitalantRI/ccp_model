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
library(shinyBS)

header <- dashboardHeader(title = "CCP Model")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Model parameters", tabName = "parameters", icon = icon("gear")),
    menuItem("Epidemic estimates", icon = icon("table"), tabName = "epidemic"
    ),
    menuItem("Collections model", tabName = "collections", icon = icon("bar-chart-o")),
    menuItem("Supply model", tabName = "modeloutput", icon = icon("bar-chart-o")),
    menuItem("Scenario analysis", tabName = "scenarios", icon = icon("balance-scale-left", lib = "font-awesome")),
    menuItem("About", tabName = "about", icon = icon("at", lib = "font-awesome"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "parameters",
            fluidRow(
              column(width = 3,
                     box(
                       title = "Simulation options", width = NULL, solidHeader = TRUE, status = "danger",
                       dateRangeInput(
                         "sim_dates",
                         label = "Simulation period",
                         start = lubridate::ymd("2020-03-15"),
                         end = lubridate::today() + lubridate::days(89),
                         min = lubridate::ymd("2020-02-01"),
                         format = "yyyy-mm-dd", 
                         startview = "month", 
                         weekstart = 0,
                         language = "en", width = NULL, autoclose = TRUE
                       ),
                       bsTooltip("sim_dates", "Starting the simulation before donor recruitment starts will allow recruitment of potential CCP donors who were discharged from hospital before donor recruitment began.",
                                 placement = "bottom", trigger = "hover",
                                 options = NULL),
                       dateInput("recruitment_start_date", label = "CCP donor recruitment start:", 
                                 value = lubridate::ymd("2020-04-01"), 
                                 min = NULL, max = NULL,
                                 format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                 language = "en", width = NULL, autoclose = TRUE,
                                 datesdisabled = NULL, daysofweekdisabled = NULL),
                       bsTooltip("recruitment_start_date", 
                                 "Set donor recruitment start earlier than CCP collection start if donors should be recruited and scheduled before collections began.",
                                 placement = "bottom", trigger = "hover",
                                 options = NULL),
                       dateInput("collection_start_date", label = "CCP collection start:", 
                                 value = lubridate::ymd("2020-04-01"), 
                                 min = NULL, max = NULL,
                                 format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                 language = "en", width = NULL, autoclose = TRUE,
                                 datesdisabled = NULL, daysofweekdisabled = NULL),
                       numericInput(
                         "n_simulations",
                         label = "Number of microsimulations",
                         value = 1,
                         min = 1,
                         max = 48,
                         step = 1
                       ),
                       bsTooltip("n_simulations", 
                                 "Running more simulations increases the stability and reliability of results but slows computation. We suggest keeping this at 1 when experimenting with the tool so results generate more quickly, but increasing to >10 when more robust results are needed.",
                                 placement = "bottom", trigger = "hover",
                                 options = NULL),
                       p(em(textOutput("n_cores"))),
                       conditionalPanel(
                         condition = "input.n_simulations > 1",
                         selectInput(
                           "summary_func",
                           label = "Summarize across simulations using",
                           c(
                             "Median" = "median",
                             "Mean" = "mean"
                           ),
                           selected = "median"
                         )
                       ),
                       selectInput(
                         "historical_collections",
                         label = "Historical collections:",
                         c(
                           "Simulate full period" = "sim_all",
                           "Upload collections  data" = "upload"
                         ),
                         selected = "sim_all"
                       ),
                       bsTooltip("historical_collections", 
                                 "If you wish to upload your organization’s collections data, select ‘Upload collections data’.",
                                 placement = "bottom", trigger = "hover",
                                 options = NULL),
                       conditionalPanel(
                         condition = "input.historical_collections == 'upload'",
                         fileInput("historical_collections_file", "Choose CSV File",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv"))
                       ),
                       radioButtons("constrain_collections",
                                    label = "Constrain max collections:",
                                    c(
                                      "No constraint" = "no",
                                      "Perecentage of theoretical max" = "prop_max",
                                      "Multiple of historical moving average" = "mult_hist_ma"
                                    ),
                                    selected = "no"),
                       conditionalPanel(
                         condition = "input.constrain_collections == 'prop_max'",
                         sliderInput(
                           "collection_perc_of_capacity",
                           label = "Percentage of theoretical maximum:",
                           min = 1,
                           max = 100,
                           value = 50,
                           step = 1
                         )
                       ),
                       conditionalPanel(
                         condition = "input.constrain_collections == 'mult_hist_ma'",
                         sliderInput(
                           "collection_mult_ma",
                           label = "Multiple of 14-day moving average:",
                           min = 1,
                           max = 10,
                           value = 3,
                           step = 0.1
                         ),
                       ),
                       checkboxInput("limit_collection_growth", "Limit collection growth", value = FALSE, width = NULL),
                       bsTooltip("limit_collection_growth", 
                                 "If you are uploading historical data, you may wish to enforce a limit on day-over-day collection growth to avoid a ‘jump’ in collections when switching over to simulated collections.",
                                 placement = "bottom", trigger = "hover",
                                 options = NULL),
                       conditionalPanel(
                         condition = "input.limit_collection_growth == true",
                         sliderInput(
                           "max_collection_growth_perc",
                           label = "Maximal % day-over-day collection growth",
                           min = 0,
                           max = 200,
                           value = 10,
                           step = 1
                         )
                       ),
                       sliderInput(
                         "sim_reduced_perc",
                         label = "Scale down microsimulation (% of recovered):",
                         min = 10,
                         max = 100,
                         value = 100,
                         step = 10
                       ),
                       bsTooltip("sim_reduced_perc", 
                                 "Scaling down the number of agents can decrease the time it takes to run each simulation. We recommend setting this below 100% if you are simulating a large epidemic and the tool is updating too slowly.",
                                 placement = "bottom", trigger = "hover",
                                 options = NULL),
                     ),
                     box(
                       title = "Collection & screening", width = NULL, solidHeader = TRUE, status = "primary",
                       sliderInput("perc_female", "Percentage of recovered female", min = 0, max = 100, value = 48, step = 1),
                       sliderInput("perc_female_HLA_pos", "Percentage female donors HLA-Ab positive", min = 0, max = 100, value = 10, step = 1),
                       sliderInput("perc_inf_pos", "Percentage of units TTI marker positive", min = 0, max = 100, value = 0.2, step = 0.1),
                       sliderInput("perc_other_deferral", "Percentage of donors deferred for other reasons", min = 0, max = 20, value =2, step = 0.1),
                       sliderInput("perc_failed", "Percentage of donations with failed phlebotomy", min = 0, max = 20, value = 1, step = 0.1),
                       sliderInput("perc_Ab_release", "Percentage of donors meeting SARS-CoV-2 Ab release criterion",  min = 0, max = 100, value = 75, step = 1)
                     )
              ),
              
              column(width = 3,
                     box(
                       title = "Recruitment", width = NULL, solidHeader = TRUE, status = "primary",
                       numericInput(
                         "max_recruitment",
                         label = "Maximum number of donors recruited/day",
                         value = 50,
                         min = 1,
                         max = 1000,
                         step = 1
                       ),
                       sliderInput(
                         "perc_eligible",
                         label = "Percentage of recovered patients eligible",
                         min = 0,
                         max = 100,
                         value = 100,
                         step = 5
                       ),
                       bsTooltip("perc_eligible", 
                                 "Set to a number less than 100% if some recovered individuals do not meet eligibility criteria.",
                                 placement = "bottom", trigger = "hover",
                                 options = NULL),
                       numericInput(
                         "delay_eligibility",
                         label = "Delay between discharge/recovery and eligibility",
                         value = 14,
                         min = 14,
                         max = 30,
                         step = 0.5
                       ),
                       numericInput(
                         "delay_recruitment",
                         label = "Delay between eligibility and possible recruitment",
                         value = 0,
                         min = 0,
                         max = 120,
                         step = 0.5
                       ),
                       numericInput(
                         "period_temp_ineligible",
                         label = "Period ineligible after donating",
                         value = 7,
                         min = 1,
                         max = 60,
                         step = 1
                       ),
                       bsTooltip("period_temp_ineligible", 
                                 "The U.S. FDA allows CCP donors to return as soon as 7 days after their last donation, but blood centers may have different policies.",
                                 placement = "bottom", trigger = "hover",
                                 options = NULL),
                       sliderInput(
                         "perc_willing",
                         label = "Percentage of eligible donors willing and referred",
                         min = 0,
                         max = 100,
                         value = 15,
                         step = 1
                       ),
                       sliderInput(
                         "male_relative_propensity_perc",
                         label = "Males relative propensity to donate",
                         min = 0,
                         max = 100,
                         value = 86,
                         step = 1
                       ),
                       bsTooltip("male_relative_propensity_perc", 
                                 "At Vitalant, we observed that women were more likely to present for first-time CCP donation. Set this to a number less than 100% to reflect a decrease in likelihood of donating for men.",
                                 placement = "bottom", trigger = "hover",
                                 options = NULL),
                       numericInput(
                         "duration_eligibility",
                         label = "Duration of eligibility",
                         value = 180,
                         min = 30,
                         max = 365,
                         step = 1
                       ),
                       bsTooltip("duration_eligibility", 
                                 "Once this many days since recovery is reached, donors will be made ineligble. This is less important for analyses with shorter time horizons.",
                                 placement = "bottom", trigger = "hover",
                                 options = NULL)
                     )
              ),
              column(width = 3,
                     box(
                       title = "Donor return", width = NULL, solidHeader = TRUE, status = "primary",
                       div(actionButton("return_info", "More information"), align = "center"),
                       sliderInput(
                         "scaling_factor_2nd",
                         label = "Scaling factor for return time (2nd donation)",
                         min = 0.1,
                         max = 2.2,
                         value = 1.0,
                         step = 0.1
                       ),
                       sliderInput(
                         "scaling_factor_3rd",
                         label = "Scaling factor for return time (3rd donation)",
                         min = 0.1,
                         max = 1.4,
                         value = 1.0,
                         step = 0.1
                       ),
                       sliderInput(
                         "scaling_factor_4th",
                         label = "Scaling factor for return time (4th and later donations)",
                         min = 0.1,
                         max = 1.2,
                         value = 1.0,
                         step = 0.1
                       ),
                       checkboxInput("customize_returndist", "Customize parameters", value = FALSE, width = NULL),
                       conditionalPanel(
                         condition = "input.customize_returndist == true",
                         numericInput(
                           "K_2nd",
                           label = "K (return for 2nd donation)",
                           value = 0.44,
                           min = 0.01,
                           max = 1.0,
                           step = 0.01
                         ),
                         numericInput(
                           "lambda_2nd",
                           label = "lambda (return for 2nd donation)",
                           value = 0.025,
                           min = 0.001,
                           max = 0.060,
                           step = 0.001
                         ),
                         numericInput(
                           "K_3rd",
                           label = "K (return for 3rd donation)",
                           value = 0.70,
                           min = 0.01,
                           max = 1.0,
                           step = 0.01
                         ),
                         numericInput(
                           "lambda_3rd",
                           label = "lambda (return for 3rd donation)",
                           value = 0.039,
                           min = 0.001,
                           max = 0.060,
                           step = 0.001
                         ),
                         numericInput(
                           "K_4th",
                           label = "K (return for 4th and later donations)",
                           value = 0.81,
                           min = 0.01,
                           max = 1.0,
                           step = 0.01
                         ),
                         numericInput(
                           "lambda_4th",
                           label = "lambda (return for 4th and later donations)",
                           value = 0.050,
                           min = 0.001,
                           max = 0.060,
                           step = 0.001
                         ),
                       ),
                       div(actionButton("view_returndists", "View distributions"), align = "center"),
                     ),
                     box(
                       title = "Production & demand", width = NULL, solidHeader = TRUE, status = "primary",
                       sliderInput("production_lag","Production lag (collection to release):", min = 0, max = 21, value = 3, step = 1),
                       sliderInput("perc_hosp_ccp","Percentage of acute care patients receiving CCP:", min = 0, max = 100, value = 10, step = 1),
                       numericInput("doses_per_patient_hosp","CCP units/acute care patient:", min = 0, max = 5, value = 1, step = 0.1),
                       sliderInput("perc_icu_ccp","Percentage of critical care patients receiving CCP:", min = 0, max = 100, value = 30, step = 1),
                       numericInput("doses_per_patient_icu","CCP units/critical care patient:", min = 0, max = 5, value = 1, step = 0.1),
                       sliderInput("demand_lag","Demand lag (admission to administration):", min = 0, max = 21, value = 2, step = 1)
                     )
                     
              ),
              column(width = 3,
                     box(
                       title = "Collection capacity", width = NULL, solidHeader = TRUE, status = "primary",
                       numericInput(
                         "n_fixed",
                         label = "Number of fixed apheresis machines",
                         value = 25,
                         min = 1,
                         max = 1000,
                         step = 1
                       ),
                       numericInput(
                         "n_mobile",
                         label = "Number of mobile apheresis machines",
                         value = 25,
                         min = 1,
                         max = 1000,
                         step = 1
                       ),
                       sliderInput(
                         "turns_per_day_fixed",
                         label = "Maximum procedures/day (fixed)",
                         min = 0,
                         max = 5,
                         value = 3,
                         step = 0.1
                       ),
                       sliderInput(
                         "turns_per_day_mobile",
                         label = "Maximum procedures/day (mobile)",
                         min = 0,
                         max = 5,
                         value = 4,
                         step = 0.1
                       ),
                       sliderInput(
                         "staffed_days_pm_fixed",
                         label = "Staffed days per month (fixed)",
                         min = 0,
                         max = 30,
                         value = 20,
                         step = 1
                       ),
                       sliderInput(
                         "staffed_days_pm_mobile",
                         label = "Staffed days per month (mobile)",
                         min = 0,
                         max = 30,
                         value = 20,
                         step = 1
                       ),
                       sliderInput(
                         "perc_machine_time_fixed",
                         label = "Percentage of fixed machine time available for CCP",
                         min = 0,
                         max = 100,
                         value = 40,
                         step = 1
                       ),
                       sliderInput(
                         "perc_machine_time_mobile",
                         label = "Percentage of mobile machine time available for CCP",
                         min = 0,
                         max = 100,
                         value = 40,
                         step = 1
                       ),
                       sliderInput(
                         "ccp_units_per_collection",
                         label = "Average CCP units/procedure",
                         min = 1,
                         max = 5,
                         value = 3.4,
                         step = 0.1
                       )
                     )
              )
            )
    ),
    tabItem(
      tabName = "epidemic",
      fluidRow(
        column(width = 3,
               box(
                 title = "Epidemic parameters", width = NULL, solidHeader = TRUE, status = "primary",
                 selectInput(
                   "epidemic_est_type",
                   label = "Epidemic estimate source",
                   c(
                     #"Covid Act Now model" = "covidactnow",
                     "IHME model" = "ihme",
                     "Upload estimates" = "file"
                   ),
                   selected = "ihme"
                 ),
                 conditionalPanel(
                   condition = "input.epidemic_est_type == 'ihme' || input.epidemic_est_type == 'covidactnow'",
                   selectInput(
                     "region",
                     label = "Region",
                     c(
                       "United States" = "USA",
                       "Alabama" = "AL",
                       "Alaska" = "AK",
                       "Arizona" = "AZ",
                       "Arkansas" = "AR",
                       "California" = "CA",
                       "Colorado" = "CO",
                       "Connecticut" = "CT",
                       "Delaware" = "DE",
                       "Florida" = "FL",
                       "Georgia" = "GA",
                       "Hawaii" = "HI",
                       "Idaho" = "ID",
                       "Illinois" = "IL",
                       "Indiana" = "IN",
                       "Iowa" = "IA",
                       "Kansas" = "KS",
                       "Kentucky" = "KY",
                       "Louisiana" = "LA",
                       "Maine" = "ME",
                       "Maryland" = "MD",
                       "Massachusetts" = "MA",
                       "Michigan" = "MI",
                       "Minnesota" = "MN",
                       "Mississippi" = "MS",
                       "Missouri" = "MO",
                       "Montana" = "MT",
                       "Nebraska" = "NE",
                       "Nevada" = "NV",
                       "New Hampshire" = "NH",
                       "New Jersey" = "NJ",
                       "New Mexico" = "NM",
                       "New York" = "NY",
                       "North Carolina" = "NC",
                       "North Dakota" = "ND",
                       "Ohio" = "OH",
                       "Oklahoma" = "OK",
                       "Oregon" = "OR",
                       "Pennsylvania" = "PA",
                       "Rhode Island" = "RI",
                       "South Carolina" = "SC",
                       "South Dakota" = "SD",
                       "Tennessee" = "TN",
                       "Texas" = "TX",
                       "Utah" = "UT",
                       "Vermont" = "VT",
                       "Virginia" = "VA",
                       "Washington" = "WA",
                       "West Virginia" = "WV",
                       "Wisconsin" = "WI",
                       "Wyoming" = "WY",
                       "District of Columbia" = "DC",
                       "Puerto Rico" = "PR"
                     ),
                     selected = "AL"
                   )
                 ),
                 conditionalPanel(
                   condition = "input.epidemic_est_type == 'covidactnow'",
                   radioButtons("can_recovered_type",
                                label = "Estimates of recovered:",
                                c(
                                  "All recovered" = "recovered",
                                  "Discharged from hospital" = "discharged"
                                ),
                                selected = "discharged"),
                   conditionalPanel(
                     condition = "input.can_recovered_type == 'discharged'",
                     checkboxInput("compute_prop_hospitalized", "Calculate percentage hospitalized", value = TRUE, width = NULL),
                     conditionalPanel(
                       condition = "input.can_recovered_type == 'discharged' && input.compute_prop_hospitalized == false",
                       sliderInput("percent_hospitalized","Percentage hospitalized:", min = 0.5, max = 10, value = 2, step = 0.5)
                     )
                   ),
                   sliderInput("percent_direct_icu","Percentage of hospitalizations direct to ICU:", min = 0, max = 25, value = 4, step = 0.5),
                   sliderInput("percent_stepped_up_icu","Percentage of hospitalizations stepped up to ICU:", min = 0, max = 50, value = 12, step = 0.5),
                   sliderInput("delay_admission_icu","Average time to ICU step-up:", min = 0, max = 25, value = 2, step = 1)
                 ),
                 
                 conditionalPanel(
                   condition = "input.epidemic_est_type == 'ihme'",
                   radioButtons("hosp_est_type",
                                label = "Estimates of hospital admissions:",
                                c(
                                  "IHME mean" = "mean",
                                  "IHME lower" = "lower",
                                  "IHME upper" = "upper"
                                ),
                                selected = "mean"),
                   radioButtons("icu_est_type",
                                label = "Estimates of ICU admissions:",
                                c(
                                  "IHME mean" = "mean",
                                  "IHME lower" = "lower",
                                  "IHME upper" = "upper"
                                ),
                                selected = "mean"),
                   em("Epidemic estimates are sourced from the "),
                   a("Institute for Health Metrics and Evaluation", href="http://www.healthdata.org", target="_blank"),
                   em("at the University of Washington and are reproduced under the terms of the "),
                   a("Creative Commons Attribution-NonCommercial 4.0 International License.", href="https://creativecommons.org/licenses/by-nc/4.0/", target="_blank"),
                   
                 ),
                 conditionalPanel(
                   condition = "input.epidemic_est_type == 'file'",
                   p("Your file must contain the following columns: 'date',
                   'discharges' (number of cases discharged from hospital care,
                   and 'icu_admissions' (number of new admissions to ICU), and
                   must match the simulation dates exactly."),
                   fileInput("epidemic_est_input_file", "Choose CSV File",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv"))
                   
                 )
               )
        ),
        column(width = 4,
               box(
                 width = NULL,
                 status = "primary",
                 tableOutput("epidata_table")
               )
        ),
        column(width = 5,
               box(
                 title = "Recoveries/discharges",
                 width = NULL,
                 status = "primary",
                 plotOutput("discharges_plot", height = "220px")
               ),
               box(
                 title = "Hospital admissions",
                 width = NULL,
                 status = "primary",
                 plotOutput("hosp_admissions_plot", height = "220px")
               ),
               box(
                 title = "ICU admissions",
                 width = NULL,
                 status = "primary",
                 plotOutput("icu_admissions_plot", height = "220px")
               )
        )
        
        
      )
    ),
    tabItem(
      tabName = "collections",
      fluidRow(
        box(
          width = 6,
          status = "warning",
          plotOutput("collection_sim_plot", height = "500px"),
          conditionalPanel(
            condition = "input.sim_reduced_perc < 100",
            br(),br(),
            p("Collections will be scaled up in the supply model.", style="color:red")
          ),
          checkboxInput("scale_collection_plot_donors", "Scale plot to show all donors", value = FALSE, width = NULL),
          radioButtons("smooth_collection_plot",
                       label = "Smooth lines using Loess regression?",
                       c("Yes" = TRUE,
                         "No" = FALSE
                       ),
                       selected = TRUE)
        ),
        box(
          width = 6,
          status = "warning",
          tableOutput("collection_sim_table")
        )
      )
    ),
    tabItem(
      tabName = "modeloutput",
      fluidRow(
        column(
          width = 9,
          box(
            width = NULL,
            status = "warning",
            plotOutput("production_sim_plot", height = "690px")
          )
        ),
        column(
          width = 3,
          fluidRow(
            valueBox(
              uiOutput("collection_capacity_procedures"), "MAX COLLECTION PROCEDURES/DAY", 
              icon = icon("line-chart"), 
              color = "green",
              width = 12
            )  
          ),
          fluidRow(
            valueBox(
              uiOutput("collection_capacity_units"), "MAX UNITS COLLECTED/DAY", 
              icon = icon("line-chart"), 
              color = "purple",
              width = 12
            )
          ),
          fluidRow(
            valueBox(
              uiOutput("prop_units_released"), "AVE. PERCENTAGE UNITS RELEASED",
              icon = icon("line-chart"),
              color = "blue",
              width = 12
            )
          ),
          fluidRow(
            valueBox(
              uiOutput("n_units_released"), "MAX UNITS RELEASED/DAY",
              icon = icon("line-chart"),
              color = "orange",
              width = 12
            )
          ),
          fluidRow(
            valueBox(
              uiOutput("total_units_released"), "TOTAL UNITS RELEASED",
              icon = icon("line-chart"),
              color = "red",
              width = 12
            )
          ),
          fluidRow(
            valueBox(
              uiOutput("total_units_instock"), "TOTAL UNITS IN STOCK",
              icon = icon("line-chart"),
              color = "red",
              width = 12
            )
          )
        )
      ),
      fluidRow(
        box(
          width = 4,
          status = "primary",
          multiInput(
            inputId = "plot_variables", label = "Variables to plot:",
            choices = c("units_collected", "units_released",
                        "demand", "utilized", "remaining_stock", "unmet_demand"),
            selected = c("units_released","demand")
          ),
          radioButtons("limit_y",
                       label = "Scale plot with stockpile?",
                       c("Yes" = FALSE,
                         "No" = TRUE
                       ),
                       selected = TRUE),
          radioButtons("smooth_production_plot",
                       label = "Smooth lines using Loess regression?",
                       c("Yes" = TRUE,
                         "No" = FALSE
                       ),
                       selected = FALSE)
        ),
        box(
          width = 8,
          status = "warning",
          #title = "Table",
          tableOutput("production_sim_table")
        )
      )
    ),
    tabItem(tabName = "scenarios",
            fluidRow(
              column(width = 5,
                     box(
                       title = "Notice", width = NULL, solidHeader = TRUE, status = "danger",
                       p("Scenario analysis functionality is still under development.")
                     ),
                     box(
                       title = "Scenarios", width = NULL, solidHeader = TRUE, status = "primary",
                       textInput("scenario_name", "Senario name"),
                       selectInput(
                         "scenario_variable",
                         label = "Parameter to vary",
                         c(
                           "Percentage of acute care patients receiving CCP" = "perc_hosp_ccp",
                           "CCP units/acute care patient" = "doses_per_patient_hosp",
                           "Percentage of critical care patients receiving CCP" = "perc_icu_ccp",
                           "CCP units/critical care patient" = "doses_per_patient_icu",
                           "Production lag (collection to release)" = "production_lag",
                           "Demand lag (admission to administration)" = "demand_lag"
                         )
                       ),
                       conditionalPanel(
                         condition = "input.scenario_variable == 'perc_hosp_ccp'",
                         sliderInput("s_perc_hosp_ccp","Percentage of acute care patients receiving CCP:", min = 0, max = 100, value = 10, step = 1),
                       ),
                       conditionalPanel(
                         condition = "input.scenario_variable == 'doses_per_patient_hosp'",
                         numericInput("s_doses_per_patient_hosp","CCP units/acute care patient:", min = 0, max = 5, value = 1, step = 0.1),
                       ),
                       conditionalPanel(
                         condition = "input.scenario_variable == 'perc_icu_ccp'",
                         sliderInput("s_perc_icu_ccp","Percentage of critical care patients receiving CCP:", min = 0, max = 100, value = 30, step = 1),
                       ),
                       conditionalPanel(
                         condition = "input.scenario_variable == 'doses_per_patient_icu'",
                         numericInput("s_doses_per_patient_icu","CCP units/critical care patient:", min = 0, max = 5, value = 1, step = 0.1),
                       ),
                       conditionalPanel(
                         condition = "input.scenario_variable == 'production_lag'",
                         sliderInput("s_production_lag","Production lag (collection to release):", min = 0, max = 21, value = 3, step = 1),
                       ),
                       conditionalPanel(
                         condition = "input.scenario_variable == 'demand_lag'",
                         sliderInput("s_demand_lag","Demand lag (admission to administration):", min = 0, max = 21, value = 2, step = 1)
                       ),
                       div(actionButton("add_scenario", "Add scenario"), 
                           actionButton("clear_scenarios", "Clear scenarios"),
                           align = "center"),
                     )
                     
              ),
              column(width = 7,
                     box(
                       #title = "Plot",
                       width = NULL,
                       status = "warning",
                       plotOutput("scenario_plot", height = "500px")
                     ),
                     box(
                       width = NULL,
                       status = "primary",
                       multiInput(
                         inputId = "plot_variables_scenarios", label = "Variables to plot:",
                         choices = c("units_collected", "units_released",
                                     "demand", "utilized", "remaining_stock", "unmet_demand"),
                         selected = c("units_released","demand")
                       ),
                       radioButtons("limit_y_scenarios",
                                    label = "Scale plot with stockpile?",
                                    c("Yes" = FALSE,
                                      "No" = TRUE
                                    ),
                                    selected = TRUE),
                       radioButtons("smooth_scenario_plot",
                                    label = "Smooth lines using Loess regression?",
                                    c("Yes" = TRUE,
                                      "No" = FALSE
                                    ),
                                    selected = TRUE)
                     )
              )
            )
    ),
    tabItem(
      tabName = "about",
      fluidRow(
        column(width = 12,
               box(
                 title = "About this tool", width = NULL, solidHeader = TRUE, status = "warning",
                 p("This tool was developed by researchers at Vitalant Research 
                   Institute to model COVID-19 Convalescent Plasama collection 
                   and distribution to inform policy and operations."),
                 p("The model has two main components, an agent-based 
                   microsimulation used to simulate the donor recruitment and 
                   collection process, and a linked production and demand model 
                   to project CCP release, distribution and inventory. The model 
                   utilizes estimates and projections of COVID-19 patients 
                   discharged from hospital supplied by an external epidemic 
                   model to create agents that can be recruited as CCP donors. 
                   New hospital admissions from the same epidemic model are used 
                   to inform the CCP demand model."),
                 h3("Model structure"),
                 p("The structure of the microsimulation model is shown in the figure below."),
                 div(img(src="model_diagram.png", height = "300px"), style="text-align: center;"),
                 p("The production and demand model applies multipliers to the 
                   output of the microsimulation model to project the number of 
                   CCP units utilized and stockpiled over time."),
                 h3("Authors"),
                 tags$ul(
                   tags$li(a("Eduard Grebe", href="mailto:EGrebe@vitalant.org"), " (Vitalant Research Institute, University of California San Francisco)"), 
                   tags$li(a("W. Alton Russell", href="mailto:altonr@stanford.edu"), " (Stanford University, Vitalant Research Institute)"), 
                   tags$li(a("Brian Custer", href="mailto:BCuster@vitalant.org"), " (Vitalant Research Institute, University of California San Francisco)")
                 ),
                 h3("Acknowledgements"),
                 p("The authors thank the following individuals for information on CCP operations and advice on model development:"),
                 tags$ul(
                   tags$li("Larry J. Dumont (Vitalant Research Institute, Geisel School of Medicine at Dartmouth, University of Colorado School of Medicine)"), 
                   tags$li("Ralph R. Vassallo (Vitalant, University of New Mexico School of Medicine)"), 
                   tags$li("Cliff Numark (Vitalant)"),
                   tags$li("Travis Glanzer (Vitalant)"),
                   tags$li("Paula Villalobos-Jimenez (Vitalant)"),
                   tags$li("Janice Douke (Vitalant)"),
                   tags$li("Roxanne Tata (Vitalant)"),
                   tags$li("Jackie Vannoy (Vitalant)"),
                   tags$li("Sarina Swan (Vitalant)"),
                   tags$li("Candelaria Acosta (Vitalant)")
                 ),
                 p("The authors further thank ", a("Nathan Geffen", href = "https://www.simhub.online/nathan/", target="_blank"), "for technical advice and acknowledge taking direct inspiration from his Python microsimulation code, and Andrea Stone for legal advice."),
                 em("This software is made possible by numerous open source projects, including the ", 
                    a("R", href = "https://www.r-project.org", target="_blank"), 
                    " and ",
                    a("Python", href = "https://www.python.org", target="_blank"),
                    " programming languages, the ",
                    a("Shiny", href = "https://shiny.rstudio.com", target="_blank"),
                    " framework, the ",
                    a("tidyverse", href = "https://www.tidyverse.org", target="_blank"),
                    " suite of R packages and the ",
                    a("Numpy", href = "https://numpy.org", target="_blank"),
                    " and ",
                    a("Scipy", href = "https://www.scipy.org", target="_blank"),
                    " Python packages."),
                 h3("Epidemic estimates"),
                 p("Epidemic estimates are sourced from the ", 
                   a("Institute for Health Metrics and Evaluation", href="http://www.healthdata.org", target="_blank"), 
                   "at the University of Washington and are reproduced under the terms of the ",
                   a("Creative Commons Attribution-NonCommercial 4.0 International License.", href="https://creativecommons.org/licenses/by-nc/4.0/", target="_blank")
                 ),
                 h3("Usage terms"),
                 p("Copyright © 2020 Vitalant and W. Alton Russell. All rights reserved."),
                 p("This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version."),
                 p("The software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details."),
                 em("The source code can be accessed on ", a("Github", href = "https://github.com/VitalantRI/ccp_model", target="_blank")),
                 br(),
                 br(),
                 div(img(src="VRI_logo.png", height = "150px"), style="text-align: center;"),
                 br(),
                 div(img(src="UCSF_logo.png", height = "100px", style="padding-right: 50px;"), 
                     img(src="Stanford_logo.png", height = "125px"),
                     style="text-align: center;"),
               )
        )
      )
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)