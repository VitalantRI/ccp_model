setwd("~/dev/ccp_model/manuscript_factors_driving_availability/")
library(data.table)
library(arrow)
library(lubridate)
library(ggplot2)
library(gsubfn)
library(gridExtra)
theme_set(theme_bw())
states <- c("MA", "IN", "LA", "CT", "MS", "VA", "MD", "NY", "IL", "NJ", "CA")
##ONE TIME DATA CLEANING for 9/5/20 run
# 
# report <- fread("G:/My Drive/Blood Transfusion/ccpmodelling_localonly/concat_report_2020_09_07.csv")
# report[ , run_batch_sim := paste0(run_id, "_", formatC(sim_id, width = 3, flag = "0"), formatC(batch, width = 3, flag = "0"))]
# report[ , run_sim_id := NULL]
# report[ , uniqueN(run_batch_sim), by = state]
# ## Due to timeouts, some states have >10K simulations. Removing excess sims
# #MA
# report[state == "MA",uniqueN(run_batch_sim), by = run_id]
# report <- report[!(state == "MA" & run_id == 287179)]
# #MS
# report[state == "MS",uniqueN(run_batch_sim), by = run_id]
# report <- report[!(state == "MS" & run_id == 466365)]
# #LA
# report[state == "LA",uniqueN(run_batch_sim), by = c("run_id", "parametricRT")]
# report[state == "LA" & parametricRT == 0 & run_id == 508264,
#        uniqueN(run_batch_sim),
#        by = c("batch")]
# report[state == "LA" & parametricRT == 0 & run_id == 508264 & batch < 8,
#        uniqueN(run_batch_sim)]
# report <- report[!(state == "LA" & parametricRT == 0 & run_id == 508264 & batch >= 8)]
# #NY
# report[state == "NY",uniqueN(run_batch_sim), by = c("run_id", "parametricRT")]
# report <- report[!(state == "NY" & (run_id == 520895 | run_id == 763137))]
# #Verify 10K sims per state/parametric combo
# report[ , uniqueN(run_batch_sim), by = c("state", "parametricRT")]
# #Save clean version
# fwrite(report, "G:/My Drive/Blood Transfusion/ccpmodelling_localonly/concat_report_clean_2020_09_07.csv")


# FUNCTIONS -------------
process_report <- function(report, params){
  report[ , run_batch_sim_state := paste0(run_id, "_", 
                                          formatC(sim_id, width = 3, flag = "0"), "_",
                                          formatC(batch, width = 3, flag = "0"), "_",
                                          state)]
  params[ , run_batch_sim_state := paste0(run_id, "_", 
                                          formatC(id, width = 3, flag = "0"), "_",
                                          formatC(batch, width = 3, flag = "0"), "_",
                                          state)]
  
  params <- params[run_batch_sim_state %in% report[ , unique(run_batch_sim_state)]]
  report <- report[run_batch_sim_state %in% params[ , unique(run_batch_sim_state)]]
  
  # Metric: percent capacity utilized
  report <- report[params[ , c("run_batch_sim_state", "max_collections")], on="run_batch_sim_state"]
  report <- report[!is.na(state)]
  report[ , perc_capacity_used := collections/max_collections]
  
  report[ , V1 := NULL]
  report[ , `Unnamed: 0` := NULL]
  
  return(list(report = report, params = params))
}


demand_met <- function(units_avail, demand){
  demand_met <- c(min(demand[1], units_avail[1]))
  stock <- c(units_avail[1] - demand_met[1])
  unmet_demand <- c(demand[1] - demand_met[1])
  for (i in 2:length(demand)){
    demand_met <- c(demand_met, min(stock[i-1]+units_avail[i], demand[i]))
    stock <- c(stock, stock[i-1] + units_avail[i] - demand_met[i])
    unmet_demand <- c(unmet_demand, demand[i] - demand_met[i])
  }
  
  return(list(demand_met, stock, unmet_demand))
}

add_demand <- function(report){
  # Create data.table with all run_batch_sim_state codes and demand for iterations 0:199 for corresponding state
  demand_params <- unique(report[, c("run_batch_sim_state", "state")])
  demand_params[ , CCP_prob_needed := runif(nrow(demand_params), min = .1, max = .45)]
  demand_params[ , CCP_units_per_pt := runif(nrow(demand_params), min = 1.5, max = 4)]
  demand_params[ , CCP_delay := sample(1:5, nrow(demand_params), replace = TRUE)]
  
  setkey(demand_params, state); setkey(admits_all, state)
  demand <- merge(demand_params, admits_all, all=TRUE, by=.EACHI, allow.cartesian = TRUE)
  
  demand[CCP_delay == 1, Adm_lag :=  shift(admissions, n=1, type = "lag", fill = 0), by = run_batch_sim_state]
  demand[CCP_delay == 2, Adm_lag :=  shift(admissions, n=2, type = "lag", fill = 0), by = run_batch_sim_state]
  demand[CCP_delay == 3, Adm_lag :=  shift(admissions, n=3, type = "lag", fill = 0), by = run_batch_sim_state]
  demand[CCP_delay == 4, Adm_lag :=  shift(admissions, n=4, type = "lag", fill = 0), by = run_batch_sim_state]
  demand[CCP_delay == 5, Adm_lag :=  shift(admissions, n=5, type = "lag", fill = 0), by = run_batch_sim_state]
  demand[ , demand := round(CCP_prob_needed*CCP_units_per_pt*Adm_lag)]
  
  # Read demand into report
  report <- report[demand[ , c("run_batch_sim_state", "iteration", "demand")], on = c("run_batch_sim_state", "iteration")]
  
  #Calculate demand met, stock, unmet demand
  report[ , c("demand_met", "stock", "unmet_demand") := demand_met(units, demand), 
          by=run_batch_sim_state]
  report[ , perc_demand_unmet := ifelse(demand == 0, 0, unmet_demand/demand)]
  report[ , perc_demand_met := ifelse(demand == 0, 1, demand_met/demand)]
  
  return(list(report = report, demand = demand))
}


# Read and pre-process --------

# # Extracting arrivals from COVID Act NOw

admits_all <- data.table(
  date = POSIXct(),
  admissions = numeric(),
  discharges = numeric(),
  state = character(),
  iteration = numeric()
)
for (state in states){
  state_data <- data.table(read_feather(paste0("./data/covidactnow_", state, "_2020-09-08.feather")))
  start_date = state_data[discharges > 0, min(date)]
  end_date = start_date + ddays(199)
  admits <- state_data[date >= start_date & date <= end_date, c("date", "admissions", "discharges")]
  admits[ , state := state]
  admits[ , iteration := 0:199]
  admits_all <- rbind(admits_all, admits)
}
rm(admits); rm(state_data)
admits_all[ , perc_admit := admissions/max(admissions), by = state]
admits_all[ , perc_disch := discharges/max(discharges), by = state]

dt_population <- data.table(
  state = c('NJ', 'MA', 'IN', 'LA', 'CT', 'MS', 'VA', 'MD', 'NY', 'IL', 'CA'),
  pop = c(8882190, 6892503, 6732219, 4648794, 3565287, 2976149, 8535519, 
         6045680, 19453561, 12671821, 39512223)
)


#% peak discharge only
ggplot() + 
  geom_line(data=admits_all[state %in% c("CA", "MD", "MS", "NY")], 
            aes(y = perc_disch, x = iteration, color="% peak discharges"),
            size=1)+
  facet_wrap(vars(state), nrow = 1)+
  theme_void()+
  theme(legend.position="none")+
  scale_x_continuous(limits=c(15, 200))
ggsave("figs/epi_trajectories_CA_MD_MS_NY.png",
       width = 4.7, height = 1, units = "in")
# 2020-09-07
# report <- fread("G:/My Drive/Blood Transfusion/ccpmodelling_localonly/concat_report_2020_09_07.csv")
# params <- fread("G:/My Drive/Blood Transfusion/ccpmodelling_localonly/concat_params_2020_09_07.csv")
# 
# report_emp <- report[parametricRT == 0]
# report_par <- report[parametricRT == 1]
# rm(report)
# 
# list[report_emp, params_emp] <- process_report(report_emp, params)
# list[report_emp, demand_emp] <- add_demand(report_emp)
# fwrite(report_emp,
#        "G:/My Drive/Blood Transfusion/ccpmodelling_localonly/report_emp_2020_09_07.csv")
# fwrite(demand_emp,
#        "G:/My Drive/Blood Transfusion/ccpmodelling_localonly/demand_emp_2020_09_07.csv")
# fwrite(params_emp,
#        "G:/My Drive/Blood Transfusion/ccpmodelling_localonly/params_emp_2020_09_07.csv")
# 
# list[report_par, params_par] <- process_report(report_par, params)
# list[report_par, demand_par] <- add_demand(report_par)
# fwrite(report_par,
#        "G:/My Drive/Blood Transfusion/ccpmodelling_localonly/report_par_2020_09_07.csv")
# fwrite(demand_par,
#        "G:/My Drive/Blood Transfusion/ccpmodelling_localonly/demand_par_2020_09_07.csv")
# fwrite(params_par,
#        "G:/My Drive/Blood Transfusion/ccpmodelling_localonly/params_par_2020_09_07.csv")



# #2020-09-09
# report <- fread("G:/My Drive/Blood Transfusion/ccpmodelling_localonly/concat_report_2020_09_09.csv")
# params <- fread("G:/My Drive/Blood Transfusion/ccpmodelling_localonly/concat_params_2020_09_09.csv")
#2020-09-23
report <- fread("gunzip -cq outputs/concat_report_2020_09_23.csv.gz")
report[ , .N/200, by = c("state", "parametricRT")]
report <- report[!(run_id == 737014 & batch == 18)]
params <- fread("gunzip -cq outputs/concat_params_2020_09_23.csv.gz")

report_emp <- report[parametricRT == 0]
report_par <- report[parametricRT == 1]
rm(report)

list[report_emp, params_emp] <- process_report(report_emp, params)
report_emp[ , uniqueN(run_batch_sim_state), by = c("state", "parametricRT")]
list[report_emp, demand_emp] <- add_demand(report_emp)


list[report_par, params_par] <- process_report(report_par, params)
report_par[ , uniqueN(run_batch_sim_state), by = c("state", "parametricRT")]
params_par[ , uniqueN(run_batch_sim_state), by = c("state", "parametricRT")]

list[report_par, demand_par] <- add_demand(report_par)

write_feather(report_par, "report_par_2020_09_23.feather")
write_feather(report_emp, "report_emp_2020_09_23.feather")
write_feather(demand_par, "demand_par_2020_09_23.feather")
write_feather(demand_emp, "demand_emp_2020_09_23.feather")

write_feather(params_par, "params_par_2020_09_23.feather")
write_feather(params_emp, "params_emp_2020_09_23.feather")




##
#
# Analyzing results - Fixed return
#
## ---------------

# Read already pre-processed
# report_emp <- fread("G:/My Drive/Blood Transfusion/ccpmodelling_localonly/report_emp_2020_09_09.csv")
report_emp <- data.table(read_feather("report_emp_2020_09_23.feather"))
# CAPACITY QUANTILE PLOT
report_emp <- report_emp[ !is.na(sim_id)]

capac_quants_by_state <- report_emp[, 
                                    list(prob = paste0(seq(0, 100, 10),"%"),
                                         quant = quantile(perc_capacity_used, probs=seq(0, 1, .1))), 
                                    by = c("state", "iteration")]


capac_quants_by_state_wide <- dcast(capac_quants_by_state, state + iteration ~ prob, value.var = "quant")



ggplot() + 
  #geom_ribbon(data=quants_by_state_wide, aes(x=iteration, ymin=`0%`, ymax=`50%`,alpha="0-50th"), fill="darkcyan") +
  geom_ribbon(data=capac_quants_by_state_wide, aes(x=iteration, ymin=`0%`, ymax=`60%`,fill="<60th")) +
  geom_ribbon(data=capac_quants_by_state_wide, aes(x=iteration, ymin=`60%`, ymax=`70%`,fill="60-70th")) +
  geom_ribbon(data=capac_quants_by_state_wide, aes(x=iteration, ymin=`70%`, ymax=`80%`,fill="70-80th")) +
  geom_ribbon(data=capac_quants_by_state_wide, aes(x=iteration, ymin=`80%`, ymax=`90%`,fill="80-90th")) +
  geom_ribbon(data=capac_quants_by_state_wide, aes(x=iteration, ymin=`90%`, ymax=`100%`,fill=">90th")) +
  geom_line(data=capac_quants_by_state_wide, aes(x=iteration, y = `50%`, color="Median"))+
  geom_line(data=admits_all, aes(y = perc_disch, x = iteration, color="% peak discharges"))+
  xlab("days from first discharge")+
  ylab("Percent capacity used")+
  theme(legend.position="bottom")+
  scale_color_manual(name="", 
                     breaks=c("Median", "% peak discharges" ), 
                     values= c("black", "firebrick"))+
  scale_fill_manual(name="Percentile of simulations",
                    breaks=c("<60th", "60-70th", "70-80th", "80-90th", ">90th"),
                    values = c('#0868ac', '#43a2ca', '#7bccc4', '#bae4bc', '#f0f9e8'),
                    guide = guide_legend(label.position = "bottom"))+
  facet_wrap(~state, nrow = 2)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  scale_x_continuous(breaks = seq(15, 200, 37), limits = c(15, 200))

ggsave("figs/capac_used_by_state_quantiles.png",
       width = 8.5, height = 4, units = "in")
# DEMAND MET PERCENT QUANTILE PLOT

demand_unmet_quants_by_state <- report_emp[, 
                                           list(prob = paste0(seq(0, 100, 10),"%"),
                                                quant = quantile(perc_demand_unmet, probs=seq(0, 1, .1))), 
                                           by = c("state", "iteration")]


demand_unmet_quants_by_state_wide <- dcast(demand_unmet_quants_by_state, state + iteration ~ prob, value.var = "quant")
#demand_quants_by_state_wide <- demand_quants_by_state_wide[iteration > 14]




ggplot() + 
  #geom_ribbon(data=quants_by_state_wide, aes(x=iteration, ymin=`0%`, ymax=`50%`,alpha="0-50th"), fill="darkcyan") +
  geom_ribbon(data=demand_unmet_quants_by_state_wide, aes(x=iteration, ymin=`0%`, ymax=`60%`,fill="<60th")) +
  geom_ribbon(data=demand_unmet_quants_by_state_wide, aes(x=iteration, ymin=`60%`, ymax=`70%`,fill="60-70th")) +
  geom_ribbon(data=demand_unmet_quants_by_state_wide, aes(x=iteration, ymin=`70%`, ymax=`80%`,fill="70-80th")) +
  geom_ribbon(data=demand_unmet_quants_by_state_wide, aes(x=iteration, ymin=`80%`, ymax=`90%`,fill="80-90th")) +
  geom_ribbon(data=demand_unmet_quants_by_state_wide, aes(x=iteration, ymin=`90%`, ymax=`100%`,fill=">90th")) +
  geom_line(data=demand_unmet_quants_by_state_wide, aes(x=iteration, y = `50%`, color="Median"))+
  geom_line(data=admits_all, aes(y = perc_disch, x = iteration, color="% peak discharges"))+
  xlab("days from first discharge")+
  ylab("Percent demand unmet")+
  theme(legend.position="bottom")+
  scale_color_manual(name="", 
                     breaks=c("Median", "% peak discharges" ), 
                     values= c("black", "firebrick"))+
  scale_fill_manual(name="Percentile of demand unmet",
                    breaks=c("<60th", "60-70th", "70-80th", "80-90th", ">90th"),
                    values = c('#0868ac', '#43a2ca', '#7bccc4', '#bae4bc', '#f0f9e8'),
                    guide = guide_legend(label.position = "bottom"))+
  facet_wrap(~state, nrow = 2)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%")) +
  scale_x_continuous(breaks = seq(15, 200, 37), limits = c(15, 200))

ggsave("figs/demand_unmet_by_state_quantiles.png",
       width = 8.5, height = 4, units = "in")


## Aggregate % demand unmet from day 15
dt_outcomes <- report_emp[iteration > 14, 
                          list(
                            "perc_demand_unmet" = sum(unmet_demand)/sum(demand),
                            "perc_capacity_used" = sum(collections)/sum(max_collections)
                          ), 
                          by = c("state", "run_batch_sim_state")]
ggplot(dt_outcomes)+
  geom_boxplot(aes(x = reorder(state, perc_demand_unmet, FUN= median), y = perc_demand_unmet))+
  ylab("Total percent demand unmet")+
  xlab("State")

ggsave("figs/demand_unmet_by_state_boxplot.png",
       width = 6, height = 6, units = "in")


# % DEMAND MET
demand_met_quants_by_state <- report_emp[, 
                                         list(prob = paste0(seq(0, 100, 10),"%"),
                                              quant = quantile(perc_demand_met, probs=seq(0, 1, .1))), 
                                         by = c("state", "iteration")]


demand_met_quants_by_state_wide <- dcast(demand_met_quants_by_state, state + iteration ~ prob, value.var = "quant")
#demand_quants_by_state_wide <- demand_quants_by_state_wide[iteration > 14]

ggplot() + 
  #geom_ribbon(data=quants_by_state_wide, aes(x=iteration, ymin=`0%`, ymax=`50%`,alpha="0-50th"), fill="darkcyan") +
  geom_ribbon(data=demand_met_quants_by_state_wide, aes(x=iteration, ymin=`0%`, ymax=`10%`,fill="<10th")) +
  geom_ribbon(data=demand_met_quants_by_state_wide, aes(x=iteration, ymin=`10%`, ymax=`20%`,fill="10-20th")) +
  geom_ribbon(data=demand_met_quants_by_state_wide, aes(x=iteration, ymin=`20%`, ymax=`30%`,fill="20-30th")) +
  geom_ribbon(data=demand_met_quants_by_state_wide, aes(x=iteration, ymin=`30%`, ymax=`40%`,fill="30-40th")) +
  geom_ribbon(data=demand_met_quants_by_state_wide, aes(x=iteration, ymin=`40%`, ymax=`100%`,fill=">40th")) +
  geom_line(data=demand_met_quants_by_state_wide, aes(x=iteration, y = `50%`, color="Median"))+
  geom_line(data=admits_all, aes(y = perc_disch, x = iteration, color="% peak discharges"))+
  xlab("days from first discharge")+
  ylab("Percent demand met")+
  theme(legend.position="bottom")+
  scale_color_manual(name="", 
                     breaks=c("Median", "% peak discharges" ), 
                     values= c("black", "firebrick"))+
  scale_fill_manual(name="Percentile of demand",
                    breaks=c("<10th", "10-20th", "20-30th", "30-40th", ">40th"),
                    values = c('#f0f9e8', '#bae4bc' , '#7bccc4', '#43a2ca', '#0868ac'),
                    guide = guide_legend(label.position = "bottom"))+
  facet_wrap(~state, nrow = 2)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%")) +
  scale_x_continuous(breaks = seq(15, 200, 37), limits = c(15, 200))

ggsave("figs/demand_met_by_state_quantiles.png",
       width = 9, height = 4.2, units = "in")

# Unmet demand in units
demand_unmet_u_quants_by_state <- report_emp[, 
                                             list(prob = paste0(seq(0, 100, 10),"%"),
                                                  quant = quantile(unmet_demand, probs=seq(0, 1, .1))), 
                                             by = c("state", "iteration")]


demand_unmet_u_quants_by_state_wide <- dcast(demand_unmet_u_quants_by_state, state + iteration ~ prob, value.var = "quant")
#demand_quants_by_state_wide <- demand_quants_by_state_wide[iteration > 14]




ggplot() + 
  #geom_ribbon(data=quants_by_state_wide, aes(x=iteration, ymin=`0%`, ymax=`50%`,alpha="0-50th"), fill="darkcyan") +
  geom_ribbon(data=demand_unmet_u_quants_by_state_wide, aes(x=iteration, ymin=`0%`, ymax=`60%`,fill="<60th")) +
  geom_ribbon(data=demand_unmet_u_quants_by_state_wide, aes(x=iteration, ymin=`60%`, ymax=`70%`,fill="60-70th")) +
  geom_ribbon(data=demand_unmet_u_quants_by_state_wide, aes(x=iteration, ymin=`70%`, ymax=`80%`,fill="70-80th")) +
  geom_ribbon(data=demand_unmet_u_quants_by_state_wide, aes(x=iteration, ymin=`80%`, ymax=`90%`,fill="80-90th")) +
  geom_ribbon(data=demand_unmet_u_quants_by_state_wide, aes(x=iteration, ymin=`90%`, ymax=`100%`,fill=">90th")) +
  geom_line(data=demand_unmet_u_quants_by_state_wide, aes(x=iteration, y = `50%`, color="Median"))+
  #geom_line(data=admits_all, aes(y = perc_disch, x = iteration, color="% peak discharges"))+
  xlab("days from first discharge")+
  ylab("Units of unmet demand")+
  theme(legend.position="bottom")+
  scale_color_manual(name="", 
                     breaks=c("Median", "% peak discharges" ), 
                     values= c("black", "firebrick"))+
  scale_fill_manual(name="Percentile of demand unmet",
                    breaks=c("<60th", "60-70th", "70-80th", "80-90th", ">90th"),
                    values = c('#0868ac', '#43a2ca', '#7bccc4', '#bae4bc', '#f0f9e8'),
                    guide = guide_legend(label.position = "bottom"))+
  facet_wrap(~state, nrow = 2)+
  #scale_y_continuous(labels = function(x) paste0(x*100,"%")) +
  scale_x_continuous(breaks = seq(15, 200, 37), limits = c(15, 200))


ggsave("figs/demand_unmet_units_by_state_quantiles.png",
       width = 9, height = 4.2, units = "in")


## PERCENT UNAVAILABLE/DEFERRED
report_emp[ , perc_unavailable_deferred := ifelse(donors == 0, 0, (unavailable + deferred) /  donors)]

perc_unav_def_quants_by_state <- report_emp[, 
                                            list(prob = paste0(seq(0, 100, 10),"%"),
                                                 quant = quantile(perc_unavailable_deferred, probs=seq(0, 1, .1))), 
                                            by = c("state", "iteration")]


perc_unav_def_quants_by_state_wide <- dcast(perc_unav_def_quants_by_state, state + iteration ~ prob, value.var = "quant")


ggplot() + 
  #LOWERS
  geom_ribbon(data=perc_unav_def_quants_by_state_wide, aes(x=iteration, ymin=`0%`, ymax=`10%`,fill="100%")) +
  geom_ribbon(data=perc_unav_def_quants_by_state_wide, aes(x=iteration, ymin=`10%`, ymax=`20%`,fill="80%")) +
  geom_ribbon(data=perc_unav_def_quants_by_state_wide, aes(x=iteration, ymin=`20%`, ymax=`30%`,fill="60%")) +
  geom_ribbon(data=perc_unav_def_quants_by_state_wide, aes(x=iteration, ymin=`30%`, ymax=`40%`,fill="40%")) +
  geom_ribbon(data=perc_unav_def_quants_by_state_wide, aes(x=iteration, ymin=`40%`, ymax=`60%`,fill="20%")) +
  #UPPERS
  geom_ribbon(data=perc_unav_def_quants_by_state_wide, aes(x=iteration, ymin=`90%`, ymax=`100%`,fill="100%")) +
  geom_ribbon(data=perc_unav_def_quants_by_state_wide, aes(x=iteration, ymin=`80%`, ymax=`90%`,fill="80%")) +
  geom_ribbon(data=perc_unav_def_quants_by_state_wide, aes(x=iteration, ymin=`70%`, ymax=`80%`,fill="60%")) +
  geom_ribbon(data=perc_unav_def_quants_by_state_wide, aes(x=iteration, ymin=`60%`, ymax=`70%`,fill="40%")) +
  geom_line(data=admits_all, aes(y = perc_disch, x = iteration, color="% peak discharges"))+
  geom_line(data=perc_unav_def_quants_by_state_wide, aes(x=iteration, y = `50%`, color="Median"))+
  xlab("days from first discharge")+
  ylab("Percent donors deferred or unavailable")+
  theme(legend.position="bottom")+
  scale_color_manual(name="", 
                     breaks=c("Median", "% peak discharges" ), 
                     values= c("black", "firebrick"))+
  scale_fill_manual(name="Percent simulations in range",
                    breaks=c("20%", "40%", "60%", "80%","100%"),
                    values = c('#0868ac', '#43a2ca', '#7bccc4', '#bae4bc', '#f0f9e8'))+
  facet_wrap(~state, nrow = 2)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  scale_x_continuous(breaks = seq(15, 200, 37), limits = c(15, 200))

ggsave("figs/donors_unavail_by_state_quantiles.png",
       width = 9, height = 4.2, units = "in")


## HEAT MAP STYLE CAPACITY

heat_dat_capac <- report_emp[ , 
                              list(
                                capac_level = seq(0.025, 0.975, 0.05),
                                perc_sims_at_level = 1-ecdf(perc_capacity_used)(seq(0.025, 0.975, 0.05))
                              ), by = c("state", "iteration")]


ggplot()+
  geom_tile(data = heat_dat_capac, aes(x = iteration, y = capac_level, fill = perc_sims_at_level))+
  geom_line(data=capac_quants_by_state_wide, aes(x=iteration, y = `50%`, color="Median"), linetype="dashed")+
  geom_line(data=admits_all, aes(y = perc_disch, x = iteration, color="% peak discharges"))+
  scale_fill_stepsn(colors = c('#ffffd9', '#edf8b1','#c7e9b4', '#7fcdbb','#41b6c4', '#1d91c0','#225ea8', '#0c2c84'),
                    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),#seq(0, 1, 1/8),
                    name="% Simulations",
                    labels = function(x) paste0(round(x*100),"%"),
                    guide = guide_colorsteps(barwidth = 16, show.limits = FALSE))+
  #scale_fill_binned(name="Percentile of capacity used",
  #                  breaks = seq(1, 0, -1/8),
  #                  values = c('#0c2c84', '#225ea8', '#1d91c0', '#41b6c4', '#7fcdbb', '#c7e9b4', '#edf8b1', '#ffffd9'))+
  facet_wrap(~state, nrow = 2)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"),
                     expand = c(0, 0),
                     minor_breaks = NULL)+
  scale_color_manual(name="", 
                     breaks=c("Median", "% peak discharges" ), 
                     values= c("black", "firebrick"))+
  scale_x_continuous(breaks = seq(15, 200, 37), 
                     limits = c(15, 200),
                     minor_breaks = NULL,
                     expand = c(0,0))+
  theme(legend.position="bottom", panel.spacing = unit(1, "lines"))+
  ylab("% capacity utilized")+
  xlab("Days from first discharge")

ggsave("figs/capac_used_by_state_heatmap.png",
       width = 9, height = 4.2, units = "in")

## HEAT MAP STYLE DEMAND MET

heat_dat_demand_met <- report_emp[ , 
                                   list(
                                     demand_level = seq(0.025, 0.975, 0.05),
                                     perc_sims_at_level = 1-ecdf(perc_demand_met)(seq(0.025, 0.975, 0.05))
                                   ), by = c("state", "iteration")]


ggplot()+
  geom_tile(data = heat_dat_demand_met, aes(x = iteration, y = demand_level, fill = perc_sims_at_level))+
  geom_line(data=demand_met_quants_by_state_wide, aes(x=iteration, y = `50%`, color="Median"), linetype="dashed")+
  geom_line(data=admits_all, aes(y = perc_disch, x = iteration, color="% peak discharges"))+
  scale_fill_stepsn(colors = c('#ffffd9', '#edf8b1','#c7e9b4', '#7fcdbb','#41b6c4', '#1d91c0','#225ea8', '#0c2c84'),
                    breaks = c(0, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),#seq(0, 1, 1/8),
                    name="% Simulations",
                    labels = function(x) paste0(round(x*100),"%"),
                    guide = guide_colorsteps(barwidth = 16, show.limits = FALSE))+
  #scale_fill_binned(name="Percentile of capacity used",
  #                  breaks = seq(1, 0, -1/8),
  #                  values = c('#0c2c84', '#225ea8', '#1d91c0', '#41b6c4', '#7fcdbb', '#c7e9b4', '#edf8b1', '#ffffd9'))+
  facet_wrap(~state, nrow = 2)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"),
                     expand = c(0, 0),
                     minor_breaks = NULL)+
  scale_color_manual(name="", 
                     breaks=c("Median", "% peak discharges" ), 
                     values= c("black", "firebrick"))+
  scale_x_continuous(breaks = seq(15, 200, 37), 
                     limits = c(15, 200),
                     minor_breaks = NULL,
                     expand = c(0,0))+
  theme(legend.position="bottom", panel.spacing = unit(1, "lines"))+
  ylab("% demand met")+
  xlab("Days from first discharge")

ggsave("figs/demand_met_by_state_heatmap.png",
       width = 9, height = 4.2, units = "in")

## HEAT MAP STYLE DEMAND UNMET

heat_dat_demand_unmet <- report_emp[ , 
                                     list(
                                       demand_level = seq(0.025, 0.975, 0.05),
                                       perc_sims_at_level = 1-ecdf(perc_demand_unmet)(seq(0.025, 0.975, 0.05))
                                     ), by = c("state", "iteration")]


ggplot()+
  geom_tile(data = heat_dat_demand_unmet, aes(x = iteration, y = demand_level, fill = perc_sims_at_level))+
  geom_line(data=demand_unmet_quants_by_state_wide, aes(x=iteration, y = `50%`, color="Median"), linetype="dashed")+
  geom_line(data=admits_all, aes(y = perc_disch, x = iteration, color="% peak discharges"))+
  scale_fill_stepsn(colors = c('#ffffd9', '#edf8b1','#c7e9b4', '#7fcdbb','#41b6c4', '#1d91c0','#225ea8', '#0c2c84'),
                    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1),#seq(0, 1, 1/8),
                    name="% Simulations",
                    labels = function(x) paste0(round(x*100),"%"),
                    guide = guide_colorsteps(barwidth = 16, show.limits = FALSE))+
  #scale_fill_binned(name="Percentile of capacity used",
  #                  breaks = seq(1, 0, -1/8),
  #                  values = c('#0c2c84', '#225ea8', '#1d91c0', '#41b6c4', '#7fcdbb', '#c7e9b4', '#edf8b1', '#ffffd9'))+
  facet_wrap(~state, nrow = 2)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"),
                     expand = c(0, 0),
                     minor_breaks = NULL)+
  scale_color_manual(name="", 
                     breaks=c("Median", "% peak discharges" ), 
                     values= c("black", "firebrick"))+
  scale_x_continuous(breaks = seq(15, 200, 37), 
                     limits = c(15, 200),
                     minor_breaks = NULL,
                     expand = c(0,0))+
  theme(legend.position="bottom", panel.spacing = unit(1, "lines"))+
  ylab("% demand unmet")+
  xlab("Days from first discharge")

ggsave("figs/demand_unmet_by_state_heatmap.png",
       width = 9, height = 4.2, units = "in")
##
#
# Analyzing results - Sensitivity analysis
#
## ---------------

### Sensitivity analysis
report_par <- data.table(read_feather("report_par_2020_09_23.feather"))
demand_par <- data.table(read_feather("demand_par_2020_09_23.feather"))
params_par <- data.table(read_feather("params_par_2020_09_23.feather"))


#Data table with parameters from sim
dt_sensitivity <- params_par[ , c("state", "run_batch_sim_state", "prob_willing", 
                                  "donor_return_second_scale", "donor_return_third_scale", "donor_return_later_scale",
                                  "recruitment_pm", "machines_pm")]

#Add demand parameters
dt_sensitivity <- dt_sensitivity[unique(demand_par[ , c("run_batch_sim_state", "CCP_prob_needed", 
                                                        "CCP_units_per_pt", "CCP_delay")]), on = "run_batch_sim_state"]

#Calculate metrics from simulation report
dt_outcomes <- report_par[iteration > 14, 
                   list(
                     "perc_demand_unmet" = sum(unmet_demand)/sum(demand),
                     "perc_capacity_used" = sum(collections)/sum(max_collections)
                     ), 
                   by = run_batch_sim_state]

dt_sensitivity <- dt_sensitivity[dt_outcomes, on = "run_batch_sim_state"]


#dt_sensitivity <- dt_sensitivity[dt_population, on="state"]

#dt_sensitivity[ , machines_pm := 1e6*max_collections/(3.4*0.5*pop)]
#dt_sensitivity[ , recruitment_pm := 1e6*max_recruitment/(machines_pm*pop)]
rm(report_par); rm(demand_par); rm(params_par)


# dt_models <- data.table(
#   state.i = states,
#   param = "prob_willing",
#   outcome = "perc_capacity_used"
# )
# 
# dt_models[ , model := loess(formula = paste0(outcome, "~", param), data = dt_sensitivity[state==state])]

# for(row in nrow(dt_models)){
#   dt_models[row, model := loess(formula = paste0(outcome, "~", param), data = dt_sensitivity[state==state])]
# }

capac.params <- c("prob_willing", "recruitment_pm", "machines_pm", 
                  "donor_return_second_scale")
demand.params <- c(capac.params, 
                   "CCP_prob_needed", "CCP_units_per_pt", "CCP_delay")

#Get dataframe for univariate analysis
#Quantiles
param_quants <- dt_sensitivity[ , lapply(.SD, function(x) round(quantile(x, prob = c(0.01, .25, .5, .75, 0.99)),4)), 
                                .SDcol = demand.params]

param_quants[ , quantile := c(0.01, .25, .5, .75, 0.99)]
param_quants <- melt(param_quants, id.vars = "quantile", variable.name = "param", value.name="param.val")

dt_univariate <- rbind(
  cbind(
    state = rep(states, each = nrow(param_quants[param %in% capac.params])),
    outcome = "perc_capacity_used",
    param_quants = param_quants[param %in% capac.params]
  ),
  cbind(
    state = rep(states, each = nrow(param_quants)),
    outcome = "perc_demand_unmet",
    param_quants = param_quants[param %in% demand.params]
  )
)
colnames(dt_univariate) <- c("state", "outcome", "quantile", "param", "param.val")


#Create regression models and store in nested lists
capac_loess <- list()
for(i.state in states){
  capac_loess[[i.state]]<- list()
  for(i.param in capac.params){
    f <- paste0("perc_capacity_used ~ ", i.param)
    capac_loess[[i.state]][[i.param]] <-  loess(f, data = dt_sensitivity[state == i.state])
  }
}
demand_loess <- list()
for(i.state in states){
  demand_loess[[i.state]]<- list()
  for(i.param in demand.params){
    f <- paste0("perc_demand_unmet ~ ", i.param)
    demand_loess[[i.state]][[i.param]] <-  loess(f, data = dt_sensitivity[state == i.state])
  }
}


#Plot LOESS Regression lines
dt_sensitivity_melt <- melt(dt_sensitivity, 
                            id.vars = c("state", "run_batch_sim_state", "perc_demand_unmet", "perc_capacity_used"), 
                            measure.vars = demand.params, variable.name = "param", value.name = "param.value")

# dt_sensitivity[ , don_recrut_split := ifelse(recruitment_pm < 1.25, "<1.25", ">1.25")]
# dt_sensitivity_splitmelt <- melt(dt_sensitivity, 
#                             id.vars = c("state", "run_batch_sim_state", "perc_demand_unmet", "perc_capacity_used", "don_recruit_split"), 
#                             measure.vars = "recruitment_pm", variable.name = "param", value.name = "param.value")

# lm_eqn <- function(x, y){
#   m <- lm(y ~ x);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(unname(coef(m)[1]), digits = 2),
#                         b = format(unname(coef(m)[2]), digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }



# ggplot(dt_sensitivity_splitmelt, aes(x = param.value, y = perc_capacity_used))+
#   geom_smooth(method = "lm", formula = y~x)+
#   facet_grid(rows = vars(state), cols = vars(don_recruit_split), scales = "free")+
#   xlab("Recruitment per machine")
#   
  #geom_text(label = lm_eqn(param.value,perc_capacity_used),x = Inf, y = Inf)


# for (i.state in states){
#   print(i.state)
#   print(summary(lm(perc_capacity_used~recruitment_pm, data=dt_sensitivity[recruitment_pm <= 1.25 & state == i.state])))
#   print(summary(lm(perc_capacity_used~recruitment_pm, data=dt_sensitivity[recruitment_pm > 1.25 & state == i.state])))
# }
# 
# for (i.state in states){
#   print(i.state)
#   print(summary(lm(perc_demand_unmet~recruitment_pm, data=dt_sensitivity[recruitment_pm <= 1.25 & state == i.state])))
#   print(summary(lm(perc_demand_unmet~recruitment_pm, data=dt_sensitivity[recruitment_pm > 1.25 & state == i.state])))
# }
# 


 
# ggsave(plot = ggplot(data=dt_sensitivity_melt[param %in% demand.params])+
#          geom_point(aes(x = param.value, y = perc_demand_unmet), alpha = 0.3)+
#          geom_smooth(method="loess", aes(x = param.value, y = perc_demand_unmet))+
#          facet_grid(rows =  vars(state), cols = vars(param), scales = "free_x")+
#          ylab("Total % demand unmet")+
#          scale_y_continuous(labels = function(x) paste0(x*100,"%"),
#                             expand = c(0, 0),
#                             #limits = c(0,.8),
#                             minor_breaks = NULL)+
#          theme(strip.text.x = element_text(size = 8)),
#        filename = "demand_loess_regression_all_param.png",
#        width = 9.7, height = 7.5, units = "in")

# ggsave(plot = ggplot(data=dt_sensitivity_melt[param %in% capac.params])+
#          geom_point(aes(x = param.value, y = perc_capacity_used), alpha = 0.3)+
#          geom_smooth(method="loess", aes(x = param.value, y = perc_capacity_used))+
#          facet_grid(rows =  vars(state), cols = vars(param), scales = "free_x")+
#          ylab("Total % capacity used")+
#          scale_y_continuous(labels = function(x) paste0(x*100,"%"),
#                             expand = c(0, 0),
#                             #limits = c(0,.8),
#                             minor_breaks = NULL),
#        filename = "capac_loess_regression_all_param.png",
#        width = 9.5, height = 7.5, units = "in")

# ggplot(data=dt_sensitivity_melt[state=="NY" & param %in% demand.params & param == "CCP_prob_needed"])+
#   geom_point(aes(x = param.value, y = perc_demand_unmet), size=0.5, alpha = .3, color = "#a6611a" )+
#   geom_smooth(method="loess", aes(x = param.value, y = perc_demand_unmet), color = "#018571")+
#   #facet_wrap(vars(param), scales = "free_x")+
#   ylab("Total % demand unmet")+
#   xlab("Prob. patient requires CCP")+
#   scale_y_continuous(labels = function(x) paste0(x*100,"%"),
#                      expand = c(0, 0),
#                      #limits = c(0,.8),
#                      minor_breaks = NULL)

#Predict using LOESS models and save in data.table
dt_univariate[ , quantile.val := 0]
for (i in 1:nrow(dt_univariate)){
  if (dt_univariate[i, outcome == "perc_capacity_used"]){
    dt_univariate[i, "quantile.val"] <- predict(capac_loess[[dt_univariate[i, state]]][[dt_univariate[i, param]]],
                                                dt_univariate[i, param.val])
  } else{
    dt_univariate[i, "quantile.val"] <- predict(demand_loess[[dt_univariate[i, state]]][[dt_univariate[i, param]]],
                                                dt_univariate[i, param.val])
  }

}





param_lookup <- data.table(
  param = c(
    "prob_willing",
    "recruitment_pm",
    "machines_pm",
    "donor_return_second_scale",
    "CCP_prob_needed",
    "CCP_delay",
    "CCP_units_per_pt"
    ),
  parameter_full = factor(c("Prob. willing to donate (10% - 90%)",
                           "Max. donors recruited per machine per day (0.2 - 2.0)",
                           "Machines per million population (4 - 55)",
                           "Donor return multiplier (0.5 - 2.25)",
                           "Prob. patient requires CCP (10% - 45%)",
                           "Delay from admit to CCP (1 to 5)",
                           "CCP units per patient (1.5 to 4)"
                           )),
  category = c("Donors",
               "Donors",
               "Capacity",
               "Donors",
               "Demand",
               "Demand",
               "Demand")
)

dt_univariate[ , Quantile := factor(paste0(quantile*100,"%"))]
dt_univariate<-dt_univariate[param_lookup, on="param"]

dt_univariate_wide <- dcast(dt_univariate, state + outcome + param + parameter_full + category ~ paste0("q",quantile), value.var="quantile.val")

psa_states <- c("NY", "CA", "MD", "MS")

#DEMAND
ggplot()+
  geom_segment(data=dt_univariate_wide[outcome=="perc_demand_unmet" & state %in% psa_states],
               aes(x=parameter_full, xend=parameter_full, y=q0.01, yend=q0.99), 
               size=2, color = "grey")+
  geom_point(data=dt_univariate[outcome=="perc_demand_unmet" & state %in% psa_states],
             aes(x=parameter_full, y=quantile.val, color=Quantile),
             size = 4)+
  facet_grid(cols = vars(state), rows = vars(category),
             scales = "free_y", space = "free_y")+
  coord_flip()+
  scale_color_manual(values = c('#a6611a', '#dfc27d', '#f5f5f5', '#80cdc1', '#018571'))+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"),
                     expand = c(0, 0),
                     #limits = c(0,.7),
                     minor_breaks = NULL)+
  ylab("Total % demand unmet")+
  xlab("")


ggsave("figs/univariate_sensitivity_demand.png",
       width = 9, height = 4, units = "in")


plt_demand_supp1 <- ggplot()+
  geom_segment(data=dt_univariate_wide[outcome=="perc_demand_unmet" & state %in% c("CT", "IL", "IN", "LA")],
               aes(x=parameter_full, xend=parameter_full, y=q0.01, yend=q0.99), 
               size=2, color = "grey")+
  geom_point(data=dt_univariate[outcome=="perc_demand_unmet" & state %in% c("CT", "IL", "IN", "LA")],
             aes(x=parameter_full, y=quantile.val, color=Quantile),
             size = 4)+
  facet_grid(cols = vars(state), rows = vars(category),
             scales = "free_y", space = "free_y")+
  coord_flip()+
  scale_color_manual(values = c('#a6611a', '#dfc27d', '#f5f5f5', '#80cdc1', '#018571'))+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"),
                     expand = c(0, 0),
                     #limits = c(0,.7),
                     breaks = c(.1, .3, .5),
                     minor_breaks = NULL)+
  ylab("Total % demand unmet")+
  xlab("")

plt_demand_supp2 <- ggplot()+
  geom_segment(data=dt_univariate_wide[outcome=="perc_demand_unmet" & state %in% c("MA", "NJ", "VA")],
               aes(x=parameter_full, xend=parameter_full, y=q0.01, yend=q0.99), 
               size=2, color = "grey")+
  geom_point(data=dt_univariate[outcome=="perc_demand_unmet" & state %in% c("MA", "NJ", "VA")],
             aes(x=parameter_full, y=quantile.val, color=Quantile),
             size = 4)+
  facet_grid(cols = vars(state), rows = vars(category),
             scales = "free_y", space = "free_y")+
  coord_flip()+
  scale_color_manual(values = c('#a6611a', '#dfc27d', '#f5f5f5', '#80cdc1', '#018571'))+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"),
                     expand = c(0, 0),
                     breaks = c(.2, .4, .6),
                     #limits = c(0,.7),
                     minor_breaks = NULL)+
  ylab("Total % demand unmet")+
  xlab("")



ggsave("figs/univariate_sensitivity_demand_supp.png",
       grid.arrange(plt_demand_supp1, plt_demand_supp2, ncol = 1),
       width = 9, height = 8, units = "in")


#CAPACITY
ggplot()+
  geom_segment(data=dt_univariate_wide[outcome=="perc_capacity_used" & state %in% psa_states],
               aes(x=parameter_full, xend=parameter_full, y=q0.01, yend=q0.99), 
               size=2, color = "grey")+
  geom_point(data=dt_univariate[outcome=="perc_capacity_used" & state %in% psa_states],
             aes(x=parameter_full, y=quantile.val, color=Quantile),
             size = 4)+
  facet_grid(cols = vars(state), rows = vars(category),
             scales = "free_y", space = "free_y")+
  coord_flip()+
  scale_color_manual(values = c('#a6611a', '#dfc27d', '#f5f5f5', '#80cdc1', '#018571'))+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"),
                     expand = c(0, 0),
                     limits = c(0,1),
                     breaks = c(.2, .4, .6),
                     minor_breaks = NULL)+
  ylab("Total % capacity used")+
  xlab("")


ggsave("figs/univariate_sensitivity_capacity.png",
       width = 9, height = 4, units = "in")

ggplot()+
  geom_segment(data=dt_univariate_wide[outcome=="perc_capacity_used" & !(state %in% psa_states)],
               aes(x=parameter_full, xend=parameter_full, y=q0.01, yend=q0.99), 
               size=2, color = "grey")+
  geom_point(data=dt_univariate[outcome=="perc_capacity_used" & !(state %in% psa_states)],
             aes(x=parameter_full, y=quantile.val, color=Quantile),
             size = 4)+
  facet_grid(cols = vars(state), rows = vars(category),
             scales = "free_y", space = "free_y")+
  coord_flip()+
  scale_color_manual(values = c('#a6611a', '#dfc27d', '#f5f5f5', '#80cdc1', '#018571'))+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"),
                     expand = c(0, 0),
                     limits = c(0,1),
                     breaks = c(.2, .4, .6),
                     minor_breaks = NULL)+
  ylab("Total % capacity used")+
  xlab("")


ggsave("figs/univariate_sensitivity_capacity_supp.png",
       width = 9, height = 4, units = "in")


# 
# ggplot()+
#   geom_segment(data=dt_univariate_wide[outcome=="perc_capac_used"],
#                aes(x=param, xend=param, y=q0.01, yend=q0.99))+
#   geom_point(data=dt_univariate[outcome=="perc_demand_unmet"],
#              aes(x=param, y=quantile.val, color=Quantile),
#              size = 3)+
#   facet_wrap(vars(state), nrow = 2)+
#   coord_flip()+
#   scale_color_manual(values = c('#a6611a', '#dfc27d', '#f5f5f5', '#80cdc1', '#018571'))+
#   scale_y_continuous(labels = function(x) paste0(x*100,"%"),
#                      expand = c(0, 0),
#                      minor_breaks = NULL)+
#   ylab("Percent capacity unused")+
#   xlab("Parameter")


two_way_loess <- list()
for(i.state in states){
  two_way_loess[[i.state]] <- loess(perc_demand_unmet~donor_return_second_scale+recruitment_pm, data = dt_sensitivity[state == i.state])
  print(i.state)
}



dt_twoway <- expand.grid(
  state = states,
  donor_return_second_scale = quantile(dt_sensitivity$donor_return_second_scale, seq(0.01, 0.99, .98/28)),
  recruitment_pm = quantile(dt_sensitivity$recruitment_pm, seq(0.01, 0.99, .98/20))
)


ggplot()+
  facet_wrap(vars(state))+
  geom_point(data = dt_twoway, aes(x = recruitment_pm, y = perc_demand_unmet, color = donor_return_second_scale), alpha = 0.3)

ggplot(data = dt_sensitivity_melt[param == "recruitment_pm"],
       aes(x = param.value, y = perc_demand_unmet))+
  geom_point(size =0.5, alpha = 0.5)+
  geom_smooth(method = "loess")+
  facet_wrap(vars(state))

for (i in 1:nrow(dt_twoway)){
  dt_twoway[i, "perc_demand_unmet"] <- predict(two_way_loess[[dt_twoway[i,]$state ]],
                                               dt_twoway[i, c("donor_return_second_scale", "recruitment_pm")])
  
}




ggplot()+
  geom_point(data = dt_twoway, aes(x = recruitment_pm, y = donor_return_second_scale, color = perc_demand_unmet),
             shape = 15, size = 2)+
  scale_color_stepsn(colors = c('#ffffd9', '#edf8b1','#c7e9b4', '#7fcdbb','#41b6c4', '#1d91c0','#225ea8', '#0c2c84'),
                    breaks = seq(0, .45, .05),
                    name="% demand unmet",
                    labels = function(x) paste0(round(x*100),"%"),
                    guide = guide_colorsteps(barwidth = 16, show.limits = FALSE))+
  facet_wrap(~state, nrow = 2)+
  scale_y_continuous(expand = c(0, 0),
                     minor_breaks = NULL)+
  scale_x_continuous(
                     minor_breaks = NULL,
                     expand = c(0,0))+
  theme(legend.position="bottom", panel.spacing = unit(1, "lines"))+
  ylab("Donor return (scale factor)")+
  xlab("Donor recruitment (max donors per day per machine)")

ggsave("figs/two_way_donor_sensitivity_demand.png",
       width = 9, height = 4, units = "in")

ggplot()+
  geom_point(data = dt_twoway, aes(x = recruitment_pm, y = donor_return_second_scale, color = perc_demand_unmet),
            shape = 15, size = 2)+
  scale_color_viridis_c(
    name="% demand unmet",
    labels = function(x) paste0(round(x*100),"%"))+
  facet_wrap(~state, nrow = 2)+
  scale_y_continuous(expand = c(0, 0),
                    minor_breaks = NULL)+
  scale_x_continuous(
   minor_breaks = NULL,
   expand = c(0,0))+
  theme(legend.position="bottom")+
  ylab("Donor return (scale factor)")+
  xlab("Donor recruitment (max donors per day per machine)")

ggsave("figs/two_way_donor_sensitivity_demand_gradient.png",
       width = 9, height = 4, units = "in")






two_way_loess_capac <- list()
for(i.state in states){
  two_way_loess_capac[[i.state]] <- loess(perc_capacity_used~donor_return_second_scale+recruitment_pm, data = dt_sensitivity[state == i.state])
  print(i.state)
}



dt_twoway_capac <- expand.grid(
  state = states,
  donor_return_second_scale = quantile(dt_sensitivity$donor_return_second_scale, seq(0.01, 0.99, .98/28)),
  recruitment_pm = quantile(dt_sensitivity$recruitment_pm, seq(0.01, 0.99, .98/20))
)



for (i in 1:nrow(dt_twoway_capac)){
  dt_twoway_capac[i, "perc_capacity_used"] <- predict(two_way_loess_capac[[dt_twoway_capac[i,]$state ]],
                                               dt_twoway_capac[i, c("donor_return_second_scale", "recruitment_pm")])
  
}


ggplot()+
  geom_tile(data = dt_twoway_capac, aes(x = recruitment_pm, y = donor_return_second_scale, fill = perc_capacity_used),
            width = 1, height = 1)+
  scale_fill_stepsn(colors = c('#ffffd9', '#edf8b1','#c7e9b4', '#7fcdbb','#41b6c4', '#1d91c0','#225ea8', '#0c2c84'),
                    breaks = seq(0, .45, .05),
                    name="% capacity used",
                    labels = function(x) paste0(round(x*100),"%"),
                    guide = guide_colorsteps(barwidth = 16, show.limits = FALSE))+
  facet_wrap(~state, nrow = 2)+
  scale_y_continuous(expand = c(0, 0),
                     minor_breaks = NULL)+
  scale_x_continuous(
    minor_breaks = NULL,
    expand = c(0,0))+
  theme(legend.position="bottom", panel.spacing = unit(1, "lines"))+
  ylab("Donor return (scale factor)")+
  xlab("Donor recruitment (max donors per day per machine)")

ggsave("figs/two_way_donor_sensitivity_capacity.png",
       width = 9, height = 4, units = "in")



ggplot()+
  geom_point(data = dt_twoway, aes(x = recruitment_pm, y = donor_return_second_scale, color = perc_demand_unmet),
             shape = 15, size = 2)+
  scale_color_viridis_c(
    # low = '#ffffd9',
    # mid = '#c7e9b4',
    # high = '#0c2c84',
    name="% capacity used",
    labels = function(x) paste0(round(x*100),"%"))+
  facet_wrap(~state, nrow = 2)+
  scale_y_continuous(expand = c(0, 0),
                     minor_breaks = NULL)+
  scale_x_continuous(
    minor_breaks = NULL,
    expand = c(0,0))+
  theme(legend.position="bottom", panel.spacing = unit(1, "lines"))+
  ylab("Donor return (scale factor)")+
  xlab("Donor recruitment (max donors per day per machine)")

ggsave("figs/two_way_donor_sensitivity_capacity_gradient.png",
       width = 9, height = 4, units = "in")
