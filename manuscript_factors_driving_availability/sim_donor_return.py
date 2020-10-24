import pandas as pd
import numpy as np
import scipy.stats as stats
import pyarrow.feather as feather
import requests
import json
import time
from datetime import timedelta
import sys
import getopt
import gc

def truncnorm(mu=0, sigma=1, lower=0, upper=1):
    X = stats.truncnorm((lower - mu) / sigma, (upper - mu) / sigma, loc=mu, scale=sigma)
    return X

def exp_model_func(t, K, lambd, t0 = 7, scale=1.0):
    return np.where(t < t0, 0.0, scale*K * (1.0 - np.exp(-lambd*(t-t0)*scale)))

def parse_args(args):
    state = ''
    dist = ''
    n_sims = 10000
    chunk_size = 100
    try:
        opts, args = getopt.getopt(args, "hs:r:n:c:",["state=","returndist=","n_sims=","chunk_size="])
    except getopt.GetoptError:
        print ('sim_donor_return.py -s <state> -r <return_time_distributions>')
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            print ('sim_donor_return.py -s <state> -r <return_time_distribution> -n <number_of_simulations> -c <chunk_size>')
            sys.exit()
        elif opt in ("-s", "--state"):
            state = arg
        elif opt in ("-r", "--returndist"):
            dist = arg
        elif opt in ("-n", "--n_sims"):
            n_sims = int(arg)
        elif opt in ("-c", "--chunk_size"):
            chunk_size = int(arg)
        
    if state not in ["NJ", "MA", "IN", "LA", "CT", "MS", "VA", "MD", "IL", "NY", "CA"]:
        raise Exception("Error: unexpected state code.")
    if dist not in ["empiric", "parametric", "parametric_all"]:
        raise Exception("Error: return time distribution must be one of ['empiric', 'parametric', 'parametric_all']")
    if not (isinstance(n_sims, int) and  n_sims > 0):
        raise Exception("Error: n_sims must be a positive integer")
    if not (isinstance(chunk_size, int) and chunk_size > 0 and chunk_size <= n_sims):
        raise Exception("Error: chunk_size must be a positive integer leq n_sims")
    return state, dist, n_sims, chunk_size

def main(args, 
          n_timesteps = 200,
          prob_willing_min = 0.1,
          prob_willing_max = 0.9,
          turns_per_day_min = 3.5,
          turns_per_day_max = 3.5,
          available_machine_time_min = 0.5,
          available_machine_time_max = 0.5,
          machines_pm_min = 4,
          machines_pm_max = 55,
          recruitment_per_machine_min = 0.2,
          recruitment_per_machine_max = 2,
          return_scaling_factor_min = 0.5,
          return_scaling_factor_max = 2.25,
          return_3rd_scaling_factor_max = 1.42,
          return_4th_scaling_factor_max = 1.23,
          K = 0.44068065575293935,
          lambd = 0.02564293675387818,
          K_3rd = 0.7004050311562576,
          lambd_3rd = 0.03913344618984413,
          K_4th = 0.8076846503792819,
          lambd_4th = 0.05034356712490996,
          population_sizes = {
              'NJ': 8882190, 
              'MA': 6892503, 
              'IN': 6732219, 
              'LA': 4648794, 
              'CT': 3565287, 
              'MS': 2976149, 
              'VA': 8535519, 
              'MD': 6045680, 
              'NY': 19453561, 
              'IL': 12671821, 
              'CA': 39512223}):

    state, dist, n_sims, chunk_size = parse_args(args)

    run_id = str(np.random.randint(100000, high = 999999, size = 1)[0])
    state_data = feather.read_feather("./covidactnow_" + state + "_2020-09-08.feather")
    start_date = min(state_data.date[state_data.discharges > 0])
    end_date = start_date + timedelta(days=n_timesteps-1)
    discharges = [int(i) for i in state_data[(state_data.date >= start_date) & (state_data.date <= end_date)].discharges.tolist()]
    n_iterations = len(discharges)
    #print(state_data.head())

    if dist == "empiric":
        donret_2_dist = feather.read_feather("./data/donor_return_2nd_cumprob.feather")
        donret_2_dist_list = [donret_2_dist.copy() for _ in range(n_sims)]
        donret_3_dist = feather.read_feather("./data/donor_return_3rd_cumprob.feather")
        donret_3_dist_list = [donret_3_dist.copy() for _ in range(n_sims)]
        donret_4_dist = feather.read_feather("./data/donor_return_4th_cumprob.feather")
        donret_4_dist_list = [donret_4_dist.copy() for _ in range(n_sims)]
        
    elif dist == "parametric":
        scaling_factors = np.random.uniform(return_scaling_factor_min, return_scaling_factor_max, size=n_sims)
        t = np.linspace(0, 126, 127)
        donret_2_dist_list = []
        for i in range(n_sims):
            p = exp_model_func(t, K, lambd, t0 = 7, scale=scaling_factors[i])
            donret_2_dist_list.append(pd.DataFrame({'time': np.append(t, 9999.0), 'prob': np.append(p, 1.0)}))
        donret_3_dist = feather.read_feather("./data/donor_return_3rd_cumprob.feather")
        donret_3_dist_list = [donret_3_dist.copy() for _ in range(n_sims)]
        donret_4_dist = feather.read_feather("./data/donor_return_4th_cumprob.feather")
        donret_4_dist_list = [donret_4_dist.copy() for _ in range(n_sims)]
    elif dist == "parametric_all":
        scaling_factors = np.random.uniform(return_scaling_factor_min, return_scaling_factor_max, size=n_sims)
        scaling_factos_propmax = (scaling_factors-return_scaling_factor_min)/(return_scaling_factor_max-return_scaling_factor_min)
        scaling_factors_3rd = scaling_factos_propmax*(return_3rd_scaling_factor_max-return_scaling_factor_min) + return_scaling_factor_min
        #print("Scaling 3rd:", min(scaling_factors_3rd), max(scaling_factors_3rd))
        scaling_factors_4th = scaling_factos_propmax*(return_4th_scaling_factor_max-return_scaling_factor_min) + return_scaling_factor_min
        #print("Scaling 4th:", min(scaling_factors_4th), max(scaling_factors_4th))
        t = np.linspace(0, 126, 127)
        donret_2_dist_list = []
        donret_3_dist_list = []
        donret_4_dist_list = []
        for i in range(n_sims):
            p = exp_model_func(t, K, lambd, t0 = 7, scale=scaling_factors[i])
            donret_2_dist_list.append(pd.DataFrame({'time': np.append(t, 9999.0), 'prob': np.append(p, 1.0)}))
            p = exp_model_func(t, K_3rd, lambd_3rd, t0 = 7, scale=scaling_factors_3rd[i])
            donret_3_dist_list.append(pd.DataFrame({'time': np.append(t, 9999.0), 'prob': np.append(p, 1.0)}))
            p = exp_model_func(t, K_4th, lambd_4th, t0 = 7, scale=scaling_factors_4th[i])
            donret_4_dist_list.append(pd.DataFrame({'time': np.append(t, 9999.0), 'prob': np.append(p, 1.0)}))

    population = population_sizes[state]
    prob_willing = np.random.uniform(prob_willing_min, prob_willing_max, size = n_sims)
    macines_pm = np.random.uniform(machines_pm_min, machines_pm_max, size = n_sims)
    machines = np.round(macines_pm * (population/1e6))
    turns_per_day = np.random.uniform(turns_per_day_min, turns_per_day_max, size = n_sims)
    available_machine_time = np.random.uniform(available_machine_time_min, available_machine_time_max, size = n_sims)
    max_collections = np.round(machines * turns_per_day * available_machine_time).astype(int)
    recruitment_per_machine = np.random.uniform(recruitment_per_machine_min, recruitment_per_machine_max, size = n_sims)
    max_recruitment = np.round(machines * recruitment_per_machine).astype(int)
    

    parameter_list = []
    for i in range(n_sims):
        parameter_list.append({
            "run_id": run_id,
            "iterations": n_iterations,
            "report_level": 3,
            "num_agents_init": 0,
            "recovered": discharges,
            "historical_collections": None,
            "prob_eligible": 1,
            "delay_eligibility": 14,
            "duration_eligibility": 180,
            "temp_ineligibility_period": 7,
            "donor_return_dist_type": dist,
            "donor_return_second": donret_2_dist_list[i],
            "donor_return_second_scale": scaling_factors[i] if dist == "parametric" or dist == "parametric_all" else None,
            "donor_return_third": donret_3_dist_list[i],
            "donor_return_third_scale": scaling_factors_3rd[i] if dist == "parametric_all" else None,
            "donor_return_later": donret_4_dist_list[i],
            "donor_return_later_scale": scaling_factors_4th[i] if dist == "parametric_all" else None,
            "donor_return_prob_col": "prob",
            "delay_recruitment": 0,
            "prob_willing": prob_willing[i],
            "prob_male": 0.5,
            "male_relative_propensity": 1,
            "max_recruitment": int(max_recruitment[i]),
            "recruitment_pm": recruitment_per_machine[i],
            "prob_other_deferral": 0.02,
            "prob_failed_donation": 0.01,
            "prob_hla_female": 0.09,
            "prob_tti": 0.002,
            "prob_Ab_pos": 0.93,
            "qualify_neut_titers": False,
            "prob_neut_above_360": None,
            "max_collections": int(max_collections[i]),
            "mean_units_per_collection": turns_per_day[i],
            "max_collection_growth": 9999, # essentially infinite
            "recruitment_start": 0,
            "collection_start": 0,
            "machines_pm": int(macines_pm[i]),
            "available_machine_time": available_machine_time[i]
        })
    
    n_chunks = n_sims // chunk_size
    iter_start = 0
    for j in range(n_chunks):
        simset = "statesim_" + state + "_" + dist + "_" + run_id + "_" + str(j).zfill(3)
        [p.update({"chunk_id":j}) for p in parameter_list[iter_start:(iter_start+chunk_size)]]
        csim.multi_threaded_run(
            parameter_list=parameter_list[iter_start:(iter_start+chunk_size)], 
            simulations=chunk_size,
            root_seed=None,
            processes= 20,
            simset_name=simset,
            output_report=True, 
            output_agents=True,
            output_parameters=True,
            report_dir="./outputs/",
            return_results=False)
        gc.collect()
        iter_start += chunk_size

if __name__ == '__main__':
    import collection as csim
    main(sys.argv[1:])
