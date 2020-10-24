import math
import random
import statistics
import sys
import time
import argparse
import json
import pickle
from enum import Enum, IntEnum
import multiprocessing as mp
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import pyarrow.feather as feather

class Sex(IntEnum):
    MALE = 0
    FEMALE = 1

class Eligible(IntEnum):
    INELIGIBLE = 0
    ELIGIBLE = 1
    TEMP_INELIGIBLE = 2
    UNAVAILABLE = 3
    DEFERRED = 4
    DISQUALIFIED = 5
    LEFT = 6

class Agent:

    def __init__(self, iden, iteration, parameters, rng):
        self.iden = iden
        self.sex = Sex.MALE if random.random() < parameters["prob_male"] else Sex.FEMALE
        self.hla_ab = True if self.sex == Sex.FEMALE and \
            random.random() < parameters["prob_hla_female"] else False
        self.tti = True if random.random() < parameters["prob_tti"] else False
        # Implement waning dynamic?
        self.sarscov2_ab = True if random.random() < parameters["prob_Ab_pos"] else False
        if parameters["qualify_neut_titers"]:
            self.neut_titer_360 = True if self.sarscov2_ab and random.random() < parameters["prob_neut_above_360"] else False
        else:
            self.neut_titer_360 = None

        self.discharged = iteration

        self.eligible = Eligible.INELIGIBLE
        self.eligible_ever = True if \
            random.random() < parameters["prob_eligible"] else False
        self.time_eligible = \
            math.ceil(self.discharged + parameters["delay_eligibility"]) if \
                self.eligible_ever else None
        self.time_ineligible = \
            math.ceil(self.time_eligible + parameters["duration_eligibility"]) if \
                self.eligible_ever else None
        self.deferred_until = None
        
        r = random.random()
        if self.sex == Sex.FEMALE:
            self.willing = True if r < parameters["prob_willing"] else False
        elif self.sex == Sex.MALE:
            self.willing = True if r < parameters["male_relative_propensity"]*parameters["prob_willing"] else False
        
        # self.provider_referred = True if \
        #     self.eligible_ever and self.willing and \
        #         random.random() < parameters["prob_provider_referred"] else False
        # self.self_referred = True if \
        #     self.eligible_ever and self.willing and \
        #         not self.provider_referred and \
        #             random.random() < parameters["prob_self_referred"] else False
        
        # self.available_to_recruit = True if \
        #     self.provider_referred or self.self_referred else False

        self.available_to_recruit = True if \
            self.eligible_ever and self.willing else False
        self.time_avail_recruitment = \
            math.ceil(self.time_eligible + parameters["delay_recruitment"]) if \
                self.available_to_recruit else None
        
        self.recruited = None
        self.donor = False
        self.donor_ever = False
        self.donations_given = 0
        self.previous_donation = None
        self.deferral_reason = None
        self.left_donor_pool = False
        self.reason_left_donor_pool = None

class Simulation:

    # Vectorize all parameters
    # perhaps function in R to create vectors
    def __init__(self, parameters, seed = None, progress_callback = None):
        self.parameters = parameters
        self.iden = self.parameters["id"] if "id" in self.parameters else 0 
        if seed != None:
            random.seed(seed)
            self.rng = np.random.default_rng(seed=seed)
        else:
            self.rng = np.random.default_rng()
        self.iteration = 0
        self.iterations = self.parameters["iterations"] if "iterations" in self.parameters else 365 
        self.progress_callback = progress_callback
        if self.parameters["report_level"] == 0:
            self.report_columns = [
                "sim_id",
                "iteration", 
                "collections", 
                "units"
            ]
        elif self.parameters["report_level"] == 1:
            self.report_columns = [
                "sim_id",
                "iteration", 
                "num_agents",
                "donors", 
                "active_donors",
                "collections", 
                "units"
            ]
        elif self.parameters["report_level"] == 2:
            self.report_columns = [
                "sim_id",
                "iteration", 
                "num_agents",
                "donors", 
                "eligible_donors",
                "active_donors",
                "deferred",
                "collections", 
                "units"
            ]
        elif self.parameters["report_level"] == 3:
            self.report_columns = [
                "sim_id",
                "iteration", 
                "num_agents",
                "ineligible", 
                "eligible", 
                "donors", 
                "eligible_donors",
                "active_donors",
                "temp_ineligible", 
                "deferred",
                "disqualified", 
                "unavailable",
                "collections", 
                "units"
            ]
        self.report_table = np.empty((self.iterations,len(self.report_columns)), int)
        self.num_agents = self.parameters["num_agents_init"] if "num_agents_init" in self.parameters else 1000 
        self.agents = [Agent(i, 0, self.parameters, self.rng) for i in range(0, self.num_agents)] if self.num_agents > 0 else []
        self.collections = 0
        self.units = 0 # Usable units! All released.
        if isinstance(self.parameters["max_recruitment"], int):
            self.max_recruitment = np.repeat(self.parameters["max_recruitment"], self.iterations)
        elif isinstance(self.parameters["max_recruitment"], list) and \
            len(self.parameters["max_recruitment"]) == self.iterations:
            self.max_recruitment = self.parameters["max_recruitment"]
        else:
            raise Exception("'max_recruitment' must must be an integer or list of length 'iterations'")
        if isinstance(self.parameters["max_collections"], int):
            self.max_collections = np.repeat(self.parameters["max_collections"], self.iterations)
        elif isinstance(self.parameters["max_collections"], list) and \
            len(self.parameters["max_collections"]) == self.iterations:
            self.max_collections = self.parameters["max_collections"]
        else:
            raise Exception("'max_collections' must be an integer or list of length 'iterations'")
        self.recovered = self.parameters["recovered"]
        self.donor_return_second = self.parameters["donor_return_second"][["time", self.parameters["donor_return_prob_col"]]]
        self.donor_return_third = self.parameters["donor_return_third"][["time", self.parameters["donor_return_prob_col"]]]
        self.donor_return_later = self.parameters["donor_return_later"][["time", self.parameters["donor_return_prob_col"]]]

    def spawn_agents(self):
        if self.iteration <= len(self.recovered)-1:
            n_new = int(round(self.recovered[self.iteration]))
            if n_new > 0:
                for a in [Agent(i, self.iteration, self.parameters, self.rng) for i in range(self.num_agents, self.num_agents+n_new)]:
                    self.agents.append(a)
            self.num_agents = len(self.agents)
        else:
            pass

    def advance(self):
        for a in self.agents:
            if a.eligible == Eligible.INELIGIBLE:
                if self.iteration == a.time_eligible:
                    if random.random() < self.parameters["prob_eligible"]:
                        a.eligible = Eligible.ELIGIBLE 
            if a.eligible == Eligible.ELIGIBLE and a.donor == False:
                if self.iteration == a.time_ineligible:
                    a.eligible = Eligible.INELIGIBLE
                    a.available_to_recruit = False
            if a.eligible == Eligible.ELIGIBLE and a.donor == True:
                if self.iteration == a.time_ineligible:
                    a.eligible = Eligible.DISQUALIFIED
                    a.donor = False
                    a.available_to_recruit = False
                    a.left_donor_pool = True
                    a.reason_left_donor_pool = "disqualified_ccp_window"
            if a.eligible == Eligible.UNAVAILABLE and a.donor == True:
                if self.iteration == a.time_ineligible:
                    a.eligible = Eligible.DISQUALIFIED
                    a.donor = False
                    a.available_to_recruit = False
                    a.left_donor_pool = True
                    a.reason_left_donor_pool = "disqualified_ccp_window"
            if a.donor == True and a.eligible == Eligible.TEMP_INELIGIBLE:
                if self.iteration >= a.previous_donation + \
                    self.parameters["temp_ineligibility_period"]:
                    if self.iteration < a.previous_donation + a.time_until_next_availability:
                        a.eligible = Eligible.UNAVAILABLE
                    elif self.iteration >= a.previous_donation + a.time_until_next_availability:
                        a.eligible = Eligible.ELIGIBLE
            if a.donor == True and a.eligible == Eligible.UNAVAILABLE:
                if self.iteration >= a.previous_donation + a.time_until_next_availability:
                    a.eligible = Eligible.ELIGIBLE
            # This does nothing at present
            if a.donor == True and a.eligible == Eligible.DEFERRED:
                if a.deferred_until != None:
                    if self.iteration >= a.deferred_until:
                        a.eligible = Eligible.ELIGIBLE

   
    def recruit(self):
        pool = [a for a in self.agents 
                    if a.eligible == Eligible.ELIGIBLE and \
                        a.donor == False and \
                            a.available_to_recruit == True and \
                                self.iteration >= a.time_avail_recruitment]
        random.shuffle(pool)
        n_recruited = 0
        if len(pool) > 0:
            for a in pool:
                a.donor = True
                a.donor_ever = True
                a.recruited = self.iteration
                n_recruited += 1
                if n_recruited == self.max_recruitment[self.iteration]:
                    break
    
    def collect(self):
        # Implement two machine types?
        pool = [a for a in self.agents
                    if a.donor == True and \
                        a.eligible == Eligible.ELIGIBLE]
        random.shuffle(pool)
        n_collections = 0
        n_usable_units_collected = 0
        if len(pool) > 0:
            for a in pool:
                if self.parameters["historical_collections"] != None and self.iteration <= self.parameters["collection_start"] + len(self.parameters["historical_collections"])-1 \
                    and self.parameters["historical_collections"][self.iteration - int(self.parameters["collection_start"])] == 0:
                    break
                a.donations_given += 1 # check in advance() whether should be removed from donor pool
                a.previous_donation = self.iteration
                n_collections += 1
                if random.random() < self.parameters["prob_other_deferral"]:
                    n_usable_units_collected += 0
                    a.eligible = Eligible.DEFERRED
                    a.deferred_until = None # For now indefinitely deferred in all cases
                    a.deferral_reason = "Other deferral"
                    a.available_to_recruit = False
                    a.left_donor_pool = True
                    a.reason_left_donor_pool = "indefinitely_deferred_other"
                elif random.random() < self.parameters["prob_failed_donation"]:
                    n_usable_units_collected += 0
                # Neutralization titer is only checked on first donation – we may need waning dynamic
                # Note: the elifs here means if you get deferred for HLA you never get checked for other things
                elif a.donations_given == 1 and a.hla_ab:
                    a.eligible = Eligible.DEFERRED
                    a.deferred_until = None # Indefinitely deferred
                    a.deferral_reason = "HLA Ab"
                    a.available_to_recruit = False
                    a.left_donor_pool = True
                    a.reason_left_donor_pool = "indefinitely_deferred_hla"
                    n_usable_units_collected += 0
                elif a.donations_given == 1 and a.tti:
                    a.eligible = Eligible.DEFERRED
                    a.deferred_until = None # Indefinitely deferred
                    a.deferral_reason = "TTI"
                    a.available_to_recruit = False
                    a.left_donor_pool = True
                    a.reason_left_donor_pool = "indefinitely_deferred_tti"
                    n_usable_units_collected += 0
                elif a.donations_given == 1 and not a.sarscov2_ab:
                    a.eligible = Eligible.DEFERRED
                    a.deferred_until = None # Indefinitely deferred
                    a.deferral_reason = "SARS-CoV-2 bAb negative"
                    a.available_to_recruit = False
                    a.left_donor_pool = True
                    a.reason_left_donor_pool = "indefinitely_deferred_abneg"
                    n_usable_units_collected += 0
                elif a.donations_given == 1 and self.parameters["qualify_neut_titers"] and not a.neut_titer_360:
                    a.eligible = Eligible.DEFERRED
                    a.deferred_until = None # Indefinitely deferred
                    a.deferral_reason = "Insufficient SARS-CoV-2 nAb titer"
                    a.available_to_recruit = False
                    a.left_donor_pool = True
                    a.reason_left_donor_pool = "indefinitely_deferred_neut"
                    n_usable_units_collected += 0
                else:
                    a.eligible = Eligible.TEMP_INELIGIBLE
                    r = random.random()
                    if a.donations_given == 1:
                        for time, prob in self.donor_return_second.itertuples(index=False):
                            if r < prob:
                                a.time_until_next_availability = time
                                break
                    elif a.donations_given == 2:
                        for time, prob in self.donor_return_third.itertuples(index=False):
                            if r < prob:
                                a.time_until_next_availability = time
                                break
                    elif a.donations_given > 2:
                        for time, prob in self.donor_return_later.itertuples(index=False):
                            if r < prob:
                                a.time_until_next_availability = time
                                break
                    # discrete probability distribution with support [1,4]; mean = 3.4?
                    # mean = self.parameters["mean_units_per_collection"]
                    units = self.rng.poisson(lam=self.parameters["mean_units_per_collection"])
                    units = 4 if units >= 4 else units # clip distribution at 4
                    n_usable_units_collected += units
                if self.parameters["historical_collections"] != None and self.iteration <= self.parameters["collection_start"] + len(self.parameters["historical_collections"])-1 \
                    and n_collections == self.parameters["historical_collections"][self.iteration - int(self.parameters["collection_start"])]:
                    break
                elif n_collections == self.max_collections[self.iteration]:
                    break
                # We only constrain collection growth after the historical collections data has ended
                if self.iteration > 0 and (self.parameters["historical_collections"] == None or \
                    (self.parameters["historical_collections"] != None and self.iteration > self.parameters["collection_start"] + len(self.parameters["historical_collections"])-1)):
                    # Note that self.collections is still previous iterations number of collections
                    # Use moving average rather than previous iteration's value?
                    if n_collections >= math.ceil(self.collections * self.parameters["max_collection_growth"]):
                        break
        self.collections = n_collections
        self.units = n_usable_units_collected
        # if self.iteration > 200 and self.collections < self.report_table[self.iteration-1,9]:
        #     import pdb; pdb.set_trace()

    def report(self):
        if self.parameters["report_level"] == 0:
            self.report_table[self.iteration] = [
                self.iden,
                self.iteration,
                self.collections,
                self.units
            ]
        elif self.parameters["report_level"] == 1:
            self.report_table[self.iteration] = [
                self.iden,
                self.iteration,
                self.num_agents,
                len([a for a in self.agents if a.donor == True]),
                len([a for a in self.agents if (a.eligible == Eligible.ELIGIBLE or a.eligible == Eligible.TEMP_INELIGIBLE) and a.donor == True]),
                self.collections,
                self.units
            ]
        elif self.parameters["report_level"] == 2:
            self.report_table[self.iteration] = [
                self.iden,
                self.iteration,
                self.num_agents,
                len([a for a in self.agents if a.donor == True]),
                len([a for a in self.agents if a.eligible == Eligible.ELIGIBLE and a.donor == True]),
                len([a for a in self.agents if (a.eligible == Eligible.ELIGIBLE or a.eligible == Eligible.TEMP_INELIGIBLE) and a.donor == True]),
                len([a for a in self.agents if a.eligible == Eligible.DEFERRED]),
                self.collections,
                self.units
            ]
        elif self.parameters["report_level"] == 3:
            self.report_table[self.iteration] = [
                self.iden,
                self.iteration,
                self.num_agents,
                # The filter() method is slower than list comprehension
                #len(list(filter(lambda a: a.eligible == Eligible.INELIGIBLE, self.agents))),
                len([a for a in self.agents if a.eligible == Eligible.INELIGIBLE]),
                len([a for a in self.agents if a.eligible == Eligible.ELIGIBLE]),
                len([a for a in self.agents if a.donor == True]),
                len([a for a in self.agents if a.eligible == Eligible.ELIGIBLE and a.donor == True]),
                len([a for a in self.agents if (a.eligible == Eligible.ELIGIBLE or a.eligible == Eligible.TEMP_INELIGIBLE) and a.donor == True]),
                len([a for a in self.agents if a.eligible == Eligible.TEMP_INELIGIBLE]),
                len([a for a in self.agents if a.eligible == Eligible.DEFERRED]),
                len([a for a in self.agents if a.eligible == Eligible.DISQUALIFIED]),
                len([a for a in self.agents if a.eligible == Eligible.UNAVAILABLE]),
                self.collections,
                self.units
            ]
    
    def get_agents(self):
        agents_table = []
        for a in self.agents:
            agents_table.append((
                a.iden,
                a.sex,
                a.hla_ab,
                a.tti,
                a.sarscov2_ab,
                a.neut_titer_360,
                a.discharged,
                a.eligible,
                a.eligible_ever,
                a.time_eligible,
                a.time_ineligible,
                a.deferred_until,
                a.deferral_reason,
                a.willing,
                a.available_to_recruit,
                a.time_avail_recruitment,
                a.recruited,
                a.donor,
                a.donor_ever,
                a.donations_given,
                a.previous_donation,
                a.left_donor_pool,
                a.reason_left_donor_pool
            ))

        df = pd.DataFrame(agents_table, columns=[
            "id",
            "sex",
            "hla_ab_positive",
            "tti_positive",
            "bAb_positive",
            "nAb_titer_360",
            "discharged_time",
            "eligible",
            "eligible_ever",
            "eligible_time",
            "ineligible_time",
            "deferred_until",
            "deferral_reason",
            "willing",
            "available_to_recruit",
            "avail_recruitment_time",
            "recruited_time",
            "donor",
            "donor_ever",
            "donations_given",
            "previous_donation_time",
            "left_donor_pool",
            "reason_left_donor_pool"
            ])
        return df
    
    def get_report(self):
        df = pd.DataFrame(self.report_table, columns=self.report_columns)
        return df

    def output_agents(self, file="agents.feather"):
        feather.write_feather(self.get_agents(), file)

    def output_report(self, file="report.feather"):
        feather.write_feather(self.get_report(), file)

    def plot(self, cols=["eligible"]):
        df = self.get_report()
        start = 0 #self.burn_in_iterations
        for col in cols:
            series = pd.Series(df[col][start:], index=df["iteration"][start:])
            series.plot.line(label = col)
        plt.legend()
        plt.show()

    def run(self):
        for self.iteration in range(self.iteration, self.iterations):
            self.spawn_agents()
            self.advance()
            if self.iteration >= self.parameters["recruitment_start"]:
                self.recruit()
            if self.iteration >= self.parameters["collection_start"]:
                self.collect()
            self.report()
            if self.progress_callback != None and self.iteration % 10 == 0:
                self.progress_callback(10/self.iterations)

def run_simulation(parameters, seed=None, progress_callback=None):
    s = Simulation(parameters, seed=seed, progress_callback=progress_callback)
    start = time.time()
    s.run()
    end = time.time()
    s.time_taken = end - start
    return s

def multi_threaded_run(parameter_list,
                       simulations=4,
                       processes=mp.cpu_count(),
                       root_seed = None, # Not used currently
                       simset_name = "mutiple_simulations",
                       output_report = False,
                       output_agents = False,
                       output_parameters = False,
                       report_dir = "./",
                       return_results = True
                      ):

    if len(parameter_list) == 1:
        parameter_list = [parameter_list[0].copy() for _ in range(simulations)]
    else:
        parameter_list = [p.copy() for p in parameter_list]

    if len(parameter_list) != simulations:
        print("Warning: setting num simulations to match len of parameter list",
              file=sys.stderr)
        simulations = len(parameter_list)
    
    for i in range(simulations):
        parameter_list[i]["id"] = i
    
    if root_seed == None:
        seed_list = [None] * simulations
    else:
        seed_list = [root_seed + i for i in range(simulations)]

    print("Running {} simulations in {} processes...".format(simulations, processes))
    start = time.time()
    p = mp.Pool(processes)
    res = [p.apply_async(run_simulation, args=(parameter_list[i],), kwds = {'seed':seed_list[i]}) for i in range(simulations)]
    results = [r.get() for r in res]
    end = time.time()
    print("Simulations took {} seconds".format(round(end-start)))
    
    if output_report:
        report = pd.concat([r.get_report() for r in results], 
                           ignore_index = True)
        feather.write_feather(report, report_dir + "report_" + simset_name + ".feather")
    
    if output_agents:
        digits = len(str(simulations-1))
        [p.apply_async(r.output_agents, kwds = {"file": report_dir + "agents_" + simset_name + "_" + str(r.iden).zfill(digits) + ".feather"}) for r in results]
            
    if output_parameters:
        # Pickle whole list of parameter dictionaries
        filename = report_dir + "parameters_" + simset_name + ".pickle"
        pickle.dump(parameter_list, open(filename, "wb"))
        # Create and write out dataframe
        filename = report_dir + "parameters_" + simset_name + ".feather"
        keys_to_remove = ['recovered', 'donor_return_second', 'donor_return_third', 'donor_return_later']
        [p.pop(k) for k in keys_to_remove for p in parameter_list]
        feather.write_feather(pd.DataFrame(parameter_list), filename)

    p.close()
    p.join()

    if return_results:
        return results
    else:
        pass

if __name__ == '__main__':
    pass

