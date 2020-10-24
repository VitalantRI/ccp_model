import pandas as pd
import numpy as np
import pyarrow.feather as feather
import requests
import json
from datetime import timedelta, date
import pickle

if __name__ == '__main__':

    states = ["NJ", "MA", "IN", "LA", "CT", "MS", "VA", "MD", "NY", "IL", "CA"] #"CA"

    pop = {}

    for state in states:
        print(state)
        url = "https://data.covidactnow.org/latest/us/states/" + state + ".OBSERVED_INTERVENTION.timeseries.json"
        content = requests.get(url)
        data_list = json.loads(content.content)
        actuals = pd.json_normalize(data_list['actualsTimeseries']).dropna(subset=['cumulativeConfirmedCases'])
        pr = pd.json_normalize(data_list['timeseries']) #.dropna()
        pr['date'] = pd.to_datetime(pr.date)
        pr['deaths'] = pr.cumulativeDeaths.shift(-1) - pr.cumulativeDeaths
        pr['recoveries'] = pr.currentInfected + pr.cumulativeInfected.shift(-1) - \
            pr.cumulativeInfected - pr.currentInfected.shift(-1)
        pr['prop_hospitalized'] = pr.hospitalBedsRequired / pr.currentInfected
        pr['discharges'] = np.round(pr.prop_hospitalized*pr.recoveries)
        prop_direct_icu = 0.04
        prop_stepped_up_icu = 0.12
        delay_step_up = 2
        pr['admissions'] = pr.hospitalBedsRequired.shift(-1) - pr.hospitalBedsRequired + pr.discharges + pr.deaths
        pr['icu_admissions'] = np.round(pr.admissions*prop_direct_icu + pr.admissions.shift(delay_step_up) * prop_stepped_up_icu)

        pop.update({state:actuals.population.iloc[0]})
        
        feather.write_feather(pr, "covidactnow_" + state + "_" + str(date.today()) + ".feather" )
    
    pickle.dump(pop, open("population_sizes.pickle", "wb"))
        