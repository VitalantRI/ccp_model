import pickle
import pandas as pd

filename = "parameters_statesim_NY_588337_007.pickle"

parms = pickle.load(open(filename, "rb"))

print(type(parms))