import os
import sys
import pandas as pd
import time
import pyarrow.feather as feather
import pyarrow
import shutil

### CONCAT ALL REPORTS IN GROUPS of 200

dir_name = "./outputs/"
report_names = [i for i in os.listdir(dir_name) if (i.startswith("report_") and i.endswith(".feather"))]
j=0
t1  = time.time()
for i in range(0, len(report_names)):
    report_name = report_names[i]
    report = pd.read_feather((dir_name + report_name))
    report_name_split = report_name.replace('.', '_').split('_')
    report["state"] = report_name_split[2]
    report["parametricRT"] = 1 if report_name_split[3]=="parametric" else 0
    report["run_id"] = report_name_split[4]
    report["batch"] = report_name_split[5]

    if i % 200 == 0:
        if i>0:
            t2 = time.time()
            print(str(i) + " added. " + str(t2 - t1) + "s.")
            t1 = t2
            report_concat.to_csv("./outputs/concat_report_2020_09_17_"+str(j)+".csv")
            j=j+1
        report_concat = report
    else:
        report_concat = report_concat.append(report)

#Add any remaining files if it wasn't a  multiple of 200
report_concat.to_csv("./outputs/concat_report_2020_09_17_"+str(j)+".csv")

# Then concatinate these into the final file
for i in range(j+1):
    if i == 0:
        report_all = pd.read_csv("./outputs/concat_report_2020_09_17_"+str(i)+".csv")
    else:
        this_report = pd.read_csv("./outputs/concat_report_2020_09_17_"+str(i)+".csv")
        report_all = report_all.append(this_report)
    print(str(i) + " appended")
#Save all reports to file
report_all.to_csv("./outputs/concat_report_2020_09_09.csv")
        
        
        
### PARAMETERS
f_names = [i for i in os.listdir(dir_name) if (i.startswith("parameters_") and i.endswith(".feather"))]

t1  = time.time()
for i in range(0, len(f_names)):
    f_name = f_names[i]
    param_dt = pd.read_feather((dir_name + f_name))
    f_name_split = f_name.replace('.', '_').split('_')
    param_dt["state"] = f_name_split[2]
    param_dt["parametricRT"] = 1 if f_name_split[3]=="parametric" else 0
    param_dt["run_id"] = f_name_split[4]
    param_dt["batch"] = f_name_split[5]

    if i == 0:
        params_concat = param_dt
    else:
        params_concat = params_concat.append(param_dt)
    #t2 = time.time()
    #print(str(i) + " added. " + str(t2 - t1) + "s.")
    #t1 = t2
    
params_concat.to_csv("./outputs/concat_params_2020_09_09.csv")
print("Parameters done. "+ str(time.time() - t1) + "s.")

