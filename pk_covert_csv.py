import pandas as pd
import pickle 

from os import listdir
from os.path import isfile, join

mypath = "./data"
files = [f for f in listdir(mypath) if isfile(join(mypath, f))]

for file in files:
    db = pickle.load(open(f"./data/{file}", "rb" ))
    db.to_csv(f"./csvs/{file[:-7]}.csv")