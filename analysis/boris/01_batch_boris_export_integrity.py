import os
import json
import numpy as np
import pandas as pd
import platform

if platform.system() == 'Windows':
    sep = '\\'
else:
    sep = '/'

def relativeFilePaths(directory):
    for dirpath,_,filenames in os.walk(directory):
        for f in filenames:
            yield os.path.relpath(os.path.join(dirpath, f))

borises = list(relativeFilePaths('raw'))
borises = ([file for file in borises if file.endswith(".boris")])

for file in range(0, len(borises)):
    try:
        with open(borises[file]) as f: 
            data = f.read()
        js = json.loads(data)
        df = pd.DataFrame.from_dict(js.get("observations").get(list(js.get("observations"))[0]).get("events"))
        df = df.rename(columns={0:"onset", 1:"sujeto", 2:"categoría", 3:"modificador"})
        df = df.drop([4,5], axis=1)
        df["cc"] = df.groupby(["sujeto", "categoría"]).cumcount().add(1)
        df = df.sort_values(by=["sujeto", "categoría", "modificador"])
        df['offset'] = df.groupby(["sujeto", "categoría", "modificador"])['onset'].shift(-1)
        if any(df.groupby(["sujeto", "categoría"]).max()["cc"] % 2 != 0):
            print(borises[file])
            print(df.groupby(["sujeto", "categoría"]).max()["cc"])
        # else:    
        df = df[df["cc"] % 2 != 0]
        df.iloc[:, [0, 5, 1, 2, 3]].to_csv("baked/shifties/"+borises[file].split(sep)[2].split(".")[0]+"_shifted.csv", index=False)
    except:
        print(f'{borises[file]} failed')