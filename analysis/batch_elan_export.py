import os
from bs4 import BeautifulSoup
import numpy as np
import pandas as pd
import platform

if platform.system() == 'Windows':
    sep = '\\'
    niño = "NiÃ±x"
else:
    sep = '/'
    niño = "Niñx"

def relativeFilePaths(directory):
  for dirpath,_,filenames in os.walk(directory):
    for f in filenames:
      yield os.path.relpath(os.path.join(dirpath, f))

elans = list(relativeFilePaths('raw/elan'))
elans = ([file for file in elans if file.endswith(".eaf")])

for file in range(0, len(elans)):
  try:

    with open(elans[file]) as f: 
      data = f.read()

    bs = BeautifulSoup(data, "xml")
    niñx = bs.find('TIER', {'TIER_ID':niño}).find_all("ALIGNABLE_ANNOTATION") #"Ni\u00f1x Niñx NiÃ±x
    adultx = bs.find('TIER', {'TIER_ID':'Adultx'}).find_all("ALIGNABLE_ANNOTATION")
    Onset = []
    Offset = []
    Sujeto = []

    for i in niñx:
      Onset.append(bs.find("TIME_SLOT", {"TIME_SLOT_ID":i.get("TIME_SLOT_REF1")}).get("TIME_VALUE"))
      Offset.append(bs.find("TIME_SLOT", {"TIME_SLOT_ID":i.get("TIME_SLOT_REF2")}).get("TIME_VALUE"))
      Sujeto.append("Niñx")

    for i in adultx:
      Onset.append(bs.find("TIME_SLOT", {"TIME_SLOT_ID":i.get("TIME_SLOT_REF1")}).get("TIME_VALUE"))
      Offset.append(bs.find("TIME_SLOT", {"TIME_SLOT_ID":i.get("TIME_SLOT_REF2")}).get("TIME_VALUE"))
      Sujeto.append("Adultx")

    bind = pd.DataFrame([Onset, Offset, Sujeto]).T
    bind.columns = ["Onset", "Offset", "Sujeto"]
    bind.to_csv(f"baked/elan/bindos/{elans[file].split(sep)[2].split('.')[0]}_bind.csv", index=False)
    Onset = (np.round(np.array(Onset).astype(int)/30).astype(int)).tolist()
    Offset = (np.round(np.array(Offset).astype(int)/30).astype(int)).tolist()
    bind = pd.DataFrame([Onset, Offset, Sujeto]).T
    bind.columns = ["Onset", "Offset", "Sujeto"]
    frames = np.array([*range(1, round(bind["Offset"].max()))])
    filtro = bind.loc[bind["Sujeto"] == "Niñx"]
    nts = np.repeat(0,frames.max())

    for i in filtro.iterrows():
      nts[int(i[1][0])-1:int(i[1][1])-1] = 1

    filtro = bind.loc[bind["Sujeto"] == "Adultx"]
    ats = np.repeat(0,frames.max())

    for i in filtro.iterrows():
      ats[int(i[1][0])-1:int(i[1][1])-1] = 1

    ts = pd.DataFrame([frames, nts, ats]).T
    ts.columns = ["frames", "niñx_verb", "adultx_verb"]
    ts.to_csv(f"baked/elan/tss/{elans[file].split(sep)[2].split('.')[0]}_ts.csv", index=False)
  
  except:
    print(f'{elans[file]} failed')