# -*- coding: utf-8 -*-
"""
Created on Sun Oct 28 12:27:54 2017

@author: maryluz.mouronte (email: maryluz.mouronte@ufv.es)
"""

"""
Script to extract raw trade files from .tsv files SITC coding
Uncomment file names section and modify Years list to
select the years to be processed.

Warning: This process is CPU intensive
"""

import pandas as pd
import numpy as np
import os
import os.path as path
import numpy as np
import pandas as pd
import networkx as nx
import operator
import random
from random import randint
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
from os import listdir
import shutil
import time
from collections import OrderedDict
import copy
import glob
import dask.dataframe as dd



def LeerFicheros():
 
    
    Header = ['year','origin', 'dest', 'sitc', 'export_val','import_val']
    fileNameIn = '../data/raw_data/year_origin_destination_sitc_rev2.tsv'
    pathFileIn = os.getcwd() + "/" + fileNameIn

    fileNameInP = '../data/raw_data/Pyear_origin_destination_sitc_rev2.tsv'
    pathFileInP = os.getcwd() + "/" + fileNameInP
    if path.exists(pathFileInP):
        os.remove(pathFileInP)  


    print('Leyendo fichero') 
    
    
    r = open(pathFileIn,"r")
    w = open (pathFileInP, "w")
    l = r.readline()
    cont = 1
    while l:
      year = l.split('\t')[0]

      if (year.isdigit()):
          if (int(year) in Years):
              l=l.replace('\t',';')
              w.write(l) 
          
      elif (not(year.isdigit())):
          l=l.replace('\t',';')
          w.write(l) #Escribe la cabecera
      if ((cont % 1000000) == 0):
          print(cont)  
      
      l= r.readline()
      cont=cont+1
          
          
    r.close()
    w.close()

    datosRed = pd.read_table(pathFileInP, 'engine=python', delimiter=';', header=0, encoding = "ISO-8859-1", names=Header)
     
    #print(datosRed.head(10))
    print("Esto es")
    print(datosRed)
    for i in Years:
        print("Buildind year")
        print(i)
        datosRedRes = datosRed[(datosRed.year == i)]
        datosRedRes = datosRedRes.drop(labels="sitc", axis=1)
        #print(datosRedRes.head(10))
        #datosRedRes = datosRedRes.groupby(by=['year','origin', 'dest'])['export_val','import_val'].sum()
        datosRedRes=datosRedRes.fillna(0)
        #print(datosRedRes.head(10))
        #print(datosRedRes.dtypes)
        datosRedRes = datosRedRes.groupby(['year', 'origin', 'dest'])['export_val','import_val'].sum()
        #datosRedRes = datosRedRes.groupby(['year', 'origin', 'dest'])[['export_val','import_val']].sum()
        print("Despues de agrupar")
        print(datosRedRes.head(10))
        
        fileName = '../data/raw_data/TodosYR'+str(i)+'.csv'
        pathFile = os.getcwd() + "\\" + fileName
        Header = ['year', 'origin', 'dest', 'export_val', 'import_val']  

        datosRedRes.reset_index().to_csv(pathFile, index=None, mode='w', sep=';', columns=Header)
     


#Years = [1964,1965]   
Years=list(np.arange(1962,1974))
print("Building file")
LeerFicheros()  








