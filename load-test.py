"""
simple script to test timing to load file, for comaprison
to new scheme library
"""

import os
import time
import xarray as xr
import matplotlib.pyplot as plt

df = xr.open_dataset('%s/isccp/b1/GRIDSAT-B1.1987.05.03.18.v02r01.nc'\
                     % os.environ['DATA'])
start = time.time()
var = df['irwin_cdr'].load()
test = var.values
print('time to load irwin_cdr: %f s' % (time.time()-start))

