"""
simple script to test timing to load file, for comaprison
to new scheme library
"""
import os
import time
import xarray as xr
import matplotlib.pyplot as plt
start = time.time()
df = xr.open_dataset('%s/isccp/b1/GRIDSAT-B1.1987.05.03.18.v02r01.nc'\
                     % os.environ['DATA'])

var = df['irwin_cdr'].load()
test = var.values
print('time to load irwin_cdr: %f s' % (time.time()-start))

start = time.time()
plt.figure()
df['irwin_cdr'].plot()
plt.savefig('temp.png')
print('time to plot irwin_cdr: %f s' % (time.time()-start))

