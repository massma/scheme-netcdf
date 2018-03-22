"""
simple script to test timing to load file, for comaprison
to new scheme library
"""
import os
import time
import xarray as xr
import matplotlib.pyplot as plt
import numpy as np
start = time.time()
df = xr.open_dataset('%s/isccp/b1/GRIDSAT-B1.1987.05.03.18.v02r01.nc'\
                     % os.environ['DATA'])

var = df['irwin_cdr'].load()
test = var.values
print('time to load irwin_cdr: %f s' % (time.time()-start))

start = time.time()
plt.figure()
#df['irwin_cdr'].plot()
x, y = np.meshgrid(df['lon'], df['lat'])
plt.pcolormesh(x, y, np.squeeze(var))
plt.savefig('temp.png')
print('time to plot irwin_cdr: %f s' % (time.time()-start))

var_df = var.to_dataframe()

start = time.time()
var_df.to_csv("./isccp_df.out")
print('time to write irwin_cdr: %f s' % (time.time()-start))
