## scheme-netcdf ##

Playing around with MIT/GNU Scheme's FFI, with the goal of getting netcdf functionality into mit-scheme.

### Requisites ###

1. netcdf libraries installed (I'm using version 4.4.1). The script looks in the usual places for headers and shared objects (`usr/include` `/usr/lib/x86_64-linux-gnu` ).
2. MIT/GNU Scheme Version 9.2

### Instructions ###

Run "make build" to build and "make check" to do a crude test of the package. Note that everything *should* just work if you installed MIT/GNU Scheme, GCC, and netcdf through GNU Guix. If you dont use Guix as your package manual, you will have to tweak the Makefile but that shouldn't be too bad. Autotools might be added at some point, we'll see. 

### Sample usage ###

More documentation to come, also this is all still in development and likely to change.

`(make-meta filename)` - load metadata from filename (output analogous to `ncdump -h`).

`(make-var-data meta varname)` - load data variable (defined by the string `varname`) from dataset described by meta (loaded with `make-meta`).

Currently above variable contains an association list of two members - data which is a list of all the variable's data dumped in C-order, and meta which gives summary metadata for the variable (dims, longname, etc.). While lists have disadvantages (linear access time, larger size in memory), they are very flexible. So my idea is to pass this first structure to other functions that will make more sophisticated and higher order formats for the data. The meta part of the data should have all information needed to derive any more sophisticated format we want. So, with the list as an intermediary, the format we want in the end can be a "late binding" decision. 



