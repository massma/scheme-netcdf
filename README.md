## scheme-netcdf ##

Playing around with MIT/GNU Scheme's FFI, with the goal of getting netcdf functionality into mit-scheme.

### Requisites ###

1. netcdf libraries installed (I'm using version 4.4.1). The script looks in the usual places for headers and shared objects (`usr/include` `/usr/lib/x86_64-linux-gnu` ).
2. [development version of mit-scheme] (http://git.savannah.gnu.org/cgit/mit-scheme.git/) (note this might be important, I think the FFI interface is still in development with updates since the stable version 9.2). Currently I'm working on commit [9ff7e8e8ce9215c1ba5d5ccfb1bba0939ab3f396] (http://git.savannah.gnu.org/cgit/mit-scheme.git/commit/?id=9ff7e8e8ce9215c1ba5d5ccfb1bba0939ab3f396) .
3. set an environmental variable "MITSCHEME_ROOT" which points to the top level of your scheme (i.e. where `src/` `lib/` `bin/` etc. directories are).

### Instructions ###

load  "install-netcdf.scm" into scheme, and it should build and run a couple of simple tests (defined in test-netcdf.scm). You can also run the tests by going to the "testing" directory and running `make test`.

### Sample usage ###

More documentation to come, also this is all still in development and likely to change.

`(make-meta filename)` - load metadata from filename (output analogous to `ncdump -h`).

`(make-var-data meta varname)` - load data variable (defined by the string `varname`) from dataset described by meta (loaded with `make-meta`).

Currently above variable contains an association list of two members - data which is a list of all the variable's data dumped in C-order, and meta which gives summary metadata for the variable (dims, longname, etc.). While lists have disadvantages (linear access time, larger size in memory), they are very flexible. So my idea is to pass this first structure to other functions that will make more sophisticated and higher order formats for the data. The meta part of the data should have all information needed to derive any more sophisticated format we want. So, with the list as an intermediary, the format we want in the end can be a "late binding" decision. 



