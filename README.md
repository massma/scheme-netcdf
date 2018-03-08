## scheme-netcdf ##

Playing around with MIT/GNU Scheme's FFI, with the goal of getting netcdf functionality into mit-scheme.

### Requisites ###

1. netcdf libraries installed
2. development version of mit-scheme (http://git.savannah.gnu.org/cgit/mit-scheme.git/) (note this is important, the FFI interface is still in development and has been updates since stable version 9.2)
3. set an environmental variable "MITSCHEME_ROOT" which points to the top level of your scheme (i.e. where src/ lib/ bin/ etc. directories are).

### Instructions ###

run  "install-netcdf.scm" into scheme, and it should build and run a simple test of printing the netcdf library version.

