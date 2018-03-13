#include <stdio.h>
#include <netcdf.h>

int main() {
  const char * libvers;
  libvers = nc_inq_libvers();
  printf(libvers);
}
