#include <netcdf.h>
#include <stdio.h>

extern int
test(const char * path, int * ncidp)
{
  int ret0;
  printf("path: %s", path);
  ret0 = nc_open(path, NC_NOWRITE, ncidp);
  return ret0;
}


