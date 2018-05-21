/* 
Example script adapted from:
https://www.unidata.ucar.edu/software/netcdf/docs/simple_xy_nc4_wr_8c-example.html 
*/
#include <stdlib.h>
#include <stdio.h>
#include <netcdf.h>
 
/* This is the name of the data file we will create. */
#define FILE_NAME "simple_xyz_nc4.nc"
 
/* We are writing 2D data, a 6 x 12 grid. */
#define NDIMS 3
#define NX 2
#define NY 4
#define NZ 6
 
/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}
 
int
main()
{
  int ncid, x_dimid, y_dimid, z_dimid, varid;
  int dimids[NDIMS];
  size_t chunks[NDIMS];
  int shuffle, deflate, deflate_level;
  int data_out[NX][NY][NZ];
  int x, y, z, retval;
 
  /* Set chunking, shuffle, and deflate. */
  shuffle = NC_SHUFFLE;
  deflate = 1;
  deflate_level = 1;
 
  /* Create some pretend data. If this wasn't an example program, we
   * would have some real data to write, for example, model output. */
  for (x = 0; x < NX; x++)
    for (y = 0; y < NY; y++)
      for (z = 0; z < NZ; z++)
        data_out[x][y][z] = x * NY * NZ + y*NZ + z;
 
  /* Create the file. The NC_NETCDF4 parameter tells netCDF to create
   * a file in netCDF-4/HDF5 standard. */
  if ((retval = nc_create(FILE_NAME, NC_NETCDF4, &ncid)))
    ERR(retval);
 
  /* Define the dimensions. */
  if ((retval = nc_def_dim(ncid, "x", NX, &x_dimid)))
    ERR(retval);
  if ((retval = nc_def_dim(ncid, "y", NY, &y_dimid)))
    ERR(retval);
  if ((retval = nc_def_dim(ncid, "z", NZ, &z_dimid)))
    ERR(retval);
 
  /* Set up variabe data. */
  dimids[0] = x_dimid;
  dimids[1] = y_dimid;
  dimids[2] = z_dimid;
  chunks[0] = NX/2;
  chunks[1] = NY/2;
  chunks[2] = NZ/2;
 
  /* Define the variable. */
  if ((retval = nc_def_var(ncid, "data", NC_INT, NDIMS,
                           dimids, &varid)))
    ERR(retval);
  if ((retval = nc_def_var_chunking(ncid, varid, 0, &chunks[0])))
    ERR(retval);
  if ((retval = nc_def_var_deflate(ncid, varid, shuffle, deflate,
                                   deflate_level)))
    ERR(retval);
 
  /* No need to explicitly end define mode for netCDF-4 files. Write
   * the pretend data to the file. */
  if ((retval = nc_put_var_int(ncid, varid, &data_out[0][0][0])))
    ERR(retval);
 
  /* Close the file. */
  if ((retval = nc_close(ncid)))
    ERR(retval);
 
  printf("*** SUCCESS writing example file simple_xy_nc4.nc!\n");
  return 0;
}
