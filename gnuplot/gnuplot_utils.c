#include <unistd.h>    

extern int write_var (float *var)
{
    write(STDOUT_FILENO, var, sizeof(float));
    return 0;
}

/* #include <unistd.h>     */
/* int main(int argc, char* argv[]) */
/* { */
/*     const int N = 128; */
/*     double values[N]; */
/*     int i; */
/*     for (i = 0; i < N; i++) */
/*     values[i] = i * i; */

/*     write(STDOUT_FILENO, values, N*sizeof(double)); */
/* } */
/* from https://stackoverflow.com/questions/38436120/plot-single-column-binary-file-with-gnuplot */
