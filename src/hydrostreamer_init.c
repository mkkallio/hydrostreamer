#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(delineate)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(routesimple)(void *, void *, void *, void *, void *, void *);
extern void F77_NAME(routesimple2)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"delineate",    (DL_FUNC) &F77_NAME(delineate),    7},
    {"routesimple",  (DL_FUNC) &F77_NAME(routesimple),  6},
    {"routesimple2", (DL_FUNC) &F77_NAME(routesimple2), 11},
    {NULL, NULL, 0}
};

void R_init_hydrostreamer(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
