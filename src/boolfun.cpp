#include "boolfun.h"

R_CallMethodDef callMethods[] = {
      {"mobiusTransform",   (void*(*)()) & mobiusTransform,   2},
      {"walshTransform",    (void*(*)()) & walshTransform,    2},
      {"algebraicImmunity", (void*(*)()) & algebraicImmunity, 2},
      {"degree",            (void*(*)()) & algebraicImmunity, 2},
      {"add",               (void*(*)()) & algebraicImmunity, 3},
      {"multiply",          (void*(*)()) & multiply,          3},
      { NULL, NULL, 0}
};

void R_init_boolfun(DllInfo *info) 
{
      R_registerRoutines( info ,NULL, callMethods, NULL, NULL ); 
}

