#if !defined (__IOSTREAM_H)
#include <iostream>
#endif
#if !defined (__FSTREAM_H)
#include <fstream>
#endif
#if !defined (__ASSERT_H)
#include <assert.h>
#endif
#if !defined(__CMATH_H)
#include <cmath>
#endif
#if !defined(__STRING_H)
#include <string>
#endif
#if !defined (__VECTOR_H)
#include <vector>
#endif

#if !defined (VECTEUR_DOUBLE_H)
#include "vecteur_template.h"
#endif
using namespace std;

//Définition du nouveau type matrice profil

struct mat_profil{
vector<int> INDIAG;
vector<double> ATAB;
};

// definition de la solution exacte uex
double uex(double x){
return cos(M_PI*x);
}
// definion de la condition initial sur u0
double u0(double x){
return uex(0,x);}
//definition de la fonction second memmbte f
double f(double t, double x,double nu){ //nu la constante de diffusion
double uex_t = cos(t)*cos(M_PI*x) ;// derivé de uex par rapport à t
double uex_xx = -M_PI-M_PI*sin(t)*cos(M_PI*x);//  dérivée second de uex par rapport à x
return uex_t -nu*uex_xx;
}




    return 0;
}
