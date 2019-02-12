//Pontifica Universidad Javeriana
//Analisis numerico
//Camilo cruz
// codigo en c++
#include <iostream>
#include <conio.h>
#include <math.h>
using namespace std;

int main ( )
 {
    int grado, m, remp;
    cout << "Grado del polimonoi";
    cout<<endl;
    cin >> grado;
    int a[grado], b[grado];
    cout << "Ingrese los coeficientes con su signo correspondiente ";
    cout<<endl;
    for( int i=0; i<=grado; i++){
        m = grado-i;
        cout << "ingrese le numero que acompania la base del exponente a("<< m <<") : > ";
        cout<<endl;
        cin >> a[grado-i];
    }
     cout << " usted ingreso: P(x) = ";
     cout<<endl;
     for(int i=0; i<=grado; i++){
           m=grado-i;
           if(i!=grado){
                cout << " " << a[m] << " x' " << m << " + ";
            }
           else{
               cout << " " << a[m] << " ";
             }
      }
    cout << " Coloque el valor para evaluar el P(x): ";
    cin >> remp;
    int mul=0, sum=0;
    b[grado] = a[grado];
    cout<<b[grado]<<"\n";
    for(int k=(grado-1); k>=0; k--){
        b[k]=remp*b[k+1];
        cout<<b[k]<<" 1 "<<endl;
        b[k]=b[k]+a[k];
        cout<<b[k]<<" 2 "<<endl;
         mul++;
         sum++;
    }

    cout << " Solucion:  " << b[0];
    cout << endl << endl;
    cout<< "el numero de sumas y restas es:" << sum<<endl;
    cout<< "el numero de productos es:" << mul<<endl;
    return 0;
}
