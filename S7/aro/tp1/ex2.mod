# Loïc Escales
# Algo - TP 1 - Exercice 2

set R;
/* Région */

set M;
/* Minoterie */

param d{i in R, j in M};
/* Distances (kms) d'une région à une minoterie */

param br{i in R};
/* Blé récolté (tonnes) */

param bat{i in M};
/* Blé à transporter (tonnes) */

var bt{i in R, j in M} >= 0;
/* Blé transporté (tonnes) */

minimize ctt: sum{i in R, j in M} 0.1 * d[i, j] * bt[i, j];
/* le coût total de trasport */

s.t. c1{i in R}: sum{j in M} bt[i, j] <= br[i];
/* on ne pas transporter plus que ce qui est produit */

s.t. c2{j in M}: sum{i in R} bt[i, j] = bat[j];
/* elle doit trasporter x tonnes à la minoterie */

solve;

printf 'Coût total de trasport = %f\n', ctt;
printf{i in R, j in M}:'Blé transporté (tonnes) de la région %s à la minoterie %s = %f\n', i, j, bt[i, j]; 

data;

set R := R1 R2 R3;

set M := M1 M2 M3;

param d :   M1   M2   M3 :=
        R1  210  500  400
        R2  350  300  220
        R3  550  200  250;

param br := R1 275
            R2 400
            R3 300;

param bat := M1 200
             M2 550
             M3 225;

end;