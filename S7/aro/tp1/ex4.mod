# Loïc Escales
# Algo - TP 1 - Exercice 4

# Non résolvable si la quantité souhaitée pile est demandée.

set M;
/* Mélanges */

set I;
/* Ingrédients */

param p{i in I, j in M};
/* Pourcentage d'ingrédients i pour dans un mélange m */

param c{i in M};
/* Coût (euros/kg) */

param qm;
/* Quantité de mélange souhaitée (kg) */

param qim{i in I};
/* Quantité d'ingrédients minimum */

var m{i in M} >= 0;
/* Quantité de mélange utilisé (kg) */

minimize cmf: sum{i in M} c[i] * m[i];
/* le coût du mélange final */

s.t. c1{i in I}: sum{j in M} p[i, j] * m[j] >= qim[i] * qm;
/* contraite sur la quantité d'ingrédients mimimum */

s.t. c2: sum{i in M} m[i] >= qm;
/* produire au moins la quantité souhaitée */

solve;

printf 'Le coût du mélange final = %f\n', cmf;
printf{i in M}:'Quantité de %s = %f\n', i, m[i]; 
printf:'Quantité totale = %f\n', sum{i in M} m[i];

data;

set M := M1 M2 M3 M4;

set I := Mais Graines Mineraux;

param p :         M1   M2    M3   M4 :=
        Mais      0.3  0.05  0.2  0.1
        Graines   0.1  0.3   0.15 0.1
        Mineraux  0.2  0.15  0.2  0.3;

param c := M1 0.5
           M2 0.6
           M3 0.64
           M4 0.3;

param qm := 4000;

param qim := Mais 0.2
             Graines 0.15
             Mineraux 0.25;

end;