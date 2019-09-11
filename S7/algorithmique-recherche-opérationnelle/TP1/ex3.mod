# Loïc Escales
# Algo - TP 1 - Exercice 3

set H;
/* Périodes */

param np{i in H};

param nam{i in H};
/* Nombre d'agents minimum */

var a{i in H} >= 0, integer;
/* Nombre d'agents qui travaillent à partir de la période i */

minimize nat: sum{i in H} a[i];
/* le nombre d'agents total */

s.t. c1{i in H}: a[(i - 1) mod card(H)] + a[i] >= nam[i];
/* contrainte sur le nombre d'agents minimum par période */

solve;

printf 'Le nombre d agents total = %f\n', nat;
printf{i in H}:'Nombre d agents pour la période %s = %f\n', i, a[i]; 

data;

set H := 0 1 2 3 4 5;

param nam := 0  9
             1 21
             2 25
             3 16
             4 30
             5 12;

end;