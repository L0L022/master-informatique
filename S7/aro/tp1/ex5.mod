# Loïc Escales
# Algo - TP 1 - Exercice 5

set M;
/* Mois */

param cpu{i in M};
/* Coût de production unitaire (euros) */

param d{i in M};
/* Demande (nb unités) */

param cp{i in M};
/* Capacité de production (nb unités) */

param te;
/* Taille de l'entrepôt */

param umm >= 0;
/* Nombre d'unités minimum en stock chaque mois */

param ui;
/* Nombre d'unités initialement */

var u{i in M} >= 0, <= cp[i], integer;
/* Nombre d'unités produites */

var us{i in M} >= umm, <= te, integer;
/* Nombre d'unités en stock */

minimize nut: sum{i in M} u[i] * cpu[i] + sum{i in 1..(card(M) - 1)} us[i] * 0.015 * cpu[i];
/* le nombre d'unités totale */

s.t. us_cntrnt{i in M}: us[i] = ui + sum{j in 1..i} (u[j] - d[j]);
/* définition de us */

solve;

printf 'Le nombre d unités produites totale = %i\n', nut;
printf{i in M}:'Mois %s : %i unités produites\t\t%i unités stockées\n', i, u[i], us[i]; 

data;

set M := 1 2 3 4 5 6;

param cpu := 1 240
             2 250
             3 265
             4 285
             5 280
             6 285;

param d := 1 1000
           2 4500
           3 6000
           4 5500
           5 3500
           6 4000;

param cp := 1 4000
            2 3500
            3 4000
            4 4500
            5 4000
            6 3500;

param te := 6000;

param umm := 1500;

param ui := 2750;

end;