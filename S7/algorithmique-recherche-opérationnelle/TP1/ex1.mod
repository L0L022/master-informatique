# Loïc Escales
# Algo - TP 1 - Exercice 1

param ai;
/* Argent à investir */

set O;
/* Obligations */

param ra{i in O};
/* Revenu annuel */

param ma{i in O} symbolic in {'Courte', 'Longue'};
/* Maturité */

param ri{i in O} symbolic in {'Faible', 'Élevé'};
/* Risque */

param ef{i in O} symbolic in {'Non', 'Oui'};
/* Exonération fiscale */

var mi{i in O} >= 0;
/* Montant investi */

maximize rat: sum{i in O} ra[i] * mi[i];
/* le revenu annuel total */

s.t. c1: sum{i in O: ma[i] == 'Courte'} mi[i] >= 0.5 * ai;
/* au moins 50% de l'argent doit être investi dans des obligations à court terme */

s.t. c2: sum{i in O: ri[i] == 'Élevé'} mi[i] <= 0.5 * ai;
/* au plus 50% de l'argent doit être investi dans des obligations à risque élevé */

s.t. c3: sum{i in O: ef[i] == 'Oui'} mi[i] >= 0.4 * ai;
/* au moins 40% des fonds doivent aller dans des investissements exonérés d'impôts */

s.t. c4: sum{i in O: ef[i] == 'Oui'} ra[i] * mi[i] >= 0.3 * sum{i in O} ra[i] * mi[i];
/* au moins 30% des revenus annuels doivent être exonérés d'impôts */

s.t. c5: sum{i in O} mi[i] = ai;
/* à investir en obligation */

solve;

printf 'Revenu annuel total = %.2f\n', rat;
printf{i in O}:'Mois n°%s = %f\n', i, mi[i]; 

data;

param ai := 100000;

set O := A B C D E;

param ra := A 0.1
            B 0.04
            C 0.07
            D 0.06
            E 0.08;

param ma := A 'Longue'
            B 'Courte'
            C 'Longue'
            D 'Longue'
            E 'Courte';

param ri := A 'Élevé'
            B 'Faible'
            C 'Faible'
            D 'Faible'
            E 'Élevé';

param ef := A 'Non'
            B 'Oui'
            C 'Non'
            D 'Oui'
            E 'Non';

end;