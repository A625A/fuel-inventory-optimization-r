% R ya simuló la demanda. Este modelo elige una de tres políticas validadas.
int: n;
int: capacidad;
int: perdida_maxima;
set of int: Candidatas = 1..n;
array[Candidatas] of int: id;
array[Candidatas] of int: umbral;
array[Candidatas] of int: objetivo;
array[Candidatas] of int: inventario; % centilitros del promedio calculado en R
array[Candidatas] of int: perdida;   % litros totales, redondeados hacia arriba
var Candidatas: elegida;

constraint 0 <= umbral[elegida];
constraint umbral[elegida] < objetivo[elegida];
constraint objetivo[elegida] <= capacidad;
constraint perdida[elegida] <= perdida_maxima;

% Las filas están ordenadas por R. El índice resuelve empates a 0.01 L.
solve minimize inventario[elegida] * (n + 1) + elegida;

output ["id = ", show(id[elegida]), ";\n",
        "s = ", show(umbral[elegida]), ";\n",
        "S = ", show(objetivo[elegida]), ";\n",
        "inventario_centilitros = ", show(inventario[elegida]), ";\n",
        "perdida_total_litros = ", show(perdida[elegida]), ";\n"];
