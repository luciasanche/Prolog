:- module(_,_,[assertions]).

:- doc(title, "Aut@'{o}matas Celulares Reversibles").

:- doc(author, "Luc@'{i}a S@'{a}nchez Bella, 20M081").

:- doc(module, "@section{Introducci@'{o}n}

Este m@'{o}dulo codifica un aut@'{o}mata celular, que contiene c@'{e}lulas que pueden tener dos colores @var{o} y @var{x}, el aut@'{o}mata va cambiando el color de cada c@'{e}lula aplicando un determinado conjunto de reglas.

Todos los estados deben empezar y acabar con el color @var{o}, es decir, tienen que ser de la forma @var{[o,...,o]}. Adem@'{a}s tienen que tener un m@'{i}nimo de 3 c@'{e}lulas, por lo tanto el estado inicial ser@'{a} @var{[x,o,x]}.
Se va a hablar tambi@'{e}n de pasos de evoluci@'{o}n que consisten en aplicar el conjunto de reglas proporcionado sobre el estado final del paso anterior, o el estado inicial en el caso del primer paso.
@section{Ejemplos de uso}

@begin{enumerate}
@item Aplicar un conjunto de reglas a un estado incial:
@begin{verbatim}
?- cells([o,x,x,o],r(o,o,x,o,x,x,o),Cells).

Cells = [o,x,x,x,o,o] ?
yes
@end{verbatim}

@begin{verbatim}
?- cells(Cells,r(x,x,o,x,o,x,o),[o,o,x,x,o]).

Cells = [o,x,o] ?
yes
@end{verbatim}

@begin{verbatim}
?- cells([o,x,o,o,o,x,o],R,[o,o,x,x,o,o,x,x,o]).

R = r(x,x,o,_,_,_,_) ?
yes
@end{verbatim}
@item Aplicar N pasos de evoluci@'{o}n al estado inicial [o,x,o]:
@begin{verbatim}
?- evol(N, r(x,x,x,o,o,x,o), Cells).

Cells = [o,x,o],
N = 0 ?
yes
?- 
@end{verbatim}

@begin{verbatim}
?- evol(s(0),r(o,x,o,x,x,x,o),Cells).

Cells = [o,o,x,o,o] ?
yes
?- 
@end{verbatim}

@begin{verbatim}
?-evol(s(0),R,[o,o,x,x,o]).

R = r(x,x,o,_,_,_,_) ?
yes
?- 
@end{verbatim}
@item Comprobar si se puede llegar a un estado a partir de [o,x,o] con un determinado conjunto de reglas:
@begin{verbatim}
?- ruleset(R,[o,o,x,x,o]).

R = r(x,x,o,_,_,_,_) ?
yes
?- 
@end{verbatim}

@begin{verbatim}
?- ruleset(R,[o,o,o,o,o,o,o,o,o,o,o,o,x,x,o]).

R = r(x,x,o,_,x,o,_) ?
yes
@end{verbatim}
@end{enumerate}

").
:- pred author_data(nombre, ap1, ap2, mat)
#"@var{nombre} indica el nombre del autor, @var{ap1} indica el primer apellido, @var{ap2} indica el segundo apellido y @var{mat} indica el n@'{u}mero de matr@'{i}cula. @includedef{author_data/4}".
author_data('Sanchez','Bella','Lucia','20M081').

:- pred color(c)
#"Establece cuales son los posibles colores que puede tener una c@'{e}lula @var{o} es blanco y @var{x} es negro. @includedef{color/1}".
color(o).
color(x).

:- pred rule(a,b,c,R,e)
#"Este predicado consulta una regla @var{R} y segun el valor del color de @var{a}, @var{b} y @var{c}, nos proporciona un nuevo color de @var{b} en la variable @var{d}

@includedef{rule/5}".
rule(o,o,o,_,o). % regla nula
rule(x,o,o,r(A,_,_,_,_,_,_),A) :- color(A).
rule(o,x,o,r(_,B,_,_,_,_,_),B) :- color(B).
rule(o,o,x,r(_,_,C,_,_,_,_),C) :- color(C).
rule(x,o,x,r(_,_,_,D,_,_,_),D) :- color(D).
rule(x,x,o,r(_,_,_,_,E,_,_),E) :- color(E).
rule(o,x,x,r(_,_,_,_,_,F,_),F) :- color(F).
rule(x,x,x,r(_,_,_,_,_,_,G),G) :- color(G).

:- pred cells(lista1(color),r,lista2(color))
#"@var{lista2(color)} es el resultado de aplicarle el conjunto de reglas @var{r} a la lista incial @var{lista1(color)}.

Primero comprobamos que @var{lista1(color)} empiece por @var{o}, no hace falta comprobar que hay mas de 3 ya que si
 @var{Resto}=[] se llamaria  a @pred{cells2} con solo @var{[o,A]}, que no funcionar@'{i}a ya que todos las llamadas a cells2 
 tienen 3 elementos m@'{i}nimo. Inicializamos @var{lista2(color)} con una @var{o} al principio, adem@'{a}s de la primera c@'{e}lula ya cambiada. 
 Finalmente llamamos a cells2 con los tres primeros elementos de @var{lista1(color)}.


 
@includedef{cells/3}".

cells([o,A|Resto],X,[o,Y|RestoC]) :- rule(o,o,A,X,Y), cells2([o,A|Resto],X,RestoC).

:- pred cells2(lista1(color),r,lista2(color))
#"@var{lista2(color)} es el resultado de aplicarle el conjunto de reglas @var{r} a la lista incial @var{lista1(color)}.Vamos llamando a
 @pred{cells2} de forma recursiva de tal manera que vamos recorriendo @var{lista1(color)} cambiando el color de una c@'{e}lula por iteraci@'{o}n y
  anadi@'{e}ndola a @var{lista2(color)}. Finalmente en la @'{u}ltima iteraci@'{o}n se comprueba que @var{lista1(color)} finaliza en @var{o}, se cambian
   las dos @'{u}ltimas c@'{e}lulas y se anaden con una @var{o} adicional a @var{lista2(color)}.
 
@includedef{cells2/3}".

cells2([A,B,o],X,[Y,Z,o]) :- rule(A,B,o,X,Y), rule(B,o,o,X,Z).
cells2([A,B,C|Resto],X,[Y|RestoC]) :- rule(A,B,C,X,Y), cells2([B,C|Resto],X,RestoC).

:- pred evol(N,R,lista(color))
#"@var{N} es el n@'{u}mero de pasos de evoluci@'{o}n que han tenido que realizarse para llegar a @var{lista(color)} desde el estado inicial @var{[o,x,o]}, 
siguiendo el conjunto de reglas @var{R}. El paso @var{0} 
siempre ser@'{a} la cadena inicial @var{[o,x,o]}. Si tenemos @pred{evol} con @var{N} pasos y una cadena @var{Resto}, para que se cumpla el siguiente
 paso @var{s(N)}, @var{lista(color)} de dicho paso ser@'{a} el resultado de aplicar @pred{cells} a @var{Resto}, con la misma regla @var{R} que en el 
 @pred{evol} inicial.

 
 @includedef{evol/3}".

evol(0,_,[o,x,o]).
evol(s(A),X,Sol) :- evol(A,X,Resto),cells(Resto,X,Sol).

:- pred evol2(N,R,lista(color))
#"@var{N} es el n@'{u}mero de pasos de evoluci@'{o}n desde un estado inicial
 cualquiera, siguiendo el conjunto de reglas @var{R}. Es una funcion auxiliar que se usara en @pred{steps}, id@'{e}ntica a @pred{evol} pero sin 
 restricciones sobre la lista de la que parten @var{lista(color)}, pudiendo ser distinta de @var{[o,x,o]}.
 
@includedef{evol2/3}".

evol2(0,_,[_,_,_]).
evol2(s(A),X,Sol) :- cells(Resto,X,Sol),evol2(A,X,Resto).

:- pred steps(lista(color),N)
#"@var{N} es el n@'{u}mero de pasos de evoluci@'{o}n que se tienen que llevar a cabo para poder llegar a @var{lista(color)} partiendo de una lista inicial 
de tres elementos. Si @pred{evol2} indica que hay una lista @var{lista(color)} a la que se ha llegado con @var{N} iteraciones independientemente del conjuto de reglas que 
hayan sido ultilizadas, @pred{steps} ser@'{a} cierto.
 
@includedef{steps/2}".

steps(Cells, N) :- evol2(N,_,Cells).

:- pred ruleset(R,lista(color))
#"@var{R} es el conjunto de reglas que se tienen que seguir para poder llegar a @var{lista(color)} partiendo de una lista inicial 
@var{[o,x,o]}. Primero comprobamos si se puede llegar a @var{lista(color)} con una cantidad de pasos arbitraria @var{N} mediante @pred{steps} y despu@'{e}s se
comprueba mediante @pred{evol} si se puede llegar a @var{lista(color)} con @var{N} pasos usando las reglas @var{R} proporcionadas en @pred{ruleset}.

 
@includedef{ruleset/2}".

ruleset(X,Cells) :- steps(Cells, N),evol(N,X,Cells).
