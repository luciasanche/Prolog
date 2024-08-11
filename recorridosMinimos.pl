:- module(_,_,[classic,assertions]).

:- doc(title, "Recorridos de valor m@'{i}nimo").

:- doc(author, "Luc@'{i}a S@'{a}nchez Bella, 20M081").

:- doc(module, "@section{Introducci@'{o}n} 

Se dispone de un tablero NxN donde cada casilla esta etiquetada con un operador aritmetico binario y un operando entero. Cada recorrido parte de una casilla inicial 
y el valor 0. Ademas se proporciona una lista de direcciones validas, por ejemplo, si la lista contiene (n,4) significa que se podra avanzar a la casilla superior (norte) 
un total de 4 veces en todo el recorrido. Para poder avanzar en una direccion, tiene que estar contenida en la lista. 

Una vez vas recorriendo el tablero se van aplicando las operaciones de las casillas por las que pasas al valor actual. El recorrido acaba cuando no quedan casillas (ya que solo se
puede recorrer cada casilla una unica vez).

En este modulo se definen las siguientes consultas:
@begin{enumerate}
@item Obtener todos los posibles recorridos dada una posicion inicial.
@item Obtener todos los posibles recorridos que dada una posicion inicial acaben en un valor especifico.
@item Obtener todos los posibles recorridos dado un valor final.
@item Obtener todos los posibles recorridos partiendo desde cualquier casilla.
@item Obtener el valor minimo posible partiendo de cualquier casilla y el numero de recorridos que acaban en dicho valor minimo.
@end{enumerate}

@section{Ejemplos de uso}

@begin{verbatim}
?- board2(Board), Dirs=[dir(n,4),dir(s,6),dir(e,7),dir(o,4)], generar_recorrido(pos(2,2),3,Board,Dirs,Recorrido,V).

Board = [cell(pos(1,1),op(+,1)),cell(pos(1,2),op(*,0)),cell(pos(1,3),op(-,7)),cell(pos(2,1),op(*,3)),cell(pos(2,2),op(-,2000)),cell(pos(2,3),op(+,133)),cell(pos(3,1),op(*,2)),cell(pos(3,2),op(*,97)),cell(pos(3,3),op(//,10))],
Dirs = [dir(n,4),dir(s,6),dir(e,7),dir(o,4)],
Recorrido = [(pos(2,2),-2000),(pos(1,2),0),(pos(1,3),-7),(pos(2,3),126),(pos(3,3),12),(pos(3,2),1164),(pos(3,1),2328),(pos(2,1),6984),(pos(1,1),6985)],
V = 6985 ? ;

Recorrido = [(pos(2,2),-2000),(pos(2,1),-6000),(pos(3,1),-12000),(pos(3,2),-1164000),(pos(3,3),-116400),(pos(2,3),-116267),(pos(1,3),-116274),(pos(1,2),0),(pos(1,1),1)],
V = 1 ?

yes
@end{verbatim}

@begin{verbatim}
?- board2(Board), Dirs=[dir(n,4),dir(s,6),dir(e,7),dir(o,4)], generar_recorrido(pos(2,2),3,Board,Dirs,Recorrido,12).

Board = [cell(pos(1,1),op(+,1)),cell(pos(1,2),op(*,0)),cell(pos(1,3),op(-,7)),cell(pos(2,1),op(*,3)),cell(pos(2,2),op(-,2000)),cell(pos(2,3),op(+,133)),cell(pos(3,1),op(*,2)),cell(pos(3,2),op(*,97)),cell(pos(3,3),op(//,10))],
Dirs = [dir(n,4),dir(s,6),dir(e,7),dir(o,4)],
Recorrido = [(pos(2,2),-2000),(pos(3,2),-194000),(pos(3,1),-388000),(pos(2,1),-1164000),(pos(1,1),-1163999),(pos(1,2),0),(pos(1,3),-7),(pos(2,3),126),(pos(3,3),12)] ?

yes
@end{verbatim}

@begin{verbatim}
?- board2(Board), Dirs=[dir(n,4),dir(s,6),dir(e,7),dir(o,4)], generar_recorridos(3,Board,Dirs,R,V).

Board = [cell(pos(1,1),op(+,1)),cell(pos(1,2),op(*,0)),cell(pos(1,3),op(-,7)),cell(pos(2,1),op(*,3)),cell(pos(2,2),op(-,2000)),cell(pos(2,3),op(+,133)),cell(pos(3,1),op(*,2)),cell(pos(3,2),op(*,97)),cell(pos(3,3),op(//,10))],
Dirs = [dir(n,4),dir(s,6),dir(e,7),dir(o,4)],
R = [(pos(1,1),1),(pos(2,1),3),(pos(2,2),-1997),(pos(1,2),0),(pos(1,3),-7),(pos(2,3),126),(pos(3,3),12),(pos(3,2),1164),(pos(3,1),2328)],
V = 2328 ? ;

Board = [cell(pos(1,1),op(+,1)),cell(pos(1,2),op(*,0)),cell(pos(1,3),op(-,7)),cell(pos(2,1),op(*,3)),cell(pos(2,2),op(-,2000)),cell(pos(2,3),op(+,133)),cell(pos(3,1),op(*,2)),cell(pos(3,2),op(*,97)),cell(pos(3,3),op(//,10))],
Dirs = [dir(n,4),dir(s,6),dir(e,7),dir(o,4)],
R = [(pos(1,1),1),(pos(2,1),3),(pos(3,1),6),(pos(3,2),582),(pos(2,2),-1418),(pos(1,2),0),(pos(1,3),-7),(pos(2,3),126),(pos(3,3),12)],
V = 12 ? ;

Board = [cell(pos(1,1),op(+,1)),cell(pos(1,2),op(*,0)),cell(pos(1,3),op(-,7)),cell(pos(2,1),op(*,3)),cell(pos(2,2),op(-,2000)),cell(pos(2,3),op(+,133)),cell(pos(3,1),op(*,2)),cell(pos(3,2),op(*,97)),cell(pos(3,3),op(//,10))],
Dirs = [dir(n,4),dir(s,6),dir(e,7),dir(o,4)],
R = [(pos(1,1),1),(pos(2,1),3),(pos(3,1),6),(pos(3,2),582),(pos(3,3),58),(pos(2,3),191),(pos(2,2),-1809),(pos(1,2),0),(pos(1,3),-7)],
V = -7 ?

yes
@end{verbatim}

@begin{verbatim}
?- board2(Board), Dirs=[dir(n,4),dir(s,6),dir(e,7),dir(o,4)], generar_recorridos(3,Board,Dirs,R,6).

Board = [cell(pos(1,1),op(+,1)),cell(pos(1,2),op(*,0)),cell(pos(1,3),op(-,7)),cell(pos(2,1),op(*,3)),cell(pos(2,2),op(-,2000)),cell(pos(2,3),op(+,133)),cell(pos(3,1),op(*,2)),cell(pos(3,2),op(*,97)),cell(pos(3,3),op(//,10))],
Dirs = [dir(n,4),dir(s,6),dir(e,7),dir(o,4)],
R = [(pos(1,3),-7),(pos(2,3),126),(pos(3,3),12),(pos(3,2),1164),(pos(2,2),-836),(pos(1,2),0),(pos(1,1),1),(pos(2,1),3),(pos(3,1),6)] ? ;

Board = [cell(pos(1,1),op(+,1)),cell(pos(1,2),op(*,0)),cell(pos(1,3),op(-,7)),cell(pos(2,1),op(*,3)),cell(pos(2,2),op(-,2000)),cell(pos(2,3),op(+,133)),cell(pos(3,1),op(*,2)),cell(pos(3,2),op(*,97)),cell(pos(3,3),op(//,10))],
Dirs = [dir(n,4),dir(s,6),dir(e,7),dir(o,4)],
R = [(pos(2,2),-2000),(pos(3,2),-194000),(pos(3,3),-19400),(pos(2,3),-19267),(pos(1,3),-19274),(pos(1,2),0),(pos(1,1),1),(pos(2,1),3),(pos(3,1),6)] ? ;

Board = [cell(pos(1,1),op(+,1)),cell(pos(1,2),op(*,0)),cell(pos(1,3),op(-,7)),cell(pos(2,1),op(*,3)),cell(pos(2,2),op(-,2000)),cell(pos(2,3),op(+,133)),cell(pos(3,1),op(*,2)),cell(pos(3,2),op(*,97)),cell(pos(3,3),op(//,10))],
Dirs = [dir(n,4),dir(s,6),dir(e,7),dir(o,4)],
R = [(pos(3,3),0),(pos(3,2),0),(pos(2,2),-2000),(pos(2,3),-1867),(pos(1,3),-1874),(pos(1,2),0),(pos(1,1),1),(pos(2,1),3),(pos(3,1),6)] ?

yes
@end{verbatim}

@begin{verbatim}
?- board2(Board), Dirs=[dir(n,4),dir(s,6),dir(e,7),dir(o,4)], tablero(3,Board,Dirs,VM,NR).

Board = [cell(pos(1,1),op(+,1)),cell(pos(1,2),op(*,0)),cell(pos(1,3),op(-,7)),cell(pos(2,1),op(*,3)),cell(pos(2,2),op(-,2000)),cell(pos(2,3),op(+,133)),cell(pos(3,1),op(*,2)),cell(pos(3,2),op(*,97)),cell(pos(3,3),op(//,10))],
Dirs = [dir(n,4),dir(s,6),dir(e,7),dir(o,4)],
NR = 1,
VM = -1163999 ?

yes

@end{verbatim}
").

:- pred author_data(nombre, ap1, ap2, mat)
#"@var{nombre} indica el nombre del autor, @var{ap1} indica el primer apellido, @var{ap2} indica el segundo apellido y @var{mat} 
indica el n@'{u}mero de matr@'{i}cula. @includedef{author_data/4}".

author_data('Sanchez','Bella','Lucia','20M081').

:- pred board1(Board)
#"Ejemplo 1 de tablero para realizar pruebas. @includedef{board1/1}".

board1([cell(pos(1 ,1),op(*,-3)),
cell(pos(1 ,2),op(-,1)),
cell(pos(1 ,3),op(-,4)),
cell(pos(1 ,4),op(- ,555)),
cell(pos(2 ,1),op(-,3)),
cell(pos(2 ,2),op(+ ,2000)),
cell(pos(2 ,3),op(* ,133)),
cell(pos(2 ,4),op(- ,444)),
cell(pos(3 ,1),op(*,0)),
cell(pos(3 ,2),op(* ,155)),
cell(pos(3 ,3),op(// ,2)),
cell(pos(3 ,4),op(+ ,20)),
cell(pos(4 ,1),op(-,2)),
cell(pos(4 ,2),op(- ,1000)),
cell(pos(4 ,3),op(-,9)),
cell(pos(4 ,4),op(*,4))]).

:- pred board2(Board)
#"Ejemplo 2 de tablero para realizar pruebas. @includedef{board2/1}".

board2([cell(pos(1 ,1),op(+,1)),
cell(pos(1 ,2),op(*,0)),
cell(pos(1 ,3),op(-,7)),
cell(pos(2 ,1),op(*,3)),
cell(pos(2 ,2),op(- ,2000)),
cell(pos(2 ,3),op(+ ,133)),
cell(pos(3 ,1),op(*,2)),
cell(pos(3 ,2),op(* ,97)),
cell(pos(3 ,3),op(//,10))]).

%board1(Board), Dirs=[dir(n,5),dir(s,6),dir(e,7),dir(o,4)], findall(Recorrido, generar_recorrido(pos(2,2),4,Board,Dirs,Recorrido,Valor),Recorridos),length(Recorridos, E).
%board1(Board), Dirs=[dir(n,5),dir(s,6),dir(e,7),dir(o,4)], generar_recorrido(pos(2,2),4,Board,Dirs,Recorrido,V).
%board1(Board), Dirs=[dir(n,5),dir(s,6),dir(e,7),dir(o,4)], findall(R, generar_recorridos(4,Board,Dirs,R,V),Recorridos),length(Recorridos, E).
%board1(Board), Dirs=[dir(n,5),dir(s,6),dir(e,7),dir(o,4)], generar_recorridos(4,Board,Dirs,R,V).
%board1(Board), Dirs=[dir(n,5),dir(s,6),dir(e,7),dir(o,4)], tablero(4,Board,Dirs,VM,NR).

%board2(Board), Dirs=[dir(n,4),dir(s,6),dir(e,7),dir(o,4)], generar_recorrido(pos(2,2),3,Board,Dirs,Recorrido,V).
%board2(Board), Dirs=[dir(n,4),dir(s,6),dir(e,7),dir(o,4)], generar_recorridos(3,Board,Dirs,R,V).
%board2(Board), Dirs=[dir(n,4),dir(s,6),dir(e,7),dir(o,4)], tablero(3,Board,Dirs,VM,NR).

:- pred efectuar_movimiento(pos(F1,C1), D, pos(F2,C2))
#"@var{pos(F1, C1)} indica la posicion inicial antes de efectuar el movimiento: @var{F1} indica el numero de fila del tablero y @var{C1} la columna 
@var{D} indica la direccion en la que se va a realizar el movimiento. Finalmente @var{pos(F2,C2)} indica la posicion final tras realizar el
movimiento, al igual que en la posicion inicial @var{F2} indica el numero de fila del tablero y @var{C2} la columna. @includedef{efectuar_movimiento/3}".

efectuar_movimiento(pos(F1,C1),D,pos(F2,C2)) :- 
    fila(D,A), F2 is F1 + A,
    columna(D,B), C2 is C1 + B.

:- pred fila(d,o)
#"@var{d} indica la direccion en la que se va a realizar el movimiento y @var{o} indica el incremento o decremento que se le va a realizar al numero 
de fila de la posicion inicial, por ejemplo si te diriges al norte te diriges a la fila superior, entonces
si estas en la fila 2 pasaras a la fila 1, analogamente si te diriges al sur te diriges a la fila inferior entoces pasaras a la fila 3.
Cuando te diriges al oeste o al este el numero de fila no cambia por su valor es 0.
Las direcciones posibles son norte (n), sur (s), este (e), oeste(o), noroeste (no), noreste (ne),
sureste (se) y suroeste (so). @includedef{fila/2}".

fila(n,-1).
fila(s,1).
fila(o,0).
fila(e,0).
fila(no,-1).
fila(ne,-1).
fila(se,1).
fila(so,1).

:- pred columna(d,o)
#"Es identico a @pred{fila/2}. @var{d} indica la direccion en la que se va a realizar el movimiento y @var{o} indica el incremento o decremento que se le va a realizar al numero 
de columna de la posicion inicial, por ejemplo si te diriges al este te diriges a la derecha, entonces
si estas en la columna 2 pasaras a la columna 3, analogamente si te diriges al oeste te diriges a la izquierda entoces pasaras a la columna 1.
Cuando te diriges al norte o al sur el numero de columna no cambia por su valor es 0.
Las direcciones posibles son norte (n), sur (s), este (e), oeste(o), noroeste (no), noreste (ne),
sureste (se) y suroeste (so). @includedef{columna/2}".

columna(s,0). 
columna(n,0).
columna(e,1).
columna(o,-1).
columna(no,-1).
columna(ne,1).
columna(so,-1).
columna(se,1).

:- pred movimiento_valido(N,pos(F,C), D)
#"Dado un tablero NxN, @var{pos(F,C)} indica la posicion inicial antes de efectuar el movimiento: @var{a} indica el 
numero de fila del tablero y @var{b} la columna y
@var{D} indica la direccion en la que se va a realizar el movimiento.
El predicado comprueba si al avanzar en la direccion @var{dir(D,n)} la posicion final @var{pos(F1,C1)} esta dentro de los limites del tablero. @includedef{movimiento_valido/3}".

movimiento_valido(N,pos(F,C),D) :-
    efectuar_movimiento(pos(F,C),D,pos(F1,C1)),
    F1 >= 1,
    F1 =< N,
    C1 >= 1,
    C1 =< N.

:- pred select_cell(IPos,Op,Board, NewBoard)
#"@var{Ipos} indica la posicion actual y @var{Op} indica la operacion de dicha posicion.
@var{Board} es el tablero inicial y @var{NewBoard} el tablero actualizado tras pasar por @var{Ipos}. El predicado elimina la posicion @var{Ipos} del tablero @var{Board}
para indicar que no se puede volver a pasar por ahi.
El tablero @var{NewBoard} es el tablero @var{Board} sin la casilla @var{cell(Ipos,Op)}. @includedef{select_cell/4}".

select_cell(IPos, Op, Board, NewBoard) :-
    select(cell(IPos, Op), Board, NewBoard).

:- pred select_dir(D,Dirs,NewDirs)
#"@var{D} es la ultima direccion tomada, @var{Dirs} indica la lista de direcciones validas y 
@var{NewDirs} indica la lista de direcciones validas actualizada. El predicado coge el elemento @var{D} de la lista @var{Dirs}, si @var{N} es 1 
significa que es la ultima vez que se puede tomar esa direccion y por lo tanto la borra de la lisa, si N es mayor que uno decrementa la N en una unidad y
la vuelve a meter en la lista de direcciones validas. @includedef{select_dir/3}".

select_dir(D,Dirs,NewDirs) :-
    member(dir(D,N),Dirs),
    (N is 1
        ->select(dir(D,N),Dirs,NewDirs)
        ;select(dir(D,N),Dirs,AuxDirs), append(AuxDirs, [dir(D,N1)], NewDirs),  N1 is N - 1
        
    ).

:- pred aplicar_op(op(S,N),Valor,Valor2)
#"@var{Valor2} es el resultado de aplicarle la operacion @var{op(S,N)} a @var{Valor} @includedef{aplicar_op/3}".

aplicar_op(op(S,N),Valor,Valor2) :- 
    Expr =.. [S,Valor,N],
    Valor2 is Expr.

:- pred generar_recorrido(Ipos,N,Board,DireccionesPermitidas,Recorrido,Valor)
#"@var{Ipos} es la posicion inicial a partir de la cual se va a realizar el recorrido, @var{N} es el tamano del tablero (NxN), @var{Board} es el tablero, @var{DireccionesPermitidas} 
es una lista con las direcciones que se pueden tomar y el numero de veces que se puede tomar cada una, @var{Recorrido} 
es un recorrido posible respetando todas las restricciones proporcionadas y @var{Valor} es el valor final de dicho recorrido. El predicado llama a una funcion 
auxiliar @pred{generar_recorrido_aux/10} que va sacando el recorrido recursivamente. @includedef{generar_recorrido/6}".

generar_recorrido(Ipos,N,Board,DireccionesPermitidas,Recorrido,Valor) :-
    generar_recorrido_aux(Ipos,N,Board,Board,DireccionesPermitidas,[],Recorrido,0,Valor).
    
:- pred generar_recorrido_aux(Ipos,N,Board1,Board,DireccionesPermitidas,TempRecorrido,Recorrido,TempValor,Valor)
#"@var{Ipos} es la posicion actual del recorrido, @var{N} es el tamano del tablero (NxN) y @var{Board} es una lista que contiene 
todas las casillas del tablero, mientras que @var{Board1} es una lista que contiene unicamente las casillas del tablero que no han sido visitadas.
@var{DireccionesPermitidas} es una lista con las direcciones permitidas y el numero de veces que se puede tomar cada una. @var{TempRecorrido} es el 
recorrido desde la posicion inicial hasta la posicion actual y @var{Recorrido} es el recorrido final desde la posicion inicial
hasta la ultima posicion posible, @var{TempValor} es el valor actual y @var{Valor} es el valor final al acabar todo el recorrido. 
El recorrido finaliza cuando no quedan casillas sin recorrer en el tablero.

 @includedef{generar_recorrido_aux/9}".    

generar_recorrido_aux(Pos2,_,[cell(Pos2,Op2)],_,_,TempRecorrido2,TempRecorrido3,NuevoTempValor,NuevoTempValor2) :-
      select_cell(Pos2,Op2,[cell(Pos2,Op2)],_),
      aplicar_op(Op2,NuevoTempValor,NuevoTempValor2),
      append(TempRecorrido2,[(Pos2, NuevoTempValor2)],TempRecorrido3).

generar_recorrido_aux(Ipos,N,Board1,Board,DireccionesPermitidas,TempRecorrido,Recorrido,TempValor,Valor) :-
    member(dir(D,_), DireccionesPermitidas),
    movimiento_valido(N,Ipos,D),
    efectuar_movimiento(Ipos,D,Pos2),
    member(cell(Pos2,_),Board1),
    select_cell(Ipos,Op,Board1,NewBoard),
    select_dir(D,DireccionesPermitidas, NewDirs),
    aplicar_op(Op,TempValor,NuevoTempValor),
    append(TempRecorrido,[(Ipos, NuevoTempValor)],TempRecorrido2),
    generar_recorrido_aux(Pos2,N,NewBoard,Board,NewDirs,TempRecorrido2,Recorrido,NuevoTempValor,Valor).

:- pred generar_recorridos(N,Board,DireccionesPermitidas,Recorrido,Valor)
#"Dado un tablero @var{Board} de dimension @var{NxN} y una lista @var{DireccionesPermitidas}, el predicado genera todos los recorridos @var{Recorrido} posibles
desde cualquier casilla de @var{Board}, indicando tambien el @var{Valor} de cada uno de ellos
. @includedef{generar_recorridos/5}".

generar_recorridos(N,Board,DireccionesPermitidas,Recorrido,Valor) :- 
        member(cell(pos(A,B),_),Board),
        generar_recorrido(pos(A,B),N,Board,DireccionesPermitidas,Recorrido,Valor).

:- pred tablero(N,Board,DireccionesPermitidas,ValorMinimo,NumeroDeRutas)
#"Dado un tablero @var{Board} de dimension @var{NxN} y una lista @var{DireccionesPermitidas}, el predicado encuentra cual es el valor minimo @var{ValorMinimo} que se puede 
obtener al realizar todos los recorridos posibles en ese tablero, para hallar ese valor minimo, primero encuentra todos los valores posibles con @pred{generar_recorridos/5}
y se queda con el que sea menor. Ademas, @var{NumeroDeRutas} indica el numero de recorridos que contienen ese valor minimo.
. @includedef{tablero/5}".

tablero(N,Board,DireccionesPermitidas,ValorMinimo,NumeroDeRutas) :-
    findall(Valor, (generar_recorridos(N,Board,DireccionesPermitidas,_,Valor)),Valores), min_list(Valores, ValorMinimo), findall(Min,(member(Min,Valores), Min=ValorMinimo), Minimos), length(Minimos,NumeroDeRutas).

:- pred min_list(List, X)
#"Encuentra el elemento con valor minimo @var{X} de la lista @var{List}.@includedef{min_list/2}".
min_list([X], X).
min_list([X | Xs], Min) :-
    min_list(Xs, MinRest),
    (X < MinRest, Min = X; X >= MinRest, Min = MinRest).


    