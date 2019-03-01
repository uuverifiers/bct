%
%   //===================
%   ||   need-constants.p
%   \\===================
%
%  This example shows that if we do not add global constants
%  it is impossible to find a solution.



fof(ax, ax1,
    ( ! [X] :
        ( p(f(d), X) ) )).

fof(co, conjecture,
    ( ? [X] :
        ( p(X, g(d)) ) )).

