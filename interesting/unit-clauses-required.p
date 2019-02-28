%
%   //===================
%   ||   unit-clauses-required.p
%   \\===================
%
%   Demonstrates the necessity of adding unit-clauses.
%

fof(ax1, axiom,
    ( p(a) )).

fof(ax1, axiom,
    ( a = b )).

fof(ax1, axiom,
    ( p(c) )).

fof(co1,negated_conjecture,
    ( ~p(b) | ~p(c))).


