%----Problem axioms

fof(ax1,axiom,
    ( ! [X] :
        ( sth(X)
        | X = a))).

fof(ax2,axiom,
    ( ~ sth(b) )).

fof(ax3,axiom,
    ( what(a) )).

fof(conj,conjecture,
    ( what(b))).
