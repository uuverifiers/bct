%----Problem axioms
fof(ax1,axiom,
    ( ! [X] :
        ( feather(X)
        | ~ bird(X)))).

fof(ax2,axiom,
    ( bird(polly) )).

fof(conj,negated_conjecture,
    ( ~ feather(polly))).
