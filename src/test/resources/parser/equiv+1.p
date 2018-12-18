fof(subset_union,axiom,
    ( ! [X,Y] :
        ( subset(X,Y)
       => union(X,Y) = Y ) )).


% {(~subset/2(∀x_0,∀x_1))} v {union/2(∀x_0,∀x_1) = ∃c_0::∃c_0 = ∀x_1}
