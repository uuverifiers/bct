fof(equal_defn,axiom,
    ( ! [X,Y] :
        ( X = Y
      <=> ( subset(X,Y)
          & subset(Y,X) ) ) )).

% {∀x_0 != ∀x_1} v {subset/2(∀x_0,∀x_1)}
% {∀x_0 != ∀x_1} v {subset/2(∀x_1,∀x_0)}
% {(~subset/2(∀x_0,∀x_1))} v {(~subset/2(∀x_1,∀x_0))} v {∀x_0 = ∀x_1}
