fof(union_defn,axiom,
    ( ! [X,Y,Z] :
        ( member(Z,union(X,Y))
      <=> ( member(Z,X)
          | member(Z,Y) ) ) )).

% {union/2(∀x_0,∀x_1) = ∃c_0::(~member/2(∀x_2,∃c_0))}, {member/2(∀x_2,∀x_0)}, {member/2(∀x_2,∀x_1)}
% {(~member/2(∀x_2,∀x_0))}, {union/2(∀x_0,∀x_1) = ∃c_1::member/2(∀x_2,∃c_1)}
% {(~member/2(∀x_2,∀x_1))}, {union/2(∀x_0,∀x_1) = ∃c_2::member/2(∀x_2,∃c_2)}
  
