fof(subset_union,axiom,
    ( ! [B,C] :
        ( subset(B,C)
       => union(B,C) = C ) )).



fof(union_defn,axiom,
    ( ! [B,C,D] :
        ( member(D,union(B,C))
      <=> ( member(D,B)
          | member(D,C) ) ) )).


fof(equal_defn,axiom,
    ( ! [B,C] :
        ( B = C
      <=> ( subset(B,C)
          & subset(C,B) ) ) )).

fof(commutativity_of_union,axiom,
    ( ! [B,C] : union(B,C) = union(C,B) )).

fof(subset_defn,axiom,
    ( ! [B,C] :
        ( subset(B,C)
      <=> ! [D] :
            ( member(D,B)
           => member(D,C) ) ) )).

fof(reflexivity_of_subset,axiom,
    ( ! [B] : subset(B,B) )).

fof(equal_member_defn,axiom,
    ( ! [B,C] :
        ( B = C
      <=> ! [D] :
            ( member(D,B)
          <=> member(D,C) ) ) )).

fof(prove_idempotency_of_union,conjecture,
    ( ! [B] : union(B,B) = B )).
