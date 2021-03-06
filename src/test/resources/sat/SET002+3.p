%------------------------------------------------------------------------------
% File     : SET002+3 : TPTP v7.2.0. Released v2.2.0.
% Domain   : Set Theory
% Problem  : Idempotency of union
% Version  : [Try90] axioms : Reduced > Incomplete.
% English  :

% Refs     : [ILF] The ILF Group (1998), The ILF System: A Tool for the Int
%          : [Try90] Trybulec (1990), Tarski Grothendieck Set Theory
%          : [TS89]  Trybulec & Swieczkowska (1989), Boolean Properties of
% Source   : [ILF]
% Names    : BOOLE (62) [TS89]

% Status   : Theorem
% Rating   : 0.00 v6.4.0, 0.04 v6.3.0, 0.00 v6.1.0, 0.07 v6.0.0, 0.04 v5.3.0, 0.11 v5.2.0, 0.00 v5.0.0, 0.04 v4.0.1, 0.09 v4.0.0, 0.08 v3.7.0, 0.05 v3.3.0, 0.07 v3.2.0, 0.09 v3.1.0, 0.11 v2.7.0, 0.00 v2.2.1
% Syntax   : Number of formulae    :    8 (   3 unit)
%            Number of atoms       :   17 (   5 equality)
%            Maximal formula depth :    6 (   4 average)
%            Number of connectives :    9 (   0 ~  ;   1  |;   1  &)
%                                         (   5 <=>;   2 =>;   0 <=)
%                                         (   0 <~>;   0 ~|;   0 ~&)
%            Number of predicates  :    3 (   0 propositional; 2-2 arity)
%            Number of functors    :    1 (   0 constant; 2-2 arity)
%            Number of variables   :   17 (   0 singleton;  17 !;   0 ?)
%            Maximal term depth    :    2 (   1 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments :
%------------------------------------------------------------------------------
%---- line(boole - th(35),1833266)
fof(subset_union,axiom,
    ( ! [B,C] :
        ( subset(B,C)
       => union(B,C) = C ) )).





%---- property(reflexivity,op(subset,2,predicate))
fof(reflexivity_of_subset,axiom,
    ( ! [B] : subset(B,B) )).


%---- line(boole - th(62),1833685)
fof(prove_idempotency_of_union,conjecture,
    ( ! [B] : union(B,B) = B )).

%------------------------------------------------------------------------------
