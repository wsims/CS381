% Authors: Dan Lin (lintzu), Will Sims (simsw), Cameron Friel (frielc)
% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(Child, Parent) :- parent(Parent, Child).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(Mother) :- parent(Mother, _), female(Mother).
isFather(Father) :- parent(Father, _), male(Father).

% 3. Define a predicate `grandparent/2`.
grandparent(Grandparent, Grandchild) :- parent(Grandparent, Parent), parent(Parent, Grandchild).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(Sibling, Siblings) :- parent(Parent, Siblings), parent(Parent, Sibling), Sibling \= Siblings.

% 5. Define two predicates `brother/2` and `sister/2`.

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.


% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.


% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- child(X,P1), sibling(P1,P2), parent(P2,Y).

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,C), ancestor(C,Y).

% Extra credit: Define the predicate `related/2`.



%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.


% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.
