% Write Prolog queries to ask the following questions. 
% You may use all thirteen predicates above; 
% assume that you have correct solutions to Problems 1 and 2.  
% to remove duplicates of any results.
% do this:
% get_unique_list(+DupList, -UniqueList).
% precondition: UniqueList does not have any duplicates.
get_unique_list([], _).
get_unique_list([H|T], UniqueList) :- 
  \+  member(H, UniqueList), 
  get_unique_list(T, [H|UniqueList]).
get_unique_list([H|T], UniqueList) :- 
  member(H, UniqueList), 
  get_unique_list(T, UniqueList).

% a) Is Charles the step father of Catherine? [should respond with no]
sln_a:- 
    ever_married_to(charles, Mo),
    parent_of(Mo, catherine),
    not(father_of(charles, catherine)).
query_a(Res):-
  sln_a, Res = yes.
query_a(Res):-
  \+ sln_a, Res = no.


% b) Who are the grandchildren of Diana? 
% [should respond with George, Charlotte, Louis, Archie and Lilibet]
query_b(Chl):-
  {Chl}/(mother_of(diana, P),
  parent_of(P, Chl)).


% c) Who are the senior royal descendants of Charles? 
% [should respond with William, George, Charlotte and Louis]
descendant_of(D, A):-
  parent_of(A, D).

% transitive closure.
% using this would avoid infinite loop.
descendant_of(D, A):-
  parent_of(A, X),
  descendant_of(D, X).

sln_c(SR):-
  senior_royal(SR),
  descendant_of(SR, charles).

  
query_c(SR):-
  {SR}/(sln_c(SR)).

% d) Who are the biological parents of both William and Harry? 
% [should respond with Charles and Diana]
sln_d(P):-
  father_of(P,william); father_of(P, william);
  mother_of(P, william); mother_of(P, william).
query_d(SlnList):-
  findall(P, sln_d(P), DupL),
  get_unique_list(DupL, SlnList).

sln_e(S):- % who has a brother
{S}/(
  parent_of(P, S),
  son_of(B, P),
  S \= B).

query_e(BL):-
  findall(B, sln_e(B), DL),
  get_unique_list(DL, BL).

