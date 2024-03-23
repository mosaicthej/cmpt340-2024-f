% It turns out that most family relationships can be defined 
% in terms of a few simple predicates.  
% Here is a set of predicates sufficient for many families:

% a) ever_married_to(person1, person2) % person1 is married to person2,
%   or was married to person2 at some point in time

% b) son_of(person1, person2)          
% person1 is a biological son of person2

% c) daughter_of(person1, person2) 
% person1 is a biological daughter of person2

% d) father_of(person1, person2)     
% person1 is the biological father of person2

% e) mother_of(person1, person2)   
% person1 is the biological mother of person2

% for above, assume that functor describes how P1 relates to P2.

% now, consider the royal family, this predicate will be helpful
% f) senior_royal(person) % person is a senior royal.


%1. [12 Points] 
% Represent the facts in the graph shown above using facts in Prolog, 
% limiting yourself to the six predicates listed above.

% ever_married_to_base(diana, charles).
% ever_married_to_base(william, catherine).
% ever_married_to_base(harry, meghan).
wife_of(dianna, charles).
wife_of(catherine, william).
wife_of(meghan, harry).
% reflexive closure.
% doing this would avoid infinite loops in the graph
ever_married_to(X,Y):- wife_of(X,Y).
ever_married_to(X,Y):- wife_of(Y,X).

son_of(william, diana). 
son_of(william, charles).
son_of(harry, diana). 
son_of(harry, charles).
son_of(george, william).
son_of(george, catherine).
son_of(louis, william).
son_of(louis, catherine).
son_of(archie, harry).
son_of(archie, meghan).

daughter_of(lilibet, harry).
daughter_of(lilibet, meghan).
daughter_of(charlotte, william).
daughter_of(charlotte, catherine).

mother_of(diana, william).
mother_of(diana, harry).
mother_of(catherine, george).
mother_of(catherine, charlotte).
mother_of(catherine, louis).
mother_of(meghan, archie).
mother_of(meghan, lilibet).

father_of(charles, william).
father_of(charles, harry).
father_of(william, george).
father_of(william, charlotte).
father_of(william, louis).
father_of(harry, archie).
father_of(harry, lilibet).

senior_royal(diana).
senior_royal(charles).
senior_royal(camilla).
senior_royal(william).
senior_royal(catherine).
senior_royal(george).
senior_royal(charlotte).
senior_royal(louis).
% 2. [63 Points] 
%     Write Prolog rules for the following additional relationships.  
%     rules should not rely on the correctness of other rules, are independent.  
%     rules should work correctly for more complex family relation structures 
%       represented using the same predicates; 
% 
%     they should not be that they only work only on this particular family tree.
%     (Note: You may need to use not, where not(p) says that p cannot be proved):
% 
%      a) aunt_of(person1, person2)                           
%      % person1 is aunt 
%      % (i.e., sister of parent or wife of sibling of parent) of person2
%      b) grandson_of(person1, person2)                   
%      % person1 is biological grandson of person2
%      c) parent_of(person1, person2)                        
%      % person1 is biological parent of person2
%      d) parents_of(person1, person2, person3)      
%      % person1 and person2 are biological parents of person3
%      e) grandparents_of(person1, person2, person3)       
%      % person1 and person2 are biological grandparents of person3     
%      f) sibling_of(person1, person2)                        
%      % person1 is biological sibling of person2
%      g) stepmother_of(person1, person2)              
%      % person1 is step mother of person2

% a)
% aunt_of(person1, person2)
% sister_of(sis, sibling).
child_of_a(Chl, Prnt) :- son_of(Chl, Prnt); daughter_of(Chl, Prnt).

sister_of(Sis, Sibling) :- 
  daughter_of(Sis, Prnt), 
  child_of_a(Sibling, Prnt), 
  Sis \= Sibling.

brother_of(Bro, Sibling) :- 
  son_of(Bro, Prnt), 
  child_of_a(Sibling, Prnt), 
  Bro \= Sibling.

aunt_of(Aunt, Nx):- %gender inclusive political correct nephew/niece/whatever
  sister_of(Aunt, Prnt), 
  child_of_a(Nx, Prnt).

aunt_of(Aunt, Nx):- 
  ever_married_to(Aunt, Uncle),
  brother_of(Uncle, Prnt),
  child_of_a(Nx, Prnt).

% b)
% grandson_of(person1, person2)
% person1 is biological grandson of person2
child_of_b(Chl, Prnt) :- son_of(Chl, Prnt); daughter_of(Chl, Prnt).
grandson_of(Gson, GPrnt) :- 
  son_of(Gson, Prnt), 
  child_of_b(Prnt, GPrnt).

% c)
% parent_of(person1, person2)
% person1 is biological parent of person2
parent_of(Prnt, Chl) :- son_of(Chl, Prnt); daughter_of(Chl, Prnt).

% d)
% parents_of(person1, person2, person3)
% person1 and person2 are biological parents of person3
parents_of(Fa, Ma, C) :- father_of(Fa, C), mother_of(Ma, C).
parents_of(Ma, Fa, C) :- father_of(Fa, C), mother_of(Ma, C).

% e)
% grandparents_of(person1, person2, person3)
% person1 and person2 are biological grandparents of person3
parents_of_e(Fa, Ma, C) :- father_of(Fa, C), mother_of(Ma, C).
parents_of_e(Ma, Fa, C) :- father_of(Fa, C), mother_of(Ma, C).

grandparents_of(GFa, GMa, GChl) :- 
  parents_of_e(GFa, GMa, Prnt), 
  parent_of(Prnt, GChl).


% f)
% sibling_of(person1, person2)
% person1 is biological sibling of person2
child_of_f(Chl, Prnt) :- son_of(Chl, Prnt); daughter_of(Chl, Prnt).
siblings_of(A, B) :- 
  child_of_f(A, Prnt), 
  child_of_f(B, Prnt), 
  A \= B.

% g)
% stepmother_of(person1, person2)
% person1 is step mother of person2
stepmother_of(StM, Chl) :- 
  ever_married_to(StM, Prnt), 
  parent_of(Prnt, Chl), 
  not(mother_of(StM, Chl)).

