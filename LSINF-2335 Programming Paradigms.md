# LSINF2335 - Programming Paradigms
---
# Project 3 report 
Youri Mouton


## Question 1
>How did you deal with scopes, as discussed in the Implementation Hints
section. Give some details.

The unifier function has four arguments: the source query, the source environment, the destination query and the destination environmemnt. Each of these contain variable or atoms in their scope to unify between environments. The unifier function works as expected and is the prolog able to backtrack.

## Question 2
>Explain briefly the core loop of your program. Do not be scared by the
appellation core loop, which may end up referring to the major part of your
program. It's simply the part of the code that is repeatedly called to prove the
satisfiability of a goal and supply the appropriate bindings.

>In particular, we are interested in the values that change each iteration. If your
core loop is a recursion function, these would the successive values taken by
the function's parameters. Explain what these values represent and how they
change.

The macro `?-` calls the method query with variable argumetns which takes the raw query as a list and iterates over it. It finds the matching rules in the current query by the predicate name: rules are stored in the db as a seq where the key is the predicate name and it's arity and the the rules for that predicates as values. For example here's how to get the matching rules for `father/2`:

```scheme
=> (dosync (get @*db* '[father 1]))
#{((father youri))} ...
```

The rules returned by `findrules` are in the format of a `defrecord` that helps the main loop `interpret` to call `unify`. `interpret` is a method using `recur` to loop on a queue containing the query goals and an accumulator. Each result is placed on the accumulator allowing proper backtracking. 

The Goal record when running:

```scheme
=> (<- (father youri))
=> (?- (father X))
```

Looks like this:

```scheme
[#miniprolog.core.Goal{
	:head (father youri),
	:terms nil, :env {}, 
	:parent #miniprolog.core.Goal{
		:head (father X), 
		:terms [(father X)], 
		:env {}, 
		:parent nil}}
```

As you can see each goal has the predicate, an environment and a parent environment.

The aforementionned methods are heavily commented in the source code which helps since it is non trivial.