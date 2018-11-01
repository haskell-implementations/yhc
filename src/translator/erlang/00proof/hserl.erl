-module (hserl).

-export ([hslist/1, force/1, mk_apply/3, identity/1, seq/2, errstub/1,
          floor/1, ceiling/1, atom_to_list/1, send/2, spawn/1,
          mkeapp/3, addarg/2, erlcall/1]).

%% ERL monad support: evaluate the first argument (implicitly) and
%% pass it to the second.

seq (A, B) -> force (A), force (B).

%% Force a Haskell-produced object to WHNF.

%% CAF forces by calling it.

force ({'@caf', M, F}) -> force ((M:F) ());

force ({'@ap', {'@caf', M, F}, Arity, Args}) -> 
  force ({'@ap', (M:F) (), Arity, Args});

%% Normal saturated application.

force ({'@ap', Func, Arity, Args}) when (Arity == length (Args)) -> 
  force (apply (force (Func), Args));

%% Partial application: should not happen when curried aliases are used:
%% report an error.

force (A = {'@ap', _Func, Arity, Args}) when (Arity > length (Args)) -> 
  erlang:error (partial_app, [A]);
  
%% Oversaturated application: it is expected that Func is a curried
%% alias of an existing n-ary function, n > 1, so one argument is taken
%% from A|Rgs, and Func is applied to it, yielding another function
%% application of the result to the remainder of Args. Arity should be 1.

force ({'@ap', Func, 1, [A|Rgs]}) when (length (Rgs) > 0) -> 
  Ap = apply (force(Func), [A]),
  force ({'@ap', Ap, 1, Rgs});

%% Catch this: oversaturated application of a n-ary function. Should not happen.

force (A = {'@ap', _Func, Arity, Args}) when (Arity > 1) and (Arity < length (Args)) ->
  erlang:error (oversaturated, [A]);


%% An Erlang list lazily converted to a Haskell list.

force ({'@lst', []}) -> {'@dt', '.EOL'};

force ({'@lst', [H|T]}) -> {'@dt', '.CONS', H, {'@lst', T}};

force (X) -> X.

%% Produce a thunk for later evaluation on Haskell side.

mk_apply ({'@ap', Func, Arity, Args0}, _, Args) -> {'@ap', Func, Arity, Args0 ++ Args} ;

mk_apply ({M, F}, Arity, Args) -> {'@ap', {M, F}, Arity, Args}.

%% Convert a Haskell list (@lst-tagged tuple, or .CONS/.EOL application)
%% to Erlang-consumable form. The list must be finite otherwise the function
%% would hang. List elements will be forced to their WHNF's as well (but hslist
%% will not implicitly be applied to them).

hslist ({'@lst', L}) -> L;

hslist ({'@dt', '.EOL'}) -> [];

hslist ({'@dt', '.CONS', H, T}) -> [force (H) | hslist (T)];

hslist ({'@ap', Func, Arity, Args}) -> hslist (force ({'@ap', force(Func), Arity, Args}));

hslist (X) -> throw ({"Not a Haskell list", X}).

%% Identity function: returns its result.

identity (X) -> X.

%% Math operations.
%% Source: http://www.trapexit.org/Floating_Point_Rounding

floor (X) ->
    T = trunc (X),
    case X - T < 0 of
        true -> T - 1;
        false -> T
    end.

ceiling (X) ->
    T = trunc (X),
    case X - T < 0 of
        true -> T;
        false -> T + 1
    end.

%% Atom to list with proper list wrapping.

atom_to_list (A) -> {'@lst', erlang:atom_to_list (A)}.

%% Wrapper for spawn. It takes a function from message type to Boolean.
%% It declares a local function which loops receiving messages and passing them
%% to the given function. If the function returns true, loop continues
%% otherwise exits.

loop (F) ->
  receive (X) ->
    K = force (F (X)),
    loop (K)
  end.

spawn ({'@dt', _, F}) -> 
  erlang:spawn (fun() -> loop (force(F)) end).

%% Wrapper for send.

send (Pid, Msg) -> 
  Pid ! Msg.

%% Stub function to generate error message with given contents.

errstub (S) -> erlang:error (wrong_context, hslist (S)).

%% Function to start building an Erlang function application.

mkeapp (M, F, A1) -> {{M, F}, [force(A1)]}.

%% Function to add an argument to an Erlang function application.

addarg ({{M, F}, Args}, An) -> {{M, F}, Args ++ [force(An)]}.

%% Function to perform the call of an Erlang function applied to arguments.

erlcall ({Func, Args}) -> apply (Func, Args).


