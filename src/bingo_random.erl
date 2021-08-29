-module(bingo_random).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([shuffle/1]).

%% Randomly shuffle the specified List according to uniform distribution.
-spec shuffle([T]) -> [T].
shuffle(List) ->
    shuffle(List, []).

-spec shuffle([T], [T]) -> [T].
shuffle([], State) ->
    State;
shuffle([E], State) ->
    shuffle([], [E | State]);
shuffle(List, State) ->
    N = rand:uniform(length(List)),
    {Next, Rest} = delete_at(N, List),
    shuffle(Rest, [Next | State]).

%% Deletes and returns the element at the specified position from the
%% given List.
-spec delete_at(pos_integer(), [T]) -> {T, [T]} | error.
delete_at(N, List) when is_integer(N), is_list(List) ->
    case is_valid_position(N, List) of
        false ->
            error;
        true ->
            {First, [Target|Second]} = lists:split(N - 1, List),
            {Target, First ++ Second}
    end;
delete_at(_, _) ->
    error.

%% Returns `true` if `N` is greater than or equal to 1 and less than or
%% equal to `length(List)`.
-spec is_valid_position(pos_integer(), list()) -> boolean().
is_valid_position(N, List) when is_integer(N), is_list(List) ->
    N >= 1 andalso N =< length(List);
is_valid_position(_,_) ->
    false.

%%
%% TESTS.
%%

-ifdef(TEST).

shuffle_test_() ->
    [?_assertEqual([], shuffle([])),
     ?_assertEqual([a], shuffle([a])),
     ?_assertEqual(2, length(shuffle([a, b]))),
     ?_assertEqual(3, length(shuffle([a, b, c])))
    ].

delete_at_test_() ->
    [?_assertEqual({a, [b, c]}, delete_at(1, [a, b, c])),
     ?_assertEqual({b, [a, c]}, delete_at(2, [a, b, c])),
     ?_assertEqual({c, [a, b]}, delete_at(3, [a, b, c])),

     ?_assertEqual({a, [b]}, delete_at(1, [a, b])),
     ?_assertEqual({b, [a]}, delete_at(2, [a, b])),

     ?_assertEqual({a, []}, delete_at(1, [a])),

     ?_assertEqual(error, delete_at(4, [a, b, c])),
     ?_assertEqual(error, delete_at(0, [a, b, c])),
     ?_assertEqual(error, delete_at(-1, [a, b, c])),

     ?_assertEqual(error, delete_at(3, [a, b])),
     ?_assertEqual(error, delete_at(0, [a, b])),
     ?_assertEqual(error, delete_at(-1, [a, b])),

     ?_assertEqual(error, delete_at(2, [a])),
     ?_assertEqual(error, delete_at(0, [a])),
     ?_assertEqual(error, delete_at(-1, [a])),

     ?_assertEqual(error, delete_at(1, [])),
     ?_assertEqual(error, delete_at(0, [])),
     ?_assertEqual(error, delete_at(-1, []))
    ].

is_valid_position_test_() ->
    [?_assert(is_valid_position(1, [a, b, c])),
     ?_assert(is_valid_position(2, [a, b, c])),
     ?_assert(is_valid_position(3, [a, b, c])),

     ?_assert(is_valid_position(1, [a])),

     ?_assertNot(is_valid_position(0, [a, b, c])),
     ?_assertNot(is_valid_position(4, [a, b, c])),
     ?_assertNot(is_valid_position(-1, [a, b, c])),

     ?_assertNot(is_valid_position(2, [a])),
     ?_assertNot(is_valid_position(0, [a])),
     ?_assertNot(is_valid_position(-1, [a])),

     ?_assertNot(is_valid_position(1, [])),
     ?_assertNot(is_valid_position(0, [])),
     ?_assertNot(is_valid_position(-1, []))
    ].
-endif.
