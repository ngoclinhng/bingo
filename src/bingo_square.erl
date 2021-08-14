-module(bingo_square).
-include("bingo_square.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/2, from_buzzword/1]).

-type buzzword() :: #{phrase => string(), points => integer()}.

%% Creates a square from the given `phrase` and `points`.
-spec new(string(), integer()) -> bingo_square().
new(Phrase, Points) ->
    #bingo_square{phrase = Phrase, points = Points}.

%% Creates a square from the given map with `phrase` and `points` keys.
-spec from_buzzword(buzzword()) -> bingo_square().
from_buzzword(#{phrase := Phrase, points := Points}) ->
    new(Phrase, Points).

%%
%% TESTS.
%%

-ifdef(TEST).

create_upsell_square() ->
    Square = new("Upsell", 100),
    Expected = #bingo_square{
                  phrase = "Upsell",
                  points = 100,
                  marked_by = nil
                 },
    Square =:= Expected.

create_low_hanging_fruit_square() ->
    Square = new("Low hanging fruit", 400),
    Expected = #bingo_square{
                  phrase = "Low hanging fruit",
                  points = 400,
                  marked_by = nil
                 },
    Square =:= Expected.

create_from_buzzword() ->
    Square = from_buzzword(#{phrase => "Foo", points => 10}),
    Expected = #bingo_square{
                  phrase = "Foo",
                  points = 10,
                  marked_by = nil
                 },
    Square =:= Expected.

new_test_() ->
    [
     ?_assert(create_upsell_square()),
     ?_assert(create_low_hanging_fruit_square()),
     ?_assert(create_from_buzzword()),
     ?_assertException(error, function_clause, from_buzzword("foo"))
    ].

-endif.
