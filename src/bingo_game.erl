-module(bingo_game).

-include("bingo_game.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, new/2]).

-type buzzwords() :: [bingo_buzzwords:buzzword()].

%% Creates a game with a `Size` x `Size` collection of squares taken
%% randomly from the given list of `buzzwords` where each buzzword is of
%% the form `#{phrase => string(), points => integer()}`.
-spec new(pos_integer()) -> bingo_game().
new(Size) when is_integer(Size), Size >= 3 ->
    Buzzwords = bingo_buzzwords:read_buzzwords(),
    new(Size, Buzzwords).

%% Creates a game with a `Size` x `Size` collection of squares taken
%% randomly from the given list of `buzzwords` where each buzzword is of
%% the form `#{phrase => string(), points => integer()}`.
-spec new(pos_integer(), buzzwords()) -> bingo_game().
new(Size, Buzzwords) when is_integer(Size) and is_list(Buzzwords) ->
    Shuffle = bingo_random:shuffle(Buzzwords),
    {ok, Squares} = make_squares(Size, Shuffle),
    #bingo_game{squares = Squares}.

%% Returns a grid of `Size` x `Size` bingo squares from the given list
%% of buzzwords.
-spec make_squares(pos_integer(), buzzwords()) ->
          {ok, bingo_squares()} | {error, invalid_arguments}.
make_squares(Size, Buzzwords) when is_integer(Size), is_list(Buzzwords) ->
    case Size >= 1 andalso Size * Size =< length(Buzzwords) of
        true ->
            Sublist = lists:sublist(Buzzwords, Size * Size),
            List = [bingo_square:from_buzzword(B) || B <- Sublist],
            chunk_every(Size, List);
        false ->
            {error, invalid_arguments}
    end;
make_squares(_,_) ->
    {error, invalid_arguments}.

%% Chunk the given list `List` into each sublist of size `Size`.
%% Note that: the length of the specified list must be divisible by `Size`.
-spec chunk_every(pos_integer(), [T]) -> {ok, [[T]]} | {error, atom()}.
chunk_every(_Size, []) ->
    {error, invalid_arguments};
chunk_every(Size, List) when is_integer(Size), is_list(List) ->
    case Size >= 1 andalso length(List) rem Size =:= 0 of
        true ->
            {ok, chunk_every(Size, List, [])};
        false ->
            {error, invalid_arguments}
    end.

-spec chunk_every(pos_integer(), [T], [[T]]) -> [[T]].
chunk_every(_Size, [], State) ->
    lists:reverse(State);
chunk_every(Size, List, State) ->
    {Next,Rest} = lists:split(Size, List),
    chunk_every(Size, Rest, [Next | State]).


%%
%% TESTS.
%%

-ifdef(TEST).

buzzwords_list() ->
    [#{phrase => "one", points => 1},
     #{phrase => "two", points => 2},
     #{phrase => "three", points => 3},
     #{phrase => "four", points => 4},
     #{phrase => "five", points => 5},
     #{phrase => "six", points => 6},
     #{phrase => "seven", points => 7},
     #{phrase => "eight", points => 8},
     #{phrase => "nine", points => 9}
    ].

make_1x1_squares_test() ->
    Buzzwords = buzzwords_list(),
    Expected = [[#bingo_square{phrase = "one",
                               points = 1,
                               marked_by = undefined
                              }
                ]],
    {ok, Result} = make_squares(1, Buzzwords),
    ?assert(Result =:= Expected).

make_2x2_squares_test() ->
    Buzzwords = buzzwords_list(),
    Expected = [
                [#bingo_square{phrase = "one",
                               points = 1,
                               marked_by = undefined},
                 #bingo_square{phrase = "two",
                               points = 2,
                               marked_by = undefined}
                ],
                [#bingo_square{phrase = "three",
                               points = 3,
                               marked_by = undefined},
                 #bingo_square{phrase = "four",
                               points = 4,
                               marked_by = undefined}
                ]
               ],
    {ok, Result} = make_squares(2, Buzzwords),
    ?assert(Result =:= Expected).

make_3x3_squares_test() ->
    Buzzwords = buzzwords_list(),
    Expected = [
                [#bingo_square{phrase = "one",
                               points = 1,
                               marked_by = undefined
                              },
                 #bingo_square{phrase = "two",
                               points = 2,
                               marked_by = undefined
                              },
                 #bingo_square{phrase = "three",
                               points = 3,
                               marked_by = undefined
                              }
                ],
                [#bingo_square{phrase = "four",
                               points = 4,
                               marked_by = undefined
                              },
                 #bingo_square{phrase = "five",
                               points = 5,
                               marked_by = undefined
                              },
                 #bingo_square{phrase = "six",
                               points = 6,
                               marked_by = undefined
                              }
                ],
                [#bingo_square{phrase = "seven",
                               points = 7,
                               marked_by = undefined
                              },
                 #bingo_square{phrase = "eight",
                               points = 8,
                               marked_by = undefined
                              },
                 #bingo_square{phrase = "nine",
                               points = 9,
                               marked_by = undefined
                              }
                ]
               ],
    {ok, Result} = make_squares(3, Buzzwords),
    ?assert(Result =:= Expected).

make_squares_error_test_() ->
    Buzzwords = buzzwords_list(),
    [?_assert(make_squares(9, Buzzwords) =:= {error, invalid_arguments}),
     ?_assert(make_squares(8, Buzzwords) =:= {error, invalid_arguments}),
     ?_assert(make_squares(7, Buzzwords) =:= {error, invalid_arguments}),
     ?_assert(make_squares(6, Buzzwords) =:= {error, invalid_arguments}),
     ?_assert(make_squares(5, Buzzwords) =:= {error, invalid_arguments}),
     ?_assert(make_squares(4, Buzzwords) =:= {error, invalid_arguments}),
     ?_assert(make_squares(0, Buzzwords) =:= {error, invalid_arguments}),
     ?_assert(make_squares(-1, Buzzwords) =:= {error, invalid_arguments})
    ].

chunk_every_1_test() ->
    Expected = [[1], [2], [3]],
    {ok, Result} = chunk_every(1, lists:seq(1, 3)),
    ?assert(Result =:= Expected).

chunk_every_2_test() ->
    Expected = [[1, 2], [3, 4], [5, 6]],
    {ok, Result} = chunk_every(2, lists:seq(1, 6)),
    ?assert(Result =:= Expected).

chunk_every_3_test() ->
    Expected = [[1, 2, 3], [4, 5, 6], [7, 8, 9]],
    {ok, Result} = chunk_every(3, lists:seq(1,9)),
    ?assert(Expected =:= Result).

chunk_every_4_test() ->
    Expected = [[1, 2, 3, 4],
                [5, 6, 7, 8],
                [9, 10, 11, 12],
                [13, 14, 15, 16]],
    {ok, Result} = chunk_every(4, lists:seq(1,16)),
    ?assert(Expected =:= Result).

chunk_every_5_test() ->
    Expected = [[1, 2, 3, 4, 5],
                [6, 7, 8, 9, 10],
                [11, 12, 13, 14, 15],
                [16, 17, 18, 19, 20],
                [21, 22, 23, 24, 25]],
    {ok, Result} = chunk_every(5, lists:seq(1,25)),
    ?assert(Expected =:= Result).

chunk_every_error_test_() ->
    [?_assert(chunk_every(1, []) =:= {error, invalid_arguments}),
     ?_assert(chunk_every(0, []) =:= {error, invalid_arguments}),
     ?_assert(chunk_every(-1, []) =:= {error, invalid_arguments}),

     ?_assert(chunk_every(2, [1]) =:= {error, invalid_arguments}),
     ?_assert(chunk_every(0, [1]) =:= {error, invalid_arguments}),
     ?_assert(chunk_every(-1, [1]) =:= {error, invalid_arguments}),

     ?_assert(chunk_every(4, [1, 2]) =:= {error, invalid_arguments}),
     ?_assert(chunk_every(3, [1, 2]) =:= {error, invalid_arguments}),
     ?_assert(chunk_every(0, [1, 2]) =:= {error, invalid_arguments}),
     ?_assert(chunk_every(-1, [1, 2]) =:= {error, invalid_arguments}),

     ?_assert(chunk_every(8,lists:seq(1,9)) =:= {error,invalid_arguments}),
     ?_assert(chunk_every(7,lists:seq(1,9)) =:= {error,invalid_arguments}),
     ?_assert(chunk_every(6,lists:seq(1,9)) =:= {error,invalid_arguments}),
     ?_assert(chunk_every(5,lists:seq(1,9)) =:= {error,invalid_arguments}),
     ?_assert(chunk_every(4,lists:seq(1,9)) =:= {error,invalid_arguments}),
     ?_assert(chunk_every(2,lists:seq(1,9)) =:= {error,invalid_arguments}),
     ?_assert(chunk_every(0,lists:seq(1,9)) =:= {error,invalid_arguments}),
     ?_assert(chunk_every(-1,lists:seq(1,9)) =:= {error,invalid_arguments})
    ].

-endif.
