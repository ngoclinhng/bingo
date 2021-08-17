-module(bingo_game).

-include("bingo_game.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, new/2, mark_phrase/3]).

-type buzzwords() :: [bingo_buzzwords:buzzword()].

%%-------------------------------------------------------------------------
%% Function: new(Size) -> BingoGame
%%
%% Size = pos_integer()
%%   The size of the game's grid. For example, if the Size is 3, the
%%   game's grid would contain 3x3 bingo squares (9 squares in total).
%% BingoGame = bingo_game()
%%   A bingo game record.
%%
%% Description: Returns a new bingo game with a `Size` x `Size` collection
%%              of squares taken randomly from the list of buzzwords.
%%-------------------------------------------------------------------------
-spec new(pos_integer()) -> bingo_game().
new(Size) when is_integer(Size), Size >= 3 ->
    Buzzwords = bingo_buzzwords:read_buzzwords(),
    new(Size, Buzzwords).

%%-------------------------------------------------------------------------
%% Function: new(Size, Buzzwords) -> BingoGame
%%
%% Size = pos_integer()
%%   The size of the game's grid. For example, if the Size is 3, the
%%   game's grid would contain 3x3 bingo squares (9 in total).
%% Buzzwords = [#{phrase => string(), points => integer()}]
%%   The list of buzzwords from which to create a bingo game. Note that,
%%   since a game of size `Size` would require exactly `Size times Size`
%%   buzzwords, therefore the length of the given list of buzzwords must
%%   be at least `Size times Size`.
%%
%% Description: Returns a new bingo game with a `Size` x `Size` collection
%%              of bingo squares taken randomly from the list of buzzwords.
%%-------------------------------------------------------------------------
-spec new(pos_integer(), buzzwords()) -> bingo_game().
new(Size, Buzzwords) when is_integer(Size) and is_list(Buzzwords) ->
    Shuffle = bingo_random:shuffle(Buzzwords),
    {ok, Squares} = make_squares(Size, Shuffle),
    #bingo_game{squares = Squares}.

%%-------------------------------------------------------------------------
%% Function mark_phrase(Phrase, Player, Game) -> NewGame.
%%
%% Game = NewGame = bingo_game()
%%   The before and after (mark) bingo game records.
%% Phrase = string()
%%   The phrase to mark.
%% Player = bingo_player()
%%   The bingo player record who request to mark.
%%
%% Description: Finds the square in the game's squares grid that has
%%              the given `Phrase`, and marks it for the given `Player`,
%%              updates the game's scores, and checks for a bingo!.
%%-------------------------------------------------------------------------
-spec mark_phrase(bingo_phrase(), bingo_player(), bingo_game())
                 -> bingo_game().
mark_phrase(Phrase, Player, Game) when
      is_record(Game, bingo_game),
      is_record(Player, bingo_player) ->
    UpdateSquares = update_squares_with_mark(Game, Phrase, Player),
    UpdateScores = update_scores(UpdateSquares),
    UpdateScores.

%%
%% HELPERS.
%%

%% Finds the square in the squares grid of the given `Game` that has
%% the given `Phrase`, and marks it for the given `Player`.
-spec update_squares_with_mark(
        bingo_game(),
        bingo_phrase(),
        bingo_player()
       ) -> bingo_game().
update_squares_with_mark(Game, Phrase, Player)
  when is_record(Game, bingo_game),
       is_record(Player, bingo_player) ->
    #bingo_game{squares = Grid} = Game,
    GridWithMark = mark_grid(Grid, Phrase, Player),
    Game#bingo_game{squares = GridWithMark}.

%% Updates the scores map for the given `Game`.
-spec update_scores(bingo_game()) -> bingo_game().
update_scores(#bingo_game{squares = Squares} = Game) ->
    List = lists:flatten(Squares),
    Scores = lists:foldl(fun score_reducer/2, #{}, List),
    Game#bingo_game{scores = Scores}.

%% Helper function to be used in `update_scores/1`.
-spec score_reducer(bingo_square(), bingo_scores()) -> bingo_scores().
score_reducer(
  #bingo_square{marked_by = undefined, points = _Points},
  ScoreAcc
 ) ->
    ScoreAcc;
score_reducer(
  #bingo_square{marked_by = Someone, points = Points},
  ScoreAcc
 ) ->
    Name = Someone#bingo_player.name,
    Fun = fun(OldPoints) -> OldPoints + Points end,
    maps:update_with(Name, Fun, Points, ScoreAcc).

%% Finds the square in the given 2D `Grid` that has the given `Phrase`,
%% and marks it for the given `Player`.
-spec mark_grid(bingo_squares(), bingo_phrase(), bingo_player()) ->
          bingo_squares().
mark_grid(Grid, Phrase, Player) ->
    List = lists:flatten(Grid),
    ListWithMark = [mark_square(S, Phrase, Player) || S <- List],
    {ok, GridWithMark} = chunk_every(length(Grid), ListWithMark),
    GridWithMark.

%% Marks the given (unmarked) `Square` for the given `Player`.
-spec mark_square(bingo_square(), bingo_phrase(), bingo_player()) ->
          bingo_square().
mark_square(
  #bingo_square{phrase = Phrase, marked_by = undefined} = Square,
  Phrase,
  Player
 ) when is_record(Player, bingo_player) ->
    Square#bingo_square{marked_by = Player};
mark_square(#bingo_square{} = Square, _, _) ->
    Square.

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

%% mark_square tests.

mark_unmarked_square_test() ->
    Square = bingo_square:new("Some Phrase", 1),
    Player = bingo_player:new("User", "green"),
    SquareWithMark = mark_square(Square, "Some Phrase", Player),
    Expected = #bingo_square{phrase = "Some Phrase",
                             points = 1,
                             marked_by = Player},
    ?assert(SquareWithMark =:= Expected).

mark_already_marked_square_test() ->
    Phrase = "Some Phrase",
    Square = bingo_square:new(Phrase, 1),
    P = bingo_player:new("Some Player", "black"),
    FirstMark = mark_square(Square, Phrase, P),
    SecondMark = mark_square(FirstMark, Phrase, P),
    Expected = #bingo_square{phrase = Phrase,
                             points = 1,
                             marked_by = P},
    ?assert(SecondMark =:= FirstMark),
    ?assert(SecondMark =:= Expected).

mark_someone_square_test() ->
    Phrase = "Some Phrase",
    Square = bingo_square:new(Phrase, 1),
    P1 = bingo_player:new("Player1", "red"),
    P2 = bingo_player:new("Player2", "green"),
    MarkedByP1 = mark_square(Square, Phrase, P1),
    MarkAgainByP2 = mark_square(MarkedByP1, Phrase, P2),
    Expected = #bingo_square{phrase = Phrase,
                             points = 1,
                             marked_by = P1},
    ?assert(MarkAgainByP2 =:= Expected),
    ?assert(MarkAgainByP2 =:= MarkedByP1).


mark_square_test_() ->
    Square = bingo_square:new("Some Phrase", 1),
    Player = bingo_player:new("User", "green"),
    [
     ?_assert(mark_square(Square, "Foo", Player) =:= Square),
     ?_assert(mark_square(Square, "", Player) =:= Square),
     ?_assert(mark_square(Square, "Some Other Phrase", Player) =:= Square)
    ].

%% make_squares tests.

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

%% chunk_every tests.

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
