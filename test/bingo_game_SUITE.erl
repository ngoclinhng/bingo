-module(bingo_game_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("bingo_game.hrl").

-compile(export_all).
-compile(nowarn_export_all).

%%
%% Helper macros.
%%

-define(VALUE(Key, Config), proplists:get_value(Key, Config)).

-define(
   ONE,
   #bingo_square{phrase = "one", points = 1, marked_by = undefined}
  ).
-define(
   ONE(P),
   #bingo_square{phrase = "one", points = 1, marked_by = P}
  ).

-define(
   TWO,
   #bingo_square{phrase = "two", points = 2, marked_by = undefined}
  ).
-define(
   TWO(P),
   #bingo_square{phrase = "two", points = 2, marked_by = P}
  ).

-define(
   THREE,
   #bingo_square{phrase = "three", points = 3, marked_by = undefined}
  ).
-define(
   THREE(P),
   #bingo_square{phrase = "three", points = 3, marked_by = P}
  ).

-define(
   FOUR,
   #bingo_square{phrase = "four", points = 4, marked_by = undefined}
  ).
-define(
   FOUR(P),
   #bingo_square{phrase = "four", points = 4, marked_by = P}
  ).

-define(
   FIVE,
   #bingo_square{phrase = "five", points = 5, marked_by = undefined}
  ).
-define(
   FIVE(P),
   #bingo_square{phrase = "five", points = 5, marked_by = P}
  ).

-define(
   SIX,
   #bingo_square{phrase = "six", points = 6, marked_by = undefined}
  ).
-define(
   SIX(P),
   #bingo_square{phrase = "six", points = 6, marked_by = P}
  ).

-define(
   SEVEN,
   #bingo_square{phrase = "seven", points = 7, marked_by = undefined}
  ).
-define(
   SEVEN(P),
   #bingo_square{phrase = "seven", points = 7, marked_by = P}
  ).

-define(
   EIGHT,
   #bingo_square{phrase = "eight", points = 8, marked_by = undefined}
  ).
-define(
   EIGHT(P),
   #bingo_square{phrase = "eight", points = 8, marked_by = P}
  ).

-define(
   NINE,
   #bingo_square{phrase = "nine", points = 9, marked_by = undefined}
  ).
-define(
   NINE(P),
   #bingo_square{phrase = "nine", points = 9, marked_by = P}
  ).

-define(
   ASSERT_SCORES_EQUAL(ExpectedScores, Game),
   true = (Game#bingo_game.scores =:= ExpectedScores)
  ).

-define(
   ASSERT_WINNER_EQUAL(ExpectedWinner, Game),
   true = (Game#bingo_game.winner =:= ExpectedWinner)
  ).

-define(
   ASSERT_SQUARES_EQUAL(ExpectedSquares, Game),
   true = (Game#bingo_game.squares =:= ExpectedSquares)
  ).

-define(PLAYER1_NAME, "Player1").
-define(PLAYER2_NAME, "Player2").

init_per_testcase(_TestCase, Config) ->
    Player1 = bingo_player:new(?PLAYER1_NAME, "green"),
    Player2 = bingo_player:new(?PLAYER2_NAME, "red"),
    Game = create_3x3_game(),
    [{player1, Player1}, {player2, Player2}, {game, Game} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
     before_any_mark,

     player1_mark_phrase_one,
     player1_mark_phrase_two,
     player1_mark_phrase_three,
     player1_mark_phrase_four,
     player1_mark_phrase_five,
     player1_mark_phrase_six,
     player1_mark_phrase_seven,
     player1_mark_phrase_eight,
     player1_mark_phrase_nine,
     player1_mark_unknown_phrase,

     play_till_bingo
    ].

%%
%% TEST CASES.
%%

before_any_mark(Config) ->
    Game = ?VALUE(game, Config),
    ?ASSERT_SCORES_EQUAL(#{}, Game),
    ?ASSERT_WINNER_EQUAL(undefined, Game),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE, ?TWO, ?THREE],
        [?FOUR, ?FIVE, ?SIX],
        [?SEVEN, ?EIGHT, ?NINE]
       ], Game).

player1_mark_phrase_one(Config) ->
    Game = ?VALUE(game, Config),
    P = ?VALUE(player1, Config),
    NewGame = bingo_game:mark_phrase("one", P, Game),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 1}, NewGame),
    ?ASSERT_WINNER_EQUAL(undefined, NewGame),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE(P), ?TWO, ?THREE],
        [?FOUR, ?FIVE, ?SIX],
        [?SEVEN, ?EIGHT, ?NINE]
       ], NewGame).

player1_mark_phrase_two(Config) ->
    Game = ?VALUE(game, Config),
    P = ?VALUE(player1, Config),
    NewGame = bingo_game:mark_phrase("two", P, Game),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 2}, NewGame),
    ?ASSERT_WINNER_EQUAL(undefined, NewGame),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE, ?TWO(P), ?THREE],
        [?FOUR, ?FIVE, ?SIX],
        [?SEVEN, ?EIGHT, ?NINE]
       ], NewGame).

player1_mark_phrase_three(Config) ->
    Game = ?VALUE(game, Config),
    P = ?VALUE(player1, Config),
    NewGame = bingo_game:mark_phrase("three", P, Game),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 3}, NewGame),
    ?ASSERT_WINNER_EQUAL(undefined, NewGame),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE, ?TWO, ?THREE(P)],
        [?FOUR, ?FIVE, ?SIX],
        [?SEVEN, ?EIGHT, ?NINE]
       ], NewGame).

player1_mark_phrase_four(Config) ->
    Game = ?VALUE(game, Config),
    P = ?VALUE(player1, Config),
    NewGame = bingo_game:mark_phrase("four", P, Game),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 4}, NewGame),
    ?ASSERT_WINNER_EQUAL(undefined, NewGame),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE, ?TWO, ?THREE],
        [?FOUR(P), ?FIVE, ?SIX],
        [?SEVEN, ?EIGHT, ?NINE]
       ], NewGame).

player1_mark_phrase_five(Config) ->
    Game = ?VALUE(game, Config),
    P = ?VALUE(player1, Config),
    NewGame = bingo_game:mark_phrase("five", P, Game),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 5}, NewGame),
    ?ASSERT_WINNER_EQUAL(undefined, NewGame),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE, ?TWO, ?THREE],
        [?FOUR, ?FIVE(P), ?SIX],
        [?SEVEN, ?EIGHT, ?NINE]
       ], NewGame).

player1_mark_phrase_six(Config) ->
    Game = ?VALUE(game, Config),
    P = ?VALUE(player1, Config),
    NewGame = bingo_game:mark_phrase("six", P, Game),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 6}, NewGame),
    ?ASSERT_WINNER_EQUAL(undefined, NewGame),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE, ?TWO, ?THREE],
        [?FOUR, ?FIVE, ?SIX(P)],
        [?SEVEN, ?EIGHT, ?NINE]
       ], NewGame).

player1_mark_phrase_seven(Config) ->
    Game = ?VALUE(game, Config),
    P = ?VALUE(player1, Config),
    NewGame = bingo_game:mark_phrase("seven", P, Game),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 7}, NewGame),
    ?ASSERT_WINNER_EQUAL(undefined, NewGame),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE, ?TWO, ?THREE],
        [?FOUR, ?FIVE, ?SIX],
        [?SEVEN(P), ?EIGHT, ?NINE]
       ], NewGame).

player1_mark_phrase_eight(Config) ->
    Game = ?VALUE(game, Config),
    P = ?VALUE(player1, Config),
    NewGame = bingo_game:mark_phrase("eight", P, Game),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 8}, NewGame),
    ?ASSERT_WINNER_EQUAL(undefined, NewGame),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE, ?TWO, ?THREE],
        [?FOUR, ?FIVE, ?SIX],
        [?SEVEN, ?EIGHT(P), ?NINE]
       ], NewGame).

player1_mark_phrase_nine(Config) ->
    Game = ?VALUE(game, Config),
    P = ?VALUE(player1, Config),
    NewGame = bingo_game:mark_phrase("nine", P, Game),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 9}, NewGame),
    ?ASSERT_WINNER_EQUAL(undefined, NewGame),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE, ?TWO, ?THREE],
        [?FOUR, ?FIVE, ?SIX],
        [?SEVEN, ?EIGHT, ?NINE(P)]
       ], NewGame).

player1_mark_unknown_phrase(Config) ->
    Game = ?VALUE(game, Config),
    P = ?VALUE(player1, Config),
    NewGame = bingo_game:mark_phrase("unknown", P, Game),
    ?ASSERT_SCORES_EQUAL(#{}, NewGame),
    ?ASSERT_WINNER_EQUAL(undefined, NewGame),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE, ?TWO, ?THREE],
        [?FOUR, ?FIVE, ?SIX],
        [?SEVEN, ?EIGHT, ?NINE]
       ], NewGame).

play_till_bingo(Config) ->
    %% Setup ------------------------------------------------
    Game = ?VALUE(game, Config),
    P1 = ?VALUE(player1, Config),
    P2 = ?VALUE(player2, Config),
    %% Round 1 ----------------------------------------------
    R11 = bingo_game:mark_phrase("one", P1, Game),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE(P1), ?TWO, ?THREE],
        [?FOUR, ?FIVE, ?SIX],
        [?SEVEN, ?EIGHT, ?NINE]
       ], R11),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 1}, R11),
    ?ASSERT_WINNER_EQUAL(undefined, R11),
    R12 = bingo_game:mark_phrase("four", P2, R11),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE(P1), ?TWO, ?THREE],
        [?FOUR(P2), ?FIVE, ?SIX],
        [?SEVEN, ?EIGHT, ?NINE]
       ], R12),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 1, ?PLAYER2_NAME => 4}, R12),
    ?ASSERT_WINNER_EQUAL(undefined, R12),
    %% Round 2 ----------------------------------------------
    R21 = bingo_game:mark_phrase("five", P1, R12),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE(P1), ?TWO, ?THREE],
        [?FOUR(P2), ?FIVE(P1), ?SIX],
        [?SEVEN, ?EIGHT, ?NINE]
       ], R21),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 6, ?PLAYER2_NAME => 4}, R21),
    ?ASSERT_WINNER_EQUAL(undefined, R21),
    R22 = bingo_game:mark_phrase("two", P2, R21),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE(P1), ?TWO(P2), ?THREE],
        [?FOUR(P2), ?FIVE(P1), ?SIX],
        [?SEVEN, ?EIGHT, ?NINE]
       ], R22),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 6, ?PLAYER2_NAME => 6}, R22),
    ?ASSERT_WINNER_EQUAL(undefined, R22),
    %% Round 3 ----------------------------------------------
    R31 = bingo_game:mark_phrase("six", P1, R22),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE(P1), ?TWO(P2), ?THREE],
        [?FOUR(P2), ?FIVE(P1), ?SIX(P1)],
        [?SEVEN, ?EIGHT, ?NINE]
       ], R31),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 12, ?PLAYER2_NAME => 6}, R31),
    ?ASSERT_WINNER_EQUAL(undefined, R31),
    R32 = bingo_game:mark_phrase("three", P2, R31),
    ?ASSERT_SQUARES_EQUAL(
       [
        [?ONE(P1), ?TWO(P2), ?THREE(P2)],
        [?FOUR(P2), ?FIVE(P1), ?SIX(P1)],
        [?SEVEN, ?EIGHT, ?NINE]
       ], R32),
    ?ASSERT_SCORES_EQUAL(#{?PLAYER1_NAME => 12, ?PLAYER2_NAME => 9}, R32),
    ?ASSERT_WINNER_EQUAL(undefined, R32).

%%
%% HELPERS.
%%

create_3x3_game() ->
    #bingo_game{squares = create_3x3_squares()}.

create_3x3_squares() ->
    [
     [?ONE, ?TWO, ?THREE],
     [?FOUR, ?FIVE, ?SIX],
     [?SEVEN, ?EIGHT, ?NINE]
    ].
