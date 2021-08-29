-module(bingo_display).
-include("bingo_game.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([display/1]).

-define(SAD_FACE_BIN, <<240,159,153,129>>).
-define(STAR_BIN, <<226,173,144>>).

%% Display game in a nice format.
-spec display(bingo_game()) -> ok.
display(#bingo_game{squares = Squares, scores = Scores} = Game) ->
    print_squares(Squares),
    print_scores(Scores),
    print_bingo(Game).

%% Print 2d grid of squares.
-spec print_squares([[bingo_square()]]) -> ok.
print_squares(Squares) ->
    io:format("~n", []),
    ColWidth = column_width(Squares),
    Fun = fun (Row) -> print_row(Row, ColWidth) end,
    lists:foreach(Fun, Squares),
    print_row_divider(length(Squares), ColWidth).

%% Print each row.
-spec print_row([bingo_square()], integer()) -> ok.
print_row(Row, ColWidth) ->
    MapFun = fun(Square) ->
                     Text = text_in_square(Square),
                     string:pad(Text, ColWidth, trailing)
             end,
    Texts = lists:map(MapFun, Row),
    RowString = lists:join(" | ", Texts),
    print_row_divider(length(Row), ColWidth),
    io:format("| ~ts |~n", [RowString]).

%% Print row divider.
-spec print_row_divider(integer(), integer()) -> ok.
print_row_divider(NumCol, ColWidth) ->
    Cells = [lists:duplicate(ColWidth, $-) || _ <- lists:seq(1, NumCol)],
    Row = lists:join("-+-", Cells),
    io:format("+-~s-+~n", [Row]).

%% Print scores map.
-spec print_scores(bingo_scores()) -> ok.
print_scores(Scores) ->
    io:format("~n", []),
    io:format("~p~n", [Scores]).

%% Print bingo.
-spec print_bingo(bingo_game()) -> ok.
print_bingo(#bingo_game{winner = undefined}) ->
    io:format("~n", []),
    Message = [?SAD_FACE_BIN, " No Bingo (yet)"],
    io:format("~ts~n", [Message]);
print_bingo(#bingo_game{winner = Someone}) ->
    io:format("~n", []),
    Name = Someone#bingo_player.name,
    Message = [?STAR_BIN, " BINGO! ", Name, " wins!"],
    io:format("~ts~n", [Message]).

%% Takes a 2d grid (a square matrix) of bingo squares as argument and
%% returns the width of each column. The width is defined as follow:
%% it is the number of characters (returned by string:length) of the
%% square with the longest text (returned by text_in_square/1) in it.
-spec column_width([[bingo_square()]]) -> integer().
column_width(Squares) ->
    List = lists:flatten(Squares),
    Texts = [text_in_square(S) || S <- List],
    Lengths = [string:length(Text) || Text <- Texts],
    lists:max(Lengths).

%% Returns the text (to be displayed) in the specified bingo square.
-spec text_in_square(bingo_square()) -> string().
text_in_square(#bingo_square{phrase = Phrase, points = Points}) ->
    Phrase ++ " (" ++ integer_to_list(Points) ++ ")".

%%
%% TESTS.
%%

-ifdef(TEST).

-define(
   SQUARE(Phrase, Points),
   #bingo_square{phrase = Phrase, points = Points}
  ).

%% column_width tests

column_width_test_() ->
    Tests = [
             {[[?SQUARE("One", 1)]], 7},
             {
              [[?SQUARE("One", 1),                     ?SQUARE("three", 3)],
               [?SQUARE("Competitive Advantage", 255), ?SQUARE("ten", 10)]],
              27
             },
             {
              [[?SQUARE("one",1),   ?SQUARE("two",2),   ?SQUARE("three",3)],
               [?SQUARE("four",4),  ?SQUARE("five",5),  ?SQUARE("six",6)],
               [?SQUARE("seven",7), ?SQUARE("eight",8), ?SQUARE("nine",9)]],
              9
             }
            ],
    [?_assertEqual(Out, column_width(In)) || {In, Out} <- Tests].

%% text_in_square tests.

text_in_square_test_() ->
    Tests = [
             {?SQUARE("One", 1), "One (1)"},
             {?SQUARE("Customer-Driven", 450), "Customer-Driven (450)"},
             {
              ?SQUARE("Competitive Advantage", 255),
              "Competitive Advantage (255)"
             },
             {
              ?SQUARE("Proof Of Concept", 375),
              "Proof Of Concept (375)"
             }
            ],
    [?_assertEqual(Out, text_in_square(In)) || {In, Out} <- Tests].

-endif.
