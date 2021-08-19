-module(bingo_checker).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("bingo_game.hrl").

-export([is_bingo/1]).

%%-------------------------------------------------------------------------
%% Function is_bingo(Squares) -> boolean().
%%
%% Squares = [[BingoSquare]]
%%   A square matrix where each entry is a bingo square.
%% BingoSquare = bingo_square()
%%   A bingo square record.
%%
%% Description: Returns `true` if all the squares on any rows, any columns,
%%              the main (left) diagonal, and the right diagonal are
%%              marked by the same player. Returns `false`, otherwise.
%%-------------------------------------------------------------------------
-spec is_bingo(bingo_squares()) -> boolean().
is_bingo(Squares) ->
    Sequences = bingo_sequences(Squares),
    lists:any(fun all_squares_marked_by_same_player/1, Sequences).

%%
%% HELPERS.
%%

%% Takes as input a square matrix and returns a lists of all rows,
%% all columns, plus the left and right diagonals.
-spec bingo_sequences([[T]]) -> [[T]].
bingo_sequences(Matrix) ->
    LDiag = left_diagonal(Matrix),
    RDiag = right_diagonal(Matrix),
    Matrix ++ transpose(Matrix) ++ [LDiag, RDiag].

%% Returns `true` if all bingo squares in the specified squares list
%% are marked by the exact same player. Returns `false`, otherwise.
-spec all_squares_marked_by_same_player([bingo_square()]) -> boolean().
all_squares_marked_by_same_player(
  [#bingo_square{marked_by = undefined} | _]
 ) ->
    false;
all_squares_marked_by_same_player(
  [#bingo_square{marked_by = Someone} | Rest]
 ) ->
    Pred = fun(S) -> S#bingo_square.marked_by =:= Someone end,
    lists:all(Pred, Rest).

%% Transpose a 2D matrix.
-spec transpose([[T]]) -> [[T]].
transpose(Matrix) ->
    transpose(Matrix, false).

-spec transpose([[T]], boolean()) -> [[T]].
transpose([List], false = _first_zipped) ->
    lists:map(fun(X) -> [X] end, List);
transpose([List], true = _first_zipped) ->
    List;
transpose([List1, List2], false = _first_zipped) ->
    CombineFun = fun(X, Y) -> [X, Y] end,
    lists:zipwith(CombineFun, List1, List2);
transpose([List1, List2], true = _first_zipped) ->
    CombineFun = fun(X, Y) -> X ++ [Y] end,
    lists:zipwith(CombineFun, List1, List2);
transpose([List1, List2, List3], false = _first_zipped) ->
    CombineFun = fun(X, Y, Z) -> [X, Y, Z] end,
    lists:zipwith3(CombineFun, List1, List2, List3);
transpose([List1, List2, List3], true = _first_zipped) ->
    CombineFun = fun(X, Y, Z) -> X ++ [Y, Z] end,
    lists:zipwith3(CombineFun, List1, List2, List3);
transpose([List1, List2, List3 | Rest], false = _first_zipped) ->
    CombineFun = fun(X, Y, Z) -> [X, Y, Z] end,
    Head = lists:zipwith3(CombineFun, List1, List2, List3),
    transpose([Head | Rest], true);
transpose([List1, List2, List3 | Rest], true = _first_zipped) ->
    CombineFun = fun(X, Y, Z) -> X ++ [Y, Z] end,
    Head = lists:zipwith3(CombineFun, List1, List2, List3),
    transpose([Head | Rest], true).

%% Returns the left (main) diagonal of the given square matrix.
-spec left_diagonal([[T]]) -> [T].
left_diagonal(Matrix) ->
    Size = length(Matrix),
    List = lists:flatten(Matrix),
    take_every(Size + 1, List).

%% Returns the right diagonal of the given square matrix.
%% The right diagonal starting from the top right corner and go all
%% the way to the bottom left corner of the matrix.
-spec right_diagonal([[T]]) -> [T].
right_diagonal(Matrix) ->
    Rotate = rotate_90_degrees(Matrix),
    left_diagonal(Rotate).

%% Returns a list of every `N` element in the given `List`, starting
%% with the first element.
-spec take_every(integer(), [T]) -> [T].
take_every(0, List) when is_list(List) ->
    [];
take_every(1, List) when is_list(List) ->
    List;
take_every(N, List) when is_list(List), is_integer(N), N >= 2 ->
    take_every(N, List, 1, []).

%% Helper function to used in `take_every/2`.
-spec take_every(integer(), [T], integer(), [T]) -> [T].
take_every(_N, [], _Run, Ret) ->
    lists:reverse(Ret);
take_every(N, [Next | Rest], 1, Ret) ->
    take_every(N, Rest, 2, [Next | Ret]);
take_every(N, [_|Rest], N, Ret) ->
    take_every(N, Rest, 1, Ret);
take_every(N, [_|Rest], Run, Ret) ->
    take_every(N, Rest, Run + 1, Ret).

%% Rotates the given square matrix 90 degrees counter-clockwise.
-spec rotate_90_degrees([[T]]) -> [[T]].
rotate_90_degrees(Matrix) ->
    Transpose = transpose(Matrix),
    lists:reverse(Transpose).

%%
%% TESTS.
%%

-ifdef(TEST).

-define(
   PLAYER(Name, Color),
   #bingo_player{name = Name, color = Color}
  ).

-define(
   SQUARE(Phrase, Points),
   #bingo_square{phrase = Phrase, points = Points}
).

-define(
   SQUARE(Phrase, Points, Player),
   #bingo_square{phrase = Phrase, points = Points, marked_by = Player}
  ).

%% is_bingo tests.

is_bingo_true_test_() ->
    P = ?PLAYER("Some Player", "green"),
    T = [
         [[?SQUARE("s1",1,P), ?SQUARE("s2",2,P), ?SQUARE("s3",3,P)],
          [?SQUARE("s4",4),   ?SQUARE("s5",5),   ?SQUARE("s6",6)],
          [?SQUARE("s7",7),   ?SQUARE("s8",8),   ?SQUARE("s9",9)]],

         [[?SQUARE("s1",1),   ?SQUARE("s2",2),   ?SQUARE("s3",3)],
          [?SQUARE("s4",4,P), ?SQUARE("s5",5,P), ?SQUARE("s6",6,P)],
          [?SQUARE("s7",7),   ?SQUARE("s8",8),   ?SQUARE("s9",9)]],

         [[?SQUARE("s1",1),   ?SQUARE("s2",2),   ?SQUARE("s3",3)],
          [?SQUARE("s4",4),   ?SQUARE("s5",5),   ?SQUARE("s6",6)],
          [?SQUARE("s7",7,P), ?SQUARE("s8",8,P), ?SQUARE("s9",9,P)]],

         [[?SQUARE("s1",1,P), ?SQUARE("s2",2),   ?SQUARE("s3",3)],
          [?SQUARE("s4",4,P), ?SQUARE("s5",5),   ?SQUARE("s6",6)],
          [?SQUARE("s7",7,P), ?SQUARE("s8",8),   ?SQUARE("s9",9)]],

         [[?SQUARE("s1",1),   ?SQUARE("s2",2,P), ?SQUARE("s3",3)],
          [?SQUARE("s4",4),   ?SQUARE("s5",5,P), ?SQUARE("s6",6)],
          [?SQUARE("s7",7),   ?SQUARE("s8",8,P), ?SQUARE("s9",9)]],

         [[?SQUARE("s1",1),   ?SQUARE("s2",2),   ?SQUARE("s3",3,P)],
          [?SQUARE("s4",4),   ?SQUARE("s5",5),   ?SQUARE("s6",6,P)],
          [?SQUARE("s7",7),   ?SQUARE("s8",8),   ?SQUARE("s9",9,P)]],

         [[?SQUARE("s1",1,P), ?SQUARE("s2",2),   ?SQUARE("s3",3)],
          [?SQUARE("s4",4),   ?SQUARE("s5",5,P), ?SQUARE("s6",6)],
          [?SQUARE("s7",7),   ?SQUARE("s8",8),   ?SQUARE("s9",9,P)]],

         [[?SQUARE("s1",1),   ?SQUARE("s2",2),   ?SQUARE("s3",3,P)],
          [?SQUARE("s4",4),   ?SQUARE("s5",5,P), ?SQUARE("s6",6)],
          [?SQUARE("s7",7,P), ?SQUARE("s8",8),   ?SQUARE("s9",9)]]
        ],
    [?_assert(is_bingo(In)) || In <- T].

is_bingo_false_test_() ->
    P1 = ?PLAYER("Player 1", "red"),
    P2 = ?PLAYER("Player 2", "blue"),
    T = [
         [[?SQUARE("s1",1),    ?SQUARE("s2",2),    ?SQUARE("s3",3)],
          [?SQUARE("s4",4),    ?SQUARE("s5",5),    ?SQUARE("s6",6)],
          [?SQUARE("s7",7),    ?SQUARE("s8",8),    ?SQUARE("s9",9)]],

         [[?SQUARE("s1",1,P1), ?SQUARE("s2",2,P1), ?SQUARE("s3",3,P2)],
          [?SQUARE("s4",4),    ?SQUARE("s5",5),    ?SQUARE("s6",6)],
          [?SQUARE("s7",7),    ?SQUARE("s8",8),    ?SQUARE("s9",9)]],

         [[?SQUARE("s1",1,P1), ?SQUARE("s2",2),    ?SQUARE("s3",3,P1)],
          [?SQUARE("s4",4),    ?SQUARE("s5",5,P2), ?SQUARE("s6",6)],
          [?SQUARE("s7",7),    ?SQUARE("s8",8),    ?SQUARE("s9",9)]]
        ],
    [?_assertNot(is_bingo(In)) || In <- T].

%% bingo_sequences tests.

bingo_sequences_test_() ->
    Tests = [
             {[[1]], [[1], [1], [1], [1]]},

             {
              [[1, 2],
               [3, 4]],

              [[1, 2],
               [3, 4],
               [1, 3],
               [2, 4],
               [1, 4],
               [2, 3]]
             },

             {
              [[1, 2, 3],
               [4, 5, 6],
               [7, 8, 9]],

              [[1, 2, 3],
               [4, 5, 6],
               [7, 8, 9],
               [1, 4, 7],
               [2, 5, 8],
               [3, 6, 9],
               [1, 5, 9],
               [3, 5, 7]]
             }
            ],
    [?_assert(bingo_sequences(In) =:= Out) || {In, Out} <- Tests].

%% left_diagonal tests.

left_diagonal_test_() ->
    Tests = [
             {[[1]], [1]},
             {
              [[1, 2],
               [3, 4]],
              [1, 4]
             },
             {
              [[1, 2, 3],
               [4, 5, 6],
               [7, 8, 9]],
              [1, 5, 9]
             },
             {
              [[a11, a12, a13, a14],
               [a21, a22, a23, a24],
               [a31, a32, a33, a34],
               [a41, a42, a43, a44]],
              [a11, a22, a33, a44]
             }
            ],
    [?_assert(left_diagonal(In) =:= Out) || {In, Out} <- Tests].

%% right_diagonal tests.

right_diagonal_test_() ->
    Tests = [
             {[[1]], [1]},

             {
              [[1, 2],
               [3, 4]],

              [2, 3]
             },

             {
              [[1, 2, 3],
               [4, 5, 6],
               [7, 8, 9]],

              [3, 5, 7]
             },

             {
              [[1,  2,  3,  4],
               [5,  6,  7,  8],
               [9,  10, 11, 12],
               [13, 14, 15, 16]],

              [4, 7, 10, 13]
             }
            ],
    [?_assert(right_diagonal(In) =:= Out) || {In, Out} <- Tests].

%% rotate_90_degrees tests.

rotate_90_degrees_test_() ->
    Tests = [
             {[[1]], [[1]]},

             {
              [[1, 2],
               [3, 4]],

              [[2, 4],
               [1, 3]]
             },

             {
              [[1, 2, 3],
               [4, 5, 6],
               [7, 8, 9]],

              [[3, 6, 9],
               [2, 5, 8],
               [1, 4, 7]]
             },

             {
              [[1,  2,  3,  4],
               [5,  6,  7,  8],
               [9,  10, 11, 12],
               [13, 14, 15, 16]],

              [[4, 8, 12, 16],
               [3, 7, 11, 15],
               [2, 6, 10, 14],
               [1, 5, 9, 13]]
             }
            ],
    [?_assert(rotate_90_degrees(In) =:= Out) || {In, Out} <- Tests].

%% take_every tests.

take_every_test_() ->
    Tests = [
             {0, [], []},
             {0, [1], []},
             {0, [1, 2], []},
             {0, [1, 2, 3], []},
             {0, lists:seq(1, 10), []},

             {1, [], []},
             {1, [1], [1]},
             {1, [1, 2], [1, 2]},
             {1, [1, 2, 3], [1, 2, 3]},
             {1, lists:seq(1, 10), lists:seq(1, 10)},

             {2, [], []},
             {2, [1], [1]},
             {2, [1, 2], [1]},
             {2, [1, 2, 3], [1, 3]},
             {2, [1, 2, 3, 4], [1, 3]},
             {2, [1, 2, 3, 4, 5], [1, 3, 5]},

             {3, [], []},
             {3, [1], [1]},
             {3, [1, 2], [1]},
             {3, [1, 2, 3], [1]},
             {3, [1, 2, 3, 4], [1, 4]},
             {3, [1, 2, 3, 4, 5], [1, 4]},
             {3, [1, 2, 3, 4, 5, 6], [1, 4]},
             {3, [1, 2, 3, 4, 5, 6, 7], [1, 4, 7]},

             {2, lists:seq(1, 10), [1, 3, 5, 7, 9]},
             {3, lists:seq(1, 10), [1, 4, 7, 10]},
             {4, lists:seq(1, 10), [1, 5, 9]},
             {5, lists:seq(1, 10), [1, 6]},
             {6, lists:seq(1, 10), [1, 7]},
             {7, lists:seq(1, 10), [1, 8]},
             {8, lists:seq(1, 10), [1, 9]},
             {9, lists:seq(1, 10), [1, 10]},
             {10, lists:seq(1, 10), [1]},
             {11, lists:seq(1, 10), [1]},
             {12, lists:seq(1, 10), [1]},
             {15, lists:seq(1, 10), [1]},
             {2000, lists:seq(1, 10), [1]}
            ],
    [?_assert(take_every(N, In) =:= Out) || {N, In, Out} <- Tests].

%% all_squares_marked_by_same_player tests.

all_squares_marked_by_same_player_true_test_() ->
    P = ?PLAYER("Some Player", "green"),
    Tests = [
             [?SQUARE("one", 1, P)],
             [?SQUARE("one", 1, P), ?SQUARE("two", 2, P)],
             [
              ?SQUARE("one", 1, P),
              ?SQUARE("two", 2, P),
              ?SQUARE("three", 3, P)
             ],
             [
              ?SQUARE("one", 1, P),
              ?SQUARE("two", 2, P),
              ?SQUARE("three", 3, P),
              ?SQUARE("four", 4, P)
             ]
            ],
    [?_assert(all_squares_marked_by_same_player(L)) || L <- Tests].

all_squares_marked_by_same_player_false_test_() ->
    P1 = ?PLAYER("Player 1", "green"),
    P2 = ?PLAYER("Player 2", "red"),
    Tests = [
             [?SQUARE("one", 1)],

             [?SQUARE("one", 1), ?SQUARE("two", 2)],
             [?SQUARE("one", 1, P1), ?SQUARE("two", 2)],
             [?SQUARE("one", 1, P1), ?SQUARE("two", 2, P2)],

             [
              ?SQUARE("one", 1),
              ?SQUARE("two", 2),
              ?SQUARE("three", 3)
             ],
             [
              ?SQUARE("one", 1, P1),
              ?SQUARE("two", 2),
              ?SQUARE("three", 3)
             ],
             [
              ?SQUARE("one", 1, P1),
              ?SQUARE("two", 2, P1),
              ?SQUARE("three", 3)
             ],
             [
              ?SQUARE("one", 1, P1),
              ?SQUARE("two", 2, P1),
              ?SQUARE("three", 3, P2)
             ]
            ],
    [?_assertNot(all_squares_marked_by_same_player(L)) || L <- Tests].

%% transpose tests.

transpose_1xn_matrix_test_() ->
    Tests = [
             {[[10]], [[10]]},
             {[[-7, 9]], [[-7], [9]]},
             {[[0, 4, 100]], [[0], [4], [100]]},
             {[[a, b, c, d]], [[a], [b], [c], [d]]},
             {[[0, a, 10, b, 11]], [[0], [a], [10], [b], [11]]},

             {[["M11"]], [["M11"]]},
             {[["M11", "M12"]], [["M11"], ["M12"]]},
             {[["M11", "M12", "M13"]], [["M11"], ["M12"], ["M13"]]},
             {
              [["M11", "M12", "M13", "M14"]],
              [["M11"], ["M12"], ["M13"], ["M14"]]
             },
             {
              [["M11", "M12", "M13", "M14", "M15"]],
              [["M11"], ["M12"], ["M13"], ["M14"], ["M15"]]
             },
             {
              [["M11", "M12", "M13", "M14", "M15", "M16"]],
              [["M11"], ["M12"], ["M13"], ["M14"], ["M15"], ["M16"]]
             },
             {
              [["11", "12", "13", "14", "15", "16", "17"]],
              [["11"], ["12"], ["13"], ["14"], ["15"], ["16"], ["17"]]
             }
            ],
    T1 = [?_assert(transpose(In) =:= Out) || {In, Out} <- Tests],
    T2 = [?_assert(transpose(Out) =:= In) || {In, Out} <- Tests],
    T1 ++ T2.

transpose_2xn_matrix_test_() ->
    Tests = [
             {[[1], [2]], [[1, 2]]},
             {[[1, 2], [3, 4]], [[1, 3], [2, 4]]},
             {[[1, 2, 3], [4, 5, 6]], [[1, 4], [2, 5], [3, 6]]},
             {
              [[a, b, c, d], [1, 2, 3, 4]],
              [[a, 1], [b, 2], [c, 3], [d, 4]]
             }
            ],
    T1 = [?_assert(transpose(In) =:= Out) || {In, Out} <- Tests],
    T2 = [?_assert(transpose(Out) =:= In) || {In, Out} <- Tests],
    T1 ++ T2.

transpose_3xn_matrix_test_() ->
    Tests = [
             {[[1], [2], [3]], [[1, 2, 3]]},
             {[[1, 2], [3, 4], [5, 6]], [[1, 3, 5], [2, 4, 6]]},
             {
              [[1, 2, 3], [4, 5, 6], [7, 8, 9]],
              [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
             },
             {
              [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]],
              [[1, 5, 9], [2, 6, 10], [3, 7, 11], [4, 8, 12]]
             }
            ],
    T1 = [?_assert(transpose(In) =:= Out) || {In, Out} <- Tests],
    T2 = [?_assert(transpose(Out) =:= In) || {In, Out} <- Tests],
    T1 ++ T2.

transpose_4xn_matrix_test_() ->
    Tests = [
             {
              [[1], [2], [3], [4]],
              [[1, 2, 3, 4]]
             },
             {
              [[1, 2], [3, 4], [5, 6], [7, 8]],
              [[1, 3, 5, 7], [2, 4, 6, 8]]
             },
             {
              [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]],
              [[1, 4, 7, 10], [2, 5, 8, 11], [3, 6, 9, 12]]
             },
             {
              [[1, 2, 3, 4],
               [5, 6, 7, 8],
               [9, 10, 11, 12],
               [13, 14, 15, 16]],
              [[1, 5, 9, 13],
               [2, 6, 10, 14],
               [3, 7, 11, 15],
               [4, 8, 12, 16]]
             },
             {
              [[a11, a12, a13, a14, a15],
               [a21, a22, a23, a24, a25],
               [a31, a32, a33, a34, a35],
               [a41, a42, a43, a44, a45]],
              [[a11, a21, a31, a41],
               [a12, a22, a32, a42],
               [a13, a23, a33, a43],
               [a14, a24, a34, a44],
               [a15, a25, a35, a45]]
             }
            ],
    T1 = [?_assert(transpose(In) =:= Out) || {In, Out} <- Tests],
    T2 = [?_assert(transpose(Out) =:= In) || {In, Out} <- Tests],
    T1 ++ T2.

transpose_3x3_bingo_squares_test_() ->
    In = [
          [?SQUARE("one", 1), ?SQUARE("two", 2), ?SQUARE("three", 3)],
          [?SQUARE("four", 4), ?SQUARE("five", 5), ?SQUARE("six", 6)],
          [?SQUARE("seven", 7), ?SQUARE("eight", 8), ?SQUARE("nine", 9)]
         ],
    Out = [
           [?SQUARE("one", 1), ?SQUARE("four", 4), ?SQUARE("seven", 7)],
           [?SQUARE("two", 2), ?SQUARE("five", 5), ?SQUARE("eight", 8)],
           [?SQUARE("three", 3), ?SQUARE("six", 6), ?SQUARE("nine", 9)]
          ],
    [
     ?_assert(transpose(In) =:= Out),
     ?_assert(transpose(Out) =:= In)
    ].

-endif.
