-module(puissance4).

-export([start/0]).

-define(ROWS, 6).
-define(COLS, 7).

%% Start the game
start() ->
    Board = create_board(?ROWS, ?COLS),
    game_loop(Board, red, 0).

%% Create an empty board
create_board(0, _) -> [];
create_board(Rows, Cols) ->
    [create_row(Cols) | create_board(Rows - 1, Cols)].

%% Create a row with empty cells
create_row(0) -> [];
create_row(N) -> [empty | create_row(N - 1)].

%% Main game loop
game_loop(Board, Player, MvCount) ->
    print_board(Board),
    io:format("~n~p joue. Choisis une colonne (0 à ~p): ", [Player, ?COLS - 1]),
    {ok, [Col]} = io:fread("", "~d"),

    %% Vérification des valeurs saisies
    case Col of
        Col when Col >= 0 andalso Col =< ?COLS - 1 ->
            case drop_piece(Board, Col, Player) of
                {ok, NewBoard} ->
                    case check_win(NewBoard, Player) of
                        true ->
                            print_board(NewBoard),
                            io:format("~p gagne !~n", [Player]);
                        false ->
                            case MvCount + 1 >= ?ROWS * ?COLS of
                                true ->
                                    print_board(NewBoard),
                                    io:format("Match nul !~n");
                                false ->
                                    NextPlayer = next_player(Player),
                                    game_loop(NewBoard, NextPlayer, MvCount + 1)
                            end
                    end;
                {error, Reason} ->
                    io:format("Erreur: ~p~n", [Reason]),
                    game_loop(Board, Player, MvCount)
            end;
        _ ->
            io:format("Erreur: La colonne doit être entre 0 et ~p.~n", [?COLS - 1]),
            game_loop(Board, Player, MvCount)
    end.

%% Drop a piece into the board
drop_piece(Board, Col, Player) ->
    Reversed = lists:reverse(Board),
    case drop_piece_from_bottom(Reversed, Col, Player, []) of
        {ok, NewReversed} -> {ok, lists:reverse(NewReversed)};
        Error -> Error
    end.

drop_piece_from_bottom([], _, _, _) ->
    {error, column_full};
drop_piece_from_bottom([Row | Rest], Col, Player, Acc) ->
    case nth(Col, Row) of
        empty ->
            NewRow = replace_at(Col, Player, Row),
            {ok, lists:reverse(Acc) ++ [NewRow | Rest]};
        _ ->
            drop_piece_from_bottom(Rest, Col, Player, [Row | Acc])
    end.

%% Get the nth element of a list
nth(0, [H|_]) -> H;
nth(N, [_|T]) -> nth(N - 1, T).

%% Replace an element at a specific index in a list
replace_at(0, V, [_|T]) -> [V | T];
replace_at(N, V, [H|T]) -> [H | replace_at(N - 1, V, T)].

%% Check if the current player has won
check_win(Board, Player) ->
    check_all(Board, Player, 0, Board).

check_all([], _, _, _) -> false;
check_all([Row | Rest], Player, R, Board) ->
    case check_row(Row, Player, R, 0, Board) of
        true -> true;
        false -> check_all(Rest, Player, R + 1, Board)
    end.

check_row([], _, _, _, _) -> false;
check_row([_ | T], Player, R, C, Board) ->
    case check_directions(Board, Player, R, C) of
        true -> true;
        false -> check_row(T, Player, R, C + 1, Board)
    end.

%% Check all directions for a win
check_directions(Board, Player, R, C) ->
    check_dirs(Board, Player, R, C, [{1, 0}, {0, 1}, {1, 1}, {1, -1}]).

check_dirs(_, _, _, _, []) -> false;
check_dirs(Board, Player, R, C, [{Dx, Dy} | Rest]) ->
    case check_dir(Board, Player, R, C, Dx, Dy, 0) of
        true -> true;
        false -> check_dirs(Board, Player, R, C, Rest)
    end.

%% Check a single direction for a win
check_dir(_, _, _, _, _, _, 4) -> true;
check_dir(Board, Player, R, C, Dx, Dy, Count) ->
    case get_cell(Board, R, C) of
        Player -> check_dir(Board, Player, R + Dy, C + Dx, Dx, Dy, Count + 1);
        _ -> false
    end.

%% Get the cell at a specific position
get_cell(_, R, C) when R < 0; C < 0; R >= ?ROWS; C >= ?COLS ->
    invalid;
get_cell(Board, R, C) ->
    get_row(Board, R, C, 0).

get_row([], _, _, _) -> invalid;
get_row([Row | _], R, C, R) -> nth(C, Row);
get_row([_ | T], R, C, I) -> get_row(T, R, C, I + 1).

%% Print the board
print_board(Board) ->
    io:format("~n    "),
    print_indices(0, ?COLS - 1),
    print_rows(Board, 0).

print_indices(N, Max) when N > Max -> io:format("~n");
print_indices(N, Max) ->
    io:format("~w ", [N]),
    print_indices(N + 1, Max).

print_rows([], _) -> ok;
print_rows([Row | Rest], N) ->
    io:format("~2w ", [N]),
    print_cells(Row),
    print_rows(Rest, N + 1).

print_cells([]) -> io:format("~n");
print_cells([H | T]) ->
    Symbol = case H of
        red -> "R";
        yellow -> "Y";
        empty -> ".";
        _ -> "?"
    end,
    io:format(" ~s", [Symbol]),
    print_cells(T).

%% Get the next player
next_player(red) -> yellow;
next_player(yellow) -> red.
