-module(tictactoe).

-export([
    start/0,
    new_game/0,
    make_move/3,
    check_winner/1,
    is_board_full/1,
    get_valid_moves/1
]).

-define(ROWS, 3).
-define(COLS, 3).

%%% === Lancement du jeu interactif ===

start() ->
    Game = new_game(),
    play_turn(Game).

play_turn(GameState) ->
    print_board(maps:get(board, GameState)),
    Player = maps:get(current_player, GameState),
    io:format("~nJoueur ~p, entrez la ligne et la colonne (1-3) :~n", [Player]),
    {ok, [Row, Col]} = io:fread("> ", "~d ~d"),

    %% Vérification des valeurs saisies
    case {Row, Col} of
        {Row, Col} when Row >= 1 andalso Row =< 3, Col >= 1 andalso Col =< 3 ->
            case make_move(GameState, {Row, Col}, Player) of
                {ok, NewGameState} ->
                    case check_winner(NewGameState) of
                        {winner, Winner} ->
                            print_board(maps:get(board, NewGameState)),
                            io:format("~nLe joueur ~p a gagné !~n", [Winner]);
                        no_winner ->
                            case is_board_full(NewGameState) of
                                true ->
                                    print_board(maps:get(board, NewGameState)),
                                    io:format("~nMatch nul !~n");
                                false ->
                                    play_turn(NewGameState)
                            end
                    end;
                {error, Reason} ->
                    io:format("Erreur : ~p~n", [Reason]),
                    play_turn(GameState)
            end;
        _ ->
            io:format("Erreur : Vous devez entrer une valeur entre 1 et 3~n"),
            play_turn(GameState)
    end.

%%% === Logique du jeu ===

new_game() ->
    #{
        board => [
            [empty, empty, empty],
            [empty, empty, empty],
            [empty, empty, empty]
        ],
        current_player => x
    }.

make_move(GameState, {Row, Col}, Player) ->
    Board = maps:get(board, GameState),
    CurrentPlayer = maps:get(current_player, GameState),

    case CurrentPlayer of
        Player ->
            case get_cell(Board, Row, Col) of
                empty ->
                    NewBoard = set_cell(Board, Row, Col, Player),
                    NextPlayer = case Player of
                        x -> o;
                        o -> x
                    end,
                    {ok, GameState#{
                        board => NewBoard,
                        current_player => NextPlayer,
                        last_move => {Row, Col, Player}
                    }};
                _ ->
                    {error, cell_occupied}
            end;
        _ ->
            {error, not_your_turn}
    end.

get_cell(Board, Row, Col) ->
    lists:nth(Col, lists:nth(Row, Board)).

set_cell(Board, Row, Col, Value) ->
    RowList = lists:nth(Row, Board),
    NewRowList = lists:sublist(RowList, Col - 1) ++ [Value] ++ lists:nthtail(Col, RowList),
    lists:sublist(Board, Row - 1) ++ [NewRowList] ++ lists:nthtail(Row, Board).

check_winner(GameState) ->
    Board = maps:get(board, GameState),
    RowWinner = check_rows(Board),
    ColWinner = check_columns(Board),
    DiagWinner = check_diagonals(Board),
    case {RowWinner, ColWinner, DiagWinner} of
        {no_winner, no_winner, no_winner} -> no_winner;
        {Winner, _, _} when Winner /= no_winner -> {winner, Winner};
        {_, Winner, _} when Winner /= no_winner -> {winner, Winner};
        {_, _, Winner} when Winner /= no_winner -> {winner, Winner}
    end.

check_rows(Board) ->
    check_lines(Board).

check_columns(Board) ->
    Columns = [[lists:nth(Col, Row) || Row <- Board] || Col <- lists:seq(1, 3)],
    check_lines(Columns).

check_diagonals(Board) ->
    Diag1 = [lists:nth(I, lists:nth(I, Board)) || I <- lists:seq(1, 3)],
    Diag2 = [lists:nth(4 - I, lists:nth(I, Board)) || I <- lists:seq(1, 3)],
    check_lines([Diag1, Diag2]).

check_lines([]) -> no_winner;
check_lines([Line | Rest]) ->
    case Line of
        [x, x, x] -> x;
        [o, o, o] -> o;
        _ -> check_lines(Rest)
    end.

is_board_full(GameState) ->
    Board = maps:get(board, GameState),
    not lists:any(fun(Row) ->
        lists:any(fun(Cell) -> Cell == empty end, Row)
    end, Board).

get_valid_moves(GameState) ->
    Board = maps:get(board, GameState),
    [
        {Row, Col} ||
        Row <- lists:seq(1, 3),
        Col <- lists:seq(1, 3),
        get_cell(Board, Row, Col) == empty
    ].

%%% === Affichage du plateau ===

print_board(Board) ->
    io:format("~n"),
    lists:foreach(fun(Row) ->
        lists:foreach(fun(Cell) ->
            Symbol = case Cell of
                x -> "X";
                o -> "O";
                empty -> "."
            end,
            io:format(" ~s", [Symbol])
        end, Row),
        io:format("~n")
    end, Board).
