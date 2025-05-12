
%%%-------------------------------------------------------------------
%%% Module : playpal_server
%%% Description : Serveur TCP pour Playpal avec login + chat
%%%-------------------------------------------------------------------

-module(playpal_server).
-export([start/0, accept_loop/1, loop/2]).
-include("records.hrl").

-define(PORT, 5050).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [
        binary, {packet, 0},
        {active, false},
        {reuseaddr, true}
    ]),
    io:format("Serveur Playpal démarré sur le port ~p~n", [?PORT]),
    chat:start_link(),
    chat:create_table(),
    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() ->
        handle_new_client(Socket)
    end),
    accept_loop(ListenSocket).

handle_new_client(Socket) ->
    gen_tcp:send(Socket, "Entrez votre pseudo: "),
    case gen_tcp:recv(Socket, 0) of
        {ok, PseudoBin} ->
            Pseudo = string:trim(binary_to_list(PseudoBin)),
            chat:register_client(Pseudo, Socket),
            Welcome = io_lib:format("Bienvenue, ~s ! Tapez /list, /history ou /quit.\n", [Pseudo]),
            gen_tcp:send(Socket, lists:flatten(Welcome)),
            loop(Socket, Pseudo);
        _ ->
            gen_tcp:close(Socket)
    end.

loop(Socket, Username) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Line = string:trim(binary_to_list(Data)),
            case Line of
                "/quit" ->
                    gen_tcp:send(Socket, "Déconnexion...\n"),
                    chat:unregister_client(Socket),
                    gen_tcp:close(Socket);

                "/list" ->
                    Connected = chat:get_connected_users(),
                    Msg = "Joueurs connectés : " ++ string:join(Connected, ", ") ++ "\n",
                    gen_tcp:send(Socket, Msg),
                    loop(Socket, Username);

                "/history" ->
                    Messages = chat:get_history(Username),
                    lists:foreach(fun(Msg) ->
                        MsgText = io_lib:format("[~p] ~s: ~s\n",
                            [Msg#chat_message.timestamp,
                             Msg#chat_message.user,
                             Msg#chat_message.content]),
                        gen_tcp:send(Socket, lists:flatten(MsgText))
                    end, Messages),
                    loop(Socket, Username);

                _ ->
                    chat:send_message(Username, Line),
                    loop(Socket, Username)
            end;
        {error, closed} ->
            chat:unregister_client(Socket),
            ok
    end.

loop(Clients) ->
    receive
        {broadcast, Message} ->
            lists:foreach(fun(Client) -> 
                Client ! {new_message, Message} 
            end, Clients),
            loop(Clients);

        {join, Pid} ->
            io:format("Client ~p rejoint le chat~n", [Pid]),
            loop([Pid | Clients]);

        {quit, Pid} ->
            io:format("Client ~p quitte le chat~n", [Pid]),
            loop(lists:delete(Pid, Clients));

        _Other ->
            loop(Clients)
    end.
