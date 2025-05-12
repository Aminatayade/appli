%%%-------------------------------------------------------------------
%%% Module : chat
%%% Description : Module de gestion de chat (via Mnesia)
%%%-------------------------------------------------------------------

-module(chat).
-behaviour(gen_server).
-import(game_db, [add_message/1]).
-export([start_link/0, create_table/0]).
-export([register_client/2, unregister_client/1, send_message/2, get_history/1, get_connected_users/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(chat_message, {id, user, content, timestamp}).
-record(state, {clients = []}). % [{Username, Socket}]

%%% Public API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_table() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(chat_message, [
        {attributes, record_info(fields, chat_message)},
        {disc_copies, [node()]},
        {type, bag}
    ]).

register_client(Username, Socket) ->
    gen_server:cast(?MODULE, {register, Username, Socket}).

unregister_client(Socket) ->
    gen_server:cast(?MODULE, {unregister, Socket}).

send_message(User, Msg) ->
    gen_server:cast(?MODULE, {send_message, User, Msg}).

get_history(User) ->
    F = fun() ->
        mnesia:match_object(#chat_message{user = User, _ = '_'})
    end,
    case mnesia:transaction(F) of
        {atomic, Messages} -> Messages;
        _ -> []
    end.

get_connected_users() ->
    gen_server:call(?MODULE, get_users).

%%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_cast({register, Username, Socket}, State) ->
    {noreply, State#state{clients = [{Username, Socket} | State#state.clients]}};

handle_cast({unregister, Socket}, State) ->
    Filtered = lists:filter(fun({_, S}) -> S =/= Socket end, State#state.clients),
    {noreply, State#state{clients = Filtered}};

handle_cast({send_message, User, Msg}, State) ->
    Timestamp = calendar:universal_time(),
    ID = {User, Timestamp},
    Message = #chat_message{id = ID, user = User, content = Msg, timestamp = Timestamp},
    mnesia:transaction(fun() -> mnesia:write(Message) end),
    Formatted = io_lib:format("~s: ~s~n", [User, Msg]),
    lists:foreach(fun({_, Socket}) ->
        catch gen_tcp:send(Socket, Formatted)
    end, State#state.clients),
    {noreply, State}.

handle_call(get_users, _From, State) ->
    Usernames = [U || {U, _} <- State#state.clients],
    {reply, Usernames, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
send_message(Sender, Message, Timestamp) ->
    % Créer un tuple avec les informations nécessaires
    MessageData = {message, Message, Sender, Timestamp},

    % 1. Enregistrer le message dans la base de données
    game_db:add_message(MessageData),

    % 2. Afficher le message dans la console pour confirmation
    io:format("Message envoyé: ~p~n", [MessageData]),

    % 3. Diffuser le message à tous les autres clients connectés
    broadcast_message(MessageData),

    ok.
broadcast_message(Message) ->
    % Récupérer tous les PIDs des clients connectés (hypothèse : stockés dans un ETS ou une liste globale)
    case whereis(chat_server) of
        undefined ->
            io:format("Aucun serveur de chat trouvé.~n"),
            ok;
        Pid when is_pid(Pid) ->
            Pid ! {broadcast, Message},
            ok
    end.

