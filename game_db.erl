-module(game_db).


-export([
    start/0, stop/0, init/0,
    add_client/1, add_jeux/1, add_horaires/0, get_all_horaires/0, get_client/1, get_all_clients/0,
    get_jeux/1, get_horaires/1, field_index/1, update_client_field/3, update_date_derniereCo/1, get_client_jeu/1, get_client_horaire/1, get_client_jour/1,add_message/1
]).

-include("records.hrl").

start() ->
    case mnesia:system_info(schema_location) of
        undefined ->
            mnesia:create_schema([node()]);
        _ ->
            ok
    end,
    application:ensure_all_started(mnesia),
    mnesia:start(),
    init().

stop() ->
    mnesia:stop().

init() ->
    mnesia:create_table(client, [{attributes, record_info(fields, client)}]),
    mnesia:create_table(jeux, [{attributes, record_info(fields, jeux)}]),
    mnesia:create_table(horaires, [{attributes, record_info(fields, horaires)}]),
    add_horaires(),
    ok.

add_client(Client) ->
    mnesia:transaction(fun() ->
        mnesia:write(Client)
    end).

add_jeux(Jeux) ->
    mnesia:transaction(fun() ->
        mnesia:write(Jeux)
    end).

add_horaires() ->
    mnesia:transaction(fun() ->
        case mnesia:match_object(#horaires{id_horaire = '_', plage_horaire = '_', jour = '_', semaine = '_'}) of
            [] -> ajouter_creneaux_par_defaut();
            _ -> ok
        end
    end),
    ok.

get_all_horaires() ->
    mnesia:transaction(fun() ->
        mnesia:match_object(#horaires{_ = '_'})  % récupère tous les enregistrements horaires
    end).

get_client(IdJoueur) ->
    mnesia:transaction(fun() ->
        mnesia:read({client, IdJoueur})
    end).

get_all_clients() ->
    mnesia:transaction(fun() ->
        mnesia:match_object(#client{_ = '_'})
    end).

get_jeux(IdJeux) ->
    mnesia:transaction(fun() ->
        mnesia:read({jeux, IdJeux})
    end).

get_horaires(IdHoraire) ->
    mnesia:transaction(fun() ->
        mnesia:read({horaires, IdHoraire})
    end).


get_client_horaire(IdJoueur) ->
    case mnesia:transaction(fun() ->
        case mnesia:read({client, IdJoueur}) of
            [#client{num_reservation = IdHoraire}] ->
                case mnesia:read({horaires, IdHoraire}) of
                    [#horaires{plage_horaire = Plage}] ->
                        {ok, Plage};
                    [] ->
                        {error, horaire_not_found}
                end;
            [] ->
                {error, client_not_found}
        end
    end) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.





get_client_jeu(IdJoueur) ->
    case mnesia:transaction(fun() ->
        mnesia:read({client, IdJoueur})
    end) of
        {atomic, [#client{jeu_choisi = Jeu}]} ->
            {ok, Jeu};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.
    

get_client_jour(IdJoueur) ->
    case mnesia:transaction(fun() ->
        mnesia:read({client, IdJoueur})
    end) of
        {atomic, [#client{date_choisie_jeux = Date}]} ->
            {ok, Date};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.


% Position des champs dans le record #client
field_index(pseudo) -> 3;
field_index(prenom) -> 4;
field_index(nom) -> 5;
field_index(passwd) -> 6;
field_index(num_reservation) -> 8;
field_index(date_choisie_jeux) -> 9;
field_index(jeu_choisi) -> 10.

update_client_field(Id, Field, NewValue) ->
    mnesia:transaction(fun() ->
        case mnesia:read({client, Id}) of
            [Client] ->
                UpdatedClient = setelement(field_index(Field), Client, NewValue),
                mnesia:write(UpdatedClient);
            [] ->
                {error, not_found}
        end
    end).

update_date_derniereCo(Id) ->
    mnesia:transaction(fun() ->
        case mnesia:read({client, Id}) of
            [Client] ->
                Now = calendar:local_time(),
                Updated = Client#client{date_derniereCo = Now},
                mnesia:write(Updated);
            [] ->
                {error, not_found}
        end
    end).


add_message({message, Text, Sender, Timestamp}) ->
    % Format du message : "[2025-05-12] Player1: Hello, World!"
    Formatted = io_lib:format("[~p] ~p: ~s~n", [Timestamp, Sender, Text]),
    
    % Ouverture du fichier chat_log.txt en mode append
    case file:open("chat_log.txt", [append]) of
        {ok, File} ->
            % Écriture du message formaté dans le fichier
            ok = io:put_chars(File, Formatted),
            file:close(File),
            io:format("Message ajouté à la base de données: ~p~n", [{message, Text, Sender, Timestamp}]),
            ok;
        {error, Reason} ->
            io:format("Erreur lors de l'ouverture du fichier: ~p~n", [Reason]),
            {error, Reason}
    end.



ajouter_creneaux_par_defaut() ->
    %% Semaine 1
    mnesia:write(#horaires{id_horaire = 1, plage_horaire = "10h-12h", jour = "Lundi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 2, plage_horaire = "13h-16h", jour = "Lundi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 3, plage_horaire = "16h-18h", jour = "Lundi", semaine = "1"}),

    mnesia:write(#horaires{id_horaire = 4, plage_horaire = "10h-12h", jour = "Mardi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 5, plage_horaire = "14h-16h", jour = "Mardi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 6, plage_horaire = "16h-18h", jour = "Mardi", semaine = "1"}),

    mnesia:write(#horaires{id_horaire = 7, plage_horaire = "10h-12h", jour = "Mercredi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 8, plage_horaire = "14h-16h", jour = "Mercredi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 9, plage_horaire = "16h-18h", jour = "Mercredi", semaine = "1"}),

    mnesia:write(#horaires{id_horaire = 10, plage_horaire = "10h-12h", jour = "Jeudi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 11, plage_horaire = "14h-16h", jour = "Jeudi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 12, plage_horaire = "16h-18h", jour = "Jeudi", semaine = "1"}),

    mnesia:write(#horaires{id_horaire = 13, plage_horaire = "10h-12h", jour = "Vendredi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 14, plage_horaire = "14h-16h", jour = "Vendredi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 15, plage_horaire = "16h-18h", jour = "Vendredi", semaine = "1"}),

    mnesia:write(#horaires{id_horaire = 16, plage_horaire = "10h-12h", jour = "Samedi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 17, plage_horaire = "14h-16h", jour = "Samedi", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 18, plage_horaire = "16h-18h", jour = "Samedi", semaine = "1"}),

    mnesia:write(#horaires{id_horaire = 19, plage_horaire = "10h-12h", jour = "Dimanche", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 20, plage_horaire = "14h-16h", jour = "Dimanche", semaine = "1"}),
    mnesia:write(#horaires{id_horaire = 21, plage_horaire = "16h-18h", jour = "Dimanche", semaine = "1"}),

    %% Semaine 2
    mnesia:write(#horaires{id_horaire = 22, plage_horaire = "10h-12h", jour = "Lundi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 23, plage_horaire = "14h-16h", jour = "Lundi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 24, plage_horaire = "16h-18h", jour = "Lundi", semaine = "2"}),

    mnesia:write(#horaires{id_horaire = 25, plage_horaire = "10h-12h", jour = "Mardi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 26, plage_horaire = "14h-16h", jour = "Mardi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 27, plage_horaire = "16h-18h", jour = "Mardi", semaine = "2"}),

    mnesia:write(#horaires{id_horaire = 28, plage_horaire = "10h-12h", jour = "Mercredi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 29, plage_horaire = "14h-16h", jour = "Mercredi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 30, plage_horaire = "16h-18h", jour = "Mercredi", semaine = "2"}),

    mnesia:write(#horaires{id_horaire = 31, plage_horaire = "10h-12h", jour = "Jeudi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 32, plage_horaire = "14h-16h", jour = "Jeudi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 33, plage_horaire = "16h-18h", jour = "Jeudi", semaine = "2"}),

    mnesia:write(#horaires{id_horaire = 34, plage_horaire = "10h-12h", jour = "Vendredi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 35, plage_horaire = "14h-16h", jour = "Vendredi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 36, plage_horaire = "16h-18h", jour = "Vendredi", semaine = "2"}),

    mnesia:write(#horaires{id_horaire = 37, plage_horaire = "10h-12h", jour = "Samedi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 38, plage_horaire = "14h-16h", jour = "Samedi", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 39, plage_horaire = "16h-18h", jour = "Samedi", semaine = "2"}),

    mnesia:write(#horaires{id_horaire = 40, plage_horaire = "10h-12h", jour = "Dimanche", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 41, plage_horaire = "14h-16h", jour = "Dimanche", semaine = "2"}),
    mnesia:write(#horaires{id_horaire = 42, plage_horaire = "16h-18h", jour = "Dimanche", semaine = "2"}),

    io:format("Créneaux horaires ajoutés par défaut.~n").
