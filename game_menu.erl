-module(game_menu).

-export([start/0, menu/0, menu_jeu/1, create_client/0, afficher_horaires/1, afficher_tous_les_clients/0, afficher_tous_horaires/0, create_joueur_id/0,
    modifier_champ_client/1, champ_depuis_choix/1, mettre_a_jour_date_connexion/1, rechercher_client/3, jour_semaine_aujourdhui/0]).

-include("records.hrl").

start() ->
    game_db:start(),
    menu().

menu() ->
    io:format("~n--- MENU ---~n"),
    io:format("1. Créer un compte~n"),
    io:format("2. S'autentifier~n"),
    io:format("3. Afficher tous les clients~n"),
    io:format("4. Quitter~n"),
    case io:fread("Votre choix : ", "~d") of
        {ok, [1]} -> create_client(), menu();
        {ok, [2]} -> authentifier_joueur(), menu();
        {ok, [3]} -> afficher_tous_les_clients(), menu();
        {ok, [4]} -> io:format("Fermeture du programme. Au revoir !~n"), ok;
        _ -> io:format("Choix invalide. Réessayez.~n"), menu()
    end.

menu_jeu(IdJoueur) ->
    io:format("~n--- CHOIX DU JEU ---~n"),
    io:format("1. Puissance 4~n"),
    io:format("2. Tic-Tac-Toe~n"),
    io:format("3. Réserver un créneau~n"),
    io:format("4. Chat~n"),  % Nouvelle option
    io:format("5. Quitter~n"),
    case io:fread("Votre choix : ", "~d") of
        {ok, [1]} -> verif_p4(IdJoueur), menu_jeu(IdJoueur);
        {ok, [2]} -> verif_ttt(IdJoueur), menu_jeu(IdJoueur);
        {ok, [3]} -> naviguer_calendrier(IdJoueur);
        {ok, [4]} -> acceder_chat(IdJoueur);  % Accéder au chat
        {ok, [5]} -> io:format("Au revoir !~n"), ok;
        _ -> io:format("Choix invalide. Réessayez.~n"), menu_jeu(IdJoueur)
    end.

create_client() ->
    io:format("Création d'un nouveau client :~n"),
    Id = create_joueur_id(),
    {ok, [Pseudo]} = io:fread("Pseudo (string) : ", "~s"),
    {ok, [Prenom]} = io:fread("Prénom (string) : ", "~s"),
    {ok, [Nom]} = io:fread("Nom (string) : ", "~s"),
    {ok, [Passwd]} = io:fread("Mot de passe (string) : ", "~s"),
    DateDerniereCo = calendar:local_time(),
    NumReservation = 0,
    DateChoisieJeux = "Vide",
    Jeu = "Vide",
    Client = #client{
        id_joueur = Id,
        pseudo = Pseudo,
        prenom = Prenom,
        nom = Nom,
        passwd = Passwd,
        date_derniereCo = DateDerniereCo,
        num_reservation = NumReservation,
        date_choisie_jeux = DateChoisieJeux,
        jeu_choisi =  Jeu
    },
    {atomic, ok} = game_db:add_client(Client),
    io:format("Client ajouté avec succès !~n"),
    afficher_client(Id).

%%create_jeux() ->
    %%io:format("Création d'un nouveau jeu :~n"),
    %%{ok, [IdJeux]} = io:fread("Id jeu (entier) : ", "~d"),
    %%{ok, [NomJeu]} = io:fread("Nom du jeu (string) : ", "~s"),
    %%{ok, [NbInscrits]} = io:fread("Nombre de joueurs inscrits (entier) : ", "~d"),
    %%{ok, [NbTotal]} = io:fread("Nombre total de joueurs (entier) : ", "~d"),
    %%Jeux = #jeux{
        %%id_jeux = IdJeux,
        %%nom_jeu = NomJeu,
        %%nb_joueurs_inscrit_session = NbInscrits,
        %%nb_total_joueurs_session = NbTotal
    %%},
    %%{atomic, ok} = game_db:add_jeux(Jeux),
    %%io:format("Jeu ajouté avec succès !~n"),
    %%afficher_jeu(IdJeux).

%%create_horaires() ->
    %%io:format("Création d'un nouvel horaire :~n"),
    %%{ok, [IdHoraire]} = io:fread("Id horaire (entier) : ", "~d"),
    %%{ok, [PlageHoraire]} = io:fread("Plage horaire (string) : ", "~s"),
    %%{ok, [Jour]} = io:fread("Jour (string) : ", "~s"),
    %%{ok, [Semaine]} = io:fread("Semaine (string) : ", "~s"),
    %%Horaire = #horaires{
        %%id_horaire = IdHoraire,
        %%plage_horaire = PlageHoraire,
        %%jour = Jour,
        %%semaine = Semaine
    %%},
    %%{atomic, ok} = game_db:add_horaires(Horaire),
    %%io:format("Horaire ajouté avec succès !~n"),
    %%afficher_horaires(IdHoraire).

%% Fonctions pour afficher ce qu'on vient d'ajouter

afficher_client(Id) ->
    {atomic, [Client]} = game_db:get_client(Id),
    io:format("Client ajouté : ~p~n", [Client]).

%%afficher_jeu(IdJeux) ->
    %%{atomic, [Jeux]} = game_db:get_jeux(IdJeux),
    %%io:format("Jeu ajouté : ~p~n", [Jeux]).

afficher_horaires([]) ->
    io:format("Aucun horaire trouvé.~n");
afficher_horaires([H | T]) ->
    io:format("~p~n", [H]),
    afficher_horaires(T).

afficher_tous_horaires() ->
    case game_db:get_all_horaires() of
        {atomic, Horaires} when is_list(Horaires) ->
            io:format("~n--- Liste des horaires ---~n"),
            lists:foreach(fun(Horaire) ->
                io:format("~p~n", [Horaire])
            end, Horaires);
        {aborted, Reason} ->
            io:format("Erreur lors de la récupération des horaires : ~p~n", [Reason])
    end.

afficher_tous_les_clients() ->
    case game_db:get_all_clients() of
        {atomic, Clients} when is_list(Clients) ->
            io:format("~n--- Liste des clients ---~n"),
            lists:foreach(fun(Client) ->
                io:format("~p~n", [Client])
            end, Clients);
        {aborted, Reason} ->
            io:format("Erreur lors de la récupération des clients : ~p~n", [Reason])
    end.

authentifier_joueur() ->
    io:format("~n--- Authentification ---~n"),
    {ok, [Pseudo]} = io:fread("Pseudo : ", "~s"),
    {ok, [Passwd]} = io:fread("Mot de passe : ", "~s"),

    % Récupère tous les clients
    case game_db:get_all_clients() of
        {atomic, Clients} ->
            rechercher_client(Clients, Pseudo, Passwd);
        {aborted, Reason} ->
            io:format("Erreur d'accès à la base de données : ~p~n", [Reason])
    end.

rechercher_client([Client | Reste], Pseudo, Passwd) ->
    if
        Client#client.pseudo == Pseudo -> % Si le pseudo correspond
            if
                Client#client.passwd == Passwd -> % Si le mot de passe est correct
                    io:format("~nBIENVENUE ~s !~n", [Pseudo]),
                    mettre_a_jour_date_connexion(Client#client.id_joueur), % Mise à jour de la date de dernière connexion
                    io:format("Voulez-vous modifier un champ ? (o/n) "),
                    {ok, [Reponse]} = io:fread("", "~s"),
                    case string:trim(Reponse) of % Enlever les espaces et les retours à la ligne
                        "o" -> modifier_champ_client(Client#client.id_joueur),
                        menu_jeu(Client#client.id_joueur); % Si oui, on modifie un champ
                        "n" -> io:format("Vous pourrez modifier un champ à chaque connexion !~n"),
                        menu_jeu(Client#client.id_joueur); % Si non, on laisse l'utilisateur tranquille
                        _ -> io:format("Réponse invalide. Retour au menu.~n"),
                        menu_jeu(Client#client.id_joueur) % Cas de réponse invalide
                    end;
                true -> % Si le mot de passe est incorrect
                    io:format("Mot de passe incorrect. Veuillez réessayer.~n"),
                    authentifier_joueur() % Redemander l'authentification
            end;
        true -> % Si le pseudo ne correspond pas, continuer la recherche
            rechercher_client(Reste, Pseudo, Passwd)
    end;
rechercher_client([], _Pseudo, _Passwd) -> % Cas de base : Si la liste est vide
    io:format("Pseudo introuvable. Voulez-vous créer un compte ? (o/n) "),
    {ok, [Reponse]} = io:fread("", "~s"),
    case string:trim(Reponse) of
        "o" -> create_client(); % Si l'utilisateur veut créer un compte
        "n" -> io:format("Retour au menu.~n");
        _ -> io:format("Réponse invalide. Retour au menu.~n") % Cas de réponse invalide
    end.

create_joueur_id() ->
    F = fun() ->
        Clients = mnesia:match_object(#client{_ = '_'}),
        length(Clients) + 1
    end,
    {atomic, Id} = mnesia:transaction(F),
    Id.

modifier_champ_client(Id) ->
    io:format("Quel champ voulez-vous modifier ?~n"),
    io:format("1. Pseudo~n2. Prénom~n3. Nom~n4. Mot de passe~n"),
    {ok, [Choix]} = io:fread("Entrez le numéro du champ : ", "~d"),
    Champ = champ_depuis_choix(Choix),
    {ok, [Valeur]} = lire_valeur(Champ),
    Result = game_db:update_client_field(Id, Champ, Valeur),
    case Result of
        {atomic, _} -> io:format("Client mis à jour avec succès.~n");
        {aborted, {error, not_found}} -> io:format("Client non trouvé.~n");
        {aborted, Reason} -> io:format("Erreur : ~p~n", [Reason])
    end.

champ_depuis_choix(1) -> pseudo;
champ_depuis_choix(2) -> prenom;
champ_depuis_choix(3) -> nom;
champ_depuis_choix(4) -> passwd.


jeu_depuis_choix(1) -> puissance4;
jeu_depuis_choix(2) -> tictactoe.



lireJour(1) -> lundi;
lireJour(2) -> mardi;
lireJour(3) -> mercredi;
lireJour(4) -> jeudi;
lireJour(5) -> vendredi;
lireJour(6) -> samedi;
lireJour(7) -> dimanche.

lire_valeur(date_derniereCo) ->
    Date = calendar:local_time(),
    {ok, [Date]};
lire_valeur(num_reservation) ->
    io:fread("Nouvelle valeur (entier) : ", "~d");
lire_valeur(_) ->
    io:fread("Nouvelle valeur (string) : ", "~s").

mettre_a_jour_date_connexion(Id) ->
    Result = game_db:update_date_derniereCo(Id),
    case Result of
        {atomic, _} -> io:format("~n");
        {aborted, {error, not_found}} -> io:format("Client non trouvé.~n");
        {aborted, Reason} -> io:format("Erreur : ~p~n", [Reason])
    end.

%% === Navigation dans les créneaux ===
naviguer_calendrier(IdJoueur) ->
    io:format("~nPour quel jeu souhaitez-vous réserver un créneau ?~n"),
    io:format("1. Puissance 4~n"),
    io:format("2. Tictactoe~n"),
    case io:fread("Votre choix : ", "~d") of
        {ok, [Choix_Jeu]} -> Jeu = jeu_depuis_choix(Choix_Jeu), game_db:update_client_field(IdJoueur, jeu_choisi, Jeu);
        _ -> io:format("Choix invalide.~n"), naviguer_calendrier(IdJoueur)
    end,
    io:format("~n--- Navigation dans les créneaux disponibles ---~n"),
    io:format("1. Cette semaine~n"),
    io:format("2. Semaine prochaine~n"),
    case io:fread("Votre choix : ", "~d") of
        {ok, [1]} -> choisir_jour("cette semaine", IdJoueur);
        {ok, [2]} -> choisir_jour("la semaine prochaine", IdJoueur);
        _ -> io:format("Choix invalide.~n"), naviguer_calendrier(IdJoueur)
    end.

choisir_jour(SemaineChoisie, IdJoueur) ->
    io:format("~n--- Choisissez un jour ---~n"),
    Jours = ["Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche"],
    lists:foreach(fun({Idx, Jour}) ->
        io:format("~p. ~s~n", [Idx, Jour])
    end, lists:zip(lists:seq(1, length(Jours)), Jours)),
    case io:fread("Votre choix : ", "~d") of
        {ok, [Choix]} when Choix >= 1, Choix =< length(Jours) ->
            JourChoisi = lists:nth(Choix, Jours),
            Jour = lireJour(Choix),
            game_db:update_client_field(IdJoueur, date_choisie_jeux, Jour),
            afficher_creneaux(SemaineChoisie, JourChoisi, IdJoueur);
        _ ->
            io:format("Choix invalide.~n"),
            choisir_jour(SemaineChoisie, IdJoueur)
    end.

afficher_creneaux(SemaineChoisie, JourChoisi, IdJoueur) ->
    io:format("~n--- Créneaux pour ~s - ~s ---~n", [SemaineChoisie, JourChoisi]),
    {atomic, Tous} = game_db:get_all_horaires(),
    %%CreneauxFiltres = [C || C <- Tous, C#horaires.jour == JourChoisi, C#horaires.semaine == case SemaineChoisie of "cette semaine" -> "1"; "la semaine prochaine" -> "2" end],
    CreneauxFiltresNonTries = [C || C <- Tous, 
    C#horaires.jour == JourChoisi, 
    C#horaires.semaine == case SemaineChoisie of "cette semaine" -> "1"; "la semaine prochaine" -> "2" end],
    CreneauxFiltres = lists:sort(fun(C1, C2) -> C1#horaires.id_horaire =< C2#horaires.id_horaire end, CreneauxFiltresNonTries),

    case CreneauxFiltres of
        [] ->
            io:format("Aucun créneau disponible pour ~s - ~s.~n", [SemaineChoisie, JourChoisi]);
        _ ->
            lists:foreach(fun(C) ->
                io:format("ID ~p : ~s~n", [C#horaires.id_horaire, C#horaires.plage_horaire])
            end, CreneauxFiltres)
    end,
    case io:fread("Votre choix : ", "~d") of
        {ok, [IDHORAIRE]} -> game_db:update_client_field(IdJoueur, num_reservation, IDHORAIRE);
        _ -> io:format("Choix invalide.~n"), naviguer_calendrier(IdJoueur)
    end,
    menu_jeu(IdJoueur).




verif_p4(IdJoueur) ->
    case game_db:get_client_jeu(IdJoueur) of
        {ok, puissance4} ->
            case game_db:get_client_jour(IdJoueur) of
                {ok, JourChoisi} ->
                    JourActuel = jour_semaine_aujourdhui(),
                    case JourActuel =:= JourChoisi of
                        true ->
                            case game_db:get_client_horaire(IdJoueur) of
                                {ok, PlageHoraire} ->
                                    acces_jeu(PlageHoraire);
                                {error, horaire_not_found} ->
                                    io:format("Erreur : plage horaire introuvable.~n");
                                {error, client_not_found} ->
                                    io:format("Erreur : joueur non trouvé pour l'horaire.~n");
                                {error, Reason} ->
                                    io:format("Erreur inattendue horaire : ~p~n", [Reason])
                            end;
                        false ->
                            io:format("Accès refusé : aujourd'hui (~p), le jour prévu est (~p).~n", [JourActuel, JourChoisi])
                    end;
                {error, client_not_found} ->
                    io:format("Erreur : joueur non trouvé pour le jour choisi.~n");
                {error, Reason} ->
                    io:format("Erreur inattendue jour choisi : ~p~n", [Reason])
            end;
        {ok, _} ->
            io:format("Accès refusé : Aucun jeu n'est prévu, ou il ne s'agit pas de Puissance 4.~n");
        {error, client_not_found} ->
            io:format("Erreur : joueur non trouvé pour le jeu.~n");
        {error, Reason} ->
            io:format("Erreur inattendue jeu : ~p~n", [Reason])
    end.



verif_ttt(IdJoueur) ->
    case game_db:get_client_jeu(IdJoueur) of
        {ok, tictactoe} ->
            case game_db:get_client_jour(IdJoueur) of
                {ok, JourChoisi} ->
                    JourActuel = jour_semaine_aujourdhui(),
                    case JourActuel =:= JourChoisi of
                        true ->
                            case game_db:get_client_horaire(IdJoueur) of
                                {ok, PlageHoraire} ->
                                    acces_jeu2(PlageHoraire);
                                {error, horaire_not_found} ->
                                    io:format("Erreur : plage horaire introuvable.~n");
                                {error, client_not_found} ->
                                    io:format("Erreur : joueur non trouvé pour l'horaire.~n");
                                {error, Reason} ->
                                    io:format("Erreur inattendue horaire : ~p~n", [Reason])
                            end;
                        false ->
                            io:format("Accès refusé : aujourd'hui (~p), le jour prévu est (~p).~n", [JourActuel, JourChoisi])
                    end;
                {error, client_not_found} ->
                    io:format("Erreur : joueur non trouvé pour le jour choisi.~n");
                {error, Reason} ->
                    io:format("Erreur inattendue jour choisi : ~p~n", [Reason])
            end;
        {ok, _} ->
            io:format("Accès refusé : Aucun jeu n'est prévu, ou il ne s'agit pas de Tic-Tac-Toe.~n");
        {error, client_not_found} ->
            io:format("Erreur : joueur non trouvé pour le jeu.~n");
        {error, Reason} ->
            io:format("Erreur inattendue jeu : ~p~n", [Reason])
    end.






acces_jeu(PlageHoraire) when is_list(PlageHoraire) ->
    case parse_creneau(PlageHoraire) of
        {ok, {Debut, Fin}} ->
            acces_jeu({Debut, Fin});
        error ->
            io:format("Format invalide. Utiliser \"XXh-YYh\".~n")
    end;

acces_jeu({Debut, Fin}) when is_integer(Debut), is_integer(Fin) ->
    {HeureActuelle, _, _} = time_now(),
    case dans_creneau(HeureActuelle, Debut, Fin) of
        true ->
            puissance4:start();
        false ->
            io:format("Accès refusé : hors créneau horaire.~n")
    end.



acces_jeu2(PlageHoraire) when is_list(PlageHoraire) ->
    case parse_creneau(PlageHoraire) of
        {ok, {Debut, Fin}} ->
            acces_jeu2({Debut, Fin});
        error ->
            io:format("Format invalide. Utiliser \"XXh-YYh\".~n")
    end;

acces_jeu2({Debut, Fin}) when is_integer(Debut), is_integer(Fin) ->
    {HeureActuelle, _, _} = time_now(),
    case dans_creneau(HeureActuelle, Debut, Fin) of
        true ->
            tictactoe:start();
        false ->
            io:format("Accès refusé : hors créneau horaire.~n")
    end.

%% Fonction pour parser une chaîne "10h-12h" -> {10, 12}
parse_creneau(Str) ->
    case string:tokens(Str, "-") of
        [DebutStr, FinStr] ->
            case {parse_heure(DebutStr), parse_heure(FinStr)} of
                {{ok, Debut}, {ok, Fin}} ->
                    {ok, {Debut, Fin}};
                _ -> error
            end;
        _ -> error
    end.

%% Convertit "10h" -> {ok, 10}
parse_heure(Str) ->
    case string:trim(Str, trailing, "h") of
        "" -> error;
        Nombre ->
            case string:to_integer(Nombre) of
                {Int, _} -> {ok, Int};
                _ -> error
            end
    end.

dans_creneau(Heure, Debut, Fin) when Debut =< Fin ->
    Heure >= Debut andalso Heure < Fin;

dans_creneau(Heure, Debut, Fin) ->
    Heure >= Debut orelse Heure < Fin.

time_now() ->
    {_, {Hour, Min, Sec}} = calendar:local_time(),
    {Hour, Min, Sec}.



jour_semaine_aujourdhui() ->
    {Date, _Time} = calendar:universal_time(),
    DayNum = calendar:day_of_the_week(Date),
    lireJour(DayNum).

acceder_chat(IdJoueur) -> 
    io:format("~n--- CHAT ---~n"),
    io:format("Entrez 'quit' pour quitter le chat.~n"),
    boucle_chat(IdJoueur).

boucle_chat(IdJoueur) -> 
    {ok, [Message]} = io:fread("Votre message : ", "~s"),
    MessageTrimmed = string:trim(Message),
    if 
        MessageTrimmed == "quit" -> 
            io:format("Quit du chat.~n"), 
            menu_jeu(IdJoueur);
        true -> 
            envoyer_message(IdJoueur, MessageTrimmed),
            afficher_chat(IdJoueur),
            boucle_chat(IdJoueur)
    end.

envoyer_message(IdJoueur, Message) -> 
    Date = calendar:local_time(),
    MessageRecord = #message{
        id_joueur = IdJoueur,
        message = Message,
        date_envoye = Date
    },
    game_db:add_message(MessageRecord),
    io:format("Message envoyé.~n").

afficher_chat(IdJoueur) -> 
    case game_db:get_all_messages() of
        {atomic, Messages} when is_list(Messages) -> 
            io:format("Messages du chat :~n"),
            lists:foreach(fun(Message) -> 
                io:format("~p: ~s~n", [Message#message.id_joueur, Message#message.message])
            end, Messages);
        {aborted, Reason} -> 
            io:format("Erreur lors de la récupération des messages : ~p~n", [Reason])
    end.
