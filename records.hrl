-record(client, {id_joueur, pseudo, prenom, nom, passwd, date_derniereCo, num_reservation, date_choisie_jeux, jeu_choisi}).
-record(jeux, {id_jeux, nom_jeu, nb_joueurs_inscrit_session, nb_total_joueurs_session}).
-record(horaires, {id_horaire, plage_horaire, jour, semaine, datetime}).
-record(chat_message, {id, user, content, timestamp}).
-record(message, {
    id_joueur,
    message,
    date_envoye
}).
