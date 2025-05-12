-module(chat_gui).
-export([start/0, init_gui/1, handle_event/2]).

-include_lib("wx/include/wx.hrl").

start() ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), -1, "PlayPal Chat", [{size, {400, 300}}]),
    init_gui(Frame),
    wxFrame:show(Frame),
    loop(Frame).

init_gui(Frame) ->
    Panel = wxPanel:new(Frame),
    Vbox = wxBoxSizer:new(?wxVERTICAL),

    %% Zone d'affichage des messages (lecture seule)
    MsgList = wxTextCtrl:new(Panel, "", [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
    wxSizer:add(Vbox, MsgList, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

    %% Zone de saisie + bouton envoyer
    Hbox = wxBoxSizer:new(?wxHORIZONTAL),
    Input = wxTextCtrl:new(Panel, "", [{style, ?wxTE_PROCESS_ENTER}]),
    SendBtn = wxButton:new(Panel, -1, "Envoyer"),
    wxSizer:add(Hbox, Input, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(Hbox, SendBtn, [{flag, ?wxALL}, {border, 5}]),

    wxSizer:add(Vbox, Hbox, [{flag, ?wxEXPAND}]),

    wxPanel:setSizer(Panel, Vbox),

    %% Connexion des événements
    wxWindow:connect(Input, command_enter),
    wxWindow:connect(SendBtn, command_button_clicked),

    %% Envoi de l'état de l'interface dans le processus principal
    register(chat_gui_panel, Panel),
    Panel ! {gui_state, MsgList, Input},

    ok.

loop(Frame) ->
    receive
        {wx, _Id, _Obj, Event} ->
            handle_event(Frame, Event),
            loop(Frame);
        stop ->
            wxFrame:destroy(Frame),
            wx:destroy()
    end.

handle_event(Frame, #wxCommand{}) ->
    %% Récupérer le panel et ses composants
    Panel = chat_gui_panel,
    Panel ! {get_state, self()},
    receive
        {gui_state, MsgList, Input} ->
            Text = wxTextCtrl:getValue(Input),
            case Text of
                "" -> ok;
                _ ->
                    wxTextCtrl:appendText(MsgList, io_lib:format("Vous: ~s~n", [Text])),
                    wxTextCtrl:setValue(Input, ""),
                    %% ICI tu peux envoyer Text au serveur Playpal via un gen_tcp:send(Socket, Text)
                    ok
            end
    end;
handle_event(_, _) ->
    ok.
