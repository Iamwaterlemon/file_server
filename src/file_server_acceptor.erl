%%%-------------------------------------------------------------------
%%% @author liuzixiang
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 二月 2017 17:28
%%%-------------------------------------------------------------------
-module(file_server_acceptor).
-author("liuzixiang").

%% API
-export([start_link/3, loop/3]).

start_link(Transport, TransOpts, LSocket) ->
    Pid = erlang:spawn_link(?MODULE, loop, [Transport, TransOpts, LSocket]),
    {ok, Pid}.

loop(Transport, TransOpts, LSocket) ->
    case Transport:accept(LSocket, infinity) of
        {ok, Socket} ->
            ChildSpec = {{file_server_client_server, self()}, {file_server_client_server, start_link, [Socket,Transport]},
                permanent, 5000, worker, [file_server_client_server]},
            {ok, ClientPid} = supervisor:start_child(file_server_client_sup, ChildSpec),
            Transport:controlling_process(Socket, ClientPid),
            loop(Transport, TransOpts, LSocket);
        Error ->
            error_logger:error_msg("accept error:~w", [Error]),
            loop(Transport, TransOpts, LSocket)
    end.