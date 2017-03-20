%%%-------------------------------------------------------------------
%% @doc file_server public API
%% @end
%%%-------------------------------------------------------------------

-module(file_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    NumAcceptors = 10,
    Transport = file_server_tcp,
    TransOpts = [{port, 5555}],
    UdpPort = 1314,
    file_server_sup:start_link(NumAcceptors, Transport, TransOpts, UdpPort).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
