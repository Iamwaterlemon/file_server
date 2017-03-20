%%%-------------------------------------------------------------------
%% @doc file_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(file_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(NumAcceptors, Transport, TransOpts, UdpPort) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [NumAcceptors, Transport, TransOpts, UdpPort]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([NumAcceptors, Transport, TransOpts, UdpPort]) ->
    ChildSpaces = [
        {file_server_listener_sup, {file_server_listener_sup, start_link, [NumAcceptors, Transport, TransOpts]},
            permanent, 5000, supervisor, [file_server_listener_sup]},
        {file_server_client_sup, {file_server_client_sup, start_link, []},
            permanent, 5000, supervisor, [file_server_client_sup]},
        {file_server_udp_sup, {file_server_udp_sup, start_link, [UdpPort]},
            permanent, 5000, supervisor, [file_server_udp_sup]}
    ],

    {ok, { {one_for_one, 10, 1}, ChildSpaces} }.

%%====================================================================
%% Internal functions
%%====================================================================
