%%%-------------------------------------------------------------------
%%% @author liuzixiang
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 二月 2017 16:07
%%%-------------------------------------------------------------------
-module(file_server_listener_sup).
-author("liuzixiang").

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(NumAcceptors::non_neg_integer(), Transport::atom(), TransOpts::[term()]) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(NumAcceptors, Transport, TransOpts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [NumAcceptors, Transport, TransOpts]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([NumAcceptors, Transport, TransOpts]) ->
    {ok, LSocket} = Transport:listen(TransOpts),

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChildList = [{{acceptor, N}, {file_server_acceptor, start_link, [Transport, TransOpts, LSocket]},
        Restart, Shutdown, Type, [file_server_acceptor]} || N <- lists:seq(1, NumAcceptors)],

    {ok, {SupFlags, AChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
