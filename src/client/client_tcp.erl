%%%-------------------------------------------------------------------
%%% @author liuzixiang
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 二月 2017 15:50
%%%-------------------------------------------------------------------
-module(client_tcp).
-author("liuzixiang").

-behaviour(gen_server).
-include("common.hrl").

%% API
-export([start_link/0,start/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([
    ls_dir/1,
    make_dir/1,
    del_dir/1,
    touch_file/1,
    del_file/1,
    receive_file/1
]).

-define(SERVER, ?MODULE).


-record(state, {socket}).

%%%===================================================================
%%% API
%%%===================================================================

ls_dir(DirName) ->
    gen_server:call(?SERVER, {?LS_DIR, DirName}).

make_dir(DirName) ->
    gen_server:call(?SERVER, {?MAKE_DIR, DirName}).

del_dir(DirName) ->
    gen_server:call(?SERVER, {?DEL_DIR, DirName}).

touch_file(FileName) ->
    gen_server:call(?SERVER, {?TOUCH_FILE, FileName}).

del_file(FileName) ->
    gen_server:call(?SERVER, {?DEL_FILE, FileName}).

receive_file(FileName) ->
    gen_server:call(?SERVER, {?RECEIVE_FILE, FileName}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start() ->
    start_link().
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    case gen_tcp:connect(localhost, 5555, [{active, once}]) of
        {ok, Socket} ->
            Socket;
        {error, _Reason} = Err ->
            Socket = undefined,
            exit(Err)
    end,
    {ok, #state{socket = Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({OpType, Data}, _From, State = #state{socket = Socket}) ->
    case Data  of
        [] ->
            Msg = "please enter again!";
        _->
            NewData = common_packet:packet_encode(OpType, Data),
            gen_tcp:send(Socket, NewData),
            Msg = NewData

    end,
    {reply, Msg, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({tcp, Socket, Data}, State) ->
    file_server_misc:client_router(Socket, common_packet:packet_decode(Data)),
    prim_inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    io:format("tcp_closeed~n"),
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    io:format("tcp_error:~w~n", [Reason]),
    {stop, Reason, State};
handle_info(timeout, State) ->
    io:format("timeout:~w~n", [timeout]),
    {stop, normal, State};
handle_info(_Info, State) ->
    io:format("Info:~w~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================