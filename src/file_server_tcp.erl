%%%-------------------------------------------------------------------
%%% @author liuzixiang
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 二月 2017 17:40
%%%-------------------------------------------------------------------
-module(file_server_tcp).
-author("liuzixiang").

%% API
-export([name/0]).
-export([secure/0]).
-export([listen/1]).
-export([accept/2]).
-export([connect/3]).
-export([connect/4]).
-export([recv/3]).
-export([send/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([shutdown/2]).
-export([close/1]).
-export([setopts/2]).

name() -> tcp.

-spec secure() -> boolean().
secure() ->
    false.

-spec listen(Opts::[term()]) -> {ok, inet:socket()} | {error, atom()}.
listen(Opts) ->
    gen_tcp:listen(0, Opts ++
        [binary, {active, false}, {packet, raw}, {reuseaddr, true}]).

-spec accept(inet:socket(), timeout())
        -> {ok, inet:socket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
    gen_tcp:accept(LSocket, Timeout).

-spec connect(inet:ip_address() | inet:hostname(),
    inet:port_number(), any())
        -> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts) when is_integer(Port) ->
    gen_tcp:connect(Host, Port,
        Opts ++ [binary, {active, false}, {packet, raw}]).

-spec connect(inet:ip_address() | inet:hostname(),
    inet:port_number(), any(), timeout())
        -> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
    gen_tcp:connect(Host, Port,
        Opts ++ [binary, {active, false}, {packet, raw}],
        Timeout).

-spec recv(inet:socket(), non_neg_integer(), timeout())
        -> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout).

-spec send(inet:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
    gen_tcp:send(Socket, Packet).

-spec controlling_process(inet:socket(), pid())
        -> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
    gen_tcp:controlling_process(Socket, Pid).

-spec setopts(inet:socket(), [term()]) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
    prim_inet:setopts(Socket, Opts).

-spec peername(inet:socket())
        -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
    inet:peername(Socket).

-spec sockname(inet:socket())
        -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
    inet:sockname(Socket).

-spec shutdown(inet:socket(), read | write | read_write)
        -> ok | {error, atom()}.
shutdown(Socket, How) ->
    gen_tcp:shutdown(Socket, How).

-spec close(inet:socket()) -> ok.
close(Socket) ->
    gen_tcp:close(Socket).





