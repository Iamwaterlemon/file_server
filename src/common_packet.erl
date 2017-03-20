%%%-------------------------------------------------------------------
%%% @author liuzixiang
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 三月 2017 15:45
%%%-------------------------------------------------------------------
-module(common_packet).
-author("liuzixiang").
-include("common.hrl").

%% API
-export([packet_encode/2, packet_encode/3, packet_decode/1]).

packet_encode(OpType, Data) ->
    erlang:term_to_binary(#transport_protocol{op_type = OpType, data = Data}).
packet_encode(OpType, Action, Data) ->
    erlang:term_to_binary(#transport_protocol{op_type = OpType, action = Action, data = Data}).

packet_decode(DataList) when is_list(DataList) ->
    packet_decode(list_to_binary(DataList));
packet_decode(Binary)  ->
    case erlang:binary_to_term(Binary) of
        #transport_protocol{} = Tuple ->
            Tuple;
        _ ->
            error
    end.

