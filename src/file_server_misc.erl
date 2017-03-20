%%%-------------------------------------------------------------------
%%% @author liuzixiang
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%   base file operation
%%% @end
%%% Created : 04. 三月 2017 10:44
%%%-------------------------------------------------------------------
-module(file_server_misc).
-author("liuzixiang").

-include("common.hrl").
%% API
-export([client_router/2, server_router/3]).

client_router(Socket, #transport_protocol{op_type = ?SEND_FILE, action = ?TRANSPORT_FILE_BEGIN}) ->
    receive_file_loop(Socket, <<>>);
client_router(_Socket,  Data) when is_binary(Data) ->
    error_logger:info_msg("server say  binary_change:~w~n", [erlang:binary_to_list(Data)]);
client_router(_Socket,  Data) ->
    error_logger:info_msg("server say:~w~n", [Data]).

receive_file_loop(Socket, Buffer) ->
    prim_inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Packet} ->
            receive_file_loop2(Socket, Packet, Buffer);
        {udp, Socket, _Address, _Port, Packet} ->
            receive_file_loop2(Socket, Packet, Buffer);
        _Reason ->
            error_logger:error_msg("receive  error:~w~n", [_Reason])
    end.

receive_file_loop2(Socket, Packet, Buffer) ->
    error_logger:info_msg("packet:~w~n", [Packet]),
    case common_packet:packet_decode(Packet) of
        #transport_protocol{op_type = ?SEND_FILE, action = ?TRANSPORT_FILE_END, data = FileName} ->
            case file:write_file(?SAVE_FILE_DIR++FileName, Buffer) of
                ok ->
                    error_logger:info_msg("write file ok:~w~n", [FileName]);
                {error, Reason} ->
                    error_logger:error_msg("write file error:~w~n", [Reason])
            end;
        #transport_protocol{op_type = ?SEND_FILE, action = ?TRANSPORT_FILE_ERROR, data = Reason} ->
            error_logger:error_msg("receive error:~w~n", [Reason]);
        #transport_protocol{op_type = ?SEND_FILE, data = FileStream}->
            NewBuffer = <<Buffer/binary, FileStream/binary>>,
            receive_file_loop(Socket, NewBuffer);
        _Error ->
            error_logger:error_msg("receive  error:~w~n", [_Error])
    end.


server_router(Transport, SendOpt, Binary) ->
    #transport_protocol{op_type = OpType, data = Data} =NewData= common_packet:packet_decode(Binary),
    error_logger:info_msg("Newone:~w~n", [NewData]),
    server_router(OpType, Data, SendOpt, Transport).
server_router(?MAKE_DIR, Data, _, _) ->
    do_make_dir(Data);
server_router(?DEL_DIR, Data, _, _) ->
    do_delete_dir(Data);
server_router(?TOUCH_FILE, Data, _, _) ->
    do_touch_file(Data);
server_router(?DEL_FILE, Data, _, _) ->
    do_delete_file(Data);
server_router(?LS_DIR, Data, _, _) ->
    do_show_dir_detail(Data);
server_router(?RECEIVE_FILE, FileName, SendOpt, Transport) ->
    do_send_file(SendOpt, FileName, Transport);
server_router(_, _, _, _) ->
    common_packet:packet_encode(?COMMON_ERROR, " Are you kidding me! \n you should use right way!~n").

do_make_dir(Data) ->
    case file:make_dir(?FILE_SERVER_ROOT_DIR ++ Data) of
        ok ->
            common_packet:packet_encode(?MAKE_DIR, "create success!~n");
        {error, Reason} ->
            common_packet:packet_encode(?MAKE_DIR, Reason)
    end.
do_delete_dir(Data) ->
    case file:del_dir(?FILE_SERVER_ROOT_DIR ++ Data) of
        ok ->
            common_packet:packet_encode(?DEL_DIR,"delete success!~n");
        {error, Reason} ->
            common_packet:packet_encode(?DEL_DIR, Reason)
    end.
do_touch_file(Data) ->
    Return = os:cmd("touch "++?FILE_SERVER_ROOT_DIR ++ Data),
    common_packet:packet_encode(?TOUCH_FILE, Return).
do_delete_file(Data) ->
    common_packet:packet_encode(?DEL_FILE, Data).


do_show_dir_detail(_Data) ->
    case file:list_dir(?FILE_SERVER_ROOT_DIR) of
        {ok, FileNames} ->
            common_packet:packet_encode(?LS_DIR,FileNames);
        {error, Reason} ->
            common_packet:packet_encode(?LS_DIR,Reason)
    end.

do_send_file({Socket, Address, Port} = SendOpt, FileName, Transport) when is_tuple(SendOpt) ->
    Begin = common_packet:packet_encode(?SEND_FILE, ?TRANSPORT_FILE_BEGIN, ""),
    Transport:send(Socket, Address, Port, Begin),
    do_send_file2(SendOpt, FileName, Transport);
do_send_file(Socket, FileName, Transport) ->
    Begin = common_packet:packet_encode(?SEND_FILE, ?TRANSPORT_FILE_BEGIN, ""),
    Transport:send(Socket, Begin),
    do_send_file2(Socket, FileName, Transport).
do_send_file2(SendOpt, FileName, Transport) ->
    case file:open(?FILE_SERVER_ROOT_DIR ++ FileName, [read, raw, binary]) of
        {ok, RawFile} ->
            error_logger:info_msg("RawFile:~w~n", [RawFile]),
            try sendfile_loop(Transport, FileName, SendOpt, RawFile) of
                Result -> Result
            after
                ok = file:close(RawFile)
            end;
        {error, Reason} ->
            format_error_msg(Reason)
    end.

sendfile_loop(Transport, FileName, SendOpt, RawFile) ->
    case file:read(RawFile, ?CHUNK_SIZE) of
        {ok, IoData} ->
            error_logger:info_msg("IoData:~w~n", [IoData]),
            Data = common_packet:packet_encode(?SEND_FILE, IoData),
            sendfile_loop2(Transport, FileName, SendOpt, RawFile, Data);
        {error, Reason} ->
            format_error_msg(Reason);
        eof ->
            common_packet:packet_encode(?SEND_FILE, ?TRANSPORT_FILE_END, FileName)
    end.

sendfile_loop2(Transport, FileName, {Socket, Address, Port} = SendOpt, RawFile, Data) when is_tuple(SendOpt) ->
    case Transport:send(Socket, Address, Port, Data) of
        ok ->
            sendfile_loop(Transport, FileName, SendOpt, RawFile);
        {error, Reason} ->
            format_error_msg(Reason)
    end;
sendfile_loop2(Transport, FileName, Socket, RawFile, Data)  ->
    case Transport:send(Socket, Data) of
        ok ->
            sendfile_loop(Transport, FileName, Socket, RawFile);
        {error, Reason} ->
            format_error_msg(Reason)
    end.

format_error_msg(Reason) ->
    common_packet:packet_encode(?SEND_FILE, ?TRANSPORT_FILE_ERROR, Reason).