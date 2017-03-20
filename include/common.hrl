%%%-------------------------------------------------------------------
%%% @author liuzixiang
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 二月 2017 15:39
%%%-------------------------------------------------------------------
-author("liuzixiang").

-ifndef(COMMON_HRL).
-define(COMMON_HRL, common_hrl).

-define(COMMON_ERROR, 1000).
-define(MAKE_DIR, 1001).
-define(DEL_DIR, 1002).
-define(LS_DIR, 1003).
-define(TOUCH_FILE, 1004).
-define(DEL_FILE, 1005).
-define(RECEIVE_FILE, 1006).
-define(SEND_FILE, 1007).

-define(TRANSPORT_FILE_BEGIN, 1).
-define(TRANSPORT_FILE_END, 2).
-define(TRANSPORT_FILE_ERROR, 3).

-record(transport_protocol, {op_type = 0, action = 0, data}).

-define(CHUNK_SIZE, 16#1FFF).

-define(FILE_SERVER_ROOT_DIR, "/home/lemon/erlang_tmp/").
-define(SAVE_FILE_DIR, "./priv/").

-endif.