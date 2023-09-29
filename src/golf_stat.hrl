%%% @author Dan Gudmundsson
%%% @copyright (C) 2023, Dan Gudmundsson
%%% @doc
%%%        
%%% @end
%%% Created : 30 Aug 2023 by Dan
%%%-------------------------------------------------------------------

-define(DBG(F,As), io:format("~p:~w: " ++ F, [?MODULE,?LINE|As])).
-include_lib("kernel/include/logger.hrl").
