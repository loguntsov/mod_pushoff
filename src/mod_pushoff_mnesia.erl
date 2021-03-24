-module(mod_pushoff_mnesia).

-author('proger@wilab.org.ua').
-author('defeng.liang.cn@gmail.com').

-export([create/0,
  health/0,
  register_client/3,
  unregister_client/1,
  unregister_client/2,
  list_registrations/1,
  list_registrations_all/1]).

-include("logger.hrl").
-include("xmpp.hrl").

-include("mod_pushoff.hrl").

-define(RECORD(X), {X, record_info(fields, X)}).

create() ->
    mnesia_set_from_record(?RECORD(pushoff_registration)).

health() ->
    mnesia:table_info(pushoff_registration, all).

mnesia_set_from_record({Name, Fields}) ->
    ejabberd_mnesia:create(?MODULE, Name, [{disc_copies, [node()]},
                                           {type, set},
                                           {attributes, Fields}]).

-spec(register_client(jid(), backend_id(), binary()) ->
             {error, stanza_error()} |
             {registered, ok}).

register_client(Key, BackendId, Token) ->
    F = fun() ->
        MatchHeadReg =
            #pushoff_registration{key = Key, backend_id = BackendId, _='_'},
        ExistingReg =
            mnesia:select(pushoff_registration, [{MatchHeadReg, [], ['$_']}]),
        Registration =
            case ExistingReg of
                [] ->
                    #pushoff_registration{key = Key,
                                          token = Token,
                                          backend_id = BackendId,
                                          timestamp = erlang:timestamp()};

                [OldReg] ->
                    OldReg#pushoff_registration{token = Token,
                                                backend_id = BackendId,
                                                timestamp = erlang:timestamp()}
            end,
        mnesia:write(Registration),
        ok
    end,
    case mnesia:transaction(F) of
        {aborted, Reason} ->
            ?ERROR_MSG("register_client: ~p", [Reason]),
            {error, xmpp:err_internal_server_error()};
        {atomic, ok} ->
            {registered, ok}
    end.

-spec(unregister_client({key(), erlang:timestamp()}) ->
            {error, stanza_error()} |
            {unregistered, [pushoff_registration()]}).
unregister_client({Key, Timestamp}) ->
  unregister_client(Key, Timestamp).

-spec(unregister_client(key(), erlang:timestamp() | '_') ->
             {error, stanza_error()} |
             {unregistered, [pushoff_registration()]}).
unregister_client(Key, Timestamp) ->
  F = fun() ->
      [
        begin
           ?DEBUG("+++++ deleting registration ~p", [Reg]),
           mnesia:delete_object(Reg),
           Reg
        end || Reg <-
          [
            mnesia:select(pushoff_registration,
              [{#pushoff_registration{key = Key,
                timestamp = Timestamp,
                _='_'},
                [], ['$_']}])
          ]
      ]
  end,
  case mnesia:transaction(F) of
      {aborted, Reason} ->
          ?ERROR_MSG("unregister_client: ~p", [Reason]),
          {error, xmpp:err_internal_server_error()};
      {atomic, []} ->
          {error, xmpp:err_item_not_found()};
      {atomic, Result} ->
          {unregistered, Result}
  end.

-spec(list_registrations(jid()) -> {error, stanza_error()} |
                                   {registrations, [pushoff_registration()]}).

list_registrations(Key) ->
    F = fun() ->
      MatchHead = #pushoff_registration{key = Key, _='_'},
      mnesia:select(pushoff_registration, [{MatchHead, [], ['$_']}])
    end,
    case mnesia:transaction(F) of
        {aborted, Reason} ->
            ?ERROR_MSG("list_registrations: ~p", [Reason]),
            {error, xmpp:err_internal_server_error()};
        {atomic, RegList} ->
            {registrations, RegList}
    end.

list_registrations_all({LUser, LServer}) ->
  F = fun() ->
    MatchHead1 = #pushoff_registration{key = {LUser, LServer}, _='_'},
    MatchHead2 = #pushoff_registration{key = {LUser, LServer, voip}, _='_'},
    List1 = mnesia:select(pushoff_registration, [{MatchHead1, [], ['$_']}]),
    List2 = mnesia:select(pushoff_registration, [{MatchHead2, [], ['$_']}]),
    lists:merge(List1, List2)
      end,
  case mnesia:transaction(F) of
    {aborted, Reason} ->
      ?ERROR_MSG("list_registrations: ~p", [Reason]),
      {error, xmpp:err_internal_server_error()};
    {atomic, RegList} ->
      {registrations, RegList}
  end.