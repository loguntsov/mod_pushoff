%%%
-module(mod_pushoff_sql).

-author('defeng.liang.cn@gmail.com').

-export([
  register_client/3,
  unregister_client/1,
  unregister_client/2,
  list_registrations/1,
  list_registrations_all/1]).

-include("logger.hrl").
-include("mod_pushoff.hrl").
-include("ejabberd_sql_pt.hrl").

-spec(register_client(jid(), backend_id(), binary()) ->
             {error, stanza_error()} |
             {registered, ok}).
register_client(Key, BackendId, Token) ->
  ?DEBUG("register_client#start", []),
  ?DEBUG("key:~p, BackendId:~p, Token:~p~n", [Key, BackendId, Token]),

  {User, Server, PushType} = Key,
  {BackendServer, {BackendType, BackendRef}} = BackendId,
  BareJid = <<User/binary, <<"@">>, Server/binary>>,
  case ejabberd_sql:sql_query(Server,
    ?SQL("select @(bare_jid)s,@(push_type)d
          from pushoff_tbl
          where bare_jid=%(BareJid)s and push_type=%(PushType)d"))
  of
    {selected, _ , []} ->
      Now = erlang:system_time(millisecond),
      ejabberd_sql:sql_query(Server, ?SQL_INSERT(
        "pushoff_tbl",
        ["bare_jid=%(User)s",
          "push_type=%(PushType)d",
          "token=%(Token)s",
          "backend_server=%(BackendServer)s",
          "backend_id=%(BackendType)s",
          "backend_ref=%(BackendRef)s",
          "timestamp=%(Now)d"
        ])
      ),
      {registered, ok};
    {selected, _ , Result} ->
      Now = erlang:system_time(millisecond),
      ejabberd_sql:sql_query(Server, ?SQL_UPSERT_T(
        "pushoff_tbl",
        ["!bare_jid=%(User)s",
          "!push_type=%(PushType)d",
          "token=%(Token)s",
          "backend_server=%(BackendServer)s",
          "backend_id=%(BackendType)s",
          "backend_ref=%(BackendRef)s",
          "timestamp=%(Now)d"
        ])
      ),
      {registered, ok};
    _ ->
      ?ERROR_MSG("Database error occurs.", []),
      {error, xmpp:err_internal_server_error()}
  end.

-spec(unregister_client({key(), erlang:timestamp()}) ->
            {error, stanza_error()} |
            {unregistered, [pushoff_registration()]}).
unregister_client({Key, Timestamp}) ->
  {User, Server, PushType} = Key,
  BareJid = <<User/binary, <<"@">>, Server/binary>>,
  case ejabberd_sql:sql_query(Server,
    ?SQL("select @(bare_jid)s,
            @(push_type)d
            @(token)s,
            @(backend_server)s,
            @(backend_id)s,
            @(backend_ref)s,
            @(timestamp)d
          from pushoff_tbl
          where bare_jid=%(BareJid)s and push_type=%(PushType)d"))
  of
    {selected, _ , []} ->
      {unregistered, []};
    {select, _, Result} ->

      case ejabberd_sql:sql_query(Server,
        ?SQL("delete from pushoff_tbl
          where bare_jid=%(BareJid)s and timestamp=%(Timestamp)d"))
      of
        {updated, _Num} ->
          RetList = lists:foreach(fun(Record) ->
            [_Barejid, _PushType, Token, Server, BackendId, BackendRef, Time] = Record,
            #pushoff_registration{key = Key,
              token = Token,
              backend_id = {Server, {binary_to_atom(BackendId), BackendRef}},
              timestamp = binary_to_integer(Time)
            } end, Result),
          {unregistered, RetList};
        _ ->
          ?ERROR_MSG("unregister_client: databaser error", []),
          {error, xmpp:err_internal_server_error()}
      end;
    _ ->
      ?ERROR_MSG("unregister_client: databaser error", []),
      {error, xmpp:err_internal_server_error()}
  end.

-spec(unregister_client(User :: binary(), Server :: binary()) ->
             {error, stanza_error()} |
             {unregistered, [pushoff_registration()]}).
unregister_client(User, Server) ->
  BareJid = <<User/binary, <<"@">>, Server/binary>>,
  case ejabberd_sql:sql_query(Server,
    ?SQL("select @(bare_jid)s,
            @(push_type)d
            @(token)s,
            @(backend_server)s,
            @(backend_id)s,
            @(backend_ref)s,
            @(timestamp)d
          from pushoff_tbl
          where bare_jid=%(BareJid)s"))
  of
    {selected, _ , []} ->
      {unregistered, []};
    {select, _, Result} ->

      case ejabberd_sql:sql_query(Server,
        ?SQL("delete from pushoff_tbl
          where bare_jid=%(BareJid)s"))
      of
        {updated, _Num} ->
          RetList = lists:foreach(fun(Record) ->
            [BareJid, PushType, Token, Server, BackendId, BackendRef, Time] = Record,
            #pushoff_registration{key = {User, Server, binary_to_integer(PushType)},
              token = Token,
              backend_id = {Server, {binary_to_atom(BackendId), BackendRef}},
              timestamp = binary_to_integer(Time)
            } end, Result),
          {unregistered, RetList};
        _ ->
          ?ERROR_MSG("unregister_client: databaser error", []),
          {error, xmpp:err_internal_server_error()}
      end;
    _ ->
      ?ERROR_MSG("unregister_client: databaser error", []),
      {error, xmpp:err_internal_server_error()}
  end.

-spec(list_registrations(jid()) -> {error, stanza_error()} |
                                   {registrations, [pushoff_registration()]}).

list_registrations(Key) ->
  {User, Server, PushType} = Key,
  BareJid = <<User/binary, <<"@">>, Server/binary>>,
  case ejabberd_sql:sql_query(Server,
    ?SQL("select @(bare_jid)s,
            @(push_type)d
            @(token)s,
            @(backend_server)s,
            @(backend_id)s,
            @(backend_ref)s,
            @(timestamp)d
          from pushoff_tbl
          where bare_jid=%(BareJid)s and push_type=%(PushType)d"))
  of
    {selected, _ , []} ->
      {registrations, []};
    {select, _, Result} ->
      RegList = lists:foreach(fun(Record) ->
        [_Barejid, _PushType, Token, Server, BackendId, BackendRef, Time] = Record,
        #pushoff_registration{key = Key,
          token = Token,
          backend_id = {Server, {binary_to_atom(BackendId), BackendRef}},
          timestamp = binary_to_integer(Time)
        }
        end, Result),
      {registrations, RegList};
    _ ->
      ?ERROR_MSG("list_registrations: databaser error", []),
      {error, xmpp:err_internal_server_error()}
  end.

list_registrations_all({User, Server}) ->
  BareJid = <<User/binary, <<"@">>, Server/binary>>,
  case ejabberd_sql:sql_query(Server,
    ?SQL("select @(bare_jid)s,
            @(push_type)d
            @(token)s,
            @(backend_server)s,
            @(backend_id)s,
            @(backend_ref)s,
            @(timestamp)d
          from pushoff_tbl
          where bare_jid=%(BareJid)s"))
  of
    {selected, _ , []} ->
      {registrations, []};
    {select, _, Result} ->
      RegList = lists:foreach(fun(Record) ->
        [_Barejid, PushType, Token, Server, BackendId, BackendRef, Time] = Record,
        #pushoff_registration{key = {User, Server, binary_to_integer(PushType)},
          token = Token,
          backend_id = {Server, {binary_to_atom(BackendId), BackendRef}},
          timestamp = binary_to_integer(Time)
        } end, Result),
      {registrations, RegList};
    _ ->
      ?ERROR_MSG("list_registrations: databaser error", []),
      {error, xmpp:err_internal_server_error()}
  end.