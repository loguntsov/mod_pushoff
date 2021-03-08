-module(mod_pushoff_message).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  body/1, from/1, apns_push_type/1
]).

body(Payload) ->
  PushType = proplists:get_value(push_type, Payload),
  case PushType of
    hidden -> <<"Hidden Message">>;
    call -> <<"Call message">>;
    body -> proplists:get_value(body, Payload)
  end.

from(Payload) ->
  proplists:get_value(from, Payload, <<>>).

apns_push_type(Payload) ->
  proplists:get_value(apns_push_type, Payload, <<>>).
