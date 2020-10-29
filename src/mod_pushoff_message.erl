-module(mod_pushoff_message).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  body/1
]).

body(Payload) ->
  PushType = proplists:get_value(push_type, Payload),
  case PushType of
    hidden -> <<"Hidden Message">>;
    call -> <<"Call message">>;
    _ -> <<"Incoming Message">>
  end.
