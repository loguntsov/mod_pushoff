-type key() :: {binary(), binary(), atom()} | bare_jid().
-type bare_jid() :: {binary(), binary()}.
-type backend_ref() :: apns | fcm | {apns, binary()} | {fcm, binary()}.
-type backend_id() :: {binary(), backend_ref()}.

-record(pushoff_registration, {key :: key(),
                               token :: binary(),
                               backend_id :: backend_id(),
                               timestamp :: erlang:timestamp()}).

-type pushoff_registration() :: #pushoff_registration{}.
