%%%-------------------------------------------------------------------
%% @copyright Heroku (2012)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc
%% @end
%%%-------------------------------------------------------------------
-module(lsp_request).

-include_lib("ex_uri/include/ex_uri.hrl").

%% API
-export([new/2
        ,to_iolist/1]).

-record(frame, {body :: iolist(),
                msg_count :: non_neg_integer(),
                id :: iolist()}).
-record(lsp_request, {uri :: #ex_uri{},
                      logs = [] :: list(lsp_msg:msg())}).
-define(CONTENT_TYPE, <<"application/logplex-1">>).
-define(HTTP_VERSION, {1,1}).

%%====================================================================
%% API
%%====================================================================

new(Logs, Url) when is_list(Url) ->
    {ok, Uri = #ex_uri{}, _} = ex_uri:decode(Url),
    new(Logs, Uri);
new(Logs, Uri = #ex_uri{}) when is_list(Logs) ->
    #lsp_request{logs=Logs, uri=Uri}.

to_iolist(#lsp_request{uri = Uri} = Req) ->
    to_iolist(frame(Req), Uri).

frame(#lsp_request{logs=Logs}) ->
    #frame{ body =
                [ begin
                      MsgS = lsp_msg:to_iolist(Msg),
                      lsp_syslog_utils:frame(MsgS)
                  end
                  || Msg <- Logs ],
            msg_count = length(Logs),
            id = frame_id_iolist({self(), now()}) }.

frame_id_iolist(Term) ->
    << <<(hd(integer_to_list(I, 16)))>>
      || <<I:4>> <= crypto:md5(term_to_binary(Term)) >>.

to_iolist(#frame{body = Body,
                 msg_count = Count,
                 id = Id},
          URI = #ex_uri{}) ->
    AuthHeader = auth_header(URI),
    MD5Header = case logsplat_app:config(http_body_checksum, none) of
                    md5 -> [{<<"Content-MD5">>,
                             base64:encode(crypto:md5(Body))}];
                    none -> []
                end,
    Headers = MD5Header ++ AuthHeader ++
        [{<<"Content-Type">>, ?CONTENT_TYPE},
         {<<"Logplex-Msg-Count">>, integer_to_list(Count)},
         {<<"Logplex-Frame-Id">>, Id},
         {<<"User-Agent">>, user_agent()}
        ],
    cowboy_client:request_to_iolist(<<"POST">>,
                                    Headers,
                                    Body,
                                    ?HTTP_VERSION,
                                    full_host_iolist(URI),
                                    uri_ref(URI)).

%%====================================================================
%% Internal functions
%%====================================================================

auth_header(#ex_uri{authority=#ex_uri_authority{userinfo=Info}})
  when Info =/= undefined ->
    cowboy_client:auth_header(Info);
auth_header(_) ->
    [].

full_host_iolist(#ex_uri{authority=#ex_uri_authority{host=Host,
                                                     port=Port}})
  when is_integer(Port), Host =/= undefined ->
    [Host, ":", integer_to_list(Port)];
full_host_iolist(#ex_uri{authority=#ex_uri_authority{host=Host}})
  when Host =/= undefined ->
    Host.

uri_ref(#ex_uri{path=Path, q=Q}) ->
    Ref = #ex_uri_ref{path = case Path of
                                 "" -> "/";
                                 _ -> Path
                             end, q=Q},
    ex_uri:encode(Ref).

user_agent() ->
    [<<"Logsplat/">>,
     case application:get_key(logsplat, vsn) of
         undefined -> <<"unknown">>;
         {ok, Vsn} -> Vsn
     end].
