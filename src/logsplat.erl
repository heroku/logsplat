%%%-------------------------------------------------------------------
%% @copyright Heroku (2012)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc logsplat public API
%% @end
%%%-------------------------------------------------------------------
-module(logsplat).

-include_lib("ex_uri/include/ex_uri.hrl").

%% API
-export([ heroku_test/3
          ,heroku_test_req/3
        ]).

%%====================================================================
%% API
%%====================================================================

heroku_test(Count, Token, Url) when is_list(Url),
                                    is_integer(Count),
                                    Count > 0->
    RawReq = lsp_request:to_iolist(heroku_test_req(Count, Token, Url)),
    {ok, Uri = #ex_uri{}, _} = ex_uri:decode(Url),
    {ok, Pid} = lsp_http_client:start_link(Uri),
    {Pid,
     lsp_http_client:raw_request(Pid, RawReq, 5000)}.

heroku_test_req(Count, Token, Url) ->
    {ok,
     Uri0 = #ex_uri{authority = Auth},
     _} = ex_uri:decode(Url),
    NewAuth = Auth#ex_uri_authority{userinfo = "token:" ++ Token},
    Uri = Uri0#ex_uri{authority = NewAuth},
    Msgs = [ lsp_msg:heroku(now, Token, "console.1",
                            io_lib:format("Logsplat test message ~p from ~p.",
                                          [N, self()]))
             || N <- lists:seq(1, Count) ],
    lsp_request:new(Msgs, Uri).

%%====================================================================
%% Internal functions
%%====================================================================
