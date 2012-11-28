%%%-------------------------------------------------------------------
%% @copyright Heroku (2012)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Logsplat message internals
%% @end
%%%-------------------------------------------------------------------
-module(lsp_msg).

%% API
-export([ new/0
          ,new/1
          ,new/8
          ,to_iolist/1
          ,heroku/4
        ]).

-record(rfc5424,
        {facility :: lsp_syslog_utils:facility(),
         severity :: lsp_syslog_utils:severity(),
         time :: iolist() | 'now',
         host :: iolist(),
         app_name :: iolist(),
         proc_id :: iolist(),
         msg_id :: iolist(),
         msg :: iolist()
        }).

-type msg() :: #rfc5424{}.

-export_type([ msg/0 ]).

%%====================================================================
%% API
%%====================================================================

new() ->
    new(local0, debug, now, "localhost", "erlang.1", undefined, undefined,
        "lsp_msg test message.").

new(Facility, Severity, Time, Host, AppName, ProcID, MsgID, Msg) ->
    #rfc5424{facility = Facility,
             severity = Severity,
             time = Time,
             host = Host,
             app_name = AppName,
             proc_id = ProcID,
             msg_id = MsgID,
             msg = Msg}.

new(Count) when is_integer(Count), Count > 0 ->
    [ new(local0, debug, now, "localhost",
          "erlang.1", undefined, integer_to_list(N),
          io_lib:format("lsp_msg test message ~p.", [N]))
      || N <- lists:seq(1, Count) ].

to_iolist(#rfc5424{facility = Facility,
                   severity = Severity,
                   time = Time,
                   host = Host,
                   app_name = AppName,
                   proc_id = ProcID,
                   msg_id = MsgID,
                   msg = RMsg}) ->
    lsp_syslog_utils:rfc5424(Facility, Severity, expand_time(Time),
                             Host, AppName, ProcID, MsgID, RMsg).

heroku(Time, Token, PS, Msg) ->
    new(local0, info, Time, "erlang", Token, PS, undefined, Msg).

%%====================================================================
%% Internal functions
%%====================================================================

expand_time(Time) when is_atom(Time);
                       is_tuple(Time) ->
    lsp_syslog_utils:datetime(Time);
expand_time(Time) -> Time.
