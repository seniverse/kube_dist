%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(kube_proxy).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-include_lib("kernel/include/logger.hrl").

-define(report_error(Error, Reason, Child, SupName),
        ?LOG_ERROR(#{label=>{supervisor,Error},
                     report=>[{supervisor,SupName},
                              {errorContext,Error},
                              {reason,Reason},
                              {offender,[{pid, Child}]}]},
                   #{domain=>[otp,sasl],
                     report_cb=>fun logger:format_otp_report/1,
                     logger_formatter=>#{title=>"SUPERVISOR REPORT"},
                     error_logger=>#{tag=>error_report,
                                     type=>supervisor_report}})).

start_link(ServerName, Service, Options) ->
    [Node, _] = binary:split(atom_to_binary(node(), unicode), <<"@">>),
    NodeName = proplists:get_value(nodename, Options, Node),
    Timeout = proplists:get_value(timeout, Options, 5000),
    Decay = proplists:get_value(decay, Options, 50000),
    Tau = float(erlang:convert_time_unit(Decay, millisecond, nanosecond)),
    gen_server:start_link({local, ServerName}, ?MODULE, [ServerName, Service, NodeName, Timeout, Tau], []).

init([ServerName, Service, NodeName, Timeout, Tau]) ->
    ets:new(
      ServerName,
      [set,
       public,
       named_table,
       {write_concurrency, true},
       {read_concurrency, true}]),

    State =
        #{name => ServerName,
          service => Service,
          node => NodeName,
          timeout => Timeout,
          tau => Tau,
          pids => #{}},

    kube_proxy_event:add_handler(self(), Service),
    case ets:lookup(kube_endpoints, Service) of
        [{_, Names}] ->
            update_endpoints(ServerName, NodeName, Names);
        _ ->
            ok
    end,
    {ok, State}.

handle_call(Request, From, State = #{name := Name, timeout := Timeout, tau := Tau, pids := Pids}) ->
    Token = seq_trace:get_token(),
    {Pid, Mref} =
        spawn_monitor(
          fun() ->
                  seq_trace:set_token(Token),
                  handle_request(Request, From, Name, Timeout, Tau)
          end),
    {noreply, State#{pids := Pids#{Pid => {Mref, From}}}}.

handle_cast({endpoints, Names}, State = #{name := ServerName, node := NodeName}) ->
    update_endpoints(ServerName, NodeName, Names),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', Mref, process, Pid, Reason}, State = #{name := Name, pids := Pids}) ->
    case maps:take(Pid, Pids) of
        {{Mref, {Caller, Ref}}, Pids1} ->
            case Reason of
                normal ->
                    ok;
                _ ->
                    ?report_error(child_terminated, Reason, Pid, Name),
                    Caller ! {'DOWN', Ref, process, Pid, Reason}
            end,
            {noreply, State#{pids := Pids1}};
        error ->
            {noreply, State}
    end;
handle_info(Info, State) ->
    ?LOG_DEBUG("Unknown message ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

update_endpoints(Tab, NodeName, Names) ->
    New = sets:from_list(
            [binary_to_atom(<<NodeName/binary, "@", Name/binary>>, unicode)
             || Name <- Names]),
    Current = sets:from_list([Name || {Name, _, _, _, _} <- ets:tab2list(Tab)]),
    Add = sets:to_list(sets:subtract(New, Current)),
    Delete = sets:to_list(sets:subtract(Current, New)),
    ets:insert(
      Tab,
      [{Key, make_ref(), 0, 1000000000.0, erlang:monotonic_time(nanosecond)}
       || Key <- Add ]),
    [ets:delete(Tab, Key) || Key <- Delete ],
    ok.

handle_request(Request, {Caller, Ref}, Name, Timeout, Tau) ->
    case ets:tab2list(Name) of
        [] ->
            Caller ! {'DOWN', Ref, process, Name, noproc};
        Endpoints ->
            {Node, Eref, _, _, _} = pick(Endpoints, Request),
            catch ets:update_counter(Name, Node, {3, 1}),
            StartTime = erlang:monotonic_time(nanosecond),
            Result = (catch gen:call({Name, Node}, '$gen_call', Request, Timeout)),
            EndTime = erlang:monotonic_time(nanosecond),
            Rtt =
                case Result of
                    {ok, Reply} ->
                        Caller ! {Ref, Reply},
                        float(max(EndTime - StartTime, 0));
                    {'EXIT', Reason} ->
                        Caller ! {'DOWN', Ref, process, Name, Reason},
                        float(Timeout * 1000000 * 3)
                end,
            update_cost(Name, Node, Eref, EndTime, Rtt, Tau)
    end.

update_cost(Tab, Node, Ref, EndTime, Rtt, Tau) ->
    case ets:lookup(Tab, Node) of
        [{Node, Ref, Pending, Cost, Timestamp}] ->
            Td = max(EndTime - Timestamp, 0),
            W = math:exp(-float(Td) / Tau),
            Cost1 =
                if Rtt > Cost ->
                        Rtt;
                   true ->
                        Cost * W + Rtt * (1.0 - W)
                end,
            case
                ets:select_replace(
                  Tab,
                  [{{Node, Ref, Pending, Cost, Timestamp},
                    [],
                    [{{Node, Ref, Pending - 1, Cost1, EndTime}}]}
                  ])
            of
                1 ->
                    ok;
                0 ->
                    update_cost(Tab, Node, Ref, EndTime, Rtt, Tau)
            end;
        _ ->
            ok
    end.


pick([Endpoint], _) ->
    Endpoint;
pick(Endpoints, _) ->
    p2c(Endpoints).


load({_, _, Pending, Cost, _}) when Cost == 0.0, Pending /= 0 ->
    Pending + 32767;
load({_, _, Pending, Cost, _}) ->
    Cost * (Pending + 1).


p2c(Endpoints) ->
    Length = length(Endpoints),
    A = rand:uniform(Length),
    case rand:uniform(Length - 1) of
        X when X >= A ->
            B = X + 1;
        B ->
            ok
    end,

    EndpointA = lists:nth(A),
    EndpointB = lists:nth(B),
    case load(EndpointA) =< load(EndpointB) of
        true ->
            EndpointA;
        false ->
            EndpointB
    end.
