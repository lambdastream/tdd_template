%%%-------------------------------------------------------------------
%%% @author Samuel <samuelrivas@gmail.com>
%%% @copyright (C) 2009, Samuel
%%% @doc QuickCheck tests for lstd_template
%%%
%%% @end
%%% Created : 16 Nov 2009 by Samuel <samuelrivas@gmail.com>
%%%-------------------------------------------------------------------
-module(lstd_template_eqc).
-include_lib("eqc/include/eqc.hrl").

-export([prop_string_empty_list/0, prop_tokens/0, prop_parse/0, prop_string/0]).

%% Generates a non empty string
%% Any printable char but @
valid_char() ->
    ?SUCHTHAT(C, ql_gen:printable(), C =/= $@).

%% Non-empty string without any @
valid_string() ->
    ql_gen:non_empty_list(valid_char()).

%% Generates the internal representation of a string with substitutions
%% template() -> [{var, Variable::string(), Value::string()}, {text, string()}]
template() ->
    ?LET(
       {Text, VarList}, {eqc_gen:list(text()), var_list()},
       ?LET(
          RepeatedVars, repeated_vars(VarList),
          ?LET(
             Template, ql_gen:permutation(Text ++ VarList ++ RepeatedVars),
             fold_text(Template)))).

repeated_vars([]) ->
    [];
repeated_vars(VarList) ->
    eqc_gen:list(eqc_gen:elements(VarList)).

text() ->
    eqc_gen:frequency(
      [{5, {text, valid_string()}},
       {1, escaped_at}]).

var() ->
    {var, var_name(), ql_gen:string()}.

%% Make it easy to generate duplicated names, but also try with wicked, weird
%% strings
var_name() ->
    eqc_gen:frequency(
      [{1, valid_string()},
       {5, eqc_gen:vector(2, eqc_gen:elements(lists:seq($a, $d)))}]).

%% Generate a list of variables without duplicated keys
var_list() ->
    ?LET(L, eqc_gen:list(var()), remove_duplicated_keys(L)).

remove_duplicated_keys([]) ->
    [];
remove_duplicated_keys([H = {var, Name, _} | T]) ->
    case lists:keymember(Name, 2, T) of
        true ->
            remove_duplicated_keys(T);
        false ->
            [H | remove_duplicated_keys(T)]
    end.

%% Change sequences like [{text, "a"}, {text, "b"}] in [{text, "ab"}]
fold_text([{text, A}, {text, B} | T]) ->
    fold_text([{text, A ++ B} | T]);
fold_text([H | T]) ->
    [H | fold_text(T)];
fold_text([]) ->
    [].


%% Returns the string form from the internal representation of a template
to_string(Template) ->
    lists:concat([to_string_acc(X) || X <- Template]).

to_string_acc(escaped_at) ->
    "@@";
to_string_acc({var, V, _}) ->
    lstd_string:format("@~s@", [V]);
to_string_acc({text, S}) ->
    S.

%% Returns the expected token list from the internal representation of a
%% template
to_tokens(Template) ->
    lists:concat([to_tokens_acc(X) || X <- Template]).

to_tokens_acc({var, S, _}) ->
    [at, {string, S}, at];
to_tokens_acc(escaped_at) ->
    [at, at];
to_tokens_acc({text, S}) ->
    [{string, S}].

to_parsed(Template) ->
    [to_parsed_acc(X) || X <- Template].

to_parsed_acc({var, Name, _Value}) -> {var, Name};
to_parsed_acc(escaped_at) -> {text, "@"};
to_parsed_acc({text, S}) -> {text, S}.

%% Returns the expected result, after substituting variables by their values
to_result(Template) ->
    lists:concat([to_result_acc(X) || X <- Template]).

to_result_acc({var, _, S}) -> S;
to_result_acc(escaped_at) -> "@";
to_result_acc({text, S}) -> S.

to_substs([]) ->
    [];
to_substs([escaped_at | T]) ->
    to_substs(T);
to_substs([{var, Name, Value} | T]) ->
    [{Name, Value} | to_substs(T)];
to_substs([{text, _} | T]) ->
    to_substs(T).

%% Test that no substitutions leave the string intact
prop_string_empty_list() ->
    ?FORALL(
       S, valid_string(),
       try
           S =:= lstd_template:string(S, [])
       catch
          Error:Reason ->
               ?WHENFAIL(io:format("Error ~p:~p~n", [Error, Reason]), false)
       end).

prop_tokens() ->
    ?FORALL(
       T, template(),
       to_tokens(T) == lstd_template:tokens(to_string(T))).

prop_parse() ->
    ?FORALL(
       T, template(),
       to_parsed(T) == lstd_template:parse(lstd_template:tokens(to_string(T)))).

prop_string() ->
    ?FORALL(
       T, template(),
       ?LET(
	  {Substs, String, Expected},
	  {to_substs(T), to_string(T), to_result(T)},
	  ?LET(
	     Result, lstd_template:string(String, Substs),
	     ?WHENFAIL(
		io:format(
		  "~nTemplate: ~p~n"
		  "Substs  : ~p~n"
		  "Expected: ~p~n"
		  "Got     : ~p~n",
		  [String, Substs, Expected, Result]),
		Expected =:= Result)))).
