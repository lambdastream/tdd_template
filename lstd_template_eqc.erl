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

-export([prop_string_empty_list/0, prop_tokens/0]).

%% Generates a non empty string
non_empty_string() ->
    ql_gen:non_empty_list(ql_gen:printable()).

%% Generates the internal representation of a string with substitutions
%% template() -> [{var, string()}, {text, string()}]
template() ->
    eqc_gen:list(
      eqc_gen:oneof([{var, ql_gen:string()}, {text, non_empty_string()}])).

%% Returns the string form from the internal representation of a template
to_string(Template) ->
    lists:concat(to_string_acc(Template)).

to_string_acc([]) ->
    [];
to_string_acc([{var, V}| T]) ->
    [lstd_string:format("@~s@", [V])| to_string_acc(T)];
to_string_acc([{text, S}| T]) ->
    [S| to_string_acc(T)].

%% Returns the expected token list from the internal representation of a
%% template
to_tokens(Template) ->
    lists:concat([to_tokens_acc(X) || X <- Template]).

to_tokens_acc({var, S}) ->
    [at, {string, S}, at];
to_tokens_acc({text, S}) ->
    [{string, S}].

%% Test that no substitutions leave the string intact
prop_string_empty_list() ->
    ?FORALL(S, ql_gen:string(), S =:= lstd_template:string(S, [])).

prop_tokens() ->
    ?FORALL(
       T, template(),
       to_tokens(T) == lstd_template:tokens(to_string(T))).
