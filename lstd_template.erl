%%%-------------------------------------------------------------------
%%% @author Samuel <samuel.rivas@lambdastream.com>
%%% @copyright (C) 2009, Samuel
%%% @doc A module to handel string and file templates
%%%
%%% @version  {@vsn}, {@date} {@time}
%%%
%%% @end
%%% Created : 16 Nov 2009 by Samuel Rivas <samuel.rivas@lambdastream.com>
%%%-------------------------------------------------------------------
-module(lstd_template).

-export([string/2]).

%% For tests
-export([tokens/1]).

string(String, _Subs) ->
    String.

tokens([]) ->
    [];
tokens([$@ | T]) ->
    [at | tokens(T)];
tokens(S) ->
    {String, T} = lists:split(string:cspan(S, "@"), S),
    [{string, String} | tokens(T)].

