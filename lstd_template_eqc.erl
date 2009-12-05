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

-export([prop_string_empty_list/0]).

%% Test that no substitutions leave the string intact
prop_string_empty_list() ->
    ?FORALL(S, ql_gen:string(), S =:= lstd_template:string(S, [])).

