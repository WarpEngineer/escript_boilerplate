#!/usr/bin/env escript
%%% -*- erlang -*-
%%! -smp enable -sname escript_boilerplate -mnesia debug verbose

%%% - Demo file for escript boilerplate (change this for your script)
%%%   Change the 3rd line above as needed
%%%
%%% Usage:
%%%
%%%  LOG_LEVEL=7 ./main.escript -f /tmp/x -d (change this for your script)
%%%
%%% Inspired by, and based on, BASH3 Boilerplate template by Kevin van Zonneveld
%%%    ( http://bash3boilerplate.sh )
%%%
%%% Copyright (c) 2016 A. G. Madi 
%%% https://github.com/WarpEngineer/escript_boilerplate

% Boilerplate version
-define( __BOILERPLATE_VERSION__, "2016.11.7" ).

% Set script version
-define( __version, "2016.11" ).

% The following can be used if the script is turned into a module.
-author( "A. G. Madi").
-vsn( ?__BOILERPLATE_VERSION__ ).

% Set magic variables for current file, directory, os, etc
% Define the environment variables and their defaults
setup_magic_and_environment() ->
	

main([String]) ->
	    try
        N = list_to_integer(String),
	        F = fac(N),
	        io:format("factorial ~w = ~w\n", [N,F])
    catch
        _:_ ->
	            usage()
    end;
main(_) ->

% Set magic variables for current file, directory, os, etc
	{ok,__Dir} = file:get_cwd(),
	io:format("~p~n", [ init:get_plain_arguments() ] ),
	io:format("~p~n", [ ?__BOILERPLATE_VERSION__ ] ),
io:format("~p~n", [ ?FILE ] ),
io:format("~p~n", [ ?LINE ] ),
io:format("~p~n", [ ?MACHINE ] ),

io:format("~p~n", [ ?FUNCTION_NAME ] ),
io:format("~p~n", [ ?FUNCTION_ARITY ] ),

io:format("dir: ~p~n", [ __Dir ] ),


	    usage().

	usage() ->
	    io:format("usage: factorial integer\n"),
	    halt(1).

	fac(0) -> 1;
fac(N) -> N * fac(N-1).

