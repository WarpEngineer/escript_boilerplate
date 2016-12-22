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
-define( __BOILERPLATE_VERSION__, "2016.12.17" ).

% Set script version
-define( __version, "2016.12" ).

% The following can be used if the script is turned into a module.
-author( "A. G. Madi").
-vsn( ?__BOILERPLATE_VERSION__ ).

% define log colors
-define( COLOR_OUTPUT, "\x1b[36m" ).
-define( COLOR_DEBUG, "\x1b[35m" ).
-define( COLOR_INFO, "\x1b[32m" ).
-define( COLOR_NOTICE, "\x1b[34m" ).
-define( COLOR_WARNING, "\x1b[33m" ).
-define( COLOR_ERROR, "\x1b[31m" ).
-define( COLOR_CRITICAL, "\x1b[1;31m" ).
-define( COLOR_ALERT, "\x1b[1;33;41m" ).
-define( COLOR_EMERGENCY, "\x1b[1;4;5;33;41m" ).
-define( COLOR_RESET, "\x1b[0m" ).
 
% Set magic variables for current file, directory, os, etc
% Define the environment variables and their defaults
% Use process dictionary
-spec setup_magic_and_environment( ) -> term().
setup_magic_and_environment() ->
	{ ok, __Dir } = file:get_cwd(),
	put( "__dir", __Dir),
	put( "__file", filename:basename( ?FILE ) ),
	put( "__base", filename:basename( ?FILE, ".escript" ) ),
	case os:getenv( "LOG_LEVEL" ) of % 7 = debug -> 0 = emergency
		false -> put( "LOG_LEVEL", "6" );
		LEVEL -> put( "LOG_LEVEL", LEVEL )
	end,
	%TODO: look at tput colors too
	case os:getenv( "NO_COLOR" ) of % true = disable color. otherwise autodetected
		"true" -> put( "NO_COLOR", true );
		"false" -> put( "NO_COLOR", false );
		_ -> % do autodetect here
			case os:getenv( "TERM" ) of
				"xterm" -> put( "NO_COLOR", false );
				"xterm-256color" -> put( "NO_COLOR", false );
				"screen-256color" -> put( "NO_COLOR", false );
				"screen" -> put( "NO_COLOR", false );
				_ -> put( "NO_COLOR", true )
			end
	end.

% Functions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec ebp_log( Log_Level :: atom(), Log_Message :: string() ) -> ok.
ebp_log( Log_Level, Log_Message ) ->
	% TODO: if no color, make color_reset ""
	Color =
	case get( "NO_COLOR" ) of
		true -> "";
		false ->
			case Log_Level of
				output    -> ?COLOR_OUTPUT;
				debug     -> ?COLOR_DEBUG;
				info      -> ?COLOR_INFO;
				notice    -> ?COLOR_NOTICE;
				warning   -> ?COLOR_WARNING;
				error     -> ?COLOR_ERROR;
				critical  -> ?COLOR_CRITICAL;
				alert     -> ?COLOR_ALERT;
				emergency -> ?COLOR_EMERGENCY;
				_         -> ?COLOR_INFO % output a warning?
			end
	end,
	% output all to stderr unless it's 'output'
	% TODO: Add timestamp info line bash version
	case Log_Level of
		output -> io:format( "~s~s~s~n", [ Color, Log_Message, ?COLOR_RESET ] );
		_      -> io:format( standard_error, "~s~s~s~n", [ Color, Log_Message, ?COLOR_RESET ] )
	end.	  

-spec call_log( Log_Level :: atom(), Log_Message :: string(), In_Level :: string() ) -> ok.
call_log( Log_Level, Log_Message, In_Level ) ->
	case get( "LOG_LEVEL" ) of
		Level when Level >= In_Level ->
			ebp_log( Log_Level, Log_Message );
		_ ->
			ok
	end.
-spec ebp_emergency( Log_Message :: string() ) -> ok.
ebp_emergency( Log_Message ) -> ebp_log( emergency, Log_Message ), erlang:halt(1).
-spec ebp_alert( Log_Message :: string() ) -> ok.
ebp_alert( Log_Message ) -> call_log( alert, Log_Message, "1" ).
-spec ebp_critical( Log_Message :: string() ) -> ok.
ebp_critical( Log_Message ) -> call_log( critical, Log_Message, "2" ).
-spec ebp_error( Log_Message :: string() ) -> ok.
ebp_error( Log_Message ) -> call_log( error, Log_Message, "3" ).
-spec ebp_warning( Log_Message :: string() ) -> ok.
ebp_warning( Log_Message ) -> call_log( warning, Log_Message, "4" ).
-spec ebp_notice( Log_Message :: string() ) -> ok.
ebp_notice( Log_Message ) -> call_log( notice, Log_Message, "5" ).
-spec ebp_info( Log_Message :: string() ) -> ok.
ebp_info( Log_Message ) -> call_log( info, Log_Message, "6" ).
-spec ebp_debug( Log_Message :: string() ) -> ok.
ebp_debug( Log_Message ) -> call_log( debug, Log_Message, "7" ).
-spec ebp_output( Log_Message :: string() ) -> ok.
ebp_output( Log_Message ) -> ebp_log( output, Log_Message ).


%%%  Define this list in order to provide available command line arguments.
%%%  The list will also be used to create the usage/help output.
%%%  Another option is to create a usage string like the one defined below.
%%%  Only define one or the other. If this one is defined, USAGE_STR will be ignored.
%%%  Undefine by setting to 'undefined' as follows:
%%%      -define( USAGE_LIST, undefined ).
%%%     TODO: put this back when done testing.
-define( USAGE_LIST, undefined ).
%-define( USAGE_LIST,
%	 [
%%	  { short, long,         arg,   description,                                           required, default }
%	  { "-f",  "--file",     true,  "Filename to process",                                 true,     undefined },
%	  { "-t",  "--temp",     true,  "Location of tempfile",                                false,    "/tmp/bar" },
%	  { "-v",  undefined,    false, "Enable verbose mode, print script as it is executed", false,    undefined },
%	  { "-d",  "--debug",    false, "Enables debug mode",                                  false,    undefined },
%	  { "-h",  "--help",     false, "This page",                                           false,    undefined },
%	  { "-n",  "--no-color", false, "Disable color output",                                false,    undefined },
%	  { "-1",  "--one",      false, "Do just one thing",                                   false,    undefined },
%	  { "-V",  "--version",  false, "Show version and exit",                               false,    undefined }
%	 ] ).

%%%  Define this usage string in order to provide available command line arguments.
%%%  The string will be parsed into a list like the one defined above.  The parsing
%%%  is not bulletproof so be precise in your syntax.
%%%  It is generally better in every case to define a usage list instead of a string, as shown
%%%  above.  Parsing of this string is provided here since it is available in the original
%%%  Bash boilerplate script on which this escript is based. 
%%%  Only define one or the other. If USAGE_LIST is defined, this one will be ignored.
%%%  Undefine by setting to 'undefined' as follows:
%%%      -define( USAGE_STR, undefined ).
%-define( USAGE_STR, undefined ).
-define( USAGE_STR, "
  -f --file  [arg] Filename to process. Required.
  -t --temp  [arg] Location of tempfile. Default=\"/tmp/bar\"
  -v               Enable verbose mode, print script as it is executed
  -d --debug       Enables debug mode
  -h --help        This page
  -n --no-color    Disable color output
  -1 --one         Do just one thing
  -V --version     Show version and exit
" ).

%%	  { short, long,         arg,   description,                                           required, default }
-spec parse_usage_string( list() ) -> list( tuple() ).
parse_usage_string( [ ] ) ->
	io:format( "Got-> empty!~n", [  ] ),
	{};
parse_usage_string( [ "-" ++ S, "--" ++ L, "[arg]"  | T ] ) ->
	{ S, L, true, T, undefined, undefined }; % TODO: fix last two
parse_usage_string( [ "-" ++ S, "--" ++ L | T ] ) ->
	{ S, L, false, T, undefined, undefined }; % TODO: fix last two
parse_usage_string( [ "-" ++ S | T ] ) ->
	{ S, undefined, false, T, undefined, undefined }; % TODO: fix last two
parse_usage_string( S ) ->
	{}.

-spec parse_usage_strings( list(), list( tuple() ) ) -> list( tuple() ).
parse_usage_strings( [  ], Acc ) ->
	Acc;
parse_usage_strings( [ H | T ], Acc ) ->
	Parsed = parse_usage_string( string:tokens( H, " ") ),
	parse_usage_strings( T, [ Parsed | Acc ] ).

-spec parse_usage( ) -> ok.
parse_usage( ) ->
	% if USAGE_LIST is defined, ignore USAGE_STR.
	case ?USAGE_LIST of
		undefined ->
			case ?USAGE_STR of
				undefined ->
					ebp_emergency( "One of USAGE_STR or USAGE_LIST is required to proceed" );
				_Usage_str ->
					put( "__usage", parse_usage_strings( string:tokens( ?USAGE_STR, "\n" ), [ ] ) )
			end;
		_Usage ->
			put( "__usage", ?USAGE_LIST )
	end.

-spec help( ) -> ok.
help( ) ->
	help( "" ).
-spec help( Help_Message :: string() ) -> ok.
help( Help_Message ) ->
	io:format( standard_error, "~n~s~n~n", [ Help_Message ] ),
	usage(),
	case get( "__helptext" ) of
		undefined -> ok;
		HelpText -> io:format( standard_error, "~s~n~n", [ HelpText ] )
	end,
	erlang:halt(1).

-spec usage( ) -> ok.
usage() ->
	case get( "__usage" ) of
		undefined -> io:format( standard_error, "No usage available~n~n", [ ] );
		Usage -> io:format( standard_error, "~p~n~n", [ Usage ] )
	end.

% Parse command line options
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_args( [ ] ) ->
	ok;
parse_args( [ "--" ++ Arg | T ] ) ->
	Name = string:sub_word( Arg, 1, $= ),
	Option = string:sub_word( Arg, 2, $= ),
	io:format( "got long arg: ~p~n", [ Name ] ),
	io:format( "got long arg option: ~p~n", [ Option ] ),
	parse_args( T );
parse_args( [ "-" ++ Arg | T ] ) ->
	io:format( "got short arg: ~p~n", [ Arg ] ),
	[ Option | Rest ] = T,
	io:format( "got short arg option: ~p~n", [ Option ] ),
	parse_args( Rest );
parse_args( [ H | T ] ) ->
	io:format( "got arg: ~p~n", [ H ] ),
	parse_args( T ).

main(["test"]) -> 
	P = erlang:open_port({spawn, "bash -c test -t 1"}, [ exit_status ] ),
	receive 
		{ P, { exit_status, Data } } -> io:format("Data ~p~n", [ Data ] )
	end,
	setup_magic_and_environment(),
	parse_usage(),
	io:format( "dir: ~p~n", [ get("__dir") ] ),
	io:format( "file: ~p~n", [ get("__file") ] ),
	io:format( "base: ~p~n", [ get("__base") ] ),
	io:format( "log level: ~p~n", [ get("LOG_LEVEL") ] ),
	io:format( "color?: ~p~n", [ get("NO_COLOR") ] ),
	io:format( "term: ~p~n", [ os:getenv("TERM") ] ),
	{ { USAGE0, _, _, _ } } = ?USAGE_LIST,
	io:format( "usage0: ~p~n", [ USAGE0 ] ),
	ebp_alert( "alert" ),
	ebp_critical( "critical" ),
	ebp_error( "error" ),
	ebp_warning( "warning" ),
	ebp_notice( "notice" ),
	ebp_info( "info" ),
	ebp_debug( "debug" ),
	ebp_output( "output" ),
	help( "help test" ),
	ebp_emergency( "emergency" ),
	ok;

main( Args ) ->
	setup_magic_and_environment(),
	parse_usage(),
	parse_args( Args ),

	
	
%	io:format("~p~n", [ init:get_arguments() ] ),
%	io:format("~p~n", [ init:get_plain_arguments() ] ),
	io:format("~p~n", [ ?__BOILERPLATE_VERSION__ ] ),
	io:format("~p~n", [ ?LINE ] ),
	io:format("~p~n", [ ?MACHINE ] ), 
%	io:format("~p~n", [ ?FUNCTION_NAME ] ),
%	io:format("~p~n", [ ?FUNCTION_ARITY ] ),
%	io:format("~p~n", [ string:tokens( ?USAGE_STR, "\n" ) ] ),
	usage().



