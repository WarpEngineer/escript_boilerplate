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
%%% Copyright (c) 2016,2019 A. G. Madi 
%%% https://github.com/WarpEngineer/escript_boilerplate

-define( __MODULE_NAME__, escript_boilerplate ).

% Boilerplate version
-define( __BOILERPLATE_VERSION__, "2019.08.08" ).

% Set script version
-define( __version, "2019.08" ).

% The following can be used if the script is turned into a module.
-module( ?__MODULE_NAME__ ).
-mode( compile ).
-author( "A. G. Madi").
-vsn( ?__BOILERPLATE_VERSION__ ).

%%%  Define this list in order to provide available command line arguments
%%%  The list will also be used to create the usage/help output.
%%%  Another option is to create a usage string like the one defined below.
%%%  Only define one or the other. If this one is defined, USAGE_STR will be ignored.
%%%  Undefine by setting to 'undefined' as follows:
%%%      -define( USAGE_LIST, undefined ).
-define( USAGE_LIST,
	 [
%	  { short, long,         arg,   description,                                           required, default }
	  { "-f",  "--file",     true,  "Filename to process",                                 true,     undefined },
	  { "-t",  "--temp",     true,  "Location of tempfile",                                false,    "/tmp/bar" },
	  { "-v",  undefined,    false, "Enable verbose mode, print script as it is executed", false,    undefined },
	  { "-d",  "--debug",    false, "Enables debug mode",                                  false,    undefined },
	  { "-h",  "--help",     false, "This page",                                           false,    undefined },
	  { "-n",  "--no-color", false, "Disable color output",                                false,    undefined },
	  { "-1",  "--one",      false, "Do just one thing",                                   false,    undefined },
	  { "-V",  "--version",  false, "Show version and exit",                               false,    undefined }
	 ] ).

%%%  Define this usage string in order to provide available command line arguments.
%%%  The string will be parsed into a list like the one defined above.  The parsing
%%%  is not bulletproof so be precise in your syntax.
%%%  It is generally better in every case to define a usage list instead of a string, as shown
%%%  above.  Parsing of this string is provided here since it is available in the original
%%%  Bash boilerplate script on which this escript is based. 
%%%  Only define one or the other. If USAGE_LIST is defined, this one will be ignored.
%%%  Undefine by setting to 'undefined' as follows:
%%%      -define( USAGE_STR, undefined ).
-define( USAGE_STR, undefined ).
%-define( USAGE_STR, "
%  -f --file  [arg] Filename to process. Required.
%  -t --temp  [arg] Location of tempfile. Default=/tmp/bar
%  -v               Enable verbose mode, print script as it is executed
%  -d --debug       Enables debug mode
%  -h --help        This page
%  -n --no-color    Disable color output
%  -1 --one         Do just one thing
%  -V --version     Show version and exit
%" ).

-define( __helptext, "
 This is the escript boilerplate help text.  Feel free to add any description of your
 program or elaborate more on command-line arguments. This section is not
 parsed and will be added as-is to the help." ).

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
	{ OS1, OS2 } = os:type(),
	{ OSV1, OSV2, OSV3 } = os:version(),
	put( "OSTYPE", io_lib:format( "\"~s ~s ~b.~b.~b\"", [ OS1, OS2, OSV1, OSV2, OSV3 ] ) ),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logging functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec ebp_log( Log_Level :: atom(), Log_Message :: string() ) -> ok.
ebp_log( Log_Level, Log_Message ) ->
	{ Color, Color_Reset } =
	case get( "NO_COLOR" ) of
		true -> { "", "" };
		false ->
			case Log_Level of
				output    -> { ?COLOR_OUTPUT, ?COLOR_RESET };
				debug     -> { ?COLOR_DEBUG, ?COLOR_RESET };
				info      -> { ?COLOR_INFO, ?COLOR_RESET };
				notice    -> { ?COLOR_NOTICE, ?COLOR_RESET };
				warning   -> { ?COLOR_WARNING, ?COLOR_RESET };
				error     -> { ?COLOR_ERROR, ?COLOR_RESET };
				critical  -> { ?COLOR_CRITICAL, ?COLOR_RESET };
				alert     -> { ?COLOR_ALERT, ?COLOR_RESET };
				emergency -> { ?COLOR_EMERGENCY, ?COLOR_RESET };
				_         -> { ?COLOR_INFO, ?COLOR_RESET } % output a warning?
			end
	end,
	% output all to stderr unless it's 'output'
	{ { Y, M, D }, { H, Mn, S } } = calendar:universal_time(),	
	TimeStamp = io_lib:format( "~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b UTC", [ Y, M, D, H, Mn, S ] ),
	case Log_Level of
		output -> io:format( "~s ~s[~9.. s]~s ~s~n", [ TimeStamp, Color, Log_Level, Color_Reset, Log_Message ] );
		_      -> io:format( standard_error, "~s ~s[~9.. s]~s ~s~n", [ TimeStamp, Color, Log_Level, Color_Reset, Log_Message ] )
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Usage string parsing functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_arg_description( list() ) -> string().
get_arg_description( L ) ->
	string:join(
	  lists:takewhile( fun("Required" ++ _) -> false;
			      ("Default" ++ _) -> false;
			      (_) -> true end, L ), " "
	 ).	  

-spec get_arg_default( list() ) -> string().
get_arg_default( L ) ->
	case lists:filter( fun("Default=" ++ _) -> true;
			      ("default=" ++ _) -> true;
			      ( _ ) -> false end, L
			 ) of
		[] -> 
			undefined;
		[ Default ] -> 
			string:sub_word( Default, 2, $= )
	end.

-spec is_arg_required( list() ) -> true | false.
is_arg_required( L ) ->
	lists:member("required", lists:map(fun(X) -> string:strip(string:to_lower(X), right, $.) end, L)).

%%	  { short, long,         arg,   description,                                           required, default }
-spec parse_usage_string( list() ) -> list( tuple() ).
parse_usage_string( [ ] ) ->
	{};
parse_usage_string( [ Short = "-" ++ _S, Long = "--" ++ _L, "[arg]"  | T ] ) ->
	{ Short, Long, true, get_arg_description( T ), is_arg_required( T ), get_arg_default( T ) };
parse_usage_string( [ Short = "-" ++ _S, Long = "--" ++ _L | T ] ) ->
	{ Short, Long, false, get_arg_description( T ), is_arg_required( T ), get_arg_default( T ) };
parse_usage_string( [ Short = "-" ++ _S | T ] ) ->
	{ Short, undefined, false, get_arg_description( T ), is_arg_required( T ), get_arg_default( T ) };
parse_usage_string( _T ) ->
	{}.

-spec parse_usage_strings( list(), list( tuple() ) ) -> list( tuple() ).
parse_usage_strings( [  ], Acc ) ->
	Acc;
parse_usage_strings( [ H | T ], Acc ) ->
	case parse_usage_string( string:tokens( H, " ") ) of
		{} -> parse_usage_strings( T, Acc );
		Parsed -> parse_usage_strings( T, [ Parsed | Acc ] )
	end.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec pager( [ string() ] ) -> ok.
pager( Document ) ->
	%{ok,Rows} = io:rows(), % fails in recent versions of Erlang
	Rows = list_to_integer(string:trim(os:cmd("tput lines"))),
	case length(Document) of
		N when N > Rows - 1 ->
			{ Page, Rest } = lists:split( Rows - 1, Document ),
			lists:foreach(fun(X) -> io:format(standard_error, "~s~n", [ X  ] ) end, Page ),
			io:format(standard_error, "PRESS ENTER TO CONTINUE.....", []),
			%io:get_password(), % stopped working
			io:get_line(""),
			pager( Rest );
		_ ->
			lists:foreach(fun(X) -> io:format(standard_error, "~s~n", [ X ] ) end, Document)
	end.

-spec help( Help_Message :: string() ) -> ok.
help( Help_Message ) ->
	Out = [lists:flatten(io_lib:format("~n  ~s~n~n", [ Help_Message ]))] ++ usage() ++ string:tokens( ?__helptext, "\n" ),
	pager(Out),
	erlang:halt(1).

-spec usage( ) -> [ string() ].
usage() ->
	case get( "__usage" ) of
		undefined ->
			["No usage available"];
		Usage ->
			LongestLong = length( lists:foldr( fun( { _, undefined, _, _, _, _ }, Acc ) -> 
									   Acc;
							      ( { _, L, _, _, _, _ }, Acc ) ->
									   case length( L ) > length( Acc ) of
										   true -> L;
										   false -> Acc
									   end
							   end, "", Usage ) ),
			lists:reverse(lists:foldl( fun( { Short, Long, Arg, Description, Required, Default }, Acc ) ->
						       LongOpt = case Long of
									 undefined -> string:chars( 32, LongestLong );
									 _ -> Long ++ string:chars( 32, LongestLong - length( Long ) )
								 end,
						       ArgRequired = case Arg of
									     false -> "     ";
									     true  -> "[arg]"
								     end,
						       OptRequired = case Required of
									     false -> "";
									     true  -> "Required."
								     end,
						       DefaultStr = case Default of
									    undefined -> "";
									    _ -> Default
								    end,
						       [ lists:flatten(io_lib:format( "  ~s ~s ~s ~s ~s ~s", 
								  [ Short, LongOpt, ArgRequired, Description, OptRequired, DefaultStr ] )) | Acc ]
				       end, [], Usage )) ++ [""]
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parse command line options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_arg_info( string() ) -> tuple().
get_arg_info( Long = "--" ++ _Arg ) ->
	% look for an equal sign
	Name = string:sub_word( Long, 1, $= ),
	OptArg = string:sub_word( Long, 2, $= ),
	case lists:keyfind( Name, 2, get("__usage") ) of
		false -> help( io_lib:format( "Unknown argument: ~s", [ Long ] ) );
		Info -> erlang:append_element( Info, OptArg ) % append what's after the = sign, which could be empty.
	end;
get_arg_info( Short = "-" ++ _Arg ) ->
	case lists:keyfind( Short, 1, get("__usage") ) of
		false -> help( io_lib:format( "Unknown argument: ~s", [ Short ] ) );
		Info -> erlang:append_element( Info, [ ] ) % no = sign so just append an empty list.
	end;
get_arg_info( Arg ) ->
	help( io_lib:format( "Unknown argument: ~s", [ Arg ] ) ).

-spec parse_args( list() ) -> ok.
parse_args( [ ] ) ->
	ok;
parse_args( [ Arg = "-" ++ _R | T ] ) ->
	case get_arg_info( Arg ) of
		{ Short, Clean_Long, true, _, _, undefined, [ ] } ->
			% this option requires an argument but one was not supplied  with an = sign, and no default is available
			% take whatever is next in the list if there is something there
			case T of
				[ ] ->
					help( io_lib:format( "~s (~s) requires and argument", [ Short, Clean_Long ] ) );
				[ OptArg | Rest ] ->
					put( Short, OptArg ),
					parse_args( Rest )
			end;
		{ Short, _, true, _, _, Default, [ ] } ->
			% this option requires an argument and one was not supplied with an = sign, but a default is available
			% look at what's next in the list and if there's nothing there, use the default.
			case T of
				[ ] ->
					put( Short, Default ),
					parse_args( T );
				[ OptArg | Rest ] ->
					put( Short, OptArg ),
					parse_args( Rest )
			end;
		{ Short, _, true, _, _, _, Passed_Opt } -> 
			% this option requires an argument and one was supplied with an = sign.
			put( Short, Passed_Opt ),
			parse_args( T );
		{ Short, _, false, _, _, _, _ } -> 
			% this option does not require an argument, so just set it to true
			put( Short, true ),
			parse_args( T )
	end;
parse_args( [ H | T ] ) ->
	case get( "__args" ) of
		undefined -> put( "__args", [ H ] );
		Args -> put( "__args", [ H | Args ] )
	end,
	parse_args( T ).

-spec validate_required_args( list() ) -> ok.
validate_required_args( [  ] ) ->
	ok;
validate_required_args( [ { Short, Long, _, _, true, _ } | T ] ) ->
	case get( Short ) of
		undefined ->
			help( io_lib:format( "~s (~s) is required", [ Short, Long ] ) );
		_ ->
			ok
	end,
	validate_required_args( T );
validate_required_args( [ _ | T ] ) ->
	validate_required_args( T ).

demo() -> 
	put( "LOG_LEVEL", "7" ),
	ebp_info( io_lib:format( "__file: ~s", [ get( "__file" ) ] ) ),
	ebp_info( io_lib:format( "__dir: ~s", [ get( "__dir" ) ] ) ),
	ebp_info( io_lib:format( "__base: ~s", [ get( "__base" ) ] ) ),
	ebp_info( io_lib:format( "OSTYPE: ~s", [ get( "OSTYPE" ) ] ) ),
	ebp_info( io_lib:format( "arg_f: ~s", [ get( "-f" ) ] ) ),
	ebp_info( io_lib:format( "arg_d: ~s", [ get( "-d" ) ] ) ),
	ebp_info( io_lib:format( "arg_v: ~s", [ get( "-v" ) ] ) ),
	ebp_info( io_lib:format( "arg_h: ~s", [ get( "-h" ) ] ) ),
	ebp_output( "General output that goes to stdout regardless of log level" ),
	ebp_info( "Normal operational messages - may be harvested for reporting, measuring throughput, etc. - no action required." ),
	ebp_notice( "Events that are unusual but not error conditions - might be summarized in an email to developers or admins to spot potential problems - no immediate action required." ),
	ebp_warning( "Warning messages, not an error, but indication that an error will occur if action is not taken, e.g. file system 85% full - each item must be resolved within a given time. This is a debug message" ),
	ebp_error( "Non-urgent failures, these should be relayed to developers or admins; each item must be resolved within a given time." ),
	ebp_critical( "Should be corrected immediately, but indicates failure in a primary system, an example is a loss of a backup ISP connection." ),
	ebp_alert( "Should be corrected immediately, therefore notify staff who can fix the problem.An example would be the loss of a primary ISP connection." ),
	ebp_debug( "Debug messages. Will print when -d flag is set or LOG_LEVEL=7." ),
	ebp_emergency( "A \"panic\" condition usually affecting multiple apps/servers/sites. At this levelit would usually notify all tech staff on call." ).

% TODO: trap exit for cleanup function
main( Args ) ->
	setup_magic_and_environment(),
	parse_usage(),
	
	parse_args( Args ),

	% process command line switches
	case get( "-V" ) of
		true ->
			% version print mode
			io:format( standard_error, "Version: ~s~n", [ ?__version ] ),
			erlang:halt(1);
		_ ->
			ok
	end,
	case get( "-h" ) of
		true ->
			% help mode
			help( io_lib:format( "Help using ~s", [ get( "__file" ) ] ) );
		_ ->
			ok
	end,

	% validate required args are there
	validate_required_args( get( "__usage" ) ),
	
	% process more command line switches
	case get( "-d" ) of
		true ->
			% debug mode
			put( "LOG_LEVEL", "7" );
		_ ->
			ok
	end,
	case get( "-v" ) of
		true ->
			% verbose mode
			put( "VERBOSE", true );
		_ ->
			ok
	end,
	case get( "-n" ) of
		true ->
			% no color mode
			put( "NO_COLOR", true );
		_ ->
			ok
	end,


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%% Runtime
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	demo().
	
	

