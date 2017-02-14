:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for GSL Bindings").
% TODO: Share code for PPL, GMP, GSL.

% ===========================================================================

:- use_module(library(system), [find_executable/2]).

:- discontiguous(m_bundle_foreign_config_tool/3).

:- bundle_flag(enabled, [
    comment("Enable GSL bindings"),
    details(
      % .....................................................................
      "Set to \"yes\" if you wish to interface with the GSL (GNU Scientific\n"||
      "Library). If you choose to have the GSL interface, you should have the\n"||
      "GSL development library installed in the machine where you are\n"||
      "compiling and using it."),
    valid_values(['yes', 'no']),
    %
    default_comment("GSL detected"),
    default_value_comment(no,
        "GSL has not been detected.  If you want to use the math\n"||
        "library it is highly recommended that you stop the Ciao\n"||
        "configuration and install the GSL library first."),
    rule_default(HasGSL, has_gsl(HasGSL)),
    % rule_default('no'),
    %
    interactive([advanced])
]).

m_bundle_foreign_config_tool(ciao_gsl, gsl, 'gsl-config').

% TODO: it should consider auto_install option!
gsl_installed :-
	find_executable(~m_bundle_foreign_config_tool(ciao_gsl, gsl), _),
	% TODO: Next literal required because now GSL 32 bits is not available
	% TODO: in Linux 64 bits -- EMM.
	\+ get_platform('LINUXi686').

has_gsl(Value) :-
	( gsl_installed -> Value = yes ; Value = no ).

:- bundle_flag(auto_install, [
    comment("Auto-install GSL (third party)"),
    details([advanced],
      % .....................................................................
      "Set to \"yes\" if you want to auto-install GSL (third party)"),
    valid_values(['yes', 'no']),
    %
    rule_default('no'),
    %
    interactive([advanced])
]).

% ===========================================================================

:- use_module(ciaobld(messages_aux), [normal_message/2]).

:- use_module(library(file_utils), [string_to_file/2]).
:- use_module(library(pathnames), [path_relocate/4, path_concat/3]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(third_party_config), [foreign_config_var/4]).

% Specification of GSL (third-party component)
:- def_third_party(gsl, [
    version('1.16'),
    source_url(tar('http://ftp.gnu.org/gnu/gsl/gsl-1.16.tar.gz')),
    source_md5("e49a664db13d81c968415cd53f62bc8b"),
    %
    build_system(gnu_build_system)
]).

:- use_module(library(llists), [flatten/2]).

enabled := ~get_bundle_flag(ciao_gsl:enabled).
auto_install := ~get_bundle_flag(ciao_gsl:auto_install).

'$builder_hook'(prepare_build_bin) :-
	do_auto_install,
	prepare_bindings.

:- use_module(ciaobld(third_party_install), [auto_install/2]).
:- use_module(ciaobld(eng_defs), [bld_eng_path/3]).
:- use_module(ciaobld(builder_aux), [add_rpath/3]).

do_auto_install :-
	( auto_install(yes) -> 
	    normal_message("auto-installing GSL (third party)", []),
	    third_party_install:auto_install(ciao_gsl, gsl)
	; true
	).

prepare_bindings :-
	( enabled(yes) ->
	    normal_message("configuring GSL library", []),
	    S = ":- include(ciao_gsl(gsl_ciao)).\n",
 	    foreign_config_var(ciao_gsl, gsl, 'cflags', CompilerOpts),
 	    foreign_config_var(ciao_gsl, gsl, 'libs', LinkerOpts0),
	    fix_linker_opts(LinkerOpts0, LinkerOpts1),
	    ( auto_install(yes) ->
	        % If installed as a third party, add ./third-party/lib
	        % to the runtime library search path
	        add_rpath(local_third_party, LinkerOpts1, LinkerOpts2)
	    ; LinkerOpts2 = LinkerOpts1
	    ),
	    add_rpath(executable_path, LinkerOpts2, LinkerOpts),
	    T = ~flatten([
		    ":- extra_compiler_opts(\'"||CompilerOpts, "\').\n"||
		    ":- extra_linker_opts(\'"||LinkerOpts, "\').\n"]),
	    string_to_file(T, ~bundle_path(ciao_gsl, 'src/gsl_ciao_decl_auto.pl')),
	    % List of static libraries from GSL
	    % TODO: generalize for any other library
	    M = ~flatten(["ADD_STAT_LIBS=\'"||LinkerOpts, "\'\n"])
	;
	    LinkerOpts = "",
	    normal_message("ignoring GSL library", []),
	    S = ":- include(ciao_gsl(gsl_ciao_dummy)).\n",
	    M = ""
	),
        % TODO: See 'static_engine' at engine.hooks.pl for an example
        %   of engine that links against this library statically
	%
	% TODO: Simplify, generalize for other libs
	%
	EngOpts = [],
	GSLEng = eng_def(core, 'gsl', EngOpts),
	GSLCfgDir = ~bld_eng_path(cfgdir, GSLEng), % NOTE: not an engine
	mkpath(GSLCfgDir),
	string_to_file(M, ~path_concat(GSLCfgDir, 'config_sh')),
	string_to_file(S, ~bundle_path(ciao_gsl, 'src/ciao_gsl_auto.pl')).

:- use_module(library(lists), [append/3]).

% Remove the -L option, hack that allows to run in LINUXi686 or LINUXx86_64 --EMM:
fix_linker_opts(LinkerOpts0, LinkerOpts) :-
	( get_platform('LINUXx86_64') 
	; get_platform('LINUXi686') 
	),
	append("-L"||_, " "||LinkerOpts, LinkerOpts0),
	!.
fix_linker_opts(LinkerOpts, LinkerOpts).

