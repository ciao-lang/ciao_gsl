:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for GSL Bindings").
% TODO: Share code for PPL, GMP, GSL.

% ===========================================================================

:- use_module(ciaobld(messages_aux), [normal_message/2]).

:- use_module(library(pathnames), [path_relocate/4, path_concat/3]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(third_party_config), [foreign_config_atmlist/4]).
:- use_module(library(system), [find_executable/2]).

m_bundle_foreign_config_tool(ciao_gsl, gsl, 'gsl-config').

% Config flags for GSL (third-party component)
:- third_party_flags([
    name("GSL (third party)"), 
    bindings_name("GSL bindings"),
    allow_auto_install, % allow auto-installation
    allow_dummy % allow dummy bindings (if enabled=no)
]).

third_party_preinstalled(ciao_gsl) :-
    find_executable(~m_bundle_foreign_config_tool(ciao_gsl, gsl), _).

% Specification of GSL (third-party component)
:- def_third_party(gsl, [
    %version('1.16'),
    %source_url(tar('http://ftp.gnu.org/gnu/gsl/gsl-1.16.tar.gz')),
    %source_md5("e49a664db13d81c968415cd53f62bc8b"),
    version('2.6'),
    source_url(tar('http://ftp.gnu.org/gnu/gsl/gsl-2.6.tar.gz')),
    source_md5("bda73a3dd5ff2f30b5956764399db6e7"),
    %
    build_system(gnu_build_system)
]).

enabled := ~get_bundle_flag(ciao_gsl:enabled).
auto_install := ~get_bundle_flag(ciao_gsl:auto_install).

'$builder_hook'(prepare_build_bin) :-
    do_auto_install,
    prepare_bindings.

:- use_module(ciaobld(third_party_install), [auto_install/2]).
:- use_module(ciaobld(builder_aux), [add_rpath/3]).
:- use_module(ciaobld(builder_aux), [update_file_from_clauses/3]).
:- use_module(ciaobld(builder_aux), [update_stat_config_sh/2]).

do_auto_install :-
    ( auto_install(yes) -> 
        normal_message("auto-installing GSL (third party)", []),
        third_party_install:auto_install(ciao_gsl, gsl)
    ; true
    ).

prepare_bindings :-
    ( enabled(yes) ->
        normal_message("configuring GSL interface", []),
        S = [(:- include(ciao_gsl(gsl_ciao)))],
        foreign_config_atmlist(ciao_gsl, gsl, 'cflags', CompilerOpts),
        foreign_config_atmlist(ciao_gsl, gsl, 'libs', LinkerOpts1),
        ( auto_install(yes) ->
            % If installed as a third party, add ./third-party/lib
            % to the runtime library search path
            add_rpath(local_third_party, LinkerOpts1, LinkerOpts2)
        ; LinkerOpts2 = LinkerOpts1
        ),
        add_rpath(executable_path, LinkerOpts2, LinkerOpts),
        update_file_from_clauses([
            (:- extra_compiler_opts(CompilerOpts)),
            (:- extra_linker_opts(LinkerOpts))
          ], ~bundle_path(ciao_gsl, 'src/gsl_ciao_decl_auto.pl'), _)
    ; normal_message("ignoring GSL interface", []),
      LinkerOpts = [],
      S = [(:- include(ciao_gsl(gsl_ciao_dummy)))]
    ),
    %
    update_file_from_clauses(S, ~bundle_path(ciao_gsl, 'src/ciao_gsl_auto.pl'), _),
    %
    % TODO: See 'static_engine' at engine.hooks.pl for an example
    %   of engine that links against this library statically
    %
    % TODO: Simplify, generalize for other libs
    %
    GSLEng = eng_def(core, 'gsl', []), % NOTE: not really an engine
    update_stat_config_sh(GSLEng, LinkerOpts).
