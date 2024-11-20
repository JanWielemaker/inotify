/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2024, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(in_make,
          [ in_make/0,
            in_nomake/0
          ]).
:- use_module(library(inotify)).
:- use_module(library(debug)).
:- use_module(library(make)).
:- use_module(library(solution_sequences)).

/** <module> Automatically reload sources

This library simplifies the development cycle  by reloading source files
immediately when they are saved, regardless   of  the process that saved
the file (i.e., this may be an  external   editor).  To  use it, add the
following to the load file of your project or your `init.pl` file.

```
:- if(exists_source(library(in_make))).
:- use_module(library(in_make)).
:- initialization(in_make).
:- endif.
```

Note that the files are reloaded by   the `in_make` thread. If reloading
affects thread-local properties these may not be visible in all threads.
Examples are global variables, thread_local predicates and Prolog flags.
*/

:- dynamic
    in/1.

%!  in_make is det.
%
%   Setup monitoring all non-system loaded Prolog   source files and run
%   make/0 if any of them changes. If  a   file  from a new directory is
%   loaded, this directory is added to the set of monitored directories.
%
%   @see in_nomake/0 for stopping the service

in_make :-
    in(_),
    !,
    print_message(warning, in_make(running)).
in_make :-
    in_monitor(IN),
    thread_create(in_make_loop(IN), _, [alias(in_make)]).

in_monitor(IN) :-
    inotify_init(IN, []),
    asserta(in(IN)),
    forall(distinct(source_dir(Dir)),
           watch_prolog_dir(IN, Dir)).

watch_prolog_dir(_IN, Dir) :-
    current_prolog_flag(home, Home),
    sub_atom(Dir, 0, _, _, Home),
    !.
watch_prolog_dir(IN, Dir) :-
    inotify_add_watch(IN, Dir, [close_write]).

source_dir(Dir) :-
    source_file(File),
    file_directory_name(File, Dir).

in_make_loop(IN) :-
    repeat,
        (   inotify_read_event(IN, Ev, [])
        ->  debug(in_make(event), 'Ev: ~p', [Ev]),
            handle(Ev),
            fail
        ;   debug(in_make, 'Timeout~n', [])
        ).

handle(close_write(file(File))) =>
    source_file(File),
    make.
handle(Ev) =>
    debug(in_make(ignored), 'Ignored: ~p', [Ev]).

%!  in_nomake
%
%   Disable automatic reloading of modified source files.

in_nomake :-
    (   catch(thread_signal(in_make, abort), _, fail)
    ->  thread_join(in_make, _)
    ;   true
    ),
    retract(in(IN)),
    inotify_close(IN).

:- multifile user:message_hook/3.

user:message_hook(load_file(done(_Level,
                                 file(_File, Absolute),
                                 _Action,
                                 _LM,
                                 _TimeUsed,
                                 _ClausesCreated)),
                  _Kind, _Lines) :-
    in(IN),
    file_directory_name(Absolute, Dir),
    (   inotify_current_watch(IN, Dir)
    ->  true
    ;   watch_prolog_dir(IN, Dir)
    ),
    fail.

		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(in_make(running)) -->
    [ 'in_make/0: already running'-[] ].
