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
          [ in_make/0
          ]).
:- use_module(library(inotify)).
:- use_module(library(debug)).
:- use_module(library(make)).
:- use_module(library(solution_sequences)).

/** <module> Automatically reload sources
*/

:- dynamic
    in/1.

%!  in_make is det.
%
%   Setup monitoring all source file and run make/0 if any of them
%   changes.

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
           inotify_add_watch(IN, Dir, [close_write])).

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
    ;   inotify_add_watch(IN, Dir, [close_write])
    ),
    fail.

		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(in_make(running)) -->
    [ 'in_make/0: already running'-[] ].
