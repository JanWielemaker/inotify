/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(inotify,
          [ inotify_init/2,                % -INotify, +Options
            inotify_close/1,               % +INotify
            inotify_add_watch/3,           % +INotify, +Path, +Options
            inotify_rm_watch/2,            % +INotify, +Path
            inotify_read_event/3,          % +INotify, -Event, +Options
            inotify_current_watch/2        % ?INotify, ?Watch
          ]).

:- use_foreign_library(foreign(inotify4pl)).

/** <module> Monitor file system changes

This library provides an  interface  to   the  Linux  _inotify_ API that
generates events for changes to the file system. The interface exposes a
high level interface that is built on top of a complete encapsulation of
the low-level Linux inotify API. The   high level interface maintains an
admin of watched locations and  translates   the  events to the physical
file locations.
*/

:- dynamic  inotify/4.                     % INotify, Path, WatchID, Type
:- volatile inotify/4.

%!  inotify_init(-INotify, +Options) is det.
%
%   Create an INotify object.  Options is currently ignored.

%!  inotify_close(+INotify) is det.
%
%   Close an INotify object.

inotify_close(INotify) :-
    inotify_close_(INotify),
    retractall(inotify(INotify, _, _, _)).

%!  inotify_add_watch(+INotify, +Path, +Options) is det.
%
%   Add a watch for Path. This high  level interface maintains the known
%   watches.

inotify_add_watch(INotify, Spec, Options) :-
    spec_path(Spec, Path, Type),
    inotify_add_watch(INotify, Path, Watch, Options),
    assertz(inotify(INotify, Watch, Path, Type)).

spec_path(Spec, Path, directory) :-
    absolute_file_name(Spec, Path0, [file_errors(fail), file_type(directory)]),
    !,
    Path = Path0.
spec_path(Spec, Path, file) :-
    absolute_file_name(Spec, Path, [access(exist)]).


%!  inotify_add_watch(+INotify, +Path, -Watch, +Options) is det.
%
%   Add a _watch_ to  an  INotify  object.   Path  is  either  a file or
%   directory. Options is a list of atoms  that create the watch _mask_.
%   These options are documented with inotify(7).  The Prolog version is
%   derived from the C macro name (e.g., IN_CLOSE_WRITE) by dropping IN_
%   and turning the remainder to lower case (e.g., `close_write`).

%!  inotify_rm_watch(+INotify, +Watch) is semidet.
%
%   Remove the indicated watch. Watch is either the integer watch id, an
%   absolute path or a path specification used for inotify_add_watch/3.

inotify_rm_watch(INotify, Watch) :-
    integer(Watch), !,
    inotify_rm_watch_(INotify, Watch),
    retractall(inotify(INotify, Watch, _, _)).
inotify_rm_watch(INotify, Path) :-
    atom(Path),
    retract(inotify(INotify, Watch, Path, _Type)),
    !,
    inotify_rm_watch_(INotify, Watch).
inotify_rm_watch(INotify, Spec) :-
    spec_path(Spec, Path, Type),
    retract(inotify(INotify, Watch, Path, Type)),
    !,
    inotify_rm_watch_(INotify, Watch).

%!  inotify_current_watch(?INotify, ?Path) is nondet.
%
%   True if Path is watched by INotify.

inotify_current_watch(INotify, Path) :-
    inotify(INotify, _Watch, Path, _Type).

%!  inotify_read_event_(+INotify, -Event, +Options) is semidet.
%
%   Read and event from an INotify object.  Event is a term
%
%     - inotify(Watch, Event, Cookie, Object, Flags)
%       Where
%       - Watch is the (integer) watch reference
%       - Event is the event name
%       - Cookie is the associated cookie (for renaming)
%       - Object is one of `member(File)`, `directory` or `file`
%       - Flags is a list of one or more of the atoms `ignored`,
%         `q_overflow` or `unmount`.  Note that the `isdir` flag
%         defines the value for Object.
%
%   Options defined are:
%
%     - timeout(+Seconds)
%     If there is no event within Seconds, the predicate fails.
%     Handled through the poll() API, which implies a 1 millisecond
%     granularity and a max time of (2^31)/1000 seconds.

%!  inotify_read_event(+INotify, -Event, +Options) is semidet.
%
%   Read  and  event  from  an   INotify    object.   Event  is  a  term
%   Action(Object),   where   Object   is   one     of   file(File)   or
%   directory(Directory) and action is one of:
%
%     - access
%     File was accessed
%     - attrib
%     File metadata changed
%     - close_write
%     File opened for writing was closed.
%     - close_nowrite
%     File or directory not opened for writing was closed.
%     - create
%     File/directory  created in watched directory.
%     - delete
%     File/directory deleted from watched directory.
%     - delete_self
%     Watched file/directory was itself deleted.  The target is
%     automatically removed from the watched targets.
%     - modify
%     File was modified (e.g., write(2), truncate(2)).
%     - move_self
%     Watched file/directory was itself moved.  The admin for
%     inotify_current_watch/2 is updated to reflect the newly watched
%     object.
%     - moved_from
%     Generated for the directory containing the old filename when
%     a file is renamed.
%     - moved_to
%     Generated for the directory containing the new filename when
%     a file is renamed.
%     - open
%     File or directory was opened.

inotify_read_event(INotify, Event, Options) :-
    inotify_read_event_(INotify,
                        inotify(Watch, Action, Cookie, On0, Flags),
                        Options),
    on_object(On0, Watch, INotify, On),
    map_event(Action, Watch, Cookie, On, Flags, INotify, Event0),
    (   Event0 == again
    ->  inotify_read_event(INotify, Event, Options)
    ;   Event = Event0
    ).

on_object(file, Watch, INotify, file(File)) :-
    inotify(INotify, Watch, File, _Type).
on_object(directory, Watch, INotify, directory(Directory)) :-
    inotify(INotify, Watch, Directory, _Type).
on_object(unknown, Watch, INotify, Obj) :-
    inotify(INotify, Watch, Path, Type),
    Obj =.. [Type,Path].
on_object(member(File), Watch, INotify, file(Path)) :-
    inotify(INotify, Watch, Directory, _Type),
    atomic_list_concat([Directory, File], /, Path).

%!  map_event(+Action, +Watch, +Cookie, +On, +Flags, +INotify, -Event)

map_event(null, Watch, _Cookie, _Obj, Flags, INotify, again) :-
    memberchk(ignored, Flags),
    !,
    retractall(inotify(INotify, Watch, _, _)).
map_event(Action, _Watch, _Cookie, Obj, _Flags, _INotify, Event) :-
    Event =.. [Action,Obj].
