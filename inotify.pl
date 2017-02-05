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
            inotify_add_watch/4,           % +INotify, +Path, -Watch, +Options
            inotify_rm_watch/2,            % +INotify, +Watch
            inotify_read_event/3           % +INotify, -Event, +Options
          ]).
:- use_foreign_library(inotify4pl).

/** <module> Monitor file system changes

This library provides an  interface  to   the  Linux  _inotify_ API that
generates events for changes to the file system.
*/

%!  inotify_init(-INotify, +Options) is det.
%
%   Create an INotify object.  Options is currently ignored.

%!  inotify_close(+INotify) is det.
%
%   Close an INotify object.

%!  inotify_add_watch(+INotify, +Path, -Watch, +Options) is det.
%
%   Add a _watch_ to  an  INotify  object.   Path  is  either  a file or
%   directory. Options is a list of atoms  that create the watch _mask_.
%   These options are documented with inotify(7).  The Prolog version is
%   derived from the C macro name (e.g., IN_CLOSE_WRITE) by dropping IN_
%   and turning the remainder to lower case (e.g., close_write).

%!  inotify_rm_watch(+INotify, +Watch) is det.
%
%   Remove the indicated watch.

%!  inotify_read_event(+INotify, -Event, +Options) is semidet.
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

