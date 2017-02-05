:- use_module(inotify).

print_events(Dir) :-
    inotify_init(IN, []),
    inotify_add_watch(IN, Dir, [all]),
    repeat,
        (   inotify_read_event(IN, Ev, [])
        ->  writeln(Ev),
            fail
        ;   format('Timeout~n'),
            !
        ).

