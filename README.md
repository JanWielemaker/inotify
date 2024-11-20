# SWI-Prolog pack to access the Linux inotify API

This pack makes the Linux  notification  API   for  changes  to the file
system available to SWI-Prolog.  It provides two libraries:

  - library(inotify)
    The core library that listens to inotify events
  - library(in_make)
    _Inotify make_.  This library provides in_make/0, which causes
	Prolog to listen for modified source files and automatically
	runs make/0 if any source file changes.


## Acknowledgements

The  development  of  this  pack  has   been  made  possible  by  [Kyndi
inc](http://www.kyndi.com/).
