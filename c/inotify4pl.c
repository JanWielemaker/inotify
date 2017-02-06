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

#define PL_ARITY_AS_SIZE
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <sys/inotify.h>
#include <poll.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#define IBUF_SIZE 4096

typedef struct inref
{ atom_t         symbol;		/* associated symbol */
  int		 fd;			/* inotify descriptor */
  size_t	 len;
  struct inotify_event	*ev;
  char		 buf[IBUF_SIZE];	/* read buffer */
} inref;


		 /*******************************
		 *	 SYMBOL REFERENCES	*
		 *******************************/

static int
write_inref(IOSTREAM *s, atom_t eref, int flags)
{ inref **refp = PL_blob_data(eref, NULL, NULL);
  inref *ref = *refp;
  (void)flags;

  Sfprintf(s, "<inotify>(%p)", ref);
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GC an inotify from the atom garbage collector.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
release_inref(atom_t aref)
{ inref **refp = PL_blob_data(aref, NULL, NULL);
  inref *ref   = *refp;

  if ( (ref->fd >= 0) )
    close(ref->fd);

  PL_free(ref);

  return TRUE;
}


static void
acquire_inref(atom_t aref)
{ inref **refp = PL_blob_data(aref, NULL, NULL);
  inref *ref   = *refp;

  ref->symbol = aref;
}


static int
save_inotify(atom_t aref, IOSTREAM *fd)
{ inref **refp = PL_blob_data(aref, NULL, NULL);
  inref *ref   = *refp;
  (void)fd;

  return PL_warning("Cannot save reference to <inotify>(%p)", ref);
}


static atom_t
load_inotify(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-inotify>");
}


static PL_blob_t inotify_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "inotify",
  release_inref,
  NULL,
  write_inref,
  acquire_inref,
  save_inotify,
  load_inotify
};


static int
unify_inotify(term_t t, inref *er)
{ if ( er->symbol )
  { return PL_unify_atom(t, er->symbol);
  } else
  { return PL_unify_blob(t, &er, sizeof(er), &inotify_blob);
  }
}


static int
get_inotify(term_t t, inref **erp, int warn)
{ void *data;
  size_t len;
  PL_blob_t *type;

  if ( PL_get_blob(t, &data, &len, &type) && type == &inotify_blob )
  { inref **erd = data;
    inref *er = *erd;

    if ( er->fd >= 0 )
    { *erp = er;
      return TRUE;
    } else if ( warn )
    { PL_existence_error("inotify", t);
    }
  }

  if ( warn )
    PL_type_error("inotify", t);

  return FALSE;
}


		 /*******************************
		 *	      TRANSLATE		*
		 *******************************/

typedef struct wmask
{ uint32_t	mask;			/* Associated mask */
  const char   *flags;
  const char   *name;			/* Name of the mask */
  atom_t	atom;			/* Atom handle */
} wmask;

static wmask masks[] =
{ { IN_ACCESS,	      "+",  "access" },
  { IN_ATTRIB,	      "*",  "attrib" },
  { IN_CLOSE_WRITE,   "+",  "close_write" },
  { IN_CLOSE_NOWRITE, "*",  "close_nowrite" },
  { IN_CREATE,	      "+",  "create" },
  { IN_DELETE,	      "+",  "delete" },
  { IN_DELETE_SELF,   "",   "delete_self" },
  { IN_MODIFY,	      "+",  "modify" },
  { IN_MOVE_SELF,     "",   "move_self" },
  { IN_MOVED_FROM,    "+",  "moved_from" },
  { IN_MOVED_TO,      "+",  "moved_to" },
  { IN_OPEN,	      "*",  "open" },
  { IN_ALL_EVENTS,    NULL, "all" },
  { IN_DONT_FOLLOW,   NULL, "dont_follow" },
  { IN_EXCL_UNLINK,   NULL, "excl_unlink" },
  { IN_MASK_ADD,      NULL, "mask_add" },
  { IN_ONESHOT,       NULL, "oneshot" },
  { IN_ONLYDIR,       NULL, "onlydir" },
  { 0,		      NULL, NULL },
};


		 /*******************************
		 *	 PROLOG CONNECTION	*
		 *******************************/

static atom_t ATOM_directory;
static atom_t ATOM_file;
static atom_t ATOM_timeout;
static atom_t ATOM_unknown;

static functor_t FUNCTOR_member1;
static functor_t FUNCTOR_inotify5;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_inotify_error2;

static int
inotify_error(inref *ref)
{ term_t ex, in;

  return ( (ex = PL_new_term_ref()) &&
	   (in = PL_new_term_ref()) &&
	   unify_inotify(in, ref) &&
	   PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
			       PL_FUNCTOR, FUNCTOR_inotify_error2,
			         PL_TERM, in,
			         PL_CHARS, strerror(errno),
			       PL_VARIABLE) &&
	   PL_raise_exception(ex)
	 );
}


static foreign_t
pl_inotify_init(term_t inotify, term_t options)
{ int fd;

  if ( (fd=inotify_init1(IN_CLOEXEC)) >= 0 )
  { inref *ref;

    if ( (ref=PL_malloc(sizeof(*ref))) )
    { memset(ref, 0, sizeof(*ref));
      ref->fd = fd;

      if ( unify_inotify(inotify, ref) )
	return TRUE;
      close(fd);
      PL_free(ref);
      return FALSE;
    }
    close(fd);
    return PL_resource_error("memmory");
  }

  return inotify_error(NULL);
}

static foreign_t
pl_inotify_close(term_t inotity)
{ inref *ref;

  if ( get_inotify(inotity, &ref, TRUE) )
  { close(ref->fd);
    ref->fd = -1;
    return TRUE;
  }

  return FALSE;
}


static int
get_mask(term_t t, uint32_t *mp)
{ atom_t name;

  if ( PL_get_atom_ex(t, &name) )
  { wmask *wm;

    for(wm=masks; wm->name; wm++)
    { if ( !wm->atom )
	wm->atom = PL_new_atom(wm->name);
      if ( wm->atom == name )
      { *mp = wm->mask;
	return TRUE;
      }
    }

    return PL_domain_error("inotify_event", t);
  }

  return FALSE;
}


static atom_t
in_mask_name(uint32_t mask)
{ wmask *wm;
  static atom_t ATOM_null = 0;

  for(wm=masks; wm->flags; wm++)
  { if ( (wm->mask & mask) )
    { if ( !wm->atom )
	wm->atom = PL_new_atom(wm->name);
      return wm->atom;
    }
  }

  if ( !ATOM_null )
    ATOM_null = PL_new_atom("null");

  return ATOM_null;
}



static foreign_t
pl_inotify_add_watch(term_t inotity, term_t path, term_t watch, term_t events)
{ inref *ref;
  char *fname;

  if ( get_inotify(inotity, &ref, TRUE) &&
       PL_get_file_name(path, &fname,
			PL_FILE_OSPATH) )
  { int rc;
    uint32_t mask = 0;
    term_t tail = PL_copy_term_ref(events);
    term_t head = PL_new_term_ref();

    while(PL_get_list(tail, head, tail))
    { uint32_t m;

      if ( get_mask(head, &m) )
	mask |= m;
      else
	return FALSE;
    }
    if ( !PL_get_nil_ex(tail) )
      return FALSE;

    if ( (rc=inotify_add_watch(ref->fd, fname, mask)) >= 0 )
    { return PL_unify_integer(watch, rc);
    }

    return inotify_error(ref);
  }

  return FALSE;
}


static foreign_t
pl_inotify_rm_watch(term_t inotity, term_t watch)
{ inref *ref;
  int w;

  if ( get_inotify(inotity, &ref, TRUE) &&
       PL_get_integer_ex(watch, &w) )
  { if ( inotify_rm_watch(ref->fd, w) == 0 )
      return TRUE;

    return inotify_error(ref);
  }

  return FALSE;
}

typedef struct inflag
{ uint32_t    mask;
  const char *name;
  atom_t      atom;
} inflag;

static inflag inflags[] =
{ { IN_IGNORED,    "ignored" },
  { IN_Q_OVERFLOW, "q_overflow" },
  { IN_UNMOUNT,    "unmount" },
  { 0,		   NULL }
};

static term_t
put_flags(uint32_t mask)
{ inflag *ifg;
  term_t t, h;

  if ( !(t = PL_new_term_ref()) ||
       !(h = PL_new_term_ref()) ||
       !PL_put_nil(t) )
    return 0;

  for(ifg=inflags; ifg->name; ifg++)
  { if ( (mask & ifg->mask) )
    { if ( !ifg->atom )
	ifg->atom = PL_new_atom(ifg->name);
      if ( !PL_put_atom(h, ifg->atom) ||
	   !PL_cons_list(t, h, t) )
	return 0;
    }
  }

  return t;
}


static int
put_in_event(term_t t, const struct inotify_event *ev)
{ atom_t mask = in_mask_name(ev->mask);
  term_t name;
  term_t flags;
  atom_t type;

  if ( (ev->mask & IN_ISDIR) )
    type = ATOM_directory;
  else if ( (ev->mask & (IN_IGNORED|IN_DELETE_SELF|IN_MOVE_SELF)) )
    type = ATOM_unknown;
  else
    type = ATOM_file;

  return ( (flags = put_flags(ev->mask)) &&
           (name = PL_new_term_ref()) &&
	   (ev->len ? PL_unify_term(name, PL_FUNCTOR, FUNCTOR_member1,
					    PL_MBCHARS, ev->name)
	            : PL_unify_atom(name, type)) &&
	   PL_unify_term(t, PL_FUNCTOR, FUNCTOR_inotify5,
		              PL_INT, ev->wd,
		              PL_ATOM, mask,
		              PL_INT64, (int64_t) ev->cookie,
		              PL_TERM, name,
			      PL_TERM, flags)
	 );
}


#define addPointer(p, n) (void*)((char*)(p)+(n))
#define nextEv(ev)	 addPointer((ev), sizeof(*(ev))+(ev)->len)

static foreign_t
pl_inotify_read_event(term_t inotity, term_t event, term_t options)
{ inref *ref;
  term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  int has_timeout = FALSE;
  int timeout;

  while(PL_get_list(tail,head,tail))
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(head, &name, &arity) && arity == 1 )
    { term_t a = PL_new_term_ref();

      if ( name == ATOM_timeout )
      { double t;

	if ( PL_get_arg(1, head, a) &&
	     PL_get_float_ex(a, &t) )
	{ has_timeout = TRUE;
	  timeout = (int)(t*1000.0);
	} else
	  return FALSE;
      }
    } else
      return PL_type_error("option", head);
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( get_inotify(inotity, &ref, TRUE) )
  { term_t ev = PL_new_term_ref();

    if ( ref->ev == NULL )
    { ssize_t len;

      if ( has_timeout )
      { struct pollfd fds[1];

	for(;;)
	{ int rc;
	  fds[0].fd = ref->fd;
	  fds[0].events = POLLIN;

	  rc = poll(fds, 1, timeout);
	  if ( rc < 0 && errno == EINTR )
	  { if ( PL_handle_signals() < 0 )
	      return FALSE;
	    continue;
	  }
	  if ( rc == 0 )
	    return FALSE;
	  break;
	}
      }

      len = read(ref->fd, ref->buf, sizeof(ref->buf));

      if ( len < 0 )
	return inotify_error(ref);
      ref->len = len;
      ref->ev = (struct inotify_event*)ref->buf;
      assert((char*)nextEv(ref->ev) <= &ref->buf[ref->len]);
    }

    if ( put_in_event(ev, ref->ev) )
    { ref->ev = nextEv(ref->ev);
      if ( (char*)ref->ev >= &ref->buf[ref->len] )
	ref->ev = NULL;
      return PL_unify(event, ev);
    }
  }

  return FALSE;
}


install_t
install_inotify4pl(void)
{ ATOM_file      = PL_new_atom("file");
  ATOM_directory = PL_new_atom("directory");
  ATOM_timeout   = PL_new_atom("timeout");
  ATOM_unknown   = PL_new_atom("unknown");

  FUNCTOR_member1        = PL_new_functor(PL_new_atom("member"),        1);
  FUNCTOR_inotify5       = PL_new_functor(PL_new_atom("inotify"),       5);
  FUNCTOR_error2         = PL_new_functor(PL_new_atom("error"),         2);
  FUNCTOR_inotify_error2 = PL_new_functor(PL_new_atom("inotify_error"), 2);

  PL_register_foreign("inotify_init",	     2, pl_inotify_init,       0);
  PL_register_foreign("inotify_close_",	     1, pl_inotify_close,      0);
  PL_register_foreign("inotify_add_watch",   4, pl_inotify_add_watch,  0);
  PL_register_foreign("inotify_rm_watch_",   2, pl_inotify_rm_watch,   0);
  PL_register_foreign("inotify_read_event_", 3, pl_inotify_read_event, 0);
}
