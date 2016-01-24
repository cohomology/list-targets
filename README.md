This is a simple parser of GNU Makefiles used
internally at my workspace.

These makefiles have the following structure:

```
  STATIC_LIBS := foo \ 
                 bar/foo2/roffel

  roffel_SRC := kernel.cpp \
                sql.cpp
```

If you want to build the file sql.cpp without building anything else, 
you have to type something like:

```
   make_processor project bar/foo2/roffel/sql.o
```

This parser tries to find "bar/foo2/roffel" given "sql.cpp". Of course,
this might not be unique, but in practice, is.
