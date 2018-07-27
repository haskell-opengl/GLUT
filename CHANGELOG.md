2.7.0.14
--------
* Relaxed upper version bound for `containers`.

2.7.0.13
--------
* Relaxed upper version bound for `OpenGLRaw`.

2.7.0.12
--------
* Mac OS X: Make it possible to link against freeglut.

2.7.0.11
--------
* Linux: Try to load versioned GLUT library, too, because the unversioned one is often in *-dev packages only.

2.7.0.10
--------
* Mac OS X: Search public frameworks first, then system frameworks.

2.7.0.9
--------
* The GLUT package compiles without any additional library/framework now.
* Windows: We search for a native freeglut DLL, a MinGW freeglut DLL and a classic GLUT DLL, in that order.

2.7.0.8
--------
* Relaxed upper version bound for `OpenGLRaw`.

2.7.0.7
-------
* Removed redundant constraints.

2.7.0.6
--------
* Relaxed upper version bound for `OpenGLRaw`.

2.7.0.5
--------
* Make things work with both old and new `OpenGLRaw`/`GLURaw` packages.
* Build all examples via cabal, no need for `make` anymore.

2.7.0.4
--------
* Relaxed upper version bound for `transformers`.

2.7.0.3
--------
* Relaxed upper version bound for OpenGLRaw.
* Added CHANGELOG.md to distribution.
* Minor build/testing tweaks.

2.7.0.2
--------
* Fixed typo in shader file extension.
* Relaxed bounds for OpenGL package.
