# We directly use OpenGLRaw in this example because of an API deficiency in
# OpenGL. The problem is that we don't use cabal and OpenGLRaw's API changed, so
# there's no easy way to handle this. => Disable that example for now.
EXCLUDED_SOURCES := SmoothOpenGL3.hs

SUBDIRS := ColorTriangle
include ../examples.mk
