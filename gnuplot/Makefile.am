## Process this file with automake to produce Makefile.in
##
## Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
##     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
##     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
##     2015, 2016, 2017 Massachusetts Institute of Technology
## 
## This file is part of MIT/GNU Scheme.
## 
## MIT/GNU Scheme is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or (at
## your option) any later version.
## 
## MIT/GNU Scheme is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with MIT/GNU Scheme; if not, write to the Free Software
## Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
## USA.

ACLOCAL_AMFLAGS = -I m4 -I/usr/include

MIT_SCHEME_EXE = @MIT_SCHEME_EXE@
scmlibdir = $(libdir)/mit-scheme-@MIT_SCHEME_ARCH@

scmlib_LTLIBRARIES = gnuplot_utils-shim.la
scmlib_DATA = gnuplot_utils-types.bin gnuplot_utils-const.bin

AM_CPPFLAGS = -Wall -I$(MITSCHEME_ROOT)/src/lib

gnuplot_utils_shim_la_LIBADD = gnuplot_utils.lo
gnuplot_utils_shim_la_LDFLAGS = -module -avoid-version -shared 

noinst_PROGRAMS = gnuplot_utils-const
gnuplot_utils_const_SOURCES = gnuplot_utils-const.c gnuplot_utils-shim.h

gnuplot_utils-shim.c gnuplot_utils-const.c gnuplot_utils-types.bin: gnuplot_utils.cdecl gnuplot_utils.lo
	echo '(generate-shim "gnuplot_utils" "#include \"unistd.h\"")' \
	| $(MIT_SCHEME_EXE) --batch-mode

gnuplot_utils.lo: gnuplot_utils.c
	libtool --mode=compile $(CC) -I./ -I/usr/include -Wall -fPIC `pkg-config --cflags gnuplot_utils` -o $@ -c $<

gnuplot_utils-const.bin: gnuplot_utils-const.scm
	echo '(sf "gnuplot_utils-const")' | $(MIT_SCHEME_EXE) --batch-mode

gnuplot_utils-const.scm: gnuplot_utils-const
	./gnuplot_utils-const


CLEANFILES = gnuplot_utils-const* gnuplot_utils-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
