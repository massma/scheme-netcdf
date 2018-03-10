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

scmlib_LTLIBRARIES = netcdf-shim.la
scmlib_DATA = netcdf-types.bin netcdf-const.bin

AM_CPPFLAGS = -Wall -I$(MITSCHEME_ROOT)/src/lib

netcdf_shim_la_LIBADD = netcdf.lo
netcdf_shim_la_LDFLAGS = -module -avoid-version -shared -lnetcdf

noinst_PROGRAMS = netcdf-const
netcdf_const_SOURCES = netcdf-const.c netcdf-shim.h

netcdf-shim.c netcdf-const.c netcdf-types.bin: netcdf.cdecl netcdf.lo
	echo '(generate-shim "netcdf" "#include \"netcdf.h\"")' \
	| $(MIT_SCHEME_EXE) --batch-mode


netcdf.lo: netcdf.c
	libtool --mode=compile $(CC) -I$(AUXDIR) -I/usr/include -I/usr/include/hdf5/serial -Wall -fPIC `pkg-config --cflags netcdf` -o $@ -c $<

netcdf-const.bin: netcdf-const.scm
	echo '(sf "netcdf-const")' | $(MIT_SCHEME_EXE) --batch-mode

netcdf-const.scm: netcdf-const
	./netcdf-const

netcdf.bin netcdf.bci netcdf.com netcdf-unx.pkd: \
    netcdf.scm netcdf-const.bin netcdf-types.bin
	echo '(load "compile.scm")' | $(MIT_SCHEME_EXE) --batch-mode

CLEANFILES = netcdf-const* netcdf-shim.c
CLEANFILES += *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd
