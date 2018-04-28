LDFLAGS=-L$(HOME)/.guix-profile/lib -lnetcdf -lhdf5 -fPIC

# install: build
# 	echo '(install-shim "$(DESTDIR)" "netcdf")' \
# 	| mit-scheme --batch-mode

clean:
	rm netcdf-const* netcdf-types* netcdf-shim* netcdf.com \
	netcdf.bci	netcdf.bin

check : netcdf.com
	echo '(load "test-netcdf.scm")' \
	| mit-scheme --batch-mode --library \
	$(HOME)/.guix-profile/lib/mit-scheme-x86-64:$(PWD) \
	--band all.com

build: netcdf.com
	echo "(load-option 'ffi) (c-include \"netcdf\") (cf \"netcdf\")" \
	| mit-scheme --batch-mode --library \
	$(HOME)/.guix-profile/lib/mit-scheme-x86-64:$(PWD) \
	--band all.com

netcdf.com : netcdf-shim.so netcdf-types.bin netcdf-const.bin
	echo

netcdf-shim.so: netcdf-shim.o
	echo "(link-shim)" \
	| mit-scheme --batch-mode -- -o $@ $^ $(LDFLAGS)

netcdf-shim.o: netcdf-shim.c
	echo '(compile-shim)' \
	| mit-scheme --batch-mode -- -c $<

netcdf-shim.c netcdf-const.c netcdf-types.bin: netcdf.cdecl
	echo '(generate-shim "netcdf" "#include \"netcdf.h\"")' \
	| mit-scheme --batch-mode

netcdf-const.bin: netcdf-const.scm
	echo '(sf "netcdf-const")' | mit-scheme --batch-mode

netcdf-const.scm: netcdf-const
	./netcdf-const

netcdf-const: netcdf-const.o
	gcc -o $@ $^ $(LDFLAGS)

netcdf-const.o: netcdf-const.c
	gcc `pkg-config --cflags netcdf` $(CFLAGS) -o $@ -c $<

