all: libcanonicalize.so


libcanonicalize.so: target/debug/libcanonicalize.so
	cp $< .

target/debug/libcanonicalize.so: src/lib.rs
	cargo build
