# These hacks are required for the test binaries depending on the dynamic library libstd-*.so
# See: https://github.com/rust-lang/cargo/issues/4651
coverage:
	env LD_LIBRARY_PATH="$(shell rustc --print sysroot)/lib" \
	cargo-tarpaulin --workspace --all-features --out xml

.PHONY: coverage
