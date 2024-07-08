# These hacks are required for the test binaries depending on the dynamic library libstd-*.so
# See: https://github.com/rust-lang/cargo/issues/4651
#
# We also need to exclude the derive macro from coverage because it will always show as 0% by
# virtue of executing at compile-time outside the view of Tarpaulin.
coverage:
	env LD_LIBRARY_PATH="$(shell rustc --print sysroot)/lib" \
	cargo-tarpaulin --workspace --all-features --out xml --exclude ethereum_ssz_derive

.PHONY: coverage
