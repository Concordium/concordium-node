This crate wraps [libunbound](https://unbound.nlnetlabs.nl) from
[NLnet Labs](https://nlnetlabs.nl). libunbound is an implementation of a DNS
resolver, including cache and DNSSEC validation.

The [interface provided](http://docs.rs/unbound) follows libunbound closely.
Transliterations of the libunbound
[tutorials](https://www.unbound.net/documentation/) can be found in
[examples](https://github.com/andrewtj/rust-unbound/tree/master/unbound/examples).

### Building

libunbound depends on OpenSSL which this crate relies on
[rust-openssl](https://github.com/sfackler/rust-openssl) to provide.

The following environment variables influence the build process:

* `UNBOUND_STATIC`- If specified libunbound will be linked statically.
* `UNBOUND_DIR` - Directory in which libunbound's `include` and `lib` folders may be found.
