# gen_pnet_examples

###### A collection of small, instructive examples using gen_pnet.

## Examples

### cvm

Cookie vending machine. The example used to expain the callback functions for
gen_pnet in a single gen_pnet module.

### dinner

Dining philosophers problem. A Petri net specification of the five dining
philosophers. This example shows how concurrent applications can be expressed in
gen_pnet and is, furthermore, an example of a live net.

### crosstalk

A Petri net specification of the crosstalk algorithm.

## System Requirements

- Erlang OTP 18.0 or higher
- Rebar3 3.0.0 or higher

## Resources

- [joergen7/gen_pnet](https://github.com/joergen7/gen_pnet). A generic Petri net OTP behavior.
- [joergen7/gruff](https://github.com/joergen7/gruff). A basic worker pool manager for Erlang to showcase gen_pnet. 

## Authors

- Jörgen Brandt (joergen7) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)