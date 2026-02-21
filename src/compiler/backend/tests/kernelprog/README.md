## C-Lisp PTX Generation Proof-of-Concept

Each subdirectory here has a device-side kernel, stored as `<name>.sexp`, along with
a host-side driver, stored as `<name>.driver.c`. `<name>.driver.c` files contain code
to JIT-compile and launch a kernel, along with a reference implementation against which
results are compared.

To test a kernel, run `make <kernel name>.run` from the kernel's directory. This
will
* Compile the kernel from C-Lisp to PTX
* Compile the driver from C to an executable
* Run the driver, which in turn will
  - Initialize random inputs
  - JIT the kernel and launch it
  - Run the reference implementation and compare results