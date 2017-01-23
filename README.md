# Ciao bindings for GSL

This bundle implements the Ciao bindings for the
[The GNU Scientific Library (GSL)](https://www.gnu.org/software/gsl/).
The GSL provides a large number of mathematical routines. See the
original documentation for more details.

NOTE: These bindings cover only a subset of the functionality provided
by GSL. If you need any functionality that is missing here, please
request it to the maintainers of these bindings.

Usage (with auto-installation of GSL):
```
$ ciao configure ciao_gsl --ciao_gsl:with_gsl=yes --ciao_gsl:auto_install_gsl=yes
$ ciao build ciao_gsl
```

