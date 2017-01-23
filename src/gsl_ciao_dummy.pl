:- export(properties_to_matrix/5).
:- export(matrix_to_properties/5).
:- export(matrix_destroy/1).
:- export(vector_destroy/1).
:- export(gsl_vector_alloc/2).
:- export(gsl_linalg_QR_decomp/2).
:- export(vector_to_list/3).
:- export(list_to_vector/3).
:- export(gsl_matrix_alloc/3).
:- export(gsl_linalg_HH_svx/2).
:- export(gsl_linalg_QR_unpack/4).
:- export(gsl_linalg_QR_U/2).
:- export(gsl_linalg_QR_lssolve/5).
%:- export(gsl_linalg_QR_Rsolve/2).
:- export(gsl_linalg_QR_Rsolve_over_determined/2).
:- export(gsl_linalg_QR_Rsvx/2).
:- export(gsl_set_error_handler_off/1).
:- export(gsl_version/1).
:- export(polynomial_root/5).

?- error_in_lns(_, _, warning,
'The GSL library does not seem to be installed. Using dummy (non-functional) version instead. If you would like to use the math library and related libraries such as the cost analysis please install GSL, and then run again the Ciao configuration and rebuild the system.').

:- impl_defined([properties_to_matrix/5]).
:- impl_defined([matrix_to_properties/5]).
:- impl_defined([matrix_destroy/1]).
:- impl_defined([vector_destroy/1]).
:- impl_defined([gsl_vector_alloc/2]).
:- impl_defined([gsl_linalg_QR_decomp/2]).
:- impl_defined([vector_to_list/3]).
:- impl_defined([list_to_vector/3]).
:- impl_defined([gsl_matrix_alloc/3]).
:- impl_defined([gsl_linalg_HH_svx/2]).
:- impl_defined([gsl_linalg_QR_unpack/4]).
:- impl_defined([gsl_linalg_QR_U/2]).
:- impl_defined([gsl_linalg_QR_lssolve/5]).
%:- impl_defined([gsl_linalg_QR_Rsolve/2]).
:- impl_defined([gsl_linalg_QR_Rsolve_over_determined/2]).
:- impl_defined([gsl_linalg_QR_Rsvx/2]).
:- impl_defined([gsl_set_error_handler_off/1]).
:- impl_defined([gsl_version/1]).
:- impl_defined([polynomial_root/5]).
