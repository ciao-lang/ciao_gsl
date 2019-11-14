:- use_package(foreign_interface).
:- use_package(assertions).
%:- use_package(nortchecks).

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

:- include(ciao_gsl(gsl_ciao_decl_auto)).

:- use_foreign_source(ciao_gsl(ciao_gsl)).

% ----------------------------------------------------------------------------

:- test properties_to_matrix(Elements, Length, Rows, Cols, Matrix) :
    (Elements = [1.0, 2, 3, 4, 5, 6], Length = 6, Rows = 2, Cols = 3) =>
    ( matrix_to_properties(Matrix, ElementsR, LengthR, RowsR, ColsR),
        ElementsR = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0], LengthR = 6, RowsR = 2,
        ColsR = 3, matrix_destroy(Matrix) )
# "Is the inverse of matrix_to_properties.".

:- trust pred properties_to_matrix(in(Elements), in(Length), in(Rows),
        in(Cols), go(Matrix)) :: c_double_list * c_size * c_size * c_size * address +
    (foreign, size_of(Elements, Length), returns(Matrix)).

:- trust pred matrix_to_properties(in(Matrix), go(Elements), go(Length),
        go(Rows), go(Cols)) :: address * c_double_list * c_size * c_size * c_size + (
        foreign, size_of(Elements, Length)).

:- trust pred list_to_vector(in(Elements), in(Length), go(Vector)) ::
    c_double_list * c_size * address +
    (foreign, size_of(Elements, Length), returns(Vector)).

:- trust pred vector_to_list(in(Vector), go(Elements), go(Length)) ::
    address * c_double_list * c_size + (foreign, size_of(Elements, Length)).

:- test list_to_vector(Elements, Length, Vector) :
    (Elements = [1.0, 2, 3, 4, 5, 6], Length = 6) =>
    ( vector_to_list(Vector, ElementsR, LengthR),
        ElementsR = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0], LengthR = 6,
        vector_destroy(Vector) )
# "Is the inverse of vector_to_list.".

:- trust pred matrix_destroy(in(Matrix)) :: address
    + (foreign(gsl_matrix_free)).
:- trust pred vector_destroy(in(Vector)) :: address
    + (foreign(gsl_vector_free)).

:- trust pred gsl_linalg_HH_svx(in(Matrix), in(Vector)) :: address *
    address + (foreign).

:- trust pred gsl_linalg_QR_lssolve(in(QR), in(Tau), in(B), in(X),
        in(Residual)) :: address * address * address * address * address +
    (foreign).

:- trust pred gsl_linalg_QR_decomp(in(A), in(Tau)) :: address * address
    + (foreign).

:- trust pred gsl_vector_alloc(in(Size), go(Vector)) :: c_size * address +
    (foreign, returns(Vector)).

:- trust pred gsl_matrix_alloc(in(Rows), in(Cols), go(Matrix)) :: c_size *
    c_size * address + (foreign, returns(Matrix)).

:- trust pred gsl_linalg_QR_unpack(in(QR), in(Tau), in(Q), in(R)) ::
    address * address * address * address + (foreign).

% TODO: return type ignored
:- trust pred gsl_linalg_QR_U(in(QR), in(U)) :: address * address +
    (foreign).

:- trust pred gsl_linalg_QR_Rsvx(in(R), in(X)) :: address *
    address + (foreign).

% The next is equal to gsl_linalg_R_solve:
% :- trust pred gsl_linalg_QR_Rsolve(in(QR), in(X)) :: address * address
%    + (foreign).

% But this is what I want:

% TODO: return type ignored
:- trust pred gsl_linalg_QR_Rsolve_over_determined(in(QR), in(X)) ::
    address * address + (foreign).

%:- initialization(gsl_set_error_handler_off(_)).

:- trust pred gsl_set_error_handler_off(go(ErrorHandler)) :: address +
    (foreign, returns(ErrorHandler)).


:- trust pred gsl_version(go(VersionStr)) :: string + ( foreign(get_gsl_version),
        do_not_free(VersionStr) ).

:- doc(doinclude, polynomial_root/5).
:- trust pred polynomial_root(in(LengthIn),in(LengthOut),in(X),go(Y), go(Err))::
    c_size*c_size*c_double_list * c_double_list * c_int + (foreign,size_of(X,LengthIn),size_of(Y,LengthOut)) #
 "obtains roots of a polynomial function by calling foreign C program which will call GSL solver.  @var{Err} is
 error code, 0 when GSL succeed, -1 otherwise".
