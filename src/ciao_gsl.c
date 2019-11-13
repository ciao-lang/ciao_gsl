/* Global declaration */


#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_poly.h>
#include <gsl/gsl_errno.h>

#include <ciao_prolog.h>


/* properties_to_matrix/5 */

gsl_matrix * properties_to_matrix(double *elements, size_t length, size_t rows, size_t cols)
{
  size_t i; /*, l = rows * cols;*/
  gsl_matrix *qt = gsl_matrix_alloc(rows, cols);
  for(i = 0; i < length; i++)
    qt->data[i] = elements[i];
  return qt;
}

/* matrix_to_properties/5 */

void matrix_to_properties(gsl_matrix *m, double ** list, size_t *length,
  size_t *rows, size_t *cols)
{
  size_t i;
  *length = m -> block -> size;
  /* We should reuse m -> elements in list due to it is not automatically */
  /* deallocated */
  *list = m -> block -> data;
  *rows = m -> size1;
  *cols = m -> size2;
}

/* list_to_vector/3 */

gsl_vector * list_to_vector(double * elements, size_t length)
{
  size_t i;
  gsl_vector *qt = gsl_vector_alloc(length);
  for(i = 0; i < length; i++)
    qt->data[i] = elements[i];
  return qt;
}

/* vector_to_list/3 */

void vector_to_list(gsl_vector *v, double ** list, size_t *length)
{
  size_t i;
  *length = v->size;
  *list = v->data;
}

/* gsl_linalg_QR_U/2 */

int
gsl_linalg_QR_U (const gsl_matrix * QR, gsl_matrix * R)
{
  const size_t M = QR->size1;
  const size_t N = QR->size2;

  if (R->size1 != N || R->size2 != N)
    {
      GSL_ERROR ("R matrix must be N x N", GSL_ENOTSQR);
    }
  else
    {
      size_t i, j;

      /*  Form the right triangular matrix R from a packed QR matrix */

      for (i = 0; i < N; i++)
        {
          for (j = 0; j < i; j++)
            gsl_matrix_set (R, i, j, 0.0);

          for (j = i; j < N; j++)
            gsl_matrix_set (R, i, j, gsl_matrix_get (QR, i, j));
        }

      return GSL_SUCCESS;
    }
}

/* gsl_linalg_QR_Rsolve_over_determined/2 */

int
gsl_linalg_QR_Rsolve_over_determined (const gsl_matrix * QR,
  gsl_vector * x)
{
  const size_t M = QR->size1;
  const size_t N = QR->size2;

  if (M < N)
    {
      GSL_ERROR ("QR matrix must have M>=N", GSL_EBADLEN);
    }
  else if (N != x->size)
    {
      GSL_ERROR ("matrix size must match solution size", GSL_EBADLEN);
    }
  else
    {
      gsl_matrix_const_view R = gsl_matrix_const_submatrix (QR, 0, 0, N, N);

      /* Solve R x = rhs */

      gsl_blas_dtrsv (CblasUpper, CblasNoTrans, CblasNonUnit, &(R.matrix), x);

      return GSL_SUCCESS;
    }
}

/* gsl_version/1 */

#include <gsl/gsl_version.h>

void
get_gsl_version(char ** ptr)
{
        *ptr = (char *) GSL_VERSION;
}

/* polynomial_root/5 */

/* ----------------------------------------------------------------------------
   Calling GSL library for computing the roots of polynom. Polynom given as
   input_list, and output_list will contains all root of the polynom, real and 
   complex number.
   -------------------------------------------------------------------------- */
void polynomial_root(size_t nbElmt,size_t nbElmtOut,double* input_list, double** output_list, int* errcode){
  size_t i, nb_elmt_output;
  int gsl_retval;       
  gsl_poly_complex_workspace * w = gsl_poly_complex_workspace_alloc (nbElmt);
  *output_list=(double*) ciao_malloc ((nbElmtOut)* sizeof(double)); //this ciao_malloc will be freed by ciao prolog memory memory management system

  gsl_retval=gsl_poly_complex_solve (input_list, nbElmt, w, *output_list);     
  if (gsl_retval == GSL_EFAILED) {
    /* marking for GSL unconvergence */
    (*errcode) = -1;
  }else{
    (*errcode) = 0;
  }
  gsl_poly_complex_workspace_free (w);
}
