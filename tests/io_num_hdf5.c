#include "trexio.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define TEST_BACKEND 	TREXIO_HDF5
#define TREXIO_FILE 	"test_num.h5"
#define RM_COMMAND 	"rm -rf " TREXIO_FILE

static int test_write_num (const char* file_name, const back_end_t backend) {

/* Try to write a dimensioning attribute (num variable) into the TREXIO file */

  trexio_t* file = NULL;
  trexio_exit_code rc;

  // parameters to be written
  int num = 12;

/*================= START OF TEST ==================*/

  // open file in 'write' mode
  file = trexio_open(file_name, 'w', backend);
  assert (file != NULL);

  // write numerical attribute in an empty file
  rc = trexio_write_nucleus_num(file, num);
  assert (rc == TREXIO_SUCCESS);

  // attempt to write 0 as dimensioning variable in an empty file; should FAIL and return TREXIO_INVALID_ARG_2
  rc = trexio_write_mo_num(file, 0);
  assert (rc == TREXIO_INVALID_ARG_2);

  // write numerical attribute ao_cartesian as 0
  rc = trexio_write_ao_cartesian(file, 0);
  assert (rc == TREXIO_SUCCESS);

  // close current session
  rc = trexio_close(file);
  assert (rc == TREXIO_SUCCESS);

/*================= END OF TEST ==================*/

  return 0;
}


static int test_has_num (const char* file_name, const back_end_t backend) {

/* Try to check the existence of a dimensioning attribute (num variable) in the TREXIO file */

  trexio_t* file = NULL;
  trexio_exit_code rc;

/*================= START OF TEST ==================*/

  // open file
  file = trexio_open(file_name, 'r', backend);
  assert (file != NULL);

  // check that the previously written num variable exists
  rc = trexio_has_nucleus_num(file);
  assert (rc == TREXIO_SUCCESS);

  // check that the num variable does not exist
  rc = trexio_has_mo_num(file);
  assert (rc == TREXIO_HAS_NOT);

  // close current session
  rc = trexio_close(file);
  assert (rc == TREXIO_SUCCESS);

/*================= END OF TEST ==================*/

  return 0;
}


static int test_read_num (const char* file_name, const back_end_t backend) {

/* Try to read a dimensioning attribute (num variable) from the TREXIO file */

  trexio_t* file = NULL;
  trexio_exit_code rc;

  // parameters to be read
  int num;
  int cartesian;

/*================= START OF TEST ==================*/

  // open file in 'read' mode
  file = trexio_open(file_name, 'r', backend);
  assert (file != NULL);

  // read numerical attribute from the file
  rc = trexio_read_nucleus_num(file, &num);
  assert (rc == TREXIO_SUCCESS);
  assert (num == 12);

  // read non-existing numerical attribute from the file
  rc = trexio_read_mo_num(file, &num);
  assert (rc == TREXIO_ATTR_MISSING);

  // read ao_cartesian (zero) value from the file
  rc = trexio_read_ao_cartesian(file, &cartesian);
  assert (rc == TREXIO_SUCCESS);
  assert (cartesian == 0);

  // close current session
  rc = trexio_close(file);
  assert (rc == TREXIO_SUCCESS);

/*================= END OF TEST ==================*/

  return 0;
}


int main(void) {

/*============== Test launcher ================*/

  int rc;
  rc = system(RM_COMMAND);
  assert (rc == 0);

  test_write_num (TREXIO_FILE, TEST_BACKEND);
  test_has_num   (TREXIO_FILE, TEST_BACKEND);
  test_read_num  (TREXIO_FILE, TEST_BACKEND);

  rc = system(RM_COMMAND);
  assert (rc == 0);

  return 0;
}


