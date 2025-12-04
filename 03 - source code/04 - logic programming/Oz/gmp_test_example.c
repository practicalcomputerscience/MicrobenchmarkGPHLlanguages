/* gmp_test_example.c

2025-12-04
 
Test in Ubuntu 24 LTS: OK! 

After a GMP installation, here with version 6.3.0, do this:
$ gcc -g gmp_test_example.c -o gmp_test_example -lgmp
$ ./gmp_test_example 999999999999999999999 999999999999999999999
999999999999999999998000000000000000000001
$

This works too:
$ gcc -Wall -Ofast gmp_test_example.c -o gmp_test_example -lgmp
$ ./gmp_test_example 999999999999999999999 999999999999999999999
999999999999999999998000000000000000000001
$

 */

#include <stdio.h>
#include <gmp.h>

int main (int argc, char **argv)
{
  mpz_t a, b, p;

  if (argc != 3)
    {
      printf ("Usage: %s <number> <number>\n", argv[0]);
      return 1;
    }

  /* Initialize and assign a and b from base 10 strings in argv */
  mpz_init_set_str (a, argv[1], 10);
  mpz_init_set_str (b, argv[2], 10);
  /* Initialize p */
  mpz_init (p);

  /* Multiply a and b and put the result in p */
  mpz_mul (p, a, b);

  /* Print p in decimal */
  gmp_printf ("%Zd\n", p);

  /* Since we're about to exit, no need to clear out variables */
  return 0;
}

// end of gmp_test_example.c
