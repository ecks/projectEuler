/***********************
 * RookPaths.c
 * By: David Blume
 * dcb@wdl1.wdl.loral.com (Predecrement David)
 *
 * How many unique ways can a rook move from the bottom left corner
 * of a m * n chess board to the top right right?
 *
 * Contraints: The rook may not passover a square it has already visited.
 *             What if we don't allow Down & Left moves? (much easier)
 *
 * This software is provided *as is.*  It is not guaranteed to work on
 * any platform at any time anywhere. :-)  Enjoy.
 ***********************/

#include <stdio.h>

#define kColumns 20			/* The maximum number of columns */
#define kRows	 20			/* The maximum number of rows */

/* The following rule is for you to play with. */
//#define kAllowDownLeft 		/* Whether or not to allow the rook to move D or L */

/* Visual feedback defines... */
//#undef kShowTraversals		/* Whether or nor to show each successful graph */
//#define kListResults		/* Whether or not to list each n * m result */
//#define kShowMatrix			/* Display the final results in a matrix? */

char gmatrix[kColumns][kRows];				/* the working matrix */
long result[kColumns][kRows];				/* the result array */

/*********************
 * traverse
 *
 * This is the recursive function
 *********************/
traverse (short c, short r, short i, short j )
{
	
	/* made it to the top left! increase result, retract move, and return */
	if (i == c && j == r) {

#ifdef kShowTraversals	
		short ti, tj;
		gmatrix[i][j] = 5;
		for (ti = c; ti >= 0; ti--) {
			for (tj = 0; tj <= r; tj++) {
				if (gmatrix[ti][tj] == 0)
					printf(". ");
				else if (gmatrix[ti][tj] == 1)
					printf("D ");
				else if (gmatrix[ti][tj] == 2)
					printf("R ");
				else if (gmatrix[ti][tj] == 3)
					printf("L ");
				else if (gmatrix[ti][tj] == 4)
					printf("U ");
				else if (gmatrix[ti][tj] == 5)
					printf("* ");
				}
			printf("\n");
			}
		printf("\n");
#endif

		result[i][j] = result[i][j] + 1;
		gmatrix[i][j] = 0;
		return;
		}
	
	/* try to move, left up down right */
#ifdef kAllowDownLeft
	if (i != 0 && gmatrix[i-1][j] == 0) {		/* left turn */
		gmatrix[i][j] = 1;
		traverse(c, r, i-1, j);
		}
#endif
	if (j != r && gmatrix[i][j+1] == 0) {		/* turn up */
		gmatrix[i][j] = 2;
		traverse(c, r, i, j+1);
		}
#ifdef kAllowDownLeft
	if (j != 0 && gmatrix[i][j-1] == 0) {		/* turn down */
		gmatrix[i][j] = 3;
		traverse(c, r, i, j-1);
		}
#endif
	if (i != c && gmatrix[i+1][j] == 0) {		/* turn right */
		gmatrix[i][j] = 4;
		traverse(c, r, i+1, j);
		}
	
	/* remove the marking on this square */
	gmatrix[i][j] = 0;

}

main()
{
	short i, j;
	
	/* initialize the matrix to 0 */
	for (i = 0; i < kColumns; i++) {
		for (j = 0; j < kRows; j++) {
			gmatrix[i][j] = 0;
			}
		}
	
	/* call the recursive function */
	for (i = 0; i < kColumns; i++) {
		for (j = 0; j < kRows; j++) {
			if (j >= i) {
				result[i][j] = 0;
				traverse (i, j, 0, 0);
#ifdef kListResults
				printf("For a %d x %d chessboard, there are %d unique paths.\n",
						i+1, j+1, result[i][j]); fflush(stdout);
#endif
				}
			}
		}
	/* print out the results */
#ifdef kShowMatrix
	printf("\n");
	for (i = 0; i < kColumns; i++) {
		for (j = 0; j < kRows; j++) {
			short min = (i < j) ? i : j ;
			short max = (i < j) ? j : i ;
			printf("%6d", result[min][max]);
			}
		printf("\n");
		}
#endif
}

