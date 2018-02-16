# -*- octave -*-

i=sqrt(-1);

a = rand(3,3) + i*rand(3,3);	# Generate a random nonsymmetric complex matrix

## Compute the matrix Schur vectors of the conjugate transpose of a,
## ordered so that the first p columns of zh correspond to eigenvalues
## of a with positive real part.  
##
## Because GNU Octave's 'schur' command
## only orders the other way (negative real part first), we operate with
## -(a') and then invert zh and th.

[zh,th] = schur (-(a'), "a"); zh=-zh; th=-th;

## Count the eigenvalues with positive real part.

p = sum (real (diag (th)) > 0);

## Compute the matrix to annihilate the vectors of a with negative real
## part.

z = (zh(:,1:p))';

## Now calculate the eigenvectors of a, independently.

[v, lambda] = eig (a);

## ...and operate on it with z.  The result should have (numerically)
## zero columns for those eigenvectors v(:,r) corresponding to real(
## lambda(r,r) > 0).

disp ("Eigenvalues."); diag (lambda)
disp ("Operation with z."); testans = z * v




