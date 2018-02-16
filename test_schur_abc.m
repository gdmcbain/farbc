# -*- octave -*-

function result = test_schur_abc(n=3)
  a = rand (n,n) + sqrt (-1)*rand (n,n);
  b = schur_abc (a);
  [v, lambda] = eig (a); 
  lambda = (conj(diag (lambda)))'
  disp ("For those eigenvalues with negative real part, and no others,")
  disp ("the columns of v should be annihilated.")
  result = b * v;
endfunction
