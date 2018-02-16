# -*- octave -*-

## Return the supposed eigenvalues of a(k,c,Grt,Pr).

function lambda = l(k,c,Grt,Pr)
  i = sqrt(-1);
  lambda = zeros(6,1);
  lambda(1) = sqrt(k*sqrt(-c^2*Grt^2-8*i*k*c*Grt)-i*k*c*Grt+2*k^2)/sqrt(2);
  lambda(2) = -lambda(1);
  lambda(3) = sqrt(-k*sqrt(-c^2*Grt^2-8*i*k*c*Grt)-i*k*c*Grt+2*k^2)/sqrt(2);
  lambda(4) = -lambda(3);
  lambda(5) = sqrt(k^2-i*k*c*Grt*Pr);
  lambda(6) = -lambda(5);
endfunction

