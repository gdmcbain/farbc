# -*- octave -*-

## Return the Jacobian of the Plapp equations with zero base solution.

function m = a(k,c,Grt,Pr)
  m = zeros(6,6);
  m(1,2) = 1;
  m(2,3) = 1;
  m(3,4) = 1;
  m(4,1) = -k^2*(k^2+sqrt(-1)*k*Grt*c);
  m(4,3) = 2*k^2-sqrt(-1)*k*Grt*c;
  m(4,6) = -1;
  m(5,6) = 1;
  m(6,5) = k^2-sqrt(-1)*k*Grt*Pr*c;
endfunction

