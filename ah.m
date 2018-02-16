# -*- octave -*-

function m = a(lambda)
  m = zeros(6,6);
  m(1,2) = m(3,4) = m(5,6) = 1;
  m(2,1) = m(4,3) = m(6,5) = 2*lambda;
endfunction

