# -*- octave -*-

## Return an asymptotic boundary condition matrix for the complex
## asymptotic coefficient matrix a.

function b = schur_abc(a)
  [zh, th] = schur (-(a'), "a"); # "a" puts eiv's with NEG. real part 1st
  p = sum (real (diag (-th)) > 0); # no. eiv(a) with positive real part
  b = (-zh(:,1:p))';		# 1st p cols of (-zh) into p rows of z
endfunction
