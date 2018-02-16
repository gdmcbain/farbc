ainf : matrix([0,1,0,0,0,0],
	      [0,0,1,0,0,0],
	      [0,0,0,1,0,0],
	      [-l2^2*l3^2,0,l2^2+l3^2,0,0,-1],
	      [0,0,0,0,0,1],
	      [0,0,0,0,l1^2,0]);

abc : matrix([0,0,0,0,1,-1/l1],
             [-l2*l3^2*(l2^2-l1^2), l3^2*(l2^2-l1^2), l2*(l2^2-l1^2),
		l1^2-l2^2, l1^2, -l2],
             [-l3*l2^2*(l3^2-l1^2), l2^2*(l3^2-l1^2), l3*(l3^2-l1^2),
		l1^2-l3^2, l1^2, -l3] );

a : -l1/((l1^2-l2^2)*(l1^2-l3^2));

phi(x) := a*exp(l1*x) + b*exp(l2*x) + c*exp(l3*x);
theta(x) := exp(l1*x);

decayingstate(x) := matrix([phi(x)],
		   [diff(phi(x),x)],
	           [diff(phi(x),x,2)],
	    	   [diff(phi(x),x,3)],
    		   [theta(x)],
		   [diff(theta(x),x)] );

testabc : abc . decayingstate(x);

danielsabc : matrix([0,0,0,0,-l1,1],
                    [l2*l3, -(l2+l3), 1, 0, -a*(l1^2-l1*(l2+l3)+l2*l3), 0],
	            [-l1*l2*l3, l1*l2+l2*l3+l3*l1, -l1-l2-l3, 1, 0, 0] );

	
