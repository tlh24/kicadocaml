function debugTris(x)
	function pplot(xx,yy,cc,n1,n2)
		plot(xx,yy,cc);
		plot(xx,yy,['o' cc]);
		text(xx(1),yy(1),n1); 
		text(xx(2),yy(2),n2); 
	end
	pplot(x(1:2,1),x(1:2,2),'b','a','b'); 
	pplot(x(2:3,1),x(2:3,2),'b','','c');
	indx = [1,3];
	pplot(x(indx,1),x(indx,2),'b','',''); 
	pplot(x(4:5,1),x(4:5,2),'r','d','e'); 
end