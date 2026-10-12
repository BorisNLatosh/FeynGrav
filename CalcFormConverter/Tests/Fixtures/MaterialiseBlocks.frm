#-
Off Statistics;
Symbols x,y,n;
Vectors p,q;
Indices mu,nu;
CFunctions block,original,key;
Local blocks=block(1)*(x+1)+block(2)*(x^2-1)+block(3)*(x-x)
 +block(4)*(d_(mu,nu)+p(mu)*q(nu));
Bracket+ block;
.sort
Hide blocks;
Local weights=key(1)*(original(1)+2*original(2)+original(3)+original(99))
 +key(2)*original(4)*(x+y)+key(3)*(original(1)-original(1));
Bracket+ key;
.sort
Hide weights;
#do j=1,3
Local baseline=weights[key(`j')];
id original(n?)=blocks[block(n)];
.sort
Hide baseline;
Local candidate=weights[key(`j')];
#if `NTHREADS_' > 2
.sort
#endif
id original(n?)=blocks[block(n)];
.sort
Unhide baseline;
.sort
Local residual=baseline-candidate;
.sort
#write "MATERIALISE_RESIDUAL %E",residual
Drop residual,baseline,candidate;
.sort
#enddo
.end
