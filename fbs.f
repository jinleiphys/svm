      program few_body_system
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Solution of few-body problems with the stochastic variational method
c     (svm) on Correlated Gaussian basis. The program is restricted for 
c     central interaction and L=0 orbital angular momentum. 
c     (K. Varga, Y. Suzuki, Phys. Rev. C52 (1995) 2885)
c--------------------------------------------------------------------------- 
c     Important parameters and variables:
c---------------------------------------------------------------------------
c     mnbas               : maximum dimension of the basis
c     nbas                : the dimension of the basis
c     mnpar               : maximum number of particles
c     npar                : the number of particles
c     ico                 : select the solution 
c                           ico=1, predefined basis
c                           ico=2, svm step-by-step
c                           ico=3, svm refinement
c     ibf                 : ibf=1  fermions, ibf=2 bosons
c     ipcon               : ipcon=1 the potential defined in the 
c                           input file pot.inp
c                           ipcon=2 the potential defined in the
c                           fortran subroutine pot(r)
c     npt                 : the number of potential terms in the expansion
c                           (N_t see eqs. (18), (19))     
c     no                  : the number of operators (Wigner, Majorana,
c                           Bartlett, Heisenberg) 
c                           (N_o see eqs. (18), (19))    
c     h2m                 : hbar*hbar/(2*massunit) 
c     xm(i)               : the mass of the i-th particle    
c     z(i)                : the charge of the i-th particle
c     iso(i)              : z-component of the isospin of the i-th particle
c     nspc                : the number of spin configurations
c     cspc(k)             : linear combination coefficient of the 
c                           the k-th spin configuration
c     isp(i,k)            : z-component of the spin of the i-th particle 
c                           in the k-th spin configuration  
c     he(mnbas,mnbas)     : matrix elements of the Hamiltonian
c     oe(mnbas,mnbas)     : overlap of the basis functions 
c     xn(mnbas)           : norm of the basis functions
c     a(mnbas,mnpar,mnpar): matrix of the nonlinear parameters of the basis
c     irand               : to initialize the random number generator
c                           (random numbers are generated in the [0,1]
c                           interval 
c     kk0                 : number of random trials for a given nonlinear
c                           parameter
c     mm0                 : number of repetition of an optimizing cycle
c     mnb                 : the dimension of the basis where the calculation
c                           is to be finished
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit real*8(a-h,o-z)
      common /contro/ mm0,kk0,nbas0,nbas1,mnb,ico
c     input 
      call input_data
c     transformation of coordinates
      call cm_rel_tr
c     matrix elements in spin-isospin space, symmetrization
      call me_spiso
c     selection of solution
c     solution on predefined basis
      call preset
c     svm step-by-step
      if(ico.eq.2) call svm1
c     svm refinement
      if(ico.eq.3) call svm2
      end

      subroutine preset
ccccccc
c     calculation of the matrix elements of the 
c     previously selected basis
ccccccc
      implicit real*8(a-h,o-z)
      logical*4 ex
      parameter (mnpar=6,mnbas=400)
      common /store1/ he(mnbas,mnbas),oe(mnbas,mnbas)
      common /store2/ xn(mnbas)
      common /nlpara/ a(mnbas,mnpar,mnpar)
      common /partic/ npar,kpot
      common /randomm/irand
      common /contro/ mm0,kk0,nbas0,nbas1,mnb,ico
      elow=1.d+10
c     reading the previous/predefined basis
      inquire(file='fbs.res',exist=ex)
      if(ex.eqv..false.) go to 333
      open(15,file='fbs.res')
      read(15,*)nbas0,nbas1
      id=0
      do 1200 i=1,nbas0
      read(15,*)((a(i,j,k),j=1,npar),k=1,npar)
c     matrix elements of the i-th basis state
      call mat_elem(i)
      id=id+1
      if(id.eq.50) then
      id=0
      info=2
c     calculation of the energy by diagonalization after 
c     the dimension is increased by "id"
      call diag(ener,i,info)
      write(6,*)i,ener
      endif
1200  continue
      close(15)
      if(nbas0.gt.0) then
      info=2
c     calculation of the energy by diagonalization after 
c     all the matrix elements are calculated
      call diag(ener,nbas0,info)
      write(6,*)nbas0,ener
      endif
      return
 333  nbas0=0  
      nbas1=0
      return
      end

      subroutine svm2
ccccccc
c     optimization on a fixed  basis size
ccccccc
      implicit real*8(a-h,o-z)
      parameter (mnpar=6,mnbas=400)
      dimension  z((mnpar*(mnpar+1))/2),at(mnpar,mnpar)
      common /store1/ he(mnbas,mnbas),oe(mnbas,mnbas)
      common /store2/ xn(mnbas)
      common /nlpara/ a(mnbas,mnpar,mnpar)
      common /partic/ npar,kpot
      common /randomm/irand
      common /contro/ mm0,kk0,nbas0,nbas1,mnb,ico
      elow=1.d+10
      do 10 nbas=nbas1,nbas0
c     refinement the basis by trying  to replace one of its elements
c     by a more appropriate one
      call diag(ene,nbas0,2)
      write(6,*)nbas,ene
c     exchange of the nbas-th and the last (nbas0-th) basis states
      call exchange(nbas,nbas0)
      call diag(ene,nbas0-1,2)
      elow=ene
      nvar=(npar*(npar+1))/2
      do i=1,npar-1
      do j=1,npar-1
      at(i,j)=a(nbas0,i,j)
      end do
      end do
c     transformation of the nonlinear parameters
      call trcorri(z,at)
c     trying the replace the last (nbas0-th) basis state with a better one
      call matrix2(z,nvar,nbas0,elow)
c     write out the best basis up to now
      open(10,file='fbs.res')
      write(10,*)mnb,nbas
      do 300 i=1,mnb
      write(10,*)((a(i,k,j),k=1,npar),j=1,npar)
 300  continue
      close(10)
 10   continue
      return
      end

      subroutine svm1
ccccccc
c     optimization by increasing the basis
ccccccc
      implicit real*8(a-h,o-z)
      parameter (mnpar=6,mnbas=400,mone=1)
      dimension  z((mnpar*(mnpar+1))/2)
      common /store1/ he(mnbas,mnbas),oe(mnbas,mnbas)
      common /store2/ xn(mnbas)
      common /nlpara/ a(mnbas,mnpar,mnpar)
      common /partic/ npar,kpot
      common /randomm/irand
      common /contro/ mm0,kk0,nbas0,nbas1,mnb,ico
      common /interv/ bmin,bmax,xmr((mnpar*(mnpar+1))/2)
      elo=1.d+10
      do 10 nbas=nbas0+1,mnb
      elow=elo
555   kk=0
      do 30 i=1,npar
      do 35 j=i+1,npar
c     generating  random numbers to give initial
c     values of the nonlinear parameters of the nbas-th basis state
      kk=kk+1
      xxx=ran2(irand)
c      yyy=ran2(irand)
c      ph=1.d0-2.d0*yyy
      z(kk)=1.d0/(bmin+bmax*xxx)**2
 35   continue
 30   continue
      nvar=kk      
      elo=elow
c     random optimization of the nbas-th basis state
      call matrix1(z,nvar,nbas,elo,ich0)
      if(ich0.eq.1) go to 555
c     write out the best basis up to now
      open(10,file='fbs.res')
      write(10,*)mnb,mone
      do 300 i=1,mnb
      write(10,*)((a(i,k,j),k=1,npar),j=1,npar)
 300  continue
      close(10)
 10   continue
      return
      end

      subroutine matrix1(zs,nvar,nbas,elow,ich0)
ccccccc
c     calculation of matrix elements (step-by-step)
ccccccc
      implicit real*8(a-h,o-z)
      parameter (mnpar=6,mnbas=400,ef0=100000.d0)
      dimension  hes(mnbas),oes(mnbas),as(mnpar,mnpar),
     & z((mnpar*(mnpar+1))/2),at(mnpar,mnpar),
     & zs((mnpar*(mnpar+1))/2)
      common /store1/ he(mnbas,mnbas),oe(mnbas,mnbas)
      common /store2/ xn(mnbas)
      common /nlpara/ a(mnbas,mnpar,mnpar)
      common /partic/ npar,kpot
      common /randomm/irand
      common /contro/ mm0,kk0,nbas0,nbas1,mnb,ico
      common /interv/ bmin,bmax,xmr((mnpar*(mnpar+1))/2)
      common /masses/ xm(mnpar),xred(mnpar),h2m
c     store the previous values 
      do 380 i=1,npar-1
      do 390 j=1,npar-1
      as(j,i)=a(nbas,j,i)
390   continue
380   continue
      do 350 i=1,nbas
      hes(i)=he(i,nbas)
      oes(i)=oe(i,nbas)
350   continue
      xns=xn(nbas)
c
      do 300 mm=1,mm0
      do 55 i=1,nvar
      z(i)=zs(i)
 55   continue
      do 200 kk=1,nvar
      zzs=z(kk)
      esel=elow
      do 20 k=1,kk0
 555  xxx=ran2(irand)
      yyy=ran2(irand)
      ph=1.d0-2.d0*yyy
c     generating new random nonlinear parameters
      z(kk)=1.d0/(bmin+bmax*xxx/xmr(kk))**2
c     check of  positive definiteness
      call trcorr(z,at)
      call choldc(at,ierr)
      if(ierr.eq.1) go to 555
      do 36 i=1,npar-1
      do 37 j=1,npar-1
      a(nbas,j,i)=at(i,j)
 37   continue
      xxx=ran2(irand)
 36   continue
c     matrix elements of the nbas-th basis state
      call mat_elem(nbas)
      ich0=1
      if(nbas.lt.5) ich0=2
c     check of the overcompleteness of the basis
      do 101 i=1,nbas-1
      www=dabs(oe(i,nbas)/dsqrt(oe(i,i)*oe(nbas,nbas)))
      if(www.gt.0.00001) ich0=2
      if(www.gt.0.99d0) then
      go to 555
      endif
 101  continue
      if(ich0.eq.1) then
       return
       endif
c     calculation of the ground state energy 
      info=1
      call diag(ener,nbas,info)
c     selection of the lowest energy
      if(ener.lt.esel) then
      esel=ener
      zzs=z(kk)
      endif
      if(ener.lt.elow) then
c     to store the best parameters found
      elow=ener
      do 45 i=1,nvar
      zs(i)=z(i)
 45   continue
      do 40 i=1,nbas
      hes(i)=he(i,nbas)
      oes(i)=oe(i,nbas)
 40   continue
      xns=xn(nbas)
      do 60 i=1,npar-1
      do 70 j=1,npar-1
      as(j,i)=a(nbas,j,i)
 70   continue
 60   continue
      endif
 20   continue
      z(kk)=zzs
 200  continue
 300  continue
c     recording the best parameters
      do 80 i=1,npar-1
      do 90 j=1,npar-1
      a(nbas,j,i)=as(j,i)
 90   continue
 80   continue
      do 50 i=1,nbas
      he(i,nbas)=hes(i)
      oe(i,nbas)=oes(i)
      he(nbas,i)=hes(i)
      oe(nbas,i)=oes(i)
 50   continue
      xn(nbas)=xns
      info=0
      call diag(ener,nbas,info)
      write(6,*)nbas,ener
      return
      end

      subroutine matrix2(zs,nvar,nbas,elow)
ccccccc
c     calculation of matrix elements (refinement)
ccccccc
      implicit real*8(a-h,o-z)
      parameter (mnpar=6,mnbas=400)
      dimension  hes(mnbas),oes(mnbas),as(mnpar,mnpar),
     & z((mnpar*(mnpar+1))/2),at(mnpar,mnpar),
     & zs((mnpar*(mnpar+1))/2)
      common /store1/ he(mnbas,mnbas),oe(mnbas,mnbas)
      common /store2/ xn(mnbas)
      common /nlpara/ a(mnbas,mnpar,mnpar)
      common /partic/ npar,kpot
      common /randomm/irand
      common /contro/ mm0,kk0,nbas0,nbas1,mnb,ico
      common /interv/ bmin,bmax,xmr((mnpar*(mnpar+1))/2)
c     store the previous values
      do 380 i=1,npar-1
      do 390 j=1,npar-1
      as(j,i)=a(nbas,j,i)
390   continue
380   continue
      do 350 i=1,nbas
      hes(i)=he(i,nbas)
      oes(i)=oe(i,nbas)
350   continue
      xns=xn(nbas)
      esel=elow
      do 300 mm=1,mm0
      do 55 i=1,nvar
      z(i)=zs(i)
 55   continue
      do 200 kk=1,nvar
      zzs=z(kk)
      do 20 k=1,kk0
555   xxx=ran2(irand)
      yyy=ran2(irand)
      ph=1.d0-2.d0*yyy
      if(mm*kk*k.ne.1) z(kk)=1.d0/(bmin+bmax*xxx)**2
c     check of the positive definiteness
      call trcorr(z,at)
      call choldc(at,ierr)
      if(ierr.eq.1) go to 555
      do 36 i=1,npar-1
      do 37 j=1,npar-1
      a(nbas,j,i)=at(i,j)
 37   continue
      xxx=ran2(irand)
 36   continue
c     matrix elements of the nbas-th state
      call mat_elem(nbas)
c     check of the overcompeleteness
      do 101 i=1,nbas-1
      www=dabs(oe(i,nbas)/dsqrt(oe(i,i)*oe(nbas,nbas)))
      if(www.gt.0.99d0) go to 555 
 101  continue
      info=1
c     gound state energy
      call diag(ener,nbas,info)
c     selection of the lowest energy
      if(ener.lt.esel) then
      esel=ener
      zzs=z(kk)
      endif
      if(ener.lt.elow) then
c     storing the best parameters
      elow=ener
      do 45 i=1,nvar
      zs(i)=z(i)
 45   continue
      do 40 i=1,nbas
      hes(i)=he(i,nbas)
      oes(i)=oe(i,nbas)
 40   continue
      xns=xn(nbas)
      do 60 i=1,npar-1
      do 70 j=1,npar-1
      as(j,i)=a(nbas,j,i)
 70   continue
 60   continue
      endif
 20   continue
      z(kk)=zzs
 200  continue
 300  continue
c     recording the best parameters found
      do 80 i=1,npar-1
      do 90 j=1,npar-1
      a(nbas,j,i)=as(j,i)
 90   continue
 80   continue
      do 50 i=1,nbas
      he(i,nbas)=hes(i)
      oe(i,nbas)=oes(i)
      he(nbas,i)=hes(i)
      oe(nbas,i)=oes(i)
 50   continue
      xn(nbas)=xns
      info=0
c     diagonalization
      call diag(ener,nbas,info)
      write(6,*)nbas,ener
      return
      end

      subroutine mat_elem(nbas)
ccccccc
c     calculation of matrix elements 
ccccccc
      implicit real*8(a-h,o-z)
      parameter (mnpar=6,mnbas=400,nfac=8000)
      dimension xt(mnbas),xo(mnbas),ap(mnbas,mnpar,mnpar),
     &  tr(mnpar,mnpar),xv(mnbas,0:4)
      common /masses/ xm(mnpar),xred(mnpar),h2m
      common /partic/ npar,kpot
      common /jacobi/ t(mnpar,mnpar),ti(mnpar,mnpar),
     &                b(mnpar,(mnpar*(mnpar+1))/2)
      common /nlpara/ a(mnbas,mnpar,mnpar)
      common /lambda/ xla(mnpar)
      common /detinv/ aap(mnbas,mnpar,mnpar),aapi(mnbas,mnpar,mnpar),
     &                d(mnbas)
      common /store1/ he(mnbas,mnbas),oe(mnbas,mnbas)
      common /store2/ xn(mnbas)
      common /spover/ spiso(nfac),trp(nfac,mnpar,mnpar),not
c     construction of A+A'
      np=npar-1
c     presetting variables
      do 200 m=1,nbas
      oe(m,nbas)=0.d0
      he(m,nbas)=0.d0
      oe(nbas,m)=0.d0
      he(nbas,m)=0.d0
 200  continue
c     do loop for symmetrization
      do 100 ip=1,not
      do 1 j=1,np
      do 2 i=1,np
      do 3 k=1,nbas
      ap(k,i,j)=a(k,i,j)
 3    continue
 2    continue
 1    continue
      do 4 j=1,np
      do 5 i=1,np
      tr(i,j)=trp(ip,i,j)
 5    continue
 4    continue
c     permutation of the matrix of nonlinear parameters
      call vtrafo(ap,tr,np,nbas)
      do 10 j=1,np
      do 20 i=1,np
      do 30 k=1,nbas
      aap(k,i,j)=a(k,i,j)+ap(nbas,i,j)
 30   continue
 20   continue
 10   continue
c     determinant  and inverse of A+A'
      call vinv(np,nbas) 
      call vdet(np,nbas) 
c     matrix elements
c
c     overlap
      call vove_mat(nbas,xo)
c     kinetic
      call vkin_ene(nbas,xt)
c     potential
      call vpot_ene(nbas,ip,xv)     
      if(ip.eq.1) xn(nbas)=1.d0
      do 40 m=1,nbas
      xxx=dsqrt(xn(m)*xn(nbas))
c     overlap
      oe(m,nbas)=oe(m,nbas)+spiso(ip)*xo(m)/xxx
      vvv=xv(m,0)+xv(m,1)+xv(m,2)+xv(m,3)+xv(m,4)
c     hamiltonian
      he(m,nbas)=he(m,nbas)+(spiso(ip)*xt(m)+vvv)/xxx
c     symmetric
      oe(nbas,m)=oe(m,nbas)
      he(nbas,m)=he(m,nbas)
 40   continue
 100  continue
      return
      end     

      subroutine cm_rel_tr
ccccccc
c     transformation between cm_rel and 
c     single particle coordinates
ccccccc
      implicit real*8(a-h,o-z)
      parameter (mnpar=6)
      dimension xma(mnpar),a(mnpar,mnpar),y(mnpar,mnpar),indx(mnpar)
      common /masses/ xm(mnpar),xred(mnpar),h2m
      common /partic/ npar,kpot
      common /jacobi/ t(mnpar,mnpar),ti(mnpar,mnpar),
     &                b(mnpar,(mnpar*(mnpar+1))/2)
      common /lambda/ xla(mnpar)
c     calculation of the jacobi matrix
      do 10 k=1,npar
      do 20 j=1,npar
      a(j,k)=0.d0
20    continue
10    continue
c     sum of masses
      sum=0.d0
      do 30 j=1,npar
      sum=sum+xm(j)
      xma(j)=sum   
30    continue
c     jacobi matrix
      do 40 j=1,npar-1
      a(j,j+1)=1.d0
      do 50 k=1,j
      a(j,k)=-xm(k)/xma(j)
50    continue
c     reduced mass
      xred(j)=xma(j)*xm(j+1)/xma(j+1)
40    continue
      do 60 j=1,npar
      a(npar,j)=xm(j)/xma(npar)
60    continue
      xred(npar)=xma(npar)
      do 70 j=1,npar
      do 80 i=1,npar
      t(i,j)=a(i,j)
 80   continue
 70   continue
c     inverse of jacobi matrix
      call ludcmp(a,npar,mnpar,indx,d)
      do  90 i=1,npar
      do 100 j=1,npar
      y(i,j)=0.d0
 100  continue
      y(i,i)=1.d0
  90  continue
      do 110 j=1,npar
      call lubksb(a,npar,mnpar,indx,y(1,j))
 110  continue
      do 120 i=1,npar
      do 130 j=1,npar
      ti(i,j)=y(i,j)
 130  continue
 120  continue
c     b vector
      k=0
      do 140 i=1,npar
      do 150 j=i+1,npar
      k=k+1
      do 160 m=1,npar
      b(m,k)=ti(i,m)-ti(j,m)
 160  continue
 150  continue
 140  continue
      kpot=k
c     lambda matrix of kinetic energy
      do 170 j=1,npar
      xla(j)=h2m/xred(j)
 170  continue
      return
      end 

      subroutine input_data
cccccc
c     reading the input data
cccccc
      implicit real*8(a-h,o-z)
      parameter (mnpar=6,npnt=10000,mspc=4)
      dimension z(mnpar)
      common /masses/  xm(mnpar),xred(mnpar),h2m
      common /partic/  npar,kpot
      common /potcoe/  pot((mnpar*(mnpar+1))/2)
      common /parqns1/ isp(mnpar,mspc),iso(mnpar,mspc)
      common /parqns2/ cspc(mspc),cisc(mspc),nspc,nisc
      common /randomm/ ii
      common /contro/  mm0,kk0,nbas0,nbas1,mnb,ico
      common /potrep1/ xll,xul,def(npnt),nv0
      common /potrep2/ fff(npnt,4)
      common /interv/  bmin,bmax,xmr((mnpar*(mnpar+1))/2)
      common /intera/  ipcon,npt,no,np(10,4),vp(10,4),ap(10,4),bp(10,4)
      common /potsel/  ip
      common /bosfer/  ibf
      call facc
      open(4,file='pot.inp')
c     selecting representation
      read(4,*)ipcon
c     number of potential terms and operators 
      read(4,*)npt,no
      do 20 k=1,no
      do 30 i=1,npt
c     see eq. (19)
      read(4,*)vp(i,k),ap(i,k),bp(i,k),np(i,k)
 30   continue
 20   continue
      if(no.eq.0) no=1
      close(4)
      if(ipcon.eq.1)then
c     for interpolation of the the potential 
      call potrep
      open(10,file='potrep.dat')
      read(10,*)xll,xul,nv0
      do 100 i=1,nv0
      sss=xll+(i-1)*(xul-xll)/dfloat(nv0)
      def(i)=sss
 100  continue
      do 300 k=1,no
      do 200 i=1,nv0
      read(10,*)fff(i,k)
 200  continue
 300  continue
      close(10)
      endif
      open(1,file='fbs.inp')
      read(1,*)npar
      read(1,*)(xm(i),i=1,npar)
      read(1,*)(z(i),i=1,npar)
      read(1,*)nisc
      do 400 k=1,nisc
      read(1,*)cisc(k),(iso(i,k),i=1,npar)
 400  continue
      read(1,*)nspc
      do 410 k=1,nspc
      read(1,*)cspc(k),(isp(i,k),i=1,npar)
 410  continue
      read(1,*)hh,ii,ico,ibf
      h2m=0.5d0*hh
      read(1,*)mm0,kk0,mnb
      read(1,*)bmin,bmax
      close(1)
c     coulomb potential for charged particles
      k=0
      do 40 i=1,npar
      do 50 j=i+1,npar
      k=k+1
      pot(k)=z(i)*z(j)
 50   continue
 40   continue
c     
      kk=0
      do i=1,npar
        do j=i+1,npar
         kk=kk+1
           if(pot(kk).eq.0.d0) then
             xmr(kk)=2.d0*xm(i)*xm(j)/(xm(i)+xm(j))
           else
             xmr(kk)=1.d0
           endif
        end do
      end do
      return
      end

      subroutine me_spiso
cccccc
c     m.e. in spin-isospin space
c
cccccc
      implicit real*8(a-h,o-z)
      parameter (nfac=8000,mnpar=6,ifd=12,mspc=4)
      dimension ifct(0:ifd),ia(ifd),wm((mnpar*(mnpar+1))/2),
     &  wb((mnpar*(mnpar+1))/2),wh((mnpar*(mnpar+1))/2)
      common /permu1/ c(nfac,mnpar,mnpar),p(nfac)
      common /permu2/ iexc(nfac,mnpar),nper
      common /partic/ npar,kpot
      common /parqns1/ isp(mnpar,mspc),iso(mnpar,mspc)
      common /parqns2/ cs(mspc),ci(mspc),nspc,nisc
      common /spover/ spiso(nfac),trp(nfac,mnpar,mnpar),not
      common /majora/ w(nfac,(mnpar*(mnpar+1))/2,4)
      common /intera/  ipcon,npt,no,np(10,4),vp(10,4),ap(10,4),bp(10,4)
      data (ifct(i),i=0,ifd) /1,1,2,6,24,120,720,5040,40320,362880,
     &                        3628800,39916800,479001600/
c     overlap
      k=0
c     number of permutations
      nper=ifct(npar)
      do 10 i=1,nper
c     permutations
      call permut(i,npar,ia)
      iw=0
      im=0
      ib=0
      ih=0
      ww=0.d0
      do 200 kk=1,kpot
      wm(kk)=0.d0
      wb(kk)=0.d0
      wh(kk)=0.d0
 200  continue
c     do loop over the spin configurations
      do 12 is1=1,nspc
      do 13 is2=1,nspc
c     do loop over the isospin configurations
      do 14 ii1=1,nisc
      do 15 ii2=1,nisc
      ccc=cs(is1)*cs(is2)*ci(ii1)*ci(ii2)
c     unit operator (wigner part)
      do 20 j=1,npar
      i1=j
      i2=ia(j)
c     overlap of spin-isospin components
      if(isp(i1,is1)-isp(i2,is2).ne.0) go to 5
      if(iso(i1,ii1)-iso(i2,ii2).ne.0) go to 5
 20   continue
      ww=ww+ccc
      iw=1
 5    kp=0
      if(no.eq.1) go to 13
c     majorana-part
      do 210 l=1,npar
      do 220 j=l+1,npar
      i1=l
      i2=j
      i3=ia(l)
      i4=ia(j)     
      kp=kp+1
c     matrix elements of the majorana operator with spin-isospin components
      if(isp(i1,is1)-isp(i4,is2).ne.0) go to 220
      if(isp(i2,is1)-isp(i3,is2).ne.0) go to 220
      if(iso(i1,ii1)-iso(i4,ii2).ne.0) go to 220
      if(iso(i2,ii1)-iso(i3,ii2).ne.0) go to 220
      do 230 m=1,npar
      if(m.eq.l) go to 230
      if(m.eq.j) go to 230
      k1=m
      k2=ia(m)
      if(isp(k1,is1)-isp(k2,is2).ne.0) go to 220
      if(iso(k1,ii1)-iso(k2,ii2).ne.0) go to 220
 230  continue
      im=1
      wm(kp)=wm(kp)+ccc
220   continue
210   continue
c     bartlett-part
      kp=0
      do 310 l=1,npar
      do 320 j=l+1,npar
      i1=l
      i2=j
      i3=ia(l)
      i4=ia(j)     
      kp=kp+1
c     matrix elements of the bartlett operator with spin-isospin components
      if(isp(i1,is1)-isp(i4,is2).ne.0) go to 320
      if(isp(i2,is1)-isp(i3,is2).ne.0) go to 320
      if(iso(i1,ii1)-iso(i3,ii2).ne.0) go to 320
      if(iso(i2,ii1)-iso(i4,ii2).ne.0) go to 320
      do 330 m=1,npar
      if(m.eq.l) go to 330
      if(m.eq.j) go to 330
      k1=m
      k2=ia(m)
      if(isp(k1,is1)-isp(k2,is2).ne.0) go to 320
      if(iso(k1,ii1)-iso(k2,ii2).ne.0) go to 320
 330  continue
      ib=1
      wb(kp)=wb(kp)+ccc
320   continue
310   continue
c     heisenberg-part
      kp=0
      do 410 l=1,npar
      do 420 j=l+1,npar
      i1=l
      i2=j
      i3=ia(l)
      i4=ia(j)     
      kp=kp+1
c     matrix elements of the heisenberg operator 
c     with spin-isospin components
      if(isp(i1,is1)-isp(i3,is2).ne.0) go to 420
      if(isp(i2,is1)-isp(i4,is2).ne.0) go to 420
      if(iso(i1,ii1)-iso(i4,ii2).ne.0) go to 420
      if(iso(i2,ii1)-iso(i3,ii2).ne.0) go to 420
      do 430 m=1,npar
        if(m.eq.l) go to 430
        if(m.eq.j) go to 430
      k1=m
      k2=ia(m)
      if(isp(k1,is1)-isp(k2,is2).ne.0) go to 420
      if(iso(k1,ii1)-iso(k2,ii2).ne.0) go to 420
430   continue
      ih=1
      wh(kp)=wh(kp)+ccc
420   continue
410   continue
c     end of do loops over spin configurations
 15   continue
 14   continue
 13   continue
 12   continue
c
      if(im+iw+ib+ih.eq.0) go to 10
      k=k+1
c     parity of the permutations
      call pape(ia,npar,ip)
      p(k)=1.d0*ip
      do 30 l=1,npar
      do 40 j=1,npar
      c(k,j,l)=0.d0
 40   continue
 30   continue
      do 50 l=1,npar
      c(k,ia(l),l)=1.d0
 50   continue
      spiso(k)=p(k)*ww
      do 90 kp=1,kpot
        w(k,kp,1)=ww*ip
        w(k,kp,2)=-wm(kp)*ip
        w(k,kp,3)=wb(kp)*ip
        w(k,kp,4)=-wh(kp)*ip
 90   continue
 10   continue
      not=k
      write(6,*)not
c     for transformation of the nonlinear parameters
      call crot
      do 60 k=1,not
      do 70 m=1,npar-1
      do 80 l=1,npar-1
      trp(k,l,m)=c(k,l,m)
 80   continue
 70   continue
 60   continue
      return
      end

c=======================================
c    determinant (real symmetric)
c=======================================
c    b : matrix
c    d : determinant
c=======================================
      subroutine vdet(n,nvd)
      implicit real*8 (a-h,o-z)
      parameter (mnbas=400,mnpar=6)
      common/detinv/ a(mnbas,mnpar,mnpar),b(mnbas,mnpar,mnpar),
     & d(mnbas)
      if(n.eq.1) then
      do 5 m=1,nvd
      d(m)=a(m,1,1)
 5    continue
      return
      endif
      n1=n-1
      do 10 i=1,n1
      ii=i+1
      do 50 k=ii,n
      do 60 l=1,nvd
      d(l)=a(l,i,k)/a(l,i,i)
 60   continue
      do 15 j=ii,n
      do 20 l=1,nvd
      a(l,j,k)=a(l,j,k)-a(l,j,i)*d(l)
 20   continue
 15   continue
 50   continue
 10   continue
      do 1 i=1,nvd
      d(i)=1.d0
 1    continue
      do 30 i=1,n
      do 40 l=1,nvd
      d(l)=d(l)*a(l,i,i)
40    continue
30    continue
      end

      subroutine vinv(n,nvd)
c     inverse (real symmetric matrix)
      implicit real*8(a-h,o-z)
      parameter (mnbas=400,mnpar=6)
      dimension b(mnbas,mnpar,2*mnpar),x(mnbas)
      common/detinv/ e(mnbas,mnpar,mnpar),a(mnbas,mnpar,mnpar),
     & d(mnbas)
      if(n.eq.1) then
      do 5 m=1,nvd
      a(m,1,1)=1.d0/e(m,1,1)
 5    continue
      return
      endif
      j2=n*2
      do 10 j=n+1,j2
      do 20 i=1,n
      do 30 l=1,nvd
      b(l,i,j)=0.d0
30    continue
20    continue
10    continue
      do 40 i=1,n
      iji=i+n
      do 50 l=1,nvd
      b(l,i,iji)=1.d0
50    continue
40    continue
      do 60 i=1,n
      do 70 j=1,n
      do 80 l=1,nvd
      b(l,j,i)=e(l,j,i)
80    continue
70    continue
60    continue
      do 90 i=1,n
      do 100 l=1,nvd
      x(l)=b(l,i,i)
100   continue
      do 110 k=i,j2
      do 120 l=1,nvd
      b(l,i,k)=b(l,i,k)/x(l)
120   continue
110   continue
      do 130 m=1,n
      do 140 l=1,nvd
      x(l)=b(l,m,i)
140   continue
      if (m.eq.i) goto 130
      do 150 k=i,j2
      do 160 l=1,nvd
      b(l,m,k)=b(l,m,k)-b(l,i,k)*x(l)
160   continue
150   continue
130   continue
 90   continue
      do 170 j=1,n
      do 180 i=1,n
      do 190 l=1,nvd
      a(l,i,j)=b(l,i,j+n)
190   continue
180   continue
170   continue
      return
      end

      subroutine vtrafo(a,t,npar,nvd)
      implicit real*8 (a-h,o-z)
      parameter (mnpar=6,mnbas=400)
c
c     a--->Transpose[t].a.t
c
      dimension a(mnbas,mnpar,mnpar),t(mnpar,mnpar),
     & x(mnbas,mnpar,mnpar),sum(mnbas)
c
      do 10 i=1,npar
      do 20 j=1,npar
      do 30 n=1,nvd
      sum(n)=0.d0
 30   continue
      do 40 k=1,npar
      do 50 n=1,nvd
      sum(n)=sum(n)+a(n,i,k)*t(k,j)
 50   continue
 40   continue
      do 60 n=1,nvd
      x(n,i,j)=sum(n)
 60   continue
 20   continue
 10   continue
      do 70 i=1,npar
      do 80 j=1,npar
      do 90 n=1,nvd
      sum(n)=0.d0
 90   continue
      do 100 k=1,npar
      do 110 n=1,nvd
      sum(n)=sum(n)+t(k,i)*x(n,k,j)
110   continue
100   continue
      do 120 n=1,nvd
      a(n,i,j)=sum(n)
120   continue
 80   continue
 70   continue
      return
      end

      subroutine vkin_ene(nbas,sum)
c     kinetic energy (see eq. (7))
      implicit real*8 (a-h,o-z)
      parameter (mnpar=6,mnbas=400)
      dimension sum(mnbas),x(mnbas,mnpar,mnpar),summ(mnbas)
      common /partic/ npar,kpot
      common /nlpara/ a(mnbas,mnpar,mnpar)
      common /lambda/ xla(mnpar)
      common /detinv/ aap(mnbas,mnpar,mnpar),aapi(mnbas,mnpar,mnpar),
     &                d(mnbas)
c
      np=npar-1
      do 10 i=1,np
      do 20 j=1,np
      do 30 n=1,nbas
      sum(n)=0.d0
 30   continue
      do 40 k=1,np
      do 50 n=1,nbas
      w=-a(n,i,k)*xla(k)
      sum(n)=sum(n)+w*a(n,k,j)
 50   continue
 40   continue
      do 60 n=1,nbas
      x(n,i,j)=sum(n)
 60   continue
 20   continue
 10   continue
      do 70 n=1,nbas
      sum(n)=0.d0
 70   continue
      do 80 i=1,npar
      do 90 k=1,npar
      do 100 n=1,nbas
      sum(n)=sum(n)+3.d0*aapi(n,k,i)*(x(n,k,i))
100   continue
 90   continue
 80   continue
      do 110 n=1,nbas
      summ(n)=0.d0
 110  continue
      do 120 k=1,np
      do 130 n=1,nbas
      summ(n)=summ(n)+xla(k)*a(n,k,k)*3.d0
 130  continue
 120  continue
      do 140 n=1,nbas
      sum(n)=(summ(n)+sum(n))/d(n)**1.5d0
 140  continue
      return
      end

      subroutine vove_mat(nbas,sum)
c     overlap (see eq. (6))
      implicit real*8 (a-h,o-z)
      parameter (mnpar=6,mnbas=400)
      dimension sum(mnbas)
      common /partic/ npar,kpot
      common /nlpara/ a(mnbas,mnpar,mnpar)
      common /detinv/ aap(mnbas,mnpar,mnpar),aapi(mnbas,mnpar,mnpar),
     &                d(mnbas)
c
      do 10 k=1,nbas
      sum(k)=1.d0/d(k)**1.5d0
 10   continue
c
      return
      end

      subroutine vpot_ene(nbas,ip,v)
c     matrix elements of the potential (see eq. (16))
      implicit real*8 (a-h,o-z)
      parameter (nfac=8000,mnpar=6,mnbas=400)
      dimension u(mnbas),v(mnbas,0:4),p(mnbas,0:4),y(0:4)
      common /potcoe/ pot((mnpar*(mnpar+1))/2)
      common /nlpara/ a(mnbas,mnpar,mnpar)
      common /detinv/ aap(mnbas,mnpar,mnpar),aapi(mnbas,mnpar,mnpar),
     &                d(mnbas)
      common /partic/ npar,kpot
      common /jacobi/ t(mnpar,mnpar),ti(mnpar,mnpar),
     &                b(mnpar,(mnpar*(mnpar+1))/2)
      common /majora/ wm(nfac,(mnpar*(mnpar+1))/2,4)
      common /spover/ spiso(nfac),trp(nfac,mnpar,mnpar),not
      common /intera/ ipcon,npt,no,npp(10,4),vp(10,4),ap(10,4),bp(10,4)      
c    
      np=npar-1
c     do loop over the two-body interactions
      do 1 k=0,4
      do 5 m=1,nbas
      v(m,k)=0.d0
 5    continue
 1    continue
c     summation over interacting pairs
      do 10 k=1,kpot
       y(0)=wm(ip,k,1)*pot(k)
       y(1)=wm(ip,k,1)
       y(2)=wm(ip,k,2)
       y(3)=wm(ip,k,3)
       y(4)=wm(ip,k,4)
       yy=dabs(y(1))+dabs(y(2))+dabs(y(3))+dabs(y(4))
      if(yy.eq.0.d0) go to 10
c     construction of u
      do 20 m=1,nbas
      u(m)=0.d0
 20   continue
      do 30 i=1,np
      w1=b(i,k)
      do 40 j=1,np
      w=w1*b(j,k)
      do 50 m=1,nbas 
      u(m)=u(m)+w*aapi(m,i,j)
 50   continue
 40   continue
 30   continue
c     integration over the radial potential
      call poten(nbas,u,p)
      do 70 kk=0,no
      do 60 m=1,nbas
      www=p(m,kk)/d(m)**1.5d0*y(kk)
      v(m,kk)=v(m,kk)+www
 60   continue
 70   continue
 10   continue
      return
      end

      subroutine poten(nbas,u,v)
c     see eq. (17)
      implicit real*8 (a-h,o-z)
      parameter (mnbas=400,zero=0.d0)
      dimension u(mnbas),v(mnbas,0:4)
      common /intera/  ipcon,npt,no,np(10,4),vp(10,4),ap(10,4),bp(10,4)
      data pi/3.1415926535897932384d0/
c     coulomb potential 
      do 1 m=1,nbas
        v(m,0)=0.d0
 1    continue
      nn=1
      do 2 m=1,nbas
      a=u(m)
      p=(0.5d0/a)
      ppp=potmat(p,zero,nn)/(2.d0*a)**1.5d0*4.d0/dsqrt(pi)
      v(m,0)=v(m,0)+ppp
 2    continue
c
      do 5 k=1,no
      do 10 m=1,nbas
      v(m,k)=0.d0
 10   continue
      if(ipcon.eq.1) then
      do 20 m=1,nbas
      a=u(m)
      p=0.5d0/a
      ppp=potval(p,k)/(2.d0*a)**1.5d0/pi**1.5d0
      v(m,k)=ppp
 20   continue
      endif
      if(ipcon.eq.2) then
      do 30 i=1,npt
      nn=2+np(i,k)
      do 40 m=1,nbas
      a=u(m)
      p=(0.5d0/a+ap(i,k))
      ppp=potmat(p,bp(i,k),nn)/(2.d0*a)**1.5d0*4.d0/dsqrt(pi)
      v(m,k)=v(m,k)+vp(i,k)*ppp
 40   continue
 30   continue
      endif
 5    continue
      return
      end
      
      subroutine diag(ener,miki,info)
c
c     to solve the generalized eigenvalue problem
c     ener : lowest eigenvalue
c     miki : dimension of basis
c     info : if(info.eq.1) calculate the lowest eigenvalue only 
c     mindi: basis size for approximate solution 
c     ic   : full diagonalization in each ico-th step
c
      implicit real*8 (a-h,o-z)
      parameter(mnbas=400,mindi=mnbas,ic=1,mnpar=6)
      dimension dl(mnbas),e(mnbas),od(mnbas),hd(mnbas)
      common /hawk/    v(mnbas,mnbas),r(mnbas)
      common /store1/  he(mnbas,mnbas),oe(mnbas,mnbas)
      common /diagcon/ ico
      common /nlpara/ a(mnbas,mnpar,mnpar)
      common /bali/ ica,ierr
      common /partic/ npar,kpot
c--------------------------------------------------------------------
      if(info.eq.2) go to 100 
      if(ierr.eq.1.or.ica.eq.1.or.miki.lt.1) go to 100
      if(info.eq.1) call eigval(ener,miki)
      if(info.eq.1) return
      ico=ico+1
      if(info.eq.0) call eigval2(ener,miki)
100   if(info.eq.2.or.ico.eq.ic.or.ica.eq.1.or.
     &  ierr.eq.1.or.miki.lt.1) then
      do 1 i=1,miki
      hd(i)=he(i,i)
      od(i)=oe(i,i)
  1   continue
      ifail=0
      ico=0
      call f02aef(he,mnbas,oe,mnbas,miki,r,v,mnbas,dl,e,ifail)
      open(16,file='ener.dat')
      write(16,*)'dimension  ',miki
      write(16,*)'energy(1)  ',r(1) 
      write(16,*)'energy(2)  ',r(2) 
      write(16,*)'energy(3)  ',r(3) 
      close(16)
      do 2 i=1,miki
      he(i,i)=hd(i)
      oe(i,i)=od(i)
  2   continue
      do 10 i=1,miki
      do 20 j=i,miki
      he(j,i)=he(i,j)
      oe(j,i)=oe(i,j)
  20  continue
  10  continue
      ener=r(1)
      if(miki.lt.mindi) go to 60
      do 30 i=1,miki
      ss=0.d0
      do 40 j=mindi-1,miki
      ss=ss+v(i,j)
  40  continue
      v(i,mindi-1)=ss/dsqrt(dfloat(miki-mindi+1))
  30  continue 
      ss=0.d0
      do 50 j=mindi-1,miki
      ss=ss+r(j)
  50  continue
      r(mindi-1)=ss/dfloat(miki-mindi+1)
      endif
  60  continue
      return
      end

      subroutine eigval(ener,miki)
c     lowest eigenvalue only
      implicit real*8 (a-h,o-z)
      parameter(mnbas=400,mindi=mnbas,ef0=100000.d0)
      dimension psi(mnbas),hpsi(mnbas)
      common /condor/ ee(mnbas),q(mnbas-1),mi
      common /hawk/   v(mnbas,mnbas),r(mnbas)
      common /eagle/  x1,x2
      common /store1/he(mnbas,mnbas),oe(mnbas,mnbas)
c--------------------------------------------------------------------
      mi=min(miki,mindi)
      do 2005 i=1,mi
      psi(i)=0.d0
2005  continue
c     overlap of the new basis states with 
c     the previously selected orthogonalized basis
      do 2010 i=1,mi-1
      s=0.d0
      do 2020 j=1,miki-1
      s=s+v(j,i)*oe(j,miki)
2020  continue
      psi(i)=s
2010  continue
c     norm of the new basis state 
      s=oe(miki,miki)
      do 2030 i=1,mi-1     
      s=s-psi(i)*psi(i)
2030  continue
      if(s.le.0.d0) then
      ener=ef0
      return
      endif
      xnb=dsqrt(s)
c     matrix element of the hamiltonian between the previously
c     selected and the new basis states
      do 2035 i=1,mi
      hpsi(i)=0.d0
2035  continue
      do 2040 i=1,mi-1
      s=0.d0
      do 2050 j=1,miki-1
      s=s+v(j,i)*he(j,miki)
2050  continue
      hpsi(i)=s
2040  continue
      do 2060 i=1,mi-1
      q(i)=(hpsi(i)-r(i)*psi(i))/(xnb)
2060  continue
c     matrix element of the hamiltonian between the new states
      s=he(miki,miki)
      do 2070 i=1,mi-1     
      s=s+r(i)*psi(i)*psi(i)-2.d0*hpsi(i)*psi(i)
2070  continue
      ee(mi)=s/(xnb**2)
      do 2080 i=1,mi-1
      ee(i)=r(i)
2080  continue
      if(miki.eq.1) then
      ener=ee(1)
      return
      endif
      xacc=1.0d-12
      brac=0.05d0
333   brac=2.d0*brac
      if(brac.gt.1000.d0) then
      ener=ef0
      return
      endif 
      x1=r(1)-brac
      x2=r(1)
      ifa=0
      eig=zerus(x1,x2,xacc,ifa)
      if(ifa.eq.1) go to 333
      ener=eig
      return
      end

      subroutine eigval2(ener,miki)
      implicit real*8 (a-h,o-z)
      parameter(mnbas=400,mindi=100)
      dimension psi(mindi),hpsi(mindi),a(mindi,mindi),
     & b(mindi,mindi),w(mnbas,mindi),u(mindi,mindi),e(mindi),
     & d(mindi),xx(mindi)
      common /hawk/      v(mnbas,mnbas),r(mnbas)
      common /store1/    he(mnbas,mnbas),oe(mnbas,mnbas)
c--------------------------------------------------------------------
      mi=min(miki,mindi)
      do 1 i=1,miki
      v(i,mi)=0.d0
1     continue
      v(miki,mi)=1.d0
      do 5 i=1,mi
      psi(i)=0.d0
   5  continue
c     overlap of the new basis states with 
c     the previously selected orthogonalized basis
      do 10 i=1,mi-1
      s=0.d0
      do 20 j=1,miki-1
      s=s+v(j,i)*oe(j,miki)
  20  continue
      psi(i)=s
  10  continue
c     norm of the new would be basis state 
      psi(mi)=oe(miki,miki)
c     matrix element of the hamiltonian between the previously
c     selected and the new basis states
      do 35 i=1,mi
      hpsi(i)=0.d0
  35  continue
      do 40 i=1,mi-1
      s=0.d0
      do 50 j=1,miki-1
      s=s+v(j,i)*he(j,miki)
  50  continue
      hpsi(i)=s
  40  continue
      hpsi(mi)=he(miki,miki)
      do 60 i=1,mi
      do 70 j=1,mi
      a(i,j)=0.d0
      b(i,j)=0.d0
  70  continue
  60  continue
      do 80 i=1,mi-1
      a(i,i)=r(i)
      b(i,i)=1.d0
      a(mi,i)=hpsi(i)
      a(i,mi)=hpsi(i)
      b(i,mi)=psi(i)
      b(mi,i)=psi(i)
  80  continue
      a(mi,mi)=hpsi(mi)
      b(mi,mi)=psi(mi)
      call f02aef(a,mindi,b,mindi,mi,xx,u,mindi,d,e,ifail)
      if(ifail.ne.0) write(6,*)'ifail  ',ifail
      ener=xx(1)
      do 2130 i=1,mi
      do 2140 j=1,miki
      sss=0.d0
      do 2150 k=1,mi
      sss=sss+v(j,k)*u(k,i)
2150  continue
      w(j,i)=sss
2140  continue
2130  continue
      if(mi.lt.mindi) then
      do 2160 i=1,miki
      do 2170 j=1,mi
      v(i,j)=w(i,j)
2170  continue
      r(i)=xx(i)
2160  continue
      else
      do 2165 i=1,miki
      do 2175 j=1,mi-2
      v(i,j)=w(i,j)
2175  continue
      v(i,mi-1)=(w(i,mi)+w(i,mi-1))/dsqrt(2.d0)
2165  continue 
      do 2185 i=1,mi-2
      r(i)=xx(i)
2185  continue
      r(mi-1)=(xx(mi)+xx(mi-1))/2.d0
      endif
      return
      end


      function func(x)
c     characteristic polynomial
c     (1994 october, Tokyo)
      implicit real*8(a-h,o-z)
      parameter (mnbas=400)
      common /eagle/  x1,x2
      common /condor/ e(mnbas),q(mnbas-1),n
c     
      if(x.eq.x1) x=eps(x,2)
      if(x.eq.x2) x=eps(x,1)
      w=1.d0
      prod=w
      w=w*(e(n)-x)
      do 20 j=1,n-1
      ww=q(j)*q(j)*prod/(e(j)-x)
      w=w-ww
 20   continue
      func=w
      end

      function funo(x,k)
c     norm of the eigenvectors 
c     (1994 october, Tokyo)
      implicit real*8(a-h,o-z)
      parameter (mnbas=400)
      common /condor/ e(mnbas),q(mnbas-1),n
c     
      w=((e(k)-x)/q(k))**2
      do 20 j=1,n-1
      ww=(q(j)/q(k))**2*((e(k)-x)/(e(j)-x))**2
      w=w+ww
 20   continue
      funo=dsqrt(w)
      end

      integer function p01aaf(ifail, error, srname)
c     mark 1 release.  nag copyright 1971
c     mark 3 revised
c     mark 4a revised, ier-45
c     mark 4.5 revised
c     mark 7 revised (dec 1978)
c     returns the value of error or terminates the program.
      integer error, ifail, nout
c$p 1
      double precision srname
c*** for v5 compatability only
c     test if no error detected
      if (error.eq.0) go to 20
c     determine output unit for message
c       call p01zzf for compatability, it calls x04aaf
c      call x04aaf (0,nout)
      nout = 9
c     test for soft failure
      if (mod(ifail,10).eq.1) go to 10
c     hard failure
      write (nout,99999) srname, error
c     stopping mechanism may also differ
      call p01aaz
c     stop
c     soft fail
c     test if error messages suppressed
   10 if (mod(ifail/10,10).eq.0) go to 20
      write (nout,99999) srname, error
   20 p01aaf = error
      return
99999 format (1h0, 38herror detected by nag library routine , a8,
     * 11h - ifail = , i5//)
      end

      subroutine p01aaz
c     routine to stop program
      write(6,*)'stop'
      end

      function zerus(aa,ff,acc,ifa)
c
c       func   - the external function whose zero is searched
c       z      - the three initial guess values
c       maxf   - maximum number of the iteration
c
        implicit real*8 (a-h,o-z)
        parameter (maxf=300)
        dimension z(3)
        z(1)=aa
        z(3)=ff
        z(2)=(aa+ff)*0.5d0
        g1=1.d0/func(z(1))
        g2=1.d0/func(z(2))
        g3=1.d0/func(z(3))
        do 5 ifu=1,maxf
        ic1=1
        ic2=1
        if((g1.lt.0.d0.and.g2.lt.0.d0).or.
     &     (g1.gt.0.d0.and.g2.gt.0.d0)) ic1=0
        if((g2.lt.0.d0.and.g3.lt.0.d0).or.
     &     (g2.gt.0.d0.and.g3.gt.0.d0)) ic2=0
c     no  root in the interval
        if(ic1+ic2.eq.0) then
        zerus=z(1)
        if(ifu.eq.1) ifa=1
        return
        endif
            z12=z(1)-z(2)
            z23=z(2)-z(3)
            z13=z(1)-z(3)
            www=g1*z23-g2*z13+g3*z12        
         z4=1.d+30
         if(www.ne.0.d0) 
     &       z4=(g1*z(1)*z23-g2*z(2)*z13+g3*z(3)*z12)/www
c     check bounds
c     bisection
         if(ic1.eq.1) then
         if(z4.lt.z(1).or.z4.gt.z(2)) then
         xx=z(2)
         z(3)=z(2)         
         g3=g2
         z(2)=(z(1)+xx)*0.5d0
         f2=func(z(2))
         if(dabs(f2).lt.acc) then
         zerus=z(2)
         return
         endif
         g2=1.d0/f2
         else
         xx=z(2)
         z(3)=z(2)         
         g3=g2
         z(2)=z4
         f2=func(z(2))
         if(dabs(f2).lt.acc) then
         zerus=z(2)
         return
         endif
         g2=1.d0/f2
         endif         
         endif
         if(ic2.eq.1) then
         if(z4.lt.z(2).or.z4.gt.z(3)) then
         z(1)=z(2)         
         g1=g2
         z(2)=(z(2)+z(3))*0.5d0
         f2=func(z(2))
         if(dabs(f2).lt.acc) then
         zerus=z(2)
         return
         endif
         g2=1.d0/f2
         else
         z(1)=z(2)         
         g1=g2
         z(2)=z4
         f2=func(z(2))
         if(dabs(f2).lt.acc) then
         zerus=z(2)
         return
         endif
         g2=1.d0/f2
         endif         
         endif
        if(dabs(z4-z(3)).lt.acc) go to 10
   5    continue
        zerus=z(1)
        return
  10    zerus=z4
        return
        end

        subroutine trcorr(z,a)
c       transfromation from pairwise relative coordinate-system
c       to Jacobi-system (nonlinear parameters)
        implicit real*8(a-h,o-z)
        parameter (mnpar=6)
        dimension z((mnpar*(mnpar+1))/2),a(mnpar,mnpar),
     &  g(mnpar,mnpar),xn(mnpar,mnpar)
      common /jacobi/ t(mnpar,mnpar),ti(mnpar,mnpar),
     &                b(mnpar,(mnpar*(mnpar+1))/2)
        common /partic/ npar,kpot
        k=0
        do i=1,npar
        do j=i+1,npar
        k=k+1
        xn(i,j)=dabs(z(k))
        xn(j,i)=dabs(z(k))
        end do
        xn(i,i)=0.d0
        end do
        do i=1,npar
        do j=i+1,npar
        g(i,j)=-xn(i,j)        
        g(j,i)=-xn(i,j)
        end do
        ss=0.d0
        do k=1,npar
        ss=ss+xn(i,k)
        end do
        g(i,i)=ss
        end do
        call trafo3(g,ti)
        do i=1,npar-1
        do j=1,npar-1
        a(i,j)=g(i,j)
        end do
        end do
        return
        end

      subroutine trafo3(a,t)
      implicit real*8 (a-h,o-z)
      parameter (mnpar=6)
c
c     a--->Transpose[t].a.t
c
      dimension a(mnpar,mnpar),t(mnpar,mnpar),x(mnpar,mnpar)
      common /partic/ npar,kpot
c
      do 3 i=1,npar
      do 4 j=1,npar
      sum=0.d0
      do 5 k=1,npar
      sum=sum+a(i,k)*t(k,j)
 5    continue
      x(i,j)=sum
 4    continue
 3    continue
      do 6 i=1,npar
      do 7 j=1,npar
      sum=0.d0
      do 8 k=1,npar
      sum=sum+t(k,i)*x(k,j)
 8    continue
      a(i,j)=sum
 7    continue
 6    continue
      return
      end

      subroutine rotm(carg,td)
      implicit real*8 (a-h,o-z)
      parameter (mnpar=6)
c     matrix N dimensional rotation 
      dimension carg(mnpar**2),td(mnpar,mnpar),a(mnpar,mnpar),
     & e(mnpar,mnpar)
      common /partic/ npar,kpot
      np=npar-1
      do 1 i=1,np
      do 2 j=1,np
      e(i,j)=0.d0
      if(i.eq.j) e(i,i)=1.d0
2     continue
1     continue
      kk=0
      do 3 i=1,np
      do 4 j=i+1,np
      kk=kk+1
      cc=dcos(carg(kk))
      ss=dsin(carg(kk))
      do 5 k=1,np
      do 6 l=1,np
      a(k,l)=0.d0
      if(k.eq.l) a(k,k)=1.d0
6     continue
5     continue
      a(i,i)=cc
      a(j,j)=cc
      a(i,j)=ss
      a(j,i)=-ss 
      do 7 k=1,np
      do 8 l=1,np  
      sum=0.d0         
      do 9 m=1,np
      sum=sum+a(k,m)*e(m,l)
 9    continue
      td(k,l)=sum
 8    continue
 7    continue
      do 10 k=1,np
      do 11 l=1,np
      e(k,l)=td(k,l)
11    continue
10    continue
 4    continue
 3    continue
      return
      end

      subroutine choldc(ac,ierr)
c     return ierr=1 for non-positive definite matrix
      implicit real*8(a-h,o-z)
      parameter (mnpar=6)
      dimension a(mnpar,mnpar),p(mnpar),ac(mnpar,mnpar)
      common /partic/ npar,kpot
      ierr=0
      n=npar-1
      do 1 j=1,n
      do 2 i=1,n
      a(i,j)=ac(i,j)
 2    continue
 1    continue
      do 13 i=1,n
        do 12 j=i,n
          sum=a(i,j)
          do 11 k=i-1,1,-1
            sum=sum-a(i,k)*a(j,k)
11        continue
          if(i.eq.j)then
            if(sum.le.0.) then 
            ierr=1
            return
            endif
            p(i)=sqrt(sum)
          else
            a(j,i)=sum/p(i)
          endif
12      continue
13    continue
      return
      end

      subroutine cperm
      implicit real*8(a-h,o-z)
      parameter (ifd=12,nfac=8000,mnpar=6)
      dimension ifct(0:ifd),ia(ifd)
      common /permu1/ c(nfac,mnpar,mnpar),p(nfac)
      common /permu2/ iexc(nfac,mnpar),nper
      common /partic/ np,kpot
      data (ifct(i),i=0,ifd) /1,1,2,6,24,120,720,5040,40320,362880,
     &                        3628800,39916800,479001600/
      nper=ifct(np)
      write(6,*)nper,np
      do 10 k=1,ifct(np)
      do 20 i=1,np
      do 30 j=1,np
      c(k,i,j)=0.d0
 30   continue
 20   continue
      call permut(k,np,ia)
      call pape(ia,np,ip)
      p(k)=1.d0*ip
      do 40 i=1,np
      iexc(k,i)=ia(i)
      c(k,i,ia(i))=1.d0
 40   continue
 10   continue     
      call crot
      end

      SUBROUTINE PERMUT(NRP,N,IA)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (IFD=12)
      DIMENSION IA(ifd)
      DIMENSION IFCT(0:IFD),IV(IFD+1)
      DATA (IFCT(I),I=0,IFD) /1,1,2,6,24,120,720,5040,40320,362880,
     1                        3628800,39916800,479001600/
       DO 21 I=1,N
   21  IV(I)=I
       IO=NRP-1
       DO 22 M=N-1,1,-1
       IN=IO/IFCT(M)+1
       IO=MOD(IO,IFCT(M))
       IA(N-M)=IV(IN)
       DO 23 I=IN,M
   23  IV(I)=IV(I+1)
   22  CONTINUE
       IA(N)=IV(1)
      RETURN
      END
                       
      subroutine pape(ia,n,ip)
c     parity of a given permutation
      implicit real*8(a-h,o-z)
      common /bosfer/  ibf
      parameter (ifd=10)
      dimension ia(ifd),ib(ifd)
      do 5 i=1,n
5     ib(i)=ia(i)
      ii=0
      do 10 i=1,n
      do 20 j=i,n
      if(i.eq.ib(j).and.i.ne.j) then
      mm=ib(i)
      ib(i)=ib(j)
      ib(j)=mm
      ii=ii+1
      endif    
20    continue
10    continue
      ip=1
      if(ibf.eq.1) ip=(-1)**ii  
      return
      end

      subroutine crot
c
c     T.C.T(-1)
c
      implicit real*8 (a-h,o-z)
      parameter (mnpar=6,nfac=8000)
      dimension x(nfac,mnpar,mnpar),sum(nfac)
      common /partic/ npar,kpot
      common /jacobi/ t(mnpar,mnpar),ti(mnpar,mnpar),
     &                b(mnpar,(mnpar*(mnpar+1))/2)
      common /permu1/ a(nfac,mnpar,mnpar),p(nfac)
      common /permu2/ iexc(nfac,mnpar),nper
      common /spover/ spiso(nfac),trp(nfac,mnpar,mnpar),not
c
      do 10 i=1,npar
      do 20 j=1,npar
      do 30 n=1,not
      sum(n)=0.d0
 30   continue
      do 40 k=1,npar
      do 50 n=1,not
      sum(n)=sum(n)+a(n,i,k)*ti(k,j)
 50   continue
 40   continue
      do 60 n=1,not
      x(n,i,j)=sum(n)
 60   continue
 20   continue
 10   continue
      do 70 i=1,npar
      do 80 j=1,npar
      do 90 n=1,not
      sum(n)=0.d0
 90   continue
      do 100 k=1,npar
      do 110 n=1,not
      sum(n)=sum(n)+t(i,k)*x(n,k,j)
110   continue
100   continue
      do 120 n=1,not
      a(n,i,j)=sum(n)
120   continue
 80   continue
 70   continue
      return
      end


      function ran2(idum)
      implicit real*8(a-h,o-z)
      parameter (m=714025,ia=1366,ic=150889,rm=1.4005112d-6)
      common /com1 /iy,ir(97)
      data iff /0/
      if(idum.lt.0.or.iff.eq.0)then
        iff=1
        idum=mod(ic-idum,m)
        do 11 j=1,97
          idum=mod(ia*idum+ic,m)
          ir(j)=idum
11      continue
        idum=mod(ia*idum+ic,m)
        iy=idum
      endif
      j=1+(97*iy)/m
      if(j.gt.97.or.j.lt.1)pause
      iy=ir(j)
      ran2=iy*rm
      idum=mod(ia*idum+ic,m)
      ir(j)=idum
      return
      end

      function eps(x,k)
c     x+(-1)**k*eps
c     eps: smallest number (x.ne.x+(-1)**k*eps
      implicit real*8(a-h,o-z)
      epsy=10.d-8
1     if(x.ne.x+(-1)**k*epsy) then
      epsy=epsy/2.d0
      go to 1 
      endif
      eps=x+(-1)**k*epsy*2.d0
      end



       subroutine exchange(i,j)
       implicit real*8(a-h,o-z)
       parameter (mnpar=6,mnbas=400)
       dimension ind(mnbas),c(mnbas,mnbas),
     &           xnc(mnbas),ac(mnbas,mnpar,mnpar)
       common /store1/ he(mnbas,mnbas),oe(mnbas,mnbas)
       common /store2/ xn(mnbas)
       common /nlpara/ a(mnbas,mnpar,mnpar)
       common /partic/ npar,kpot
       common /randomm/irand
       common /contro/ mm0,kk0,nbas0,nbas1,mnb,ico
c
       do 10 k=1,nbas0
       ind(k)=k
10     continue
       ind(i)=j
       ind(j)=i
c
       do 20 m=1,npar
       do 30 l=1,npar
       do 40 k=1,nbas0
       kk=ind(k)
       ac(k,l,m)=a(kk,l,m)
40     continue
30     continue
20     continue
       do 50 k=1,nbas0
       kk=ind(k)
       do 60 l=1,nbas0
       ll=ind(l)
       c(l,k)=he(ll,kk)
60     continue
50     continue
       do 70 k=1,nbas0
       do 80 l=1,nbas0
       he(l,k)=c(l,k)
80     continue
70     continue
       do 90 k=1,nbas0
       kk=ind(k)
       do 100 l=1,nbas0
       ll=ind(l)
       c(l,k)=oe(ll,kk)
100    continue
 90    continue
       do 110 k=1,nbas0
       do 120 l=1,nbas0
       oe(l,k)=c(l,k)
120    continue
110    continue
       do 130 m=1,npar
       do 140 l=1,npar
       do 150 k=1,nbas0
       a(k,l,m)=ac(k,l,m)
150    continue
140    continue
130    continue
       do 160 k=1,nbas0
       kk=ind(k)
       xnc(k)=xn(kk)
160    continue
       do 170 k=1,nbas0
       xn(k)=xnc(k)
170    continue
       return
       end



        subroutine trcorri(z,a)
c       transformation from Jacobi-system (nonlinear parameters)
c       to pairwise relative coordinate-system
        implicit real*8(a-h,o-z)
        parameter (mnpar=6)
        dimension z((mnpar*(mnpar+1))/2),a(mnpar,mnpar),
     &  g(mnpar,mnpar),xn(mnpar,mnpar)
        common /jacobi/ t(mnpar,mnpar),ti(mnpar,mnpar),
     &                b(mnpar,(mnpar*(mnpar+1))/2)
        common /partic/ npar,kpot
c
        do i=1,npar
        do j=1,npar
        g(i,j)=a(i,j)
        end do
        end do
        call trafo3(g,t)
c      
        do k=1,npar
        do j=k+1,npar
        xn(k,j)=-g(k,j)        
        xn(j,k)=-g(k,j)
        end do
        end do
c
        k=0
        do i=1,npar
        do j=i+1,npar
        k=k+1
        z(k)=xn(i,j)
        end do
        end do
        return
        end

      function potmat(p,q,n)
c     integral of r**n*exp(-p*r**2+q*r)
      implicit real*8(a-h,o-z)
      parameter (nmax=100,nnmax=301,n2=100)
      common /gyula/ f(0:nmax),df(-1:nnmax),x2n(0:n2)
      data pi/3.1415926535897932384d0/ 
      a=0.5d0*(-1)**n*(pi/p)**0.5d0
      s=0.d0
      do 20 k=0,n
      x1=dexp(f(n)-f(k)-f(n-k))
      call d1(k,p,q,x2)
      call d2(n-k,p,q,x3)
      s=s+x1*x2*x3
20    continue
      potmat=s*a
      end 

      subroutine hermite(n,x,y)
      implicit real*8 (a-h,o-z)
      parameter (nmax=100,nnmax=301,n2=100)
      common /gyula/ f(0:nmax),df(-1:nnmax),x2n(0:n2)
      y=0.d0
	  do 20 i=0,idint(0.5d0*n)
	  x1=(-1)**i*dexp(f(n)-f(n-2*i)-f(i))
	  if(x.eq.0.d0) then 
            if(n-2*i.eq.0) then
             x2=1.d0
            else
             x2=0.d0
            endif
          else
            x2=(2.d0*x)**(n-2*i)
          endif
	  y=y+x1*x2
20        continue
	  end 

      subroutine d1(n,p,q,x)
c     derivative of the exponential part
      implicit real*8 (a-h,o-z)
      parameter (nmax=100,nnmax=301,n2=100)
      common /gyula/ f(0:nmax),df(-1:nnmax),x2n(0:n2)
	  b=(0.5d0/p)**n
          x=0.d0
	  do 20 i=0,idint(0.5d0*n)
	  c=dexp(f(n)-f(n-2*i)-f(i))*power(q,n-2*i)
	  x=x+b*c*p**i
20        continue
	  end 


      function power(x,n)
      implicit real*8(a-h,o-z)
c
       if(x.eq.0.d0) then
         if(n.eq.0) then 
	  power=1.d0
         else
          power=0.d0
         endif
       else
         power=x**n
       endif
      end

      subroutine  d2(n,p,q,x)
      implicit real*8(a-h,o-z)  
      data pi/3.1415926535897932384d0/ 
          y=q/(2.d0*dsqrt(p))
      if(n.eq.0) then 
        call erfc0(y,x)
	  else
	  call hermite(n-1,y,x)	
	  x=x/(2.d0*dsqrt(p))**n*(-1)**n*2.d0/dsqrt(pi)
	  endif
	  end

          subroutine erfc0(x,y)
      implicit real*8(a-h,o-z)
      t = 1.0d0 - 7.5d0/(abs(x)+3.75d0)
      y = (((((((((((((((+3.328130055126039d-10
     *    *t-5.718639670776992d-10)*t-4.066088879757269d-9)
     *    *t+7.532536116142436d-9)*t+3.026547320064576d-8)
     *    *t-7.043998994397452d-8)*t-1.822565715362025d-7)
     *    *t+6.575825478226343d-7)*t+7.478317101785790d-7)
     *    *t-6.182369348098529d-6)*t+3.584014089915968d-6)
     *    *t+4.789838226695987d-5)*t-1.524627476123466d-4)
     *    *t-2.553523453642242d-5)*t+1.802962431316418d-3)
     *    *t-8.220621168415435d-3)*t+2.414322397093253d-2
      y=(((((y*t-5.480232669380236d-2)*t+1.026043120322792d-1)
     *    *t-1.635718955239687d-1)*t+2.260080669166197d-1)
     *    *t-2.734219314954260d-1)*t + 1.455897212750385d-1
c
      if (x.lt.0.0d0) y = 2.0d0*exp(x*x) - y
      return
      end

      subroutine facc
      implicit real*8 (a-h,o-z)
      parameter (nmax=100,nnmax=301,n2=100)
      common /gyula/ f(0:nmax),df(-1:nnmax),x2n(0:n2)
      f(0)=0.d0
      df(-1)=0.d0
      df(0)=0.d0
      x2n(0)=0.d0
      f(1)=0.d0
      df(1)=0.d0
      df(2)=dlog(2.d0)
      do i=2,nmax
      x=dble(i)
      f(i)=f(i-1)+dlog(x)
      end do 
      do i=3,nnmax
      x=dble(i)
      df(i)=df(i-2)+dlog(x)
      end do
      do i=1,n2
      x=2.d0
      x2n(i)=x2n(i-1)+dlog(x)
      end do
      return
      end

      function potval(a,ip)
c     interpolation of the potential
      implicit real*8(a-h,o-z)
      parameter (npnt=10000)
      common /potrep1/ xll,xul,def(npnt),nv0
      common /potrep2/ fff(npnt,4)
c     
      da=dlog(a)
      k=idint((da-xll)/(xul-xll)*nv0)
      k1=k+1
      k2=k+2
      y1=def(k1)
      y2=def(k2)
      f1=fff(k1,ip)
      f2=fff(k2,ip)
      potval=f1+(f2-f1)/(y2-y1)*(da-y1)
      return
      end 

      subroutine potrep
      implicit real*8 (a-h,o-z)
      parameter (npnt=10000)
      dimension zzz(npnt)
      common /potrep1/ xll,xul,def(npnt),nv0
      common /potrep2/ fff1(npnt,4)
      common /potsel/  ip
      common /intera/  ipcon,npt,no,np(10,4),vp(10,4),ap(10,4),bp(10,4)
      open(10,file='potrep.dat')
c     upper and lower limit (logaritmic scale)
      xll=-20.d0
      xul=20.d0
      nv0=8000
      write(10,*)xll,xul,nv0
      do 100 i=1,nv0
      sss=xll+(i-1)*(xul-xll)/dfloat(nv0)
      zzz(i)=-dexp(sss)
      def(i)=sss
 100  continue
      do 300 ip=1,no
      do 200 i=1,nv0
      call qqpot(zzz(i),fff)
      write(10,*)fff
 200  continue
 300  continue
      close(10)
      end 

      subroutine qqpot(zzz,fff)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     numerical integration of the potential                     c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit real*8(a-h,o-z)
      parameter (npnt=10000)
      common/param/q,pp,k
      common/dati/h,eps
      data pi/3.1415926535897932384d0/ 
      h=0.5d0
      eps=0.000001
      bn=4.d0*pi
      k=2
      q=-zzz
      a=0.0d0
      b=0.0d0
      s=0.d0
1     a=b
      b=b+h
      call qgaus(a,b,gi)
      if(dabs(gi).lt.eps) go to 2
      s=s+gi
      go to 1
2     fff=bn*s
      end	
  
      function fv(x)
      implicit real*8(a-h,o-z)
          common/param/q,p,n
          common/potsel/np
c         from gaussian
          w=q*x**2
          if(w.gt.300.d0) then
          w1=0.d0
          else
	  w1=x**n*dexp(-w)
          endif
c         potential
          w2=pot(x)
          fv=w1*w2
       end
	  
      SUBROUTINE QGAUS(A,B,SS)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(5),W(5)
      DATA X/0.1488743389d0,0.4333953941d0,0.6794095682d0,
     & 0.8650633666d0,0.9739065285d0/
      DATA W/0.2955242247d0,0.2692667193d0,0.2190863625d0,
     & 0.1494513491d0,0.0666713443d0/
      XM=0.5d0*(B+A)
      XR=0.5d0*(B-A)
      SS=0.d0
      DO 11 J=1,5
        DX=XR*X(J)
        SS=SS+W(J)*(FV(XM+DX)+FV(XM-DX))
11    CONTINUE
      SS=XR*SS
      RETURN
      END

       function pot(r)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      radial part of the potential                                    c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       implicit real*8(a-h,o-z)
       common/ potsel/ip
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       example: Minesota potential                                    c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        u=1.d0
c        vv1=200.d0*dexp(-1.487*r*r)
c        vv2=-178.0*dexp(-0.639*r*r)
c        vv3=-91.85*dexp(-0.465*r*r)
c        xw=u*0.25*(2.d0*vv1+vv2+vv3)
c        xm=(1.d0-0.5d0*u)*vv1+(0.5d0-0.25d0*u)*vv2+(0.5d0-0.25d0*u)*vv3
c        xb=0.25d0*u*vv2-0.25d0*u*vv3
c        xh=((0.5d0-0.25d0*u)*vv2+(-0.5d0+0.25d0*u)*vv3)
c        pot=0.d0
c        if(ip.eq.1) pot=xw
c        if(ip.eq.2) pot=xm
c        if(ip.eq.3) pot=xb
c        if(ip.eq.4) pot=xh
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       example:  MTV potential                                        c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       v1=1458.25
c       v2=-578.09
c       d1=3.11
c       d2=1.55
c       pot=v1*dexp(-d1*r)/r+v2*dexp(-d2*r)/r
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       example:  PT  potential   of S. A. M.                          c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      hb2=41.47d0
      hbarc=197.32858d0
      rn1=3.d0
      rn2=3.d0
      rn3=3.d0
      beta=hbarc/(hb2*rn3)
      v1=hb2*beta*beta*rn1*(rn1-1)
      v2=hb2*beta*beta*rn2*(rn2+1)
      csc=1.d0/sinh(beta*r)
      ssc=1.d0/cosh(beta*r)
      pot= v1*csc*csc-v2*ssc*ssc
c
       return
       end


      subroutine ludcmp(a,n,np,indx,d)
c
      implicit real*8(a-h,o-z)
      parameter (nmax=100,tiny=1.0d-20)
      dimension a(np,np),indx(n),vv(nmax)
      d=1.d0
      do 12 i=1,n
        aamax=0.d0
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.) pause 'singular matrix.'
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        if (j.gt.1) then
          do 14 i=1,j-1
            sum=a(i,j)
            if (i.gt.1)then
              do 13 k=1,i-1
                sum=sum-a(i,k)*a(k,j)
13            continue
              a(i,j)=sum
            endif
14        continue
        endif
        aamax=0.
        do 16 i=j,n
          sum=a(i,j)
          if (j.gt.1)then
            do 15 k=1,j-1
              sum=sum-a(i,k)*a(k,j)
15          continue
            a(i,j)=sum
          endif
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(j.ne.n)then
          if(a(j,j).eq.0.d0)a(j,j)=tiny
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      if(a(n,n).eq.0.d0)a(n,n)=tiny
      return
      end

      subroutine lubksb(a,n,np,indx,b)
      implicit real*8(a-h,o-z)
      dimension a(np,np),indx(n),b(n)
      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.d0) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        if(i.lt.n)then
          do 13 j=i+1,n
            sum=sum-a(i,j)*b(j)
13        continue
        endif
        b(i)=sum/a(i,i)
14    continue
      return
      end


      subroutine f01aef(n,a,ia,b,ib,dl,ifail)
c     mark 2 release. nag copyright 1972
c     mark 3 revised.
c     mark 4.5 revised
c
c     reduc1
c     reduction of the general symmetric eigenvalue problem a * x =
c     lambda * b * x, with symmetric matrix a and symmetric
c     positive definite matrix b, to the equivalent standard
c     problem p * z = lambda * z. the upper triangle, including
c     diagonal elements, of a and b are given in the arrays a(n,n)
c     and b(n,n). l(b = l * lt) is formed in the
c     remaining strictly lower triangle of the array b with its
c     diagonal elements in the array dl(n) , and the lower
c     triangle of the symmetric matrix p(p = inv(l) * a * inv( lt))
c     is formed in the lower triangle of the array a, including the
c     diagonal elements. hence the diagonal elements of a are lost.
c     the subroutine will fail if b, perhaps on account of rounding
c     errors, is not positive definite - sets ifail = 0 if
c     successful else ifail = 1.
c     1st december 1971
c
      integer isave, ifail, i, n, i1, j, kk, k, j1, ia, ib, p01aaf
      double precision srname
      double precision x, y, a(ia,n), b(ib,n), dl(n)
      data srname /8h f01aef /
      isave = ifail
      do 100 i=1,n
         i1 = i - 1
         do 80 j=i,n
            x = b(i,j)
            if (i1.eq.0) go to 40
            do 20 kk=1,i1
               k = i1 - kk + 1
               x = x - b(i,k)*b(j,k)
   20       continue
   40       if (i.ne.j) go to 60
            if (x.le.0.0d0) go to 320
            y = dsqrt(x)
            dl(i) = y
            go to 80
   60       b(j,i) = x/y
   80    continue
  100 continue
c     l has been formed in array b
      do 180 i=1,n
         y = dl(i)
         i1 = i - 1
         do 160 j=i,n
            x = a(i,j)
            if (i1.eq.0) go to 140
            do 120 kk=1,i1
               k = i1 - kk + 1
               x = x - b(i,k)*a(j,k)
  120       continue
  140       a(j,i) = x/y
  160    continue
  180 continue
c     the transpose of the upper triangle of
c     inv(l) * a has been formed in the lower
c     triangle of array a
      do 300 j=1,n
         j1 = j - 1
         do 280 i=j,n
            x = a(i,j)
            i1 = i - 1
            if (i1.lt.j) go to 220
            do 200 kk=j,i1
               k = i1 - kk + j
               x = x - a(k,j)*b(i,k)
  200       continue
  220       if (j1.eq.0) go to 260
            do 240 kk=1,j1
               k = j1 - kk + 1
               x = x - a(j,k)*b(i,k)
  240       continue
  260       a(i,j) = x/dl(i)
  280    continue
  300 continue
      ifail = 0
      return
  320 ifail = p01aaf(isave,1,srname)
      return
      end
      subroutine f01aff(n, im1, im2, b, ib, dl, z, iz)
c     mark 2 release. nag copyright 1972
c     mark 4.5 revised
c
c     rebaka
c     this subroutine performs, on the matrix of eigenvectors, z,
c     stored
c     in columns im1 to im2 of the array z(n,n), a backward
c     substitution
c     lt* x = z, over- writing x on z. the diagonal elements of l
c     must be stored in the array dl(n), and the remaining
c     triangle in the strictly lower triangle of the array b(n,n).
c     the subroutines f01aef and f01bdf leave l in this
c     desired form. if x denotes any column of the resultant matrix
c     x, then x satisfies xt * b * x = zt * z, where b = l * lt.
c     1st august 1971
c
      integer j, im1, im2, ii, n, i, i2, k, ib, iz
      double precision x, b(ib,n), dl(n), z(iz,im2)
      do 80 j=im1,im2
         do 60 ii=1,n
            i = n - ii + 1
            x = z(i,j)
            i2 = i + 1
            if (i2.gt.n) go to 40
            do 20 k=i2,n
               x = x - b(k,i)*z(k,j)
   20       continue
   40       z(i,j) = x/dl(i)
   60    continue
   80 continue
      return
      end
      subroutine f01ajf(n, atol, a, ia, d, e, z, iz)
c     mark 2 release. nag copyright 1972
c     mark 4 revised.
c     mark 4.5 revised
c     mark 5c revised
c
c     tred2
c     this subroutine reduces the given lower triangle of a
c     symmetric matrix, a, stored in the array a(n,n), to
c     tridiagonal form using householders reduction. the diagonal
c     of the result is stored in the array d(n) and the
c     sub-diagonal in the last n - 1 stores of the array e(n)
c     (with the additional element e(1) = 0). the transformation
c     matrices are accumulated in the array z(n,n). the array
c     a is left unaltered unless the actual parameters
c     corresponding to a and z are identical.
c     1st august 1971
c
      integer i, ia, ii, iz, j1, j, k, l, n
      double precision atol, f, g, h, hh, a(ia,n), d(n), e(n), z(iz,n)
      do 40 i=1,n
         do 20 j=1,i
            z(i,j) = a(i,j)
   20    continue
   40 continue
      if (n.eq.1) go to 280
      do 260 ii=2,n
         i = n - ii + 2
         l = i - 2
         f = z(i,i-1)
         g = 0.0d0
         if (l.eq.0) go to 80
         do 60 k=1,l
            g = g + z(i,k)*z(i,k)
   60    continue
   80    h = g + f*f
c     if g is too small for orthogonality to be
c     guaranteed the transformation is skipped
         if (g.gt.atol) go to 100
         e(i) = f
         h = 0.0d0
         go to 240
  100    l = l + 1
         g = dsqrt(h)
         if (f.ge.0.0d0) g = -g
         e(i) = g
         h = h - f*g
         z(i,i-1) = f - g
         f = 0.0d0
         do 180 j=1,l
            z(j,i) = z(i,j)/h
            g = 0.0d0
c     form element of a*u
            do 120 k=1,j
               g = g + z(j,k)*z(i,k)
  120       continue
            j1 = j + 1
            if (j1.gt.l) go to 160
            do 140 k=j1,l
               g = g + z(k,j)*z(i,k)
  140       continue
c     form element of p
  160       e(j) = g/h
            f = f + g*z(j,i)
  180    continue
c     form k
         hh = f/(h+h)
c     form reduced a
         do 220 j=1,l
            f = z(i,j)
            g = e(j) - hh*f
            e(j) = g
            do 200 k=1,j
               z(j,k) = z(j,k) - f*e(k) - g*z(i,k)
  200       continue
  220    continue
  240    d(i) = h
  260 continue
  280 e(1) = 0.0d0
      d(1) = 0.0d0
c     accumulation of transformation matrices
      do 400 i=1,n
         l = i - 1
         if (d(i).eq.0.0d0) go to 360
         do 340 j=1,l
            g = 0.0d0
            do 300 k=1,l
               g = g + z(i,k)*z(k,j)
  300       continue
            do 320 k=1,l
               z(k,j) = z(k,j) - g*z(k,i)
  320       continue
  340    continue
  360    d(i) = z(i,i)
         z(i,i) = 1.0d0
         if (l.eq.0) go to 400
         do 380 j=1,l
            z(i,j) = 0.0d0
            z(j,i) = 0.0d0
  380    continue
  400 continue
      return
      end

      subroutine f02aef(a,ia,b,ib,n,r,v,iv,dl,e,ifail)
c     mark 2 release. nag copyright 1972
c     mark 3 revised.
c     mark 4.5 revised
c
c     eigenvalues and eigenvectors of a-lambda*b
c     1st december 1971
c
      integer p01aaf,isave,ifail,n,ia,ib,iv
      double precision srname
      double precision tol,a(ia,n),b(ib,n),
     & r(n),v(iv,n),dl(n),e(n)
      data srname /8h f02aef /
      isave = ifail
      ifail = 1
      call f01aef(n, a, ia, b, ib, dl, ifail)
      if (ifail.eq.0) go to 20
      ifail = p01aaf(isave,ifail,srname)
      return
   20 tol = 2.0d0**(-50)
      call f01ajf(n, tol, a, ia, r, e, v, iv)
      tol = 2.0d0**(-55)
      ifail = 1
      call f02amf(n, tol, r, e, v, iv, ifail)
      if (ifail.eq.0) go to 40
      ifail = p01aaf(isave,2,srname)
      return
   40 call f01aff(n, 1, n, b, ib, dl, v, iv)
      return
      end
      subroutine f02amf(n,acheps,d,e,z,iz,ifail)
c     mark 2 release. nag copyright 1972
c     mark 3 revised.
c     mark 4 revised.
c     mark 4.5 revised
c
c     tql2
c     this subroutine finds the eigenvalues and eigenvectors of a
c     tridiagonal matrix, t, given with its diagonal elements in
c     the array d(n) and its sub-diagonal elements in the last n
c     - 1 stores of the array e(n), using ql transformations. the
c     eigenvalues are overwritten on the diagonal elements in the
c     array d in ascending order. the eigenvectors are formed in
c     the array z(n,n), overwriting the accumulated
c     transformations as supplied by the subroutine f01ajf. the
c     subroutine will fail if any one eigenvalue takes more than 30
c     iterations.
c     1st april 1972
c
      integer p01aaf, isave, ifail, n, i, l, j, m, i1, m1, ii, k, iz
c$p 1
      double precision srname
      double precision b, f, h, acheps, g, p, r, c, s, d(n), e(n), z(iz,
     *n)
      data srname /8h f02amf /
      isave = ifail
      if (n.eq.1) go to 40
      do 20 i=2,n
         e(i-1) = e(i)
   20 continue
   40 e(n) = 0.0d0
      b = 0.0d0
      f = 0.0d0
      do 300 l=1,n
         j = 0
         h = acheps*(dabs(d(l))+dabs(e(l)))
         if (b.lt.h) b = h
c     look for small sub-diag element
         do 60 m=l,n
            if (dabs(e(m)).le.b) go to 80
   60    continue
   80    if (m.eq.l) go to 280
  100    if (j.eq.30) go to 400
         j = j + 1
c     form shift
         g = d(l)
         h = d(l+1) - g
         if (dabs(h).ge.dabs(e(l))) go to 120
         p = h*0.5d0/e(l)
         r = dsqrt(p*p+1.0d0)
         h = p + r
         if (p.lt.0.0d0) h = p - r
         d(l) = e(l)/h
         go to 140
  120    p = 2.0d0*e(l)/h
         r = dsqrt(p*p+1.0d0)
         d(l) = e(l)*p/(1.0d0+r)
  140    h = g - d(l)
         i1 = l + 1
         if (i1.gt.n) go to 180
         do 160 i=i1,n
            d(i) = d(i) - h
  160    continue
  180    f = f + h
c     ql transformation
         p = d(m)
         c = 1.0d0
         s = 0.0d0
         m1 = m - 1
         do 260 ii=l,m1
            i = m1 - ii + l
            g = c*e(i)
            h = c*p
            if (dabs(p).lt.dabs(e(i))) go to 200
            c = e(i)/p
            r = dsqrt(c*c+1.0d0)
            e(i+1) = s*p*r
            s = c/r
            c = 1.0d0/r
            go to 220
  200       c = p/e(i)
            r = dsqrt(c*c+1.0d0)
            e(i+1) = s*e(i)*r
            s = 1.0d0/r
            c = c/r
  220       p = c*d(i) - s*g
            d(i+1) = h + s*(c*g+s*d(i))
c     form vector
            do 240 k=1,n
               h = z(k,i+1)
               z(k,i+1) = s*z(k,i) + c*h
               z(k,i) = c*z(k,i) - s*h
  240       continue
  260    continue
         e(l) = s*p
         d(l) = c*p
         if (dabs(e(l)).gt.b) go to 100
  280    d(l) = d(l) + f
  300 continue
c     order eigenvalues and eigenvectors
      do 380 i=1,n
         k = i
         p = d(i)
         i1 = i + 1
         if (i1.gt.n) go to 340
         do 320 j=i1,n
            if (d(j).ge.p) go to 320
            k = j
            p = d(j)
  320    continue
  340    if (k.eq.i) go to 380
         d(k) = d(i)
         d(i) = p
         do 360 j=1,n
            p = z(j,i)
            z(j,i) = z(j,k)
            z(j,k) = p
  360    continue
  380 continue
      ifail = 0
      return
  400 ifail = p01aaf(isave,1,srname)
      return
      end
