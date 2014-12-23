# from math import cos
import numpy as np
import numpy.random as rnd
from matplotlib import pyplot as plt
from copy import copy

plt.clf()

# The model. Change this to your favourate model.
def model(old, k, force):
    return 0.5*old + 25*old/(1+old**2) + 8*np.cos(0.8*k) + force

# Function for calcculating a rankhistogram. A rankhistogram shows 
# where an observation, called 'value', ranks in a sorted ensemble. 
# Input: the observation (value)
#       the ensemble evaluated at the observation position (ensemble)
# Output: the rank of the observation in the ensemble (r)
# The function first sorts the ensemble using np.sort, followed by
# the actual ranking. 
def rankhist(ensemble, value):
    ensemble = np.sort(ensemble)
    length=len(ensemble)
    result = True
    for i in range(length):
       if result:
           if ensemble[i] > value:
              r = i
              result = False
    if result:
       r=length
    return r
    
# The actual Particle Filter code. 
# Input: the arguments in the call to the function, see below
# Output: several plots, depending on the input values:
#         plots 1-4 are plots of the truth, observations and pure ensemble pdfs
#         plots 11-17 are plots for the Equivalent-Weights Particle Filter
#         plots 21-27 are plots for the standard Particle Filter (SIR)
#         and statistics 
#         on the goodness of fit to the truth from which the observations 
#         have been generated.
# The depending on the settings in the argument line, the code does a pure
# ensemble run without data assimilation, runs the Equivalent-Weights Particle Filter
# or runs a standard Particle Filter (Sequential Importance Resampling)
def pfModel(T=400, ns=5, mm=20, model_pure=False, pfew=False, pfpure=False, obsvar=0.1, modelvar=0.05, ivar=0.1, rrr=8.0, ccc=0.9):
    """
    T - total number of timesteps (default 400)
    ns- number of timesteps between observations (default 5)
    mm - number of ensemble member (particles) (default 20)
    model_pure - pure ensemble of model runs (default False)
    pfew - True for equal-weight PF, False if not used (default False)
    pfpure - True for pure PF (expensive), False if not used (default False)
    obsvar - observation error variance (default 0.1)
    modelvar - model error variance (default 0.05)
    ivar - initial uncertainty (standard deviation) (default 0.1)
    rrr - parameter determining strength of relaxation in Equivalent-Weights PF (default 8)
    ccc - parameter determining fraction of particle kept in Equivalent-Weights PF (default 0.9)
    """
    plt.clf()
    plt.clf()
    plt.ion()
      
    # Specific parameters for relaxation step in the proposal particle filter
    r = rrr * modelvar / obsvar 
    
    # Random seed value to abtain repreducible results
    rnd.seed(100)
    # Size of random forcing
    nforce = 1
    
    # Initialisation
    x = np.zeros(T)
    xtrue = np.zeros(T)
    xdiff = np.zeros(T)
    # Initialisa random forcing
    force = 0.0
    # Covariance model for the model errors
    corr_model = 1.0
    cov_model = modelvar * corr_model
    scov_model = np.sqrt(cov_model)
    # standard deviation of initial condition error
    si_model = np.sqrt(ivar)
 
    # Generate truth run
    x[0]=0
    x[0] += si_model * rnd.randn()
    for t in range(1,T):
        force = scov_model * rnd.randn()
        x[t] = model(x[t-1],t,force)
    xtrue = copy(x)
    
    # Pure ensemble of model runs
    if model_pure:
        # Array with statistics for plotting
        hmp = np.zeros([mm, T])
        
        for m in range(mm):
            x[0] = 0
            x[0] += si_model * rnd.randn()
            for t in range(1,T):
                force = scov_model * rnd.randn()
                x[t] = model(x[t-1],t,force)
            hmp[m] = x
        
        # Output histograms
        nbins = max(mm/10,10)
        
        plt.figure(1)
        plt.hist(hmp[:,0], nbins)
        plt.title('pdf at time zero')
        
        plt.figure(2)
        plt.hist(hmp[:,T/2], nbins)
        plt.title('pdf half way')
        
        plt.figure(3)
        plt.hist(hmp[:,T-1], nbins)
        plt.title('pdf at final time')
        plt.show()
    
    # Observations
    if pfew or pfpure:
        # number of measurement times
        nm = T / ns
        y = np.zeros(nm)
        rank=np.zeros(nm)
        
        # Generate observation covariance matrix - diagonal
        msqrt_cov = np.sqrt(obsvar)
        inv_cov = 1.0/obsvar
        
        # Generate observation time series
        for t in range(nm):
            y[t] = xtrue[(t+1)*ns-1] + msqrt_cov * rnd.randn()
        
        e = np.ones(nm - 1) * obsvar
        
        # Plot the truth and the observation over time
        plt.figure(4)
        plt.plot(xtrue[:], 'g-')
        plt.title('True solution')
        plt.errorbar(ns*np.arange(1,nm), y[0:nm-1], e, fmt='rx')
        plt.show()
        
    if pfew:
        rnd.seed(0)
        # particle values
        xp = np.zeros(mm)
        # hulp variable
        ymyp = 0
        # mean particle value
        xpm = np.zeros(T)
        # weights
        wp = np.ones(mm)/mm
        # extra array for states
        xpnew = copy(xp)
        # extra array for weights
        wpnew = copy(wp)
        # array with statistics for plotting
        hpf = np.zeros([mm,T])
        # array for proposal weights
        wpp = np.zeros(mm)
        # initialisation of several constants to zero
        hqht = 0.0
        inv_qn = 0.0
        inv_q = 0.0
        c = np.zeros(mm)
        kgain = 0.0
        oldxp = np.zeros(mm)
        xxhulp = np.zeros(mm)
        xtest = 0.0
        nwp = np.zeros(T/ns)
        # measure is the observation operator 
        measure = 1
        # calculate HQH^T
        hqht = cov_model * measure *measure
        # calculate (HQHT+R)^{-1}
        inv_qn = 1.0/(hqht + obsvar)
        # calculate Q^{-1}
        inv_q = 1.0/cov_model
        # calculate K=QHT (HQHT+R)^{-1}
        kgain = cov_model * measure * inv_qn
        # calculate R^{-1}HT K
        k_hulp = inv_cov * measure * kgain
        
        # a counting index
        mcn = 0
        for t in range(T):
            for m in range(mm):
                if(t==0):
                    xp[m] = xtrue[0] + si_model * rnd.randn()
                else:
                    # this is only true at measurement times
                    if(((t % ns) == 0) and (t > 0)):
                        force = 0
                        xp[m] = model(xp[m], t, force)
                    else:
                        # new scheme: implicit nudging
                        tt=0.0
                        tt = (1.0*t - mcn*ns)/(1.0*ns)
                        if(tt > 0.3):
                        #         temp = np.exp(tt)
                            temp=tt-0.3
                        else:
                            temp = 0
                        
                        force = nforce * scov_model * rnd.randn()
                        
                        xp[m] = model(xp[m],t+1,force)
                        
                        nudge = r * temp * (y[mcn] - xp[m])
                        xp[m] += nudge
                        
                        nudge += force
                        # calculate proposal -log(weights) p/q
                        hulp = nudge * nudge * inv_q - force * force * inv_q
                        wpp[m] += 0.5*hulp
            # this is only true at measurement times
            if((t%ns == 0) and (t > 0)):
                mcn += 1
                # Find maximum -log(weights) for each partice
                for m in range(mm):
                    ymyp = y[mcn-1] - xp[m]
                    c[m] = wpp[m] + 0.5 * ymyp * inv_qn * ymyp
                # Sort them in accending order
                ccsort = np.sort(c)
                # retain ccc*100%, rest comes back in via resampling
                cc = ccsort[ccc*mm]
                
                oldxp = copy(xp)
                # calculate x^*=f(x)+alpha K(y-Hf(x))
                for m in range(mm):
                    if c[m] <= cc:
                        ymyp = y[mcn-1] - xp[m]
                        aaa = 0.5 * ymyp * k_hulp * ymyp
                        bbb = 0.5 * ymyp * inv_cov * ymyp - cc + wpp[m]
                        alpha = 1.0 + np.sqrt(1.0 - bbb/aaa + 0.00000001)
                        xp[m] = xp[m] + alpha * kgain * ymyp
                    
               # test for equal weights 
                for m in range(mm):
                    ymyp = y[mcn-1] - xp[m]
                    xtest = xp[m] - oldxp[m]
                    a = 0.5 * ymyp * inv_cov * ymyp + 0.5 * xtest * inv_q * xtest
                    c[m] = wpp[m] + a

                factor = 1.e-5
                
                # add random part of proposal and recalculate -log(weights)
                for m in range(mm):
                    random_number = rnd.randn()
                    xp[m] += factor * scov_model * random_number
                    ymyp = y[mcn-1] - xp[m]
                    xtest = xp[m] - oldxp[m]
                    a = 0.5 * ymyp * inv_cov * ymyp + 0.5 * xtest * inv_q * xtest
                    awpp = 0.5*scov_model*random_number * inv_q * scov_model * random_number
                    awpp = 0 #mixture density
                    wp[m] = wpp[m]+a-awpp

                # calculate true weights, avoiding infinities
                minwp = np.min(wp)
                for m in range(mm):
                    wp[m] = np.exp(-wp[m] + minwp)
                wp[:] /= sum(wp)
                
                # particle mean
                xpm[t] = 0.0
                for i in range(mm):
                    xpm[t] += xp[i] * wp[i]
                
                # resampling
                wpnew[0] = copy(wp[0])
                for m in range(1,mm):
                    wpnew[m] = wpnew[m-1] + wp[m]
                rr = rnd.rand()/mm
                nn = 0
                for m in range(mm):
                    while(rr > wpnew[nn]):
                        nn += 1
                    xpnew[m] = copy(xp[nn])
                    rr += 0.9999/mm
                
                # Determine effective number of particles N_eff=1/(sum weights^2)
                nwp[mcn-1] = 0.0
                for i in range(mm):
                    nwp[mcn-1] += wp[i]*wp[i]
                # effective number of particles
                nwp[mcn-1] = 1.0/nwp[mcn-1]
                
                # The new ensemble
                xp = copy(xpnew)
                wp[:] = 1.0/mm
                wpp[:] = 0.0
            
                rank[mcn-1]=rankhist(xp,y[mcn-1]) 

            # statistics for plots
            for m in range(mm):
                hpf[m,t] = xp[m]
            
            # calculate mean
            xpm[t] = 0.0
            for m in range(mm):
                xpm[t] += xp[m] * wp[m]

        # end of T loop
        
        # Output histograms
        # ax=plt.gca()
        # ax.set_ylim(None)
        nbins = max(mm/10,10)
        plt.figure(11)
        plt.hist(hpf[:,0], bins=nbins)
        plt.title('EW PF pdf at time zero')
        
        plt.figure(12)
        plt.hist(hpf[:,T/2], nbins)
        plt.title('EW PF pdf half way')
        
        plt.figure(13)
        plt.hist(hpf[:,T-1], bins=nbins)
        plt.title('EW PF pdf at final time')
        
        # Generate plots to illustrate the behaviour of the PF
        plt.figure(14)
        for i in range(T):
            xdiff[i] = (xpm[i]-xtrue[i])*(xpm[i]-xtrue[i])
        plt.plot(range(T),xdiff,'k-')
        plt.title('difference squared EW PF and truth')
        plt.show()
        
        plt.figure(15)
        e = np.ones(nm - 1) * np.sqrt(obsvar)
        for i in range(mm):
            plt.plot(range(T), hpf[i,:], 'g-')
        plt.errorbar(ns*np.arange(1,nm),y[0:nm-1],e,fmt='rx')
        plt.plot(range(T),xtrue,'k-')
        plt.title('evolution of EW Partticle filter ensemble, truth, and observations')
        plt.show()
        
        plt.figure(16)
        nwphulp=np.zeros(nm-1)
        for i in range(nm-1):
            nwphulp[i]=ns*(i)
        plt.plot(nwphulp[0:nm-2],nwp[0:nm-2])
        plt.ylim(0,mm)
        plt.title('EW Particle Filter effective number of particles at observation times')
        plt.show()
    
        nbins = mm+1
        plt.figure(17)
        plt.hist(rank,bins=nbins)
        plt.title('rank histogram EW Particle Filter')
        
    # end of if pfew
    
    # Particle filter pure
    if pfpure:
        rnd.seed(0)
        # initialise several variables to zero
        xp = np.zeros(mm)
        ymyp = 0
        xppm = np.zeros(T)
        wp = np.ones(mm)/mm
        xpnew = copy(xp)
        wpnew = copy(wp)
        hppf = np.zeros([mm,T])
        nwp = np.zeros(nm)
        
        # a counting index
        mcn = 0
        
        for t in range(T):
            for m in range(mm):
                if(t == 0):
                    xp[m] = xtrue[0] + si_model * rnd.randn()
                else:
                    force = scov_model * rnd.randn()
                    xp[m] = model(xp[m], t, force)
                    
            if((t%ns == 0) and (t > 1)):
                mcn += 1
                # calculate -log(weights)
                for m in range(mm):
                    ymyp = y[mcn-1] - xp[m]
                    a = 0.5*ymyp * ymyp * inv_cov
                    wp[m] = a
                # Normalize weights, avoiding infinities
                amin = np.min(wp)
                wp -= amin
                wp = np.exp(-wp)
                wp = wp / np.sum(wp)
                
                # Determine effective number of particles N_eff = 1/(sum weights^2)
                nwp[mcn-1] = 0.0
                for i in range(mm):
                    nwp[mcn-1] += wp[i]**2
                # effective number of particles
                nwp[mcn-1] = 1/nwp[mcn-1]
                
                # particle mean
                xppm[t] = 0.0
                for i in range(mm):
                    xppm[t] += xp[i] * wp[i]
                
                # Some form of universal resampling
                wpnew[0] = wp[0]
                for m in range(1,mm):
                    wpnew[m] = wpnew[m-1] + wp[m]
                rr = rnd.rand()/mm
                nn=1
                for m in range(mm):
                    while(rr > wpnew[nn]):
                        nn += 1
                    xpnew[m] = xp[nn]
                    rr += 0.9999/mm
                
                # The new ensemble
                xp = copy(xpnew)
                wp[:] = 1.0/mm
                rank[mcn-1]=rankhist(xp,y[mcn-1]) 
            
            for m in range(mm):
                hppf[m,t] = xp[m]
            
            xppm[t] = 0.0
            for m in range(mm):
                xppm[t] += xp[m] * wp[m]
        
        # Output histograms
        
        nbins = max(mm/10,10)
        plt.figure(21)
        plt.hist(hppf[:,0],nbins)
        plt.title('pdf at time zero')
        
        plt.figure(22)
        plt.hist(hppf[:,T/2],nbins)
        plt.title('pdf half way')
        
        plt.figure(23)
        plt.hist(hppf[:,T-1],nbins)
        plt.title('pdf at final time')
        
        plt.figure(24)
        for i in range(T):
            xdiff[i] = (xppm[i]-xtrue[i])*(xppm[i]-xtrue[i])
        plt.plot(range(T), xdiff, 'k-')
        plt.title('difference squared Standard PF and truth')

        plt.figure(25)
        e = np.ones(nm-1) * obsvar
        for i in range(mm):
            plt.plot(range(T), hppf[i],'g-')
        plt.errorbar(ns*np.arange(1,nm), y[0:nm-1], e, fmt='rx')
        plt.plot(range(T),xtrue,'k-')
        plt.title('evolution of Standard PF ensemble, truth, and observations')
        
        plt.figure(26)
        nwphulp=np.zeros(nm-1)
        for i in range(nm-1):
            nwphulp[i]=ns*(i)
        plt.plot(nwphulp[0:nm-2],nwp[0:nm-2])
        plt.ylim(0,mm)
        plt.title('Standard PF effective number of particles at observation times')
        plt.show()
    
        nbins = mm+1
        plt.figure(27)
        plt.hist(rank,bins=nbins)
        plt.title('rank histogram standard Particle Filter')
        
    # Output statistics

    if model_pure:
        print( 'Time=0 ',hmp[:,0].mean())
        print ('Time=T/2 ',hmp[:,T/2].mean())
        print ('Time=T ',hmp[:,T-1].mean())

    if pfpure:
        sx = 0
        for t in range(T):
            sx += ((xtrue[t]-xppm[t])**2)/T
        print ('Time-mean difference Pure PF and truth squared  ', np.sqrt(sx))
    
    if pfew:
        sx = 0
        for t in range(T):
            sx += ((xtrue[t]-xpm[t])**2)/T
        print ('Time-mean difference PF with proposal and truth squared  ', np.sqrt(sx))
        
    plt.ioff()
    plt.show()
        

if __name__ == '__main__':

    pfModel(ns=1,pfew=True)
