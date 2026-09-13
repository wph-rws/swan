"""Check the Komen algebra and its analytic Jacobian, not SWAN performance.

Uses a synthetic positive 36 x 25 spectrum, fixed depth, SWAN's logarithmic
quadrature and linear tail contributions. No breaking cap or switches.
Run with OPENBLAS_NUM_THREADS=1 python3 verify_rank_one.py.
"""
import json
import numpy as np

rng = np.random.default_rng(4151)
f = np.geomspace(.04, 1., 25)
sigma = 2*np.pi*f
theta = np.arange(36)*2*np.pi/36
depth, grav = 10., 9.81
k = np.maximum(sigma**2/grav, sigma/np.sqrt(grav*depth))
for _ in range(15):
    t = np.tanh(k*depth)
    k -= (grav*k*t-sigma**2)/(grav*(t+k*depth*(1-t*t)))
dlog = np.log(f[-1]/f[0])/(len(f)-1)
dtheta = 2*np.pi/len(theta)
half = np.exp(dlog/2)
tail_a = 1/(4*(1+4*(half-1)))
tail_e = 1/(3*(1+3*(half-1)))
we = sigma**2*dlog*dtheta
wa = sigma*dlog*dtheta
wb = we/np.sqrt(k)
we[-1] *= 1+tail_e/dlog
wa[-1] *= 1+tail_a/dlog
wb[-1] *= 1+tail_a/dlog
we, wa, wb, kk = (np.repeat(v, len(theta)) for v in [we,wa,wb,k])
n = (np.exp(-(np.log(f/.15)/.55)**2)[:,None]*(.02+np.maximum(np.cos(theta),0)**4)).ravel()
n *= .5/(we@n)
C1, C2 = 2.36e-5, 3.02e-3

def original_sink(n):
    E, A, B = we@n, wa@n, wb@n
    K = (E/B)**2
    mean_sigma = E/A
    Ck = C1*(kk/K)*(K*np.sqrt(E)/np.sqrt(C2))**4
    return Ck*mean_sigma*(kk/K)*n

def coefficient(n):
    E,A,B = we@n,wa@n,wb@n
    return C1/C2**2*E**7/(A*B**4)

a = coefficient(n)
E,A,B = we@n,wa@n,wb@n
u = kk**2*n
v = a*(7*we/E-wa/A-4*wb/B)
direction = n*rng.normal(size=n.size)
analytic_jv = a*kk**2*direction+u*(v@direction)
step = 1e-5
finite_jv = (original_sink(n+step*direction)-original_sink(n-step*direction))/(2*step)
relative = lambda x,y: float(np.linalg.norm(x-y)/np.linalg.norm(y))
diagonal = .02 + a*kk**2
rhs = rng.normal(size=n.size)
z = rhs/diagonal
q = u/diagonal
sherman_morrison = z-q*(v@z)/(1+v@q)
dense = np.linalg.solve(np.diag(diagonal)+np.outer(u,v), rhs)
result = {
    'kind': 'algebra check on synthetic spectrum; not a SWAN integration or speed benchmark',
    'spectral_bins': n.size,
    'original_vs_reduced_formula_relative_l2': relative(a*u, original_sink(n)),
    'analytic_jv_vs_central_difference_relative_l2': relative(analytic_jv,finite_jv),
    'rank_one_solve_vs_dense_relative_l2': relative(sherman_morrison,dense),
    'sink_cubic_homogeneity_relative_l2': relative(original_sink(1.3*n),1.3**3*original_sink(n)),
    'rank_one_denominator': float(1+v@q),
}
assert result['original_vs_reduced_formula_relative_l2'] < 1e-12
assert result['analytic_jv_vs_central_difference_relative_l2'] < 1e-7
assert result['rank_one_solve_vs_dense_relative_l2'] < 1e-12
print(json.dumps(result,indent=2))
