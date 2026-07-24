#!/usr/bin/env python3
"""Verificatieplots so-rp_swan: 41.31 vs 41.51 en de stapsgewijze fysica-restore."""
import re, os
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

#  Paths are derived from this script's own location, so the plots can be
#  regenerated from any checkout rather than only from one working copy.
HERE = os.path.dirname(os.path.abspath(__file__))
RUNS = os.path.join(os.path.dirname(HERE), "runs")
OUT  = HERE
os.makedirs(OUT, exist_ok=True)

# Okabe-Ito (colorblind-safe)
C = dict(green="#009E73", verm="#D55E00", orange="#E69F00",
         blue="#0072B2", purple="#CC79A7", grey="#555555")

# run-key -> (label, kleur)
RUNSPEC = [
    ("bss4131_converged",                    "41.31 (BSS-binary)",            C["green"]),
    ("upstream4131_converged",               "41.31 (vers gecompileerd)",     C["grey"]),
    ("current_converged",                    "41.51 default (Westhuysen)",    C["verm"]),
    ("current_converged_gen3_komen",         "41.51 + GEN3 KOMEN",            C["orange"]),
    ("current_converged_gen3_komen_drag_fit","41.51 + GEN3 KOMEN DRAG FIT",   C["blue"]),
    ("current_converged_legacy_defaults",    "41.51 + legacy defaults",       C["purple"]),
]

EXCV = -1.0   # SWAN-exceptiewaarde is -9 voor droge/landpunten; alles <EXCV wegfilteren

def load_tab(run):
    """Return Xp,Yp,Depth,Hsig arrays (rauw, incl. eventuele -9 landpunten)."""
    xs, ys, dep, hs = [], [], [], []
    with open(f"{RUNS}/{run}/uitvoerpunten.tab") as f:
        for ln in f:
            if ln.lstrip().startswith("%"): continue
            p = ln.split()
            if len(p) < 12: continue
            try:
                xs.append(float(p[0])); ys.append(float(p[1]))
                dep.append(float(p[2])); hs.append(float(p[3]))
            except ValueError: continue
    return (np.array(xs), np.array(ys), np.array(dep), np.array(hs))

def load_conv(run):
    """Return array of 'accuracy OK in X %' per iteration (index0=iter1)."""
    vals = []
    with open(f"{RUNS}/{run}/PRINT") as f:
        for ln in f:
            m = re.search(r"accuracy OK in\s+([0-9.]+)\s*% of wet", ln)
            if m: vals.append(float(m.group(1)))
    return np.array(vals)

raw = {k: load_tab(k) for k, _, _ in RUNSPEC}
conv = {k: load_conv(k) for k, _, _ in RUNSPEC}

# Gedeeld nat-masker: sluit de 3 landpunten (Hsig=-9) uit; identiek in alle runs.
REFKEY = "bss4131_converged"
wet = raw[REFKEY][3] > EXCV
NDRY = int((~wet).sum())
data = {k: tuple(a[wet] for a in raw[k]) for k in raw}   # gefilterd op natte punten
mean_hs = {k: data[k][3].mean() for k, _, _ in RUNSPEC}
REF = mean_hs["bss4131_converged"]          # echte 41.31 referentie
DEF = mean_hs["current_converged"]          # 41.51 default
GAP = DEF - REF

# ---------------------------------------------------------------- Fig 1: ladder
fig, ax = plt.subplots(figsize=(9.2, 4.6))
order = list(RUNSPEC)                        # top->bottom as listed but reverse for barh
labels = [s[1] for s in order][::-1]
colors = [s[2] for s in order][::-1]
vals   = [mean_hs[s[0]] for s in order][::-1]
ypos = np.arange(len(vals))
ax.barh(ypos, vals, color=colors, edgecolor="white", height=0.7, zorder=3)
ax.axvline(REF, color=C["green"], ls="--", lw=1.4, zorder=2)
ax.text(REF, len(vals)-0.35, f" 41.31-referentie = {REF:.4f} m",
        color=C["green"], va="center", ha="left", fontsize=8.5)
for y, v in zip(ypos, vals):
    pct = 100*(v-REF)/GAP if GAP else 0
    lab = f"{v:.4f} m"
    if abs(v-REF) > 1e-4:
        lab += f"   (+{v-REF:+.4f}, {pct:.0f}% v/h gat)".replace("++","+")
    ax.text(v+0.004, y, lab, va="center", ha="left", fontsize=8.5)
ax.set_yticks(ypos); ax.set_yticklabels(labels, fontsize=9)
ax.set_xlim(0, max(vals)*1.28)
ax.set_xlabel("Gemiddelde Hsig over 132 natte uitvoerpunten [m]")
ax.set_title("so-rp: stapsgewijze restore van 41.31-fysica in 41.51\n"
             "(20 m/s uit 310°, NAP +3,00 m, MXITST=50)", fontsize=11)
ax.grid(axis="x", color="0.85", zorder=0)
for sp in ("top","right","left"): ax.spines[sp].set_visible(False)
fig.tight_layout(); fig.savefig(f"{OUT}/fig1_hsig_ladder.png", dpi=150); plt.close(fig)

# ---------------------------------------------------------------- Fig 2: convergentie
fig, ax = plt.subplots(figsize=(8.8, 5.0))
for k, lab, col in RUNSPEC:
    if k == "upstream4131_converged":  # valt samen met BSS -> stippel
        y = conv[k]; ax.plot(np.arange(1,len(y)+1), y, color=col, lw=1.3, ls=":", label=lab)
    else:
        y = conv[k]; ax.plot(np.arange(1,len(y)+1), y, color=col, lw=1.8, label=lab)
    # markeer laatste iteratie
    ax.plot(len(conv[k]), conv[k][-1], "o", color=col, ms=5)
ax.axhline(98.0, color="0.3", ls="--", lw=1.1)
ax.text(1, 98.4, "98% convergentiecriterium", fontsize=8.5, color="0.3")
ax.set_xlabel("Iteratie"); ax.set_ylabel("Accuratesse OK [% natte roosterpunten]")
ax.set_title("Convergentieverloop: Westhuysen (41.51) itereert trager", fontsize=11)
ax.set_xlim(1, 36); ax.set_ylim(0, 101)
ax.legend(fontsize=8.3, loc="lower right", framealpha=0.95)
ax.grid(color="0.9")
for sp in ("top","right"): ax.spines[sp].set_visible(False)
fig.tight_layout(); fig.savefig(f"{OUT}/fig2_convergentie.png", dpi=150); plt.close(fig)

# ---------------------------------------------------------------- Fig 3: ruimtelijk
x, y, dep, hs31 = data["bss4131_converged"]
_,_,_, hs51 = data["current_converged"]
_,_,_, hsleg = data["current_converged_gen3_komen_drag_fit"]
d_def = hs51 - hs31
d_leg = hsleg - hs31
vmax = max(abs(d_def).max(), abs(d_leg).max())

fig, axs = plt.subplots(2, 2, figsize=(12.5, 8.2))
def scat(ax, c, title, cmap, vmin=None, vmax=None, cbl=""):
    s = ax.scatter(x/1000, y/1000, c=c, cmap=cmap, s=26, vmin=vmin, vmax=vmax,
                   edgecolor="0.3", linewidth=0.2)
    ax.set_title(title, fontsize=10.5); ax.set_aspect("equal")
    ax.set_xlabel("x [km]"); ax.set_ylabel("y [km]")
    cb = fig.colorbar(s, ax=ax, shrink=0.85); cb.set_label(cbl, fontsize=9)
hmax = max(hs31.max(), hs51.max()); hmin = min(hs31.min(), hs51.min())
scat(axs[0,0], hs31, "Hsig — 41.31 (BSS)", "viridis", hmin, hmax, "Hsig [m]")
scat(axs[0,1], hs51, "Hsig — 41.51 default (Westhuysen)", "viridis", hmin, hmax, "Hsig [m]")
scat(axs[1,0], d_def, "Verschil: 41.51 default − 41.31", "RdBu_r", -vmax, vmax, "ΔHsig [m]")
scat(axs[1,1], d_leg, "Verschil: 41.51 + GEN3 KOMEN DRAG FIT − 41.31", "RdBu_r", -vmax, vmax, "ΔHsig [m]")
fig.suptitle("Ruimtelijke Hsig-vergelijking over de 132 natte uitvoerpunten", fontsize=12.5)
fig.tight_layout(rect=[0,0,1,0.97]); fig.savefig(f"{OUT}/fig3_ruimtelijk.png", dpi=150); plt.close(fig)

# ---------------------------------------------------------------- Fig 4: 1:1 scatter
fig, ax = plt.subplots(figsize=(6.6, 6.4))
lo, hi = 0.9*hs31.min(), 1.05*hs51.max()
ax.plot([lo,hi],[lo,hi], color="0.4", ls="--", lw=1.2, label="1:1 (identiek aan 41.31)")
ax.scatter(hs31, hs51,  c=C["verm"],   s=22, edgecolor="white", lw=0.3,
           label="41.51 default (Westhuysen)")
ax.scatter(hs31, hsleg, c=C["blue"], s=22, edgecolor="white", lw=0.3,
           label="41.51 + GEN3 KOMEN DRAG FIT")
ax.set_xlabel("Hsig — 41.31 (BSS) [m]"); ax.set_ylabel("Hsig — 41.51-variant [m]")
ax.set_title("Punt-voor-punt: default zit systematisch te hoog,\nrestore valt op de 1:1-lijn", fontsize=11)
ax.set_xlim(lo,hi); ax.set_ylim(lo,hi); ax.set_aspect("equal")
ax.legend(fontsize=8.6, loc="upper left"); ax.grid(color="0.9")
for sp in ("top","right"): ax.spines[sp].set_visible(False)
fig.tight_layout(); fig.savefig(f"{OUT}/fig4_scatter_1op1.png", dpi=150); plt.close(fig)

# ---------------------------------------------------------------- Fig 5 & 6: gebiedsdekkende velden
import scipy.io as sio
from scipy.interpolate import griddata

def load_field(run):
    m = sio.loadmat(f"{RUNS}/{run}/scaloost_rp.mat")
    return (np.array(m['Xp'], float)/1000.0,
            np.array(m['Yp'], float)/1000.0,
            np.array(m['Hsig'], float))

FLDSPEC = [
    ("bss4131_converged",                     "41.31 (BSS)"),
    ("current_converged",                     "41.51 default (Westhuysen)"),
    ("current_converged_gen3_komen",          "41.51 + GEN3 KOMEN"),
    ("current_converged_gen3_komen_drag_fit", "41.51 + GEN3 KOMEN DRAG FIT"),
]
# kromlijnige mesh: coord-gaten (droge/exterieure cellen) 1x opvullen in index-ruimte
FX, FY, _ = load_field("bss4131_converged")
ny, nx = FX.shape
jj, ii = np.mgrid[0:ny, 0:nx]
def _fill(A):
    ok = np.isfinite(A); miss = ~ok
    pts = np.column_stack([jj[ok], ii[ok]])
    lin = griddata(pts, A[ok], (jj[miss], ii[miss]), method="linear")
    nn  = griddata(pts, A[ok], (jj[miss], ii[miss]), method="nearest")
    out = A.copy(); out[miss] = np.where(np.isfinite(lin), lin, nn); return out
FXf, FYf = _fill(FX), _fill(FY)
xf = FX[np.isfinite(FX)]; yf = FY[np.isfinite(FY)]
EXT = (xf.min(), xf.max(), yf.min(), yf.max())
Hfld = {k: load_field(k)[2] for k, _ in FLDSPEC}
HMAX = max(np.nanmax(Hfld[k]) for k, _ in FLDSPEC)

def _panel(ax, C, title, cmap, vmin, vmax):
    pm = ax.pcolormesh(FXf, FYf, np.ma.masked_invalid(C), cmap=cmap,
                       vmin=vmin, vmax=vmax, shading="nearest", rasterized=True)
    ax.set_title(title, fontsize=10.5); ax.set_aspect("equal")
    ax.set_xlim(EXT[0], EXT[1]); ax.set_ylim(EXT[2], EXT[3])
    ax.set_xlabel("x [km]"); ax.set_ylabel("y [km]"); ax.set_facecolor("white")
    return pm

# Fig 5: absolute Hsig-velden
fig, axs = plt.subplots(2, 2, figsize=(12.6, 8.8), constrained_layout=True)
for ax, (k, lab) in zip(axs.ravel(), FLDSPEC):
    pm = _panel(ax, Hfld[k], lab, "viridis", 0, HMAX)
fig.colorbar(pm, ax=axs, shrink=0.82, label="Hsig [m]")
fig.suptitle("Gebiedsdekkend Hsig-veld over het so-rp-rekenrooster (20 m/s uit 310°)", fontsize=13)
fig.savefig(f"{OUT}/fig5_veld_hsig.png", dpi=150); plt.close(fig)

# Fig 6: verschilvelden t.o.v. 41.31
DIFFSPEC = [s for s in FLDSPEC if s[0] != "bss4131_converged"]
diffs = {k: Hfld[k] - Hfld["bss4131_converged"] for k, _ in DIFFSPEC}
dmax = max(np.nanmax(np.abs(diffs[k])) for k, _ in DIFFSPEC)
fig, axs = plt.subplots(1, 3, figsize=(15.5, 5.4), constrained_layout=True)
for ax, (k, lab) in zip(axs.ravel(), DIFFSPEC):
    pm = _panel(ax, diffs[k], f"{lab}  −  41.31", "RdBu_r", -dmax, dmax)
fig.colorbar(pm, ax=axs, shrink=0.8, label="ΔHsig [m]")
fig.suptitle("Verschil met 41.31 over het hele rooster — de restore dooft het verschil uit", fontsize=13)
fig.savefig(f"{OUT}/fig6_veld_verschil.png", dpi=150); plt.close(fig)
print("Veldplots: max |Δ| default =", round(np.nanmax(np.abs(diffs['current_converged'])),3),
      "| max |Δ| restore =", round(np.nanmax(np.abs(diffs['current_converged_gen3_komen_drag_fit'])),3))

# ---------------------------------------------------------------- samenvatting
print("REF (41.31) =", round(REF,4), "| DEF (41.51) =", round(DEF,4), "| gat =", round(GAP,4))
for k, lab, _ in RUNSPEC:
    print(f"  {lab:34s} mean={mean_hs[k]:.4f}  iters={len(conv[k])}  eind={conv[k][-1]:.2f}%")
print("Npunten:", len(hs31), "| max |Δ| default =", round(abs(d_def).max(),4),
      "| max |Δ| restore =", round(abs(d_leg).max(),4))
print("Plots weggeschreven naar", OUT)
