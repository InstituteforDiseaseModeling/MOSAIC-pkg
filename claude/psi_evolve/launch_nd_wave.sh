#!/usr/bin/env bash
# ND REFINEMENT WAVE. usage: nohup ./launch_nd_wave.sh > logs/nd_wave.log 2>&1 &
#
# ND (plain DLinear) is the best arm on MAE/R2corr/R2sse/bias/degen but wins by
# being SMOOTH AND FLAT (sd_ratio 0.84, the lowest of any arm) and is WORSE than
# the LSTM in 6 of 16 countries carrying 26% of the burden weight -- CMR most
# spectacularly, +430%. This wave asks whether that is intrinsic to a linear
# trunk or an artefact of three defects plus an unreachable knob.
#
# Each arm is ONE change stacked on the previous, so the ladder attributes.
# Defaults reproduce the scored `ND` exactly, so `ND` itself is the control.
#
#   NDe   pad = edge                  DEFECT. Zero-padding the moving average
#                                     drags the trend toward the feature mean at
#                                     both window edges, and the worst-hit
#                                     position is the LAST timestep -- the
#                                     forecast anchor. Zeng et al. replicate.
#   NDr   + l2 = 5e-4                 DEFECT. dlinear was the only trunk whose
#                                     kernels were unregularised; lstm/gru/tcn
#                                     all carry hp$l2.
#   NDk9  + kernel = 9                The knob that was unreachable until today
#                                     (read by the model, never passed by
#                                     run_rolling_cv_suitability). At T = 13,
#                                     k = 5 splits trend/remainder near-evenly;
#                                     k = 9 pushes far more into the trend.
#   NDi   + individual                DLinear-I: per-feature linear maps rather
#                                     than one shared map over the 38 channels.
#                                     ~38x the trunk weights and ~2.4x the
#                                     runtime (measured in smoke).
#
# NOT in this wave, and why: RevIN. Toner & Darlow (arXiv:2403.14587) find that
# for DLinear specifically the detrending already does RevIN's job, and
# arXiv:2510.04667 reports RevIN failing catastrophically under extreme
# outliers -- which describes this panel (bounded [0,1], many near-zero
# countries, COD saturated). It would be a coin flip dressed as a refinement.
set -u
cd "$HOME/psi_evolve" || exit 1
THRESH=${PSI_QUEUE_THRESH:-60}
free_cores () {
  ps -eo pcpu,args --sort=-pcpu | grep -v "[r]un_arm.R" \
    | awk 'NR>1 && $1>50 {c+=$1} END {printf "%.0f", c/100}'
}
for SPEC in "NDe:PSI_DLIN_PAD=edge" \
            "NDr:PSI_DLIN_PAD=edge PSI_DLIN_L2=5e-4" \
            "NDk9:PSI_DLIN_PAD=edge PSI_DLIN_L2=5e-4 PSI_DLIN_KERNEL=9" \
            "NDi:PSI_DLIN_PAD=edge PSI_DLIN_L2=5e-4 PSI_DLIN_INDIVIDUAL=1"; do
  ARM="${SPEC%%:*}"; ENVS="${SPEC#*:}"
  [ -f STOP ] && { echo "$(date -Is) STOP present; halted"; exit 1; }
  n=$(ls "psi_cache_${ARM}"/psi_*.csv 2>/dev/null | wc -l)
  if [ "$n" -ge 10 ]; then echo "$(date -Is) $ARM already has $n/10 -- skipping"; continue; fi
  echo "$(date -Is) waiting for capacity to launch $ARM"
  while [ "$(free_cores)" -gt "$THRESH" ]; do sleep 300; done
  echo "$(date -Is) launching $ARM with: $ENVS"
  env PSI_TRUNK=dlinear $ENVS PSI_EPOCH_SELECT=2 ./launch_arm.sh "$ARM" 10 10 12 || {
      echo "$(date -Is) launch of $ARM REFUSED -- skipping"; continue; }
  sleep 120
  while [ "$(pgrep -fc '[r]un_arm.R')" -gt 0 ]; do sleep 180; done
  echo "$(date -Is) $ARM finished: $(ls psi_cache_${ARM}/psi_*.csv 2>/dev/null | wc -l)/10 cutoffs"
done
echo "$(date -Is) ND wave complete"
