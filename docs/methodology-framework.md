# A Robust Research Framework for a Reef–Tourism Compound Indicator

**Satellite-based early-warning classes for managing coral reefs under tourism pressure**
_Case study: Maya Bay (Ko Phi Phi Leh), transferable to Lakshadweep and other reef-tourism sites_

---

## 1. Objective and research questions

**Aim.** Build a defensible, satellite-derived **compound indicator** that combines (a) reef *condition* and (b) tourism *pressure relative to carrying capacity*, and outputs **management classes** that tell a park authority *when to trigger precautionary measures* — before the reef reaches a critical state.

**Primary research question.**
Does tourism pressure at Maya Bay measurably degrade reef condition, and can this relationship be turned into an operational, threshold-based decision tool?

**Working hypotheses.**
- **H1 (pressure → state):** Periods/pixels of *high* tourism-pressure proxies are associated with *lower* reef-condition proxies (and *higher* macroalgae), after controlling for climate and season.
- **H2 (natural experiment):** Reef condition *improved* during the 2018–2022 closure (tourism ≈ 0) relative to a low-tourism **control reef** exposed to the same climate — and *declined/algalised* after reopening.
- **H0 (the honest null):** The apparent coral-condition changes are driven by **climate (marine heatwaves), water-clarity artefacts, or macroalgae**, *not* tourism. The framework must be able to reject tourism as the driver, not just confirm it.

> The value of the study is *not* assuming tourism harms the reef — it is building a design that can **distinguish** tourism impact from climate impact, and encode the answer into a decision rule.

---

## 2. Conceptual framework (DPSIR)

The framework is organised with the standard **Driver–Pressure–State–Impact–Response** model, which maps cleanly onto Maya Bay:

| DPSIR element | Maya Bay meaning | Satellite / data proxy |
|---|---|---|
| **Driver** | Tourism demand for the site | Regional tourism infrastructure, arrivals |
| **Pressure** | Footfall, boat traffic, trampling, resuspension, nutrients | Nighttime lights, **boat counts**, **turbidity anomaly**, visitor records |
| **State** | Live-coral cover vs macroalgae; benthic condition | Coral bottom index + **Coral-vs-Algae** heuristic; DHW |
| **Impact** | Reef degradation, algal phase-shift | Trend + phase in coral/algae proxies |
| **Response** | Closure, visitor cap, boat ban, time slots, seasonal closure | Carrying-capacity thresholds → **management classes (this framework's output)** |

The **compound indicator sits at the Pressure × State intersection**, and the **management classes are the Response**.

---

## 3. Study design — the closure as a natural experiment

Maya Bay offers a rare **quasi-experimental** design: tourism was switched **off (2018–2022)** and **on again (2022→)**. This is far stronger than correlation alone.

**Design: Interrupted Time Series with a Control site (a BACI-style design).**

- **Periods:** `PRE` (≤ Jun 2018, heavy tourism) · `DURING` (Jul 2018 – Dec 2021, closed) · `POST` (Jan 2022 →, capped tourism).
- **Impact site:** Maya Bay reef.
- **Control site (essential):** a nearby reef with **similar depth, geomorphology and climate exposure but little/no tourism** (e.g., a remote reef in the same Andaman/Phi Phi complex). The control experiences the *same marine heatwaves* but *not the tourism switch*.
- **Inference:** change *attributable to tourism* = (Impact-site change) − (Control-site change) across periods. This is what separates **tourism** from **climate** — the single most important methodological safeguard.

> Without a control site, any "tourism effect" is confounded by the 2016/2024 marine heatwaves, which hit the whole region. The control reef is non-negotiable for a publishable causal claim.

---

## 4. Indicators and data

### 4.1 Reef condition (STATE) — with honest handling of the coral/algae problem

The blue-green bottom index alone **cannot separate live coral from macroalgae** (both darken the bottom). So the condition axis is built from *several* components, not the index alone:

| Component | Source (in pipeline) | Direction | Note |
|---|---|---|---|
| Coral-likely fraction | **Coral-vs-Algae heuristic** (temporal stability) | ↑ good | Stable bottom = coral-likely |
| Macroalgae-likely fraction | Coral-vs-Algae heuristic (high variability) | ↑ **bad** | Fluctuating bottom = algae-likely |
| Bottom-index level & trend | Coral Health (Landsat/S2) | context | Interpret *only* with the split above |
| Thermal-stress load | **Degree Heating Weeks (OISST)** | ↑ bad | Bleaching driver / modifier |

**Validation of the condition axis (as you noted):** use the **Coral-vs-Algae map across PRE / DURING / POST** periods. If the post-reopening *darkening* is concentrated in **high-variability (algae-likely)** pixels — and peaks in the 2024 heatwave year — then the "coral recovery" reading is rejected and the darkening is classified as **algal phase-shift**. This is the internal validation of the whole condition axis.

### 4.2 Tourism pressure (PRESSURE) — a frank assessment of each proxy

Your intuition (VIIRS + turbidity) is partly right, but each proxy has an important caveat. Ranked by suitability **for Maya Bay footfall specifically**:

| Proxy | What it really measures | Reliability for *Maya Bay footfall* | Verdict |
|---|---|---|---|
| **Boat counts** (Sentinel-2/-1 target detection) | Vessels near the bay/pier on the image date | **Most direct** daytime-activity proxy; snapshot only | **Add** — strongest satellite proxy |
| **Turbidity anomaly** (Nechad, above monsoon baseline) | Sediment resuspension from boats, anchors, swimmers, trampling | Good *activity + stressor*, **but confounded by monsoon/waves/runoff** | Use **only as anomaly vs a seasonal/control baseline** |
| **Nighttime lights** (Black Marble/VIIRS) | Regional tourism *infrastructure* & night economy | Weak for Maya Bay itself (**no overnight stay, day-trip only**; lights are on Phi Phi Don) | Use as a **regional demand** driver, not site footfall |
| **Built-up (IBI / Dynamic World)** | Tourism *infrastructure* growth | Slow-changing; development pressure, not daily footfall | Context / driver only |
| **Actual visitor records (DNP)** | True footfall | **Gold standard** | **Get if at all possible** |

**Honest conclusion:** *No single satellite layer is a clean footfall meter.* The defensible pressure axis is a **composite**: **turbidity-anomaly + boat-density + regional nighttime-lights**, ideally **anchored/validated against official DNP visitor counts**. Turbidity is the strongest *ecologically-relevant* pressure signal because it is simultaneously an activity proxy **and** a direct coral stressor — but it must be de-confounded from monsoon (Section 8).

### 4.3 Carrying capacity (the denominator)

Carrying capacity = the reef's tolerance threshold, set by the park authority, not the satellite. Maya Bay's reopening imposed concrete limits — a **daily visitor cap (reported ≈ 4,000/day), a ban on boats entering the bay (floating pier + boardwalk), time-limited slots, and an annual Aug–Sept closure** (use the **DNP official figures** as the authoritative source). This gives the framework a *real* denominator.

- **If footfall data exists:** `Carrying Capacity = official daily/annual cap`.
- **If only satellite proxies exist:** estimate an **ecological carrying capacity** from reef area and published reef-use thresholds (e.g., divers/snorkellers per ha·yr), and treat the proxy composite as *relative* pressure normalised to the closure-period floor (≈0) and a pre-closure peak.

---

## 5. Constructing the compound indicator

Two normalised sub-indices are combined; keep them **separate first** (never average condition and pressure into one number — a park needs to see *both*).

### 5.1 Reef Condition Score (RCS ∈ [0,1], higher = healthier)

```
RCS = w1 · CoralLikely_frac
    + w2 · (1 − Macroalgae_frac)
    + w3 · norm(bottom-index level, reef-referenced)
    − w4 · norm(DHW thermal stress)
```
- Each term normalised 0–1 over the study record (or against a reference reef).
- Default equal-ish weights (`w1=0.35, w2=0.35, w3=0.15, w4=0.15`); **run a weight-sensitivity analysis** (Section 8) so conclusions don't hinge on arbitrary weights.
- **Report RCS with uncertainty**, and always alongside the coral-vs-algae split (never as "coral cover").

### 5.2 Tourism Pressure Ratio (TPR = Footfall / Carrying Capacity)

```
Footfall_proxy = a · norm(turbidity anomaly)
               + b · norm(boat density)
               + c · norm(regional nighttime lights)      (a>b>c; calibrate to DNP counts if available)

TPR = Footfall (or Footfall_proxy) / Carrying_Capacity
```
- `TPR < 1` = within capacity; `TPR > 1` = over capacity.
- Where real visitor numbers exist, **calibrate the proxy composite against them** (regression) and report the R² — this turns the proxy into a validated footfall estimate.

### 5.3 The two-axis space

Plot every time-step (and, spatially, every reef cell) in the **RCS × TPR plane**. Movement through this plane over PRE→DURING→POST is the study's central figure.

---

## 6. Management decision classes (the framework's output)

Bin each axis and add **trend direction** (the precautionary ingredient — act on *trajectory*, not just current state):

- **RCS:** Good (≥ 0.66) · Moderate (0.33–0.66) · Poor (< 0.33)
- **TPR:** Under (< 0.7) · Near (0.7–1.0) · Over (> 1.0)
- **Trend:** Improving · Stable · Declining (from the Mann-Kendall/OLS trend on RCS)

| | **TPR Under (<0.7)** | **TPR Near (0.7–1.0)** | **TPR Over (>1.0)** |
|---|---|---|---|
| **RCS Good** | 🟢 **Sustainable** — routine monitoring | 🟡 **Caution** — cap growth, tighten management | 🟠 **Act now** — reduce footfall to capacity |
| **RCS Moderate** | 🟡 **Watch** — investigate stressors | 🟠 **Precautionary** — reduce slots, restrict high-impact use | 🔴 **Critical** — mandatory footfall cut |
| **RCS Poor** | 🔵 **Climate-driven** — tourism not the driver; protect from other stressors, investigate | 🔴 **Critical** — strong reduction | 🔴 **Emergency** — temporary closure |

**Explicit precautionary trigger (your requirement — "act *before* it's critical"):**

> Initiate **precautionary measures** when **any** of these hold:
> 1. `TPR > 1.0` (over capacity), **or**
> 2. RCS **Declining for ≥ 2 consecutive periods** **and** `TPR ≥ 0.7`, **or**
> 3. `DHW > 4 °C-weeks` (bleaching risk) **and** `TPR ≥ 0.7` (heat + crowding compound), **or**
> 4. Macroalgae-likely fraction **rising for ≥ 2 periods** while `TPR ≥ 0.7` (early phase-shift under pressure).

The **🔵 Climate-driven** cell is deliberate and important: it stops managers from cutting tourism when the reef is actually declining from **heat, not people** — and redirects the response (e.g., heat-resilience measures) instead of an ineffective closure.

---

## 7. Causal inference — turning correlation into evidence

Correlation between pressure and condition is **not** enough (we already saw spurious/shared-trend and same-sensor inflation). Establish H1/H2 with:

1. **Control-site differencing (BACI):** attribute change to tourism only where the impact site diverges from the climate-matched control.
2. **Interrupted time series:** test for a **level/slope break at Jun-2018 and Jan-2022** in RCS and macroalgae, controlling for DHW.
3. **De-trending & seasonal control:** compare *year-to-year anomalies* (not raw levels) and control for monsoon months, so a shared time-trend or seasonality can't masquerade as a tourism effect.
4. **Lagged cross-correlation:** reef response lags pressure — test pressure(t) vs condition(t+lag) for lags of months–years.
5. **Coral-vs-Algae phase check:** confirm whether post-reopening darkening is algae (high variability, heatwave-timed) — the mechanism behind H1.

A tourism effect is credible **only if** it survives (1)–(4) *and* the mechanism (5) is consistent.

---

## 8. Robustness, limitations and assumptions

- **Screening, not ground truth.** Every optical layer is a screening proxy; **field/hyperspectral validation** of the coral-vs-algae split is required before firm claims.
- **Turbidity is dual-natured** (pressure *and* natural signal): only ever use it as an **anomaly vs a monsoon/control baseline**.
- **Coral index ≠ live coral** — carried through the whole framework via the coral-vs-algae split and the macroalgae term.
- **Footfall proxies are indirect** — calibrate to DNP counts; report proxy R².
- **Weight/threshold sensitivity:** publish how classes shift under ±weight and ±threshold changes; conclusions must be stable.
- **Resolution mismatch:** DHW/lights are coarse (25 km / 500 m); coral/turbidity are 10–30 m — keep coarse layers as *regional context*, fine layers as *site condition*.
- **Small ROI:** Maya Bay is tiny; use Lakshadweep atolls (larger, multi-pixel) to strengthen spatial statistics.

---

## 9. Implementation roadmap (against the current pipeline)

**Already in the pipeline (usable today):** Coral Health (Landsat long-record) · Coral-vs-Algae heuristic · Degree Heating Weeks · Nechad turbidity · Black Marble/VIIRS lights · IBI built-up · Trend (with 95% CI) · Cross-indicator correlation · reproducible report.

**To add for the full framework:**
1. **Boat-density layer** (Sentinel-2 bright-target / Sentinel-1 vessel detection) — the missing direct footfall proxy.
2. **Period/phase engine** — user supplies closure dates → pipeline outputs **PRE/DURING/POST** stats and a **breakpoint (interrupted-time-series) trend** per indicator.
3. **Control-site mode** — run the same stack over a control reef and difference the results.
4. **Turbidity-anomaly (de-seasonalised)** instead of raw turbidity.
5. **RCS & TPR calculators** + the **traffic-light classifier** (the compound-index step) → a single management-status output + the RCS×TPR trajectory figure.
6. **DNP footfall ingestion** (CSV) → calibrate the proxy composite.

**Suggested next build order:** (2) period/breakpoint engine → (5) RCS/TPR + traffic-light → (1) boat layer → (3) control-site mode.

---

## 10. One-paragraph summary

This framework treats Maya Bay's 2018–2022 closure as a **natural experiment**, measures reef **condition** (coral-vs-algae–validated, not raw index) and tourism **pressure** (turbidity-anomaly + boats + lights, normalised to the park's **carrying capacity**), and — only after separating tourism from climate with a **control reef and interrupted-time-series design** — combines them into a two-axis **RCS × TPR** space with a **traffic-light management rule** that fires **precautionary measures on trajectory, not just on crisis**. The honest core is that it can *reject* tourism as the driver (the 🔵 climate cell), which is exactly what makes a *confirmation* credible and the tool trustworthy for a park authority.

---

### Key references (methodological anchors)
- DPSIR framework — EEA (1999); Gari et al. (2015), *Ocean & Coastal Management*.
- BACI / interrupted time series — Underwood (1994); Bernal et al. (2017).
- Reef Health Index (coral vs fleshy macroalgae) — Healthy Reefs Initiative report cards; McField & Kramer (2007).
- Degree Heating Weeks / bleaching — Liu et al. (2014); NOAA Coral Reef Watch.
- Turbidity (Nechad) — Nechad, Ruddick & Park (2010), *Remote Sensing of Environment*.
- Tourism carrying capacity / Limits of Acceptable Change — Stankey et al. (1985); UNWTO carrying-capacity guidance.
- Maya Bay closure & recovery — Worachananant (management); CNN/DNP reporting on the 2018 closure and 2022 capped reopening.
