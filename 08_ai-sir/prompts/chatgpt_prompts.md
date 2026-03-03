# 🤖 ChatGPT Prompt Collection

## AI-Guided SIR Modeling Hackathon

This document contains **curated ChatGPT prompts** for the  
**AI-Guided SIR Modeling Hackathon**.

It is designed to teach participants **how to work with ChatGPT as a scientific coding co-pilot**, not as a black box.

---

## 🧠 Why Prompt Structure Matters

In this hackathon, ChatGPT is used to:

- Generate scientific Python code
- Explain epidemiological concepts
- Assist with debugging and inference
- Help write clear summaries

Poorly structured prompts often lead to:

- Wrong assumptions (units, data meaning)
- Silent modeling errors
- Overconfident but incorrect explanations

Good prompts dramatically reduce these risks.

---

## ⭐ Core Prompt Template (RECOMMENDED)

Use this template for **all scientific prompts** during the hackathon.

### 🔹 Core Prompt Template

```text
Persona:
You are an expert epidemiological modeler and Python educator.

Context:
- Dataset: biweekly measles case counts from Niamey, Niger
- Columns: biweek, community, measles
- Data represent: observed case counts (incidence)
- Time unit: biweeks
- Infectious period ≈ 2 weeks (gamma ≈ 1 per biweek)

Task:
[Describe ONE specific task clearly and precisely]

Constraints:
- Use Python compatible with Google Colab
- Allowed libraries: numpy, scipy, pandas, matplotlib
- Keep code minimal, readable, and well commented
- Do not rewrite unrelated code

Verification:
- Include at least one plot
- Include at least one sanity check
- Explain assumptions clearly

Output style:
- Step-by-step explanation
- Code blocks clearly separated
- Avoid unnecessary verbosity
```

---

## 📌 Prompting Rules (Hackathon Ground Rules)

1. **Always specify units** (biweeks, not days)
2. **Always state what the data represent** (case counts vs model states)
3. **One task per prompt**
4. **Always ask for plots**
5. **Always ask for sanity checks**
6. **Iterate in small steps**
7. **Treat ChatGPT as an assistant, not an authority**

---

## 🧩 Task-Specific Prompts

All prompts below are **instances of the Core Prompt Template**.

---

## 🧪 Prompt 1 — Load and Explore the Data

```text
Persona:
You are an expert epidemiological modeler and Python educator.

Context:
- Dataset contains biweekly measles case counts from Niamey, Niger
- Columns: biweek, community, measles

Task:
Write Python code to:
1. Load the CSV file
2. Plot measles cases over time for each community (A, B, C)
3. Briefly explain what biweekly case counts mean for modeling

Constraints:
- Use pandas and matplotlib
- Google Colab compatible

Verification:
- One time-series plot with labeled axes and legend
```

---

## 🧬 Prompt 2 — Implement the SIR Model (Biweekly Units)

```text
Persona:
You are an expert epidemiological modeler.

Context:
- Closed SIR model
- Time unit: biweeks
- Infectious period ≈ 2 weeks (gamma ≈ 1 per biweek)

Task:
Implement an SIR model using SciPy ODE solving and plot S, I, R.

Constraints:
- Use scipy.integrate
- Comment each equation

Verification:
- Plot S, I, R
- Verify S + I + R ≈ N at all times
```

---

## 📈 Prompt 3 — Feature-Based R₀ Estimation (Early Growth)

```text
Persona:
You are an expert in infectious disease modeling.

Context:
- Community A measles outbreak
- Biweekly case counts
- Early outbreak phase shows exponential growth

Task:
Estimate early exponential growth by:
1. Selecting an early biweek window
2. Fitting log(measles) vs time
3. Plotting the semi-log data and fitted line
4. Converting slope to R0 assuming gamma = 1 per biweek

Constraints:
- Use numpy or scipy
- Explain assumptions clearly

Verification:
- Semi-log plot
- Printed slope and R0 estimate
```

---

## 🔁 Prompt 4 — Incidence Mapping via Accumulator H(t)

```text
Persona:
You are an expert epidemic modeler.

Context:
- Observed data are biweekly case counts (incidence)
- SIR model outputs state variables

Task:
Extend the SIR model by:
- Adding an accumulator H(t) with dH/dt = beta * S * I / N
- Computing predicted biweekly cases as ΔH
- Aligning ΔH with observed biweeks

Constraints:
- Keep implementation minimal and readable

Verification:
- Plot ΔH over time
- Explain why ΔH matches the data structure
```

---

## 📉 Prompt 5 — Least Squares Parameter Fitting

```text
Persona:
You are an expert in numerical optimization.

Context:
- Observed data: biweekly case counts
- Model output: predicted ΔH

Task:
Fit beta (and optionally initial conditions) using least squares.

Constraints:
- Use scipy.optimize
- Use sensible initial values from early-growth estimates

Verification:
- Overlay plot of observed vs fitted cases
- Print best-fit parameter values
```

---

## 🎯 Prompt 6 — Poisson Likelihood Inference

```text
Persona:
You are an expert in statistical inference for epidemic models.

Context:
- Observed data are count data
- Poisson noise is appropriate
- μ_t = rho * ΔH_t (rho = reporting probability)

Task:
Implement maximum likelihood estimation assuming:
y_t ~ Poisson(μ_t)

Constraints:
- Use parameter transforms (log for beta, logit for rho)
- Explain why transforms are needed

Verification:
- Plot observed vs expected counts
- Print estimated parameters
```

---

## 📊 Prompt 7 — Profile Likelihood (Uncertainty)

```text
Persona:
You are an expert in likelihood-based inference.

Context:
- Poisson likelihood already implemented

Task:
Create a profile likelihood for beta by:
1. Fixing beta over a grid
2. Re-optimizing remaining parameters
3. Plotting profile log-likelihood
4. Indicating an approximate 95% confidence interval

Constraints:
- Keep computation lightweight

Verification:
- Profile likelihood plot
- Explanation of uncertainty interpretation
```

---

## 🧾 Prompt 8 — Generate a Markdown Summary

```text
Persona:
You are preparing a short scientific report for hackathon participants.

Task:
Write a Markdown-formatted summary covering:
- Model description
- Parameter estimates and R0
- Comparison of least squares vs Poisson inference
- Interpretation of uncertainty
- Suggested next steps (SEIR, time-varying beta)

Constraints:
- Scientific but accessible language
- Use Markdown headers and bullet points

Verification:
- Clean Markdown output suitable for a notebook cell
```

---

## 🚫 Bad Prompt vs ✅ Good Prompt

### Learning How to Talk to ChatGPT

### Example 1 — Missing Context & Units

#### 🚫 Bad Prompt

```text
Help me fit an SIR model to this data.
```

**Problems**

- No units
- No data meaning
- Incorrect assumptions likely

---

#### ✅ Good Prompt

```text
Persona:
You are an expert epidemiological modeler and Python educator.

Context:
- Dataset: biweekly measles case counts from Niamey, Niger
- Data represent incidence
- Time unit: biweeks

Task:
Fit an SIR model using Poisson likelihood.

Verification:
- Plot observed vs fitted cases
```

---

### Example 2 — Too Many Tasks at Once

#### 🚫 Bad Prompt

```text
Build an SIR model, fit it, explain R0, and say if the outbreak is controlled.
```

#### ✅ Good Prompt

```text
Task:
Implement and simulate the SIR model only.
```

➡️ Follow with a second prompt for inference.

---

### Example 3 — No Verification

#### 🚫 Bad Prompt

```text
Estimate beta and gamma.
```

#### ✅ Good Prompt

```text
Task:
Estimate beta using Poisson likelihood.

Verification:
- Plot observed vs expected counts
- Print log-likelihood
```

---

### Example 4 — Treating ChatGPT as an Oracle

#### 🚫 Bad Prompt

```text
Is this model correct?
```

#### ✅ Good Prompt

```text
Task:
Critically review the model assumptions and list weaknesses.
```

---

## 🧠 Final Takeaways

- ChatGPT is a **co-pilot**, not a decision-maker
- You control:
  - Assumptions
  - Units
  - Validation

- Good prompts → good science
- Bad prompts → confident nonsense

Happy hacking! 🚀
