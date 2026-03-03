# 🧑‍🏫 Organizer Guide

## AI-Guided SIR Modeling Hackathon

This document provides **step-by-step guidance for organizers and facilitators** running the **AI-Guided SIR Modeling Hackathon**.

It covers preparation, materials, facilitation flow, and post-hackathon follow-up to ensure a smooth and effective event.

---

## 🎯 Hackathon Goals (Organizer View)

The goals of this hackathon are to:

- Teach **core epidemic modeling concepts** using a simple SIR framework
- Demonstrate **parameter inference** from real outbreak data
- Show how **ChatGPT / LLMs** can accelerate scientific coding and reasoning
- Deliver a **fully reproducible, beginner-friendly** modeling workflow in 2 hours

This is a **guided learning hackathon**, not a competitive event.

---

## ⏱ Hackathon Format

- **Duration:** 2 hours
- **Mode:** Live guided demo + participant hands-on coding
- **Platform:** Google Colab
- **Audience:** Mixed background (data science, public health, general technical)

---

## 📦 Organizer Preparation Checklist

### 1️⃣ Google Colab Notebook (Required)

Prepare **one master notebook** that participants will copy.

**Notebook requirements**

- Runs top-to-bottom without errors
- Uses only standard scientific Python libraries
- Includes `# TODO:` markers for participant interaction
- Includes **checkpoint plots** after each major section

**Recommended sections**

1. Title & objectives
2. Dataset description (Niamey measles)
3. Imports & configuration
4. Data loading & exploratory plots
5. Feature-based R₀ estimation
6. SIR ODE model (biweekly time scale)
7. Least squares fitting
8. Poisson likelihood inference
9. Profile likelihood / uncertainty
10. Auto-generated Markdown summary

---

### 2️⃣ Dataset Preparation

Ensure the dataset is included in the repository:

- File: `data/niamey.csv`
- Columns:
  - `biweek`
  - `community`
  - `measles`

**Important modeling notes to highlight**

- Data represent **biweekly case counts**
- Infectious period ≈ **2 weeks**
- Time unit in models = **biweeks**
- Source: Grais et al. (2006)

---

### 3️⃣ Pre-Recorded ChatGPT Demonstrations

To maintain pacing, prepare **short screen recordings** of ChatGPT interactions.

**Recommended clips**

1. Notebook structure & assumptions
2. SIR model implementation
3. Feature-based R₀ estimation
4. Incidence mapping (ΔH)
5. Least squares vs Poisson inference
6. Profile likelihood interpretation

**Guidelines**

- Each clip: 30–90 seconds
- Show the full prompt + ChatGPT response
- Avoid scrolling or editing unrelated content

---

### 4️⃣ Slides (Optional but Recommended)

Prepare a lightweight slide deck (10–12 slides):

- Hackathon objectives
- What LLMs are / are not
- SIR model diagram
- Dataset context
- Inference workflow overview
- Common pitfalls & validation checks
- Suggested extensions

Slides should **support**, not replace, the notebook.

---

## 🧭 Facilitation Guide (During the Hackathon)

### Key Emphases for Facilitators

- Reinforce **units** (biweeks, not days)
- Clarify what the data represent (counts vs states)
- Encourage frequent plotting and sanity checks
- Treat ChatGPT as an **assistant**, not an oracle

### Time Management Tips

- Prioritize:
  - SIR simulation
  - Feature-based R₀
  - One inference method (Least-squares or Likelihood)
- Profile likelihood is optional if time is tight

---

## ⚠️ Common Pitfalls to Watch For

| Issue                               | How to Address                     |
| ----------------------------------- | ---------------------------------- |
| Confusing prevalence with incidence | Re-emphasize ΔH mapping            |
| Incorrect time units                | Repeat “biweeks” frequently        |
| Over-trusting ChatGPT output        | Ask participants to validate plots |
| Overfitting early                   | Start with community A only        |

---

## 📊 Expected Participant Outcomes

By the end of the hackathon, participants should have:

- A working SIR model
- At least one fitted parameter set
- One observed vs fitted plot
- A basic understanding of uncertainty

---

## 🧾 Post-Hackathon Actions

After the hackathon, organizers should:

- Share the final notebook link
- Publish slides (if used)
- Invite participants to fork the repository
- Optionally share extension challenges (SEIR, stochastic models)

---

## 📌 Reuse & Adaptation

This hackathon is designed to be reusable:

- For short courses
- For academic workshops
- For internal training

Organizers are encouraged to fork and adapt the materials as needed.

---

## 📬 Questions or Issues

If issues arise during preparation or delivery, please raise them via:

- GitHub Issues
- Internal organizer communication channels

---

Thank you for helping deliver a clear, accessible, and impactful **AI-guided modeling hackathon**.
