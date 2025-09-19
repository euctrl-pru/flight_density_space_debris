
# DASC 2025: Preparing for Potential Closure of European Airspaces due to Re-entering Space Objects 

This repository contains the source materials and pointers to the supporting code for the paper:

**Preparing for Potential Closure of European Airspaces due to Re-entering Space Objects**  
*Enrico Spinielli, Rainer Koelle, Quinten Goens*  
EUROCONTROL, Aviation Intelligence Unit, Brussels, Belgium

---

## 🛰️ Overview

The increasing frequency of uncontrolled space debris re-entries poses an emerging threat to aviation safety over Europe. This study proposes a data-driven approach to support decision-making for potential airspace closures in response to such re-entries.

We present a methodology that:
- Uses actual flight trajectory data from EUROCONTROL's Network Manager
- Employs H3 geospatial indexing to generate hourly and daily flight densities
- Computes mid-air impact probabilities for debris re-entry corridors
- Aims to inform authorities and stakeholders of exposure and required mitigation actions

![collision expectation on 2024-07-05](https://github.com/euctrl-pru/flight_density_space_debris/blob/main/media/figures/collision_expectation_2024-07-05_3.png?raw=true)

---

## 📄 Abstract

With the growth in space activities and orbital objects, the likelihood of uncontrolled space object re-entries intersecting flight paths increases. This paper presents a practical methodology for computing time-varying flight density maps based on actual flight trajectories using H3 geospatial bins. These maps, when combined with modeled re-entry paths and risk buffers, yield dynamic mid-air impact probabilities. The results can support authorities in determining when to implement airspace closures or re-routings.

---

## 📁 Repository Structure

```
.
├── media/                  # Figures, preamble, acronyms, and HTML/LaTeX header includes
├── _extensions/            # LaTeX extensions
├── R/                      # Scripts for trajectory processing, visualizing and H3 binning
├── abstract_dasc25.qmd     # Abstract Quarto manuscript
├── paper.qmd               # Quarto manuscript (IEEE format)
├── paper.pdf               # PDF manuscript (IEEE format)
├── README.md               # This file
└── media/references.bib    # Bibliography
```

---

## 🛠️ Reproducing the Paper

This project uses [Quarto](https://quarto.org) for multi-format scientific publishing.

### Requirements

- [Quarto](https://quarto.org/docs/get-started/)
- A working [LaTeX distribution](https://quarto.org/docs/tools/latex.html)
- R (for statistical scripts, if applicable)

### Rendering the Paper

```bash
quarto render paper.qmd --to ieee-pdf
# Or for HTML output:
quarto render paper.qmd --to ieee-html
```

---

## 📊 Data & Methodology

- **Flight Density**: Constructed using historical trajectories recorded by EUROCONTROL's Network Manager.
- **Spatial Binning**: Flight points are aggregated using [Uber’s H3](https://h3geo.org) hexagonal indexing system.
- **Risk Estimation**: Mid-air probabilities are computed along potential re-entry corridors.
- **Demonstration Case**: Includes an analysis of an expected re-entry from a 1972 failed USSR Venus mission.

---

## 📬 Contact

- Enrico Spinielli — [enrico.spinielli@eurocontrol.int](mailto:enrico.spinielli@eurocontrol.int)
- Rainer Koelle — [rainer.koelle@eurocontrol.int](mailto:rainer.koelle@eurocontrol.int)
- Quinten Goens — [quinten.goens@eurocontrol.int](mailto:quinten.goens@eurocontrol.int)

---

## 📄 Citation

If you use or refer to this work, please cite it as:

```bibtex
@article{spinielli2025airspace,
  title     = {Preparing for Potential Closure of European Airspaces due to Re-entering Space Objects},
  author    = {Spinielli, Enrico and Koelle, Rainer and Goens, Quinten},
  year      = {2025},
  institution = {EUROCONTROL}
}
```

*Developed by the Aviation Intelligence Unit at EUROCONTROL.*
