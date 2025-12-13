# 🏎️ F1 Telemetry Hub: Addressing Complexity

### [Live Demo: Click Here to Launch App](https://kishorebalajisivaprakash-tcd.shinyapps.io/f1-dashboard/)

**Tech Stack:** R, Shiny, Plotly, Tidyverse, bslib

### Dashboard Preview
[<img width="1900" height="1076" alt="image" src="https://github.com/user-attachments/assets/2e587c33-375e-435b-9213-5b6c7009dabf" />](https://github.com/kishorebalaji55/Data-Visualisation-F1-Dashboard-using-R-Shiny/blob/main/f1-dashboard.png)

## 📖 Overview
Formula 1 is often described as a data problem disguised as a sport. Standard broadcast graphics often focus on the *result* (who won?) rather than the *process* (how did they win?).

The **F1 Telemetry Hub** is an interactive dashboard designed to disentangle the multi-dimensional complexity of Grand Prix racing. Using the Ergast F1 dataset (1950–2023), it visualizes the relationships between **Qualifying Performance**, **Race Pace**, and **Strategic Interventions** (Pit Stops).

## 🚀 Key Features

The application is divided into four analytical layers, moving from micro-analysis to macro-trends:

### 1. 🏁 Race Day Analysis (Micro)
* **Lap Trace (Bump Chart):** Visualizes position changes lap-by-lap, allowing users to trace specific battles through the "spaghetti" of race data.
* **Position Heatmap:** Uses color intensity to highlight "recovery drives" (moving from the back to the front) instantly.
* **Pit Stop Strategy:** A scatter plot linking lap number to pit stop duration, identifying undercut/overcut strategies.
* **Pace Consistency:** Violin plots showing the distribution of lap times to identify driver consistency vs. outliers.

### 2. 📈 Multi-Year Trends (Meso)
* **Constructor Supremacy:** Tracks total team points over decades to visualize eras of dominance (e.g., Ferrari in 2000s, Mercedes in 2010s).
* **Win Share:** Interactive donut chart showing the percentage of wins per team for a selected era.

### 3. 🧠 Strategy Correlation (Macro)
* **Strategy Impact Matrix:** A high-dimensional scatter plot linking:
    * *X-Axis:* Grid Position (Potential)
    * *Y-Axis:* Finishing Position (Result)
    * *Size:* Pit Stop Count (Strategy)
    * *Insight:* Bubbles above the diagonal line indicate drivers who outperformed their car's pace.
* **Overtaking Efficiency:** Bar chart ranking teams by average positions gained per race.

### 4. 🎬 Animated Insights (Novelty)
* **Championship Bar Race:** A dynamic, animated bar chart that replays the season's points battle round-by-round.
* **Grid vs. Result Animation:** Animates the Strategy Matrix over the course of a season to show evolving team performance.

## 🛠️ Installation & Usage

### Prerequisites
* R (Version 4.0 or higher)
* RStudio

### Setup
1.  **Clone the repository:**
    ```bash
    git clone https://github.com/kishorebalaji55/F1-Telemetry-Hub.git
    ```
2.  **Install dependencies:**
    Open RStudio and run the following command in the console:
    ```r
    install.packages(c("shiny", "shinydashboard", "tidyverse", "plotly", "shinyWidgets", "bslib"))
    ```
3.  **Data Setup:**
    Ensure the following CSV files are in the root directory (same folder as `app.R`):
    * `races.csv`
    * `drivers.csv`
    * `constructors.csv`
    * `results.csv`
    * `lap_times.csv`
    * `pit_stops.csv`
    *(Data sourced from the [Ergast API via Kaggle](https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020))*

4.  **Run the App:**
    Open `app.R` in RStudio and click the **"Run App"** button (or run `shiny::runApp()`).

## 📜 Credits
* **Data Source:** Ergast Developer API (Historical F1 Data)
* **Visualization Libraries:** Plotly (Interactive charts), ggplot2 (Static base)
* **Theme:** `bslib` (Cyborg Dark Mode)
