
# **HydroExplore Shiny App**  
*Your Gateway to Exploring Watershed Hydrology at Hubbard Brook*

---

## Purpose & Goals

The **HydroExplore** app was developed to help analyze **precipitation and streamflow trends** in the **Hubbard Brook Experimental Forest**. Users can:

- Detect high and low flow events, and snowfall changes over time.
- Easily explore data across all nine watersheds at Hubbard Brook.
- Gain insights whether you're a scientist, student, or just curious—no affiliation required!

A core goal of the app is to highlight watershed differences, providing a simple yet powerful platform for hydrological exploration.

---

## Description & Overview

HydroExplore consists of six interactive pages, each tailored to a unique way of exploring hydrology data across time:

---

### Trend Analysis

**Key Features:**
- Compare multiple watersheds on a single graph.
- Analyze baseflow and precipitation trends.
- Interactive date slider with automatically updating statistics.

---

### Monthly Analysis

**Key Features:**
- Compare the same month across multiple years.
- Add trendlines to visualize long-term changes.
- View streamflow-to-precipitation ratios (Q/P lines).

---

### Heatmap Analysis

**Key Features:**
- Identify the largest rainfall or streamflow events over a chosen time range.
- Customize the number of high-flow days shown.
- See the temporal distribution of extreme events—zoom in on years or span decades.

---

### Yearly Analysis

**Key Features:**
- View precipitation, streamflow, and snow data by water year.
- Focused filters allow comparisons across specific years.
- Explore how seasonal patterns and annual trends shift over time.

---

### See Tables

**Key Features:**
- Download pre-joined datasets used throughout the app.
- Designed for easy use in future analysis or custom research.

---

### User Guide

**Key Features:**
- A quick primer to help new users understand each tab.
- Get oriented and start exploring in minutes.

---

## Key Functions & Features

Below are several standout components of HydroExplore that make it especially effective for hydrological data analysis:

---

### Baseflow Graphs

- Calculated using the **EcohydRology** package.
- A consistent, filter-based method to identify baseflow across watersheds.
- Helps isolate precipitation-driven peaks.

---

### Q/P Lines

- Shows streamflow as a proportion of precipitation.
- Implemented in the Monthly Analysis tab to avoid divide-by-zero errors.
- Offers insights into evapotranspiration and groundwater recharge.

---

### Trendlines

- Modeled using **ARIMA**, rather than simple linear fits.
- Captures time series dynamics and reveals subtle changes over time.

---

### Heatmap Analysis

- User-driven filters: date range, variable, number of days.
- Uses value ranking and filtering to spotlight peak events.
- Great for zooming in on extreme hydrologic moments.

---

### Water Year Analysis

- Adjusted year range (October to September) for hydrologic accuracy.
- Especially useful for snow data—view full snowpack and melt cycles clearly.
- Easily compare multiple water years side-by-side.

---

## How It Works

HydroExplore uses data collected from 1956 to 2023 at Hubbard Brook:

- Precipitation
- Streamflow
- Snow depth and snow water equivalent (where available)

Each tab presents the data through a different lens, enabling users to interact, filter, and download based on their goals.

---

## How to Use It

Start on the **User Guide** tab to get oriented. Then:

1. Choose your variable(s) and watershed(s).
2. Select a date range.
3. Dive into visualizations.
4. Download tables as needed.

---


<details>
  <summary>Click to view what the app should look like</summary>

  ![Trend Analysis](https://github.com/user-attachments/assets/8e48023f-7e8f-41bc-a5ad-239538f1003b)

  ![Monthly Analysis](https://github.com/user-attachments/assets/3b6b10a2-8af8-4086-bcf7-0feb81717a8a)

  ![Heatmap Analysis](https://github.com/user-attachments/assets/d92788e4-a099-4dfa-b408-834719d2b637)

  ![Yearly Analysis](https://github.com/user-attachments/assets/0f6d9c0c-25ae-4cb1-a95d-73b9ff5975de)

</details>

## Known Issues

- **Snow data is incomplete** for several watersheds:
  - Watersheds 2, 5, 9: No snow data.
  - Watersheds 3, 4, 7: Missing snow data post-1980.
- This issue originates from the Hubbard Brook dataset itself, not the app.

Currently, this is the only known issue.

---

## Contact Info / Authors

**HydroExplore** was developed by:

- Michael Dunlap  
- Humzah Naved  
- Owen Egan

For questions, feedback, or collaboration inquiries, please contact:  
**JP Gannon** — jpgannon@vt.edu


# **HydroExplore Shiny App**  
*Explore Watershed Hydrology Like Never Before*

---

## What is HydroExplore?

**HydroExplore** is a user-friendly Shiny app designed to let researchers, students, and anyone curious about hydrology explore trends in **precipitation, streamflow, and snow** across the nine watersheds at the **Hubbard Brook Experimental Forest**. With data stretching from **1956 to 2023**, HydroExplore allows both focused and broad exploration—across days, months, or entire water years.

---

## Why We Built This App

While much of Hubbard Brook's data is publicly available, it often requires specialized skills to work with. Our goal was to make a **visually rich, interactive, and accessible tool** for exploring key hydrological patterns—whether you want to examine extreme events, compare watersheds, or explore long-term trends.

---

## What the App Looks Like

<details>
  <summary>Click to view what the app should look like</summary>

  ![Trend Analysis](https://github.com/user-attachments/assets/8e48023f-7e8f-41bc-a5ad-239538f1003b)

  ![Monthly Analysis](https://github.com/user-attachments/assets/3b6b10a2-8af8-4086-bcf7-0feb81717a8a)

  ![Heatmap Analysis](https://github.com/user-attachments/assets/d92788e4-a099-4dfa-b408-834719d2b637)

  ![Yearly Analysis](https://github.com/user-attachments/assets/0f6d9c0c-25ae-4cb1-a95d-73b9ff5975de)

</details>

---

## Key Features

### Multi-Page Navigation
The app is divided into **six pages**, each designed for a different analytical perspective:

---

###  Trend Analysis

- Compare **baseflow** and **precipitation** across multiple watersheds.
- Use sliders to select custom time ranges.
- Summary statistics automatically update for selected time windows.

---

###  Monthly Analysis

- Explore how a specific month (e.g., all Januarys) has changed across years.
- Trendlines and Q/P (streamflow/precipitation) ratios provide context for changes in evapotranspiration and baseflow recharge.

---

###  Heatmap Analysis

- Identify and rank the **largest rainfall or flow events** over custom time ranges.
- Explore up to 70 years of data to find extreme days.
- Useful for understanding the **temporal concentration of events**.

---

###  Yearly (Water Year) Analysis

- Compare hydrology across full **water years (Oct–Sept)**.
- Especially useful for understanding snowmelt and seasonal shifts.
- Easily filter by year and visualize trends side-by-side.

---

###  See Tables

- Download merged tables used in the app.
- Built for reusability—no need to prep or clean the data yourself.

---

###  User Guide

- Brief, helpful intro for each page—perfect for new users.

---

## Notable Functionality

### Baseflow Estimation

- Uses the **EcohydRology** package for a consistent, automated approach.
- Allows clearer separation of precipitation-driven peaks from groundwater flow.

### Q/P Lines

- Monthly ratio of streamflow to precipitation, providing insight into water balance.
- Helps identify **seasonal efficiency** of watersheds.

### ARIMA Trendlines

- Unlike basic linear fits, ARIMA accounts for **time series dependencies**.
- Makes long-term changes visually obvious and statistically sound.

### Flexible Heatmap Filtering

- Choose time windows and number of events.
- Enables both **broad historical exploration** and **focused event analysis**.

### Water Year Support

- App uses **hydrologic years** to better handle snow data.
- Avoids cutting snow seasons in half with calendar years.

---

## Known Data Limitations

Snow data is incomplete for some watersheds:

- **No snow data:** Watersheds 2, 5, 9  
- **Post-1980 missing snow:** Watersheds 3, 4, 7

These gaps reflect measurement limitations from the original Hubbard Brook datasets.

---

## Developed By

**Michael Dunlap**  
**Humzah Naved**  
**Owen Egan**

For questions, feedback, or collaborations, contact:  
**JP Gannon** — jpgannon@vt.edu

---

## Ready to Explore?

Open the app, choose your watershed, pick your variable, and dive in. Whether you're chasing floods, studying droughts, or tracking snowmelt, HydroExplore helps you **see the forest—and the data—for the trees**.

