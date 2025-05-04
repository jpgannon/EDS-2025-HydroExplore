
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
4. Download tables or graphs as needed.

---


<details>
  <summary>Click to view all app screenshots</summary>

  ![Home Page](images/homepage.png)

  ![Trend Analysis](images/trend_analysis.png)

  ![Monthly Analysis](images/monthly_analysis.png)

  ![Heatmap Analysis](images/heatmap_analysis.png)

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

