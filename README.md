# Bowen Island Biodiversity Conservation Planning

A comprehensive biodiversity conservation planning toolkit for Bowen Island, BC, combining spatial analysis, data processing pipelines, and interactive web visualization to support evidence-based community planning.

**Live Web Application**: <https://palenlab.shinyapps.io/Bowen-Biodiversity-WebApp/>
**Documentation Site**: <https://palen-lab.github.io/bowen.biodiversity.webapp/>

## Overview

This repository supports Bowen Island's community planning process by integrating multiple biodiversity data sources, conservation prioritization analysis, and threat assessments into accessible tools for decision-makers and residents.

### Project Components

The repository consists of two main components:

1. **[data-etl/](data-etl/)** - Data processing pipeline
   - Processes 341 species distribution models, filtering to 193 species with local observations
   - Integrates ~50 habitat layers from TEM, SEI, and field surveys
   - Runs Zonation5 conservation prioritization analysis
   - Generates threat assessments (development, wildfire, climate)
   - Produces outputs for web application and documentation
   - **See [data-etl/README.md](data-etl/README.md) for setup and execution instructions**

2. **[web-application/](web-application/)** - Interactive Shiny web app
   - Explore species distributions and habitat maps
   - View conservation priority areas
   - Analyze development threats and protected area gaps
   - Overlay multiple data layers for custom analysis
   - **See [web-application/README.md](web-application/README.md) for development and deployment**

### Key Outputs

- **Web Application**: Interactive mapping tool for exploring biodiversity data
- **Conservation Priority Map**: Zonation5 analysis identifying top 10-30% priority areas
- **Data Atlas**: Comprehensive documentation of all spatial layers
- **Technical Reports**: Methodology and findings documentation
- **Processed Datasets**: 193 species SDMs, 50+ habitat layers, threat assessments

## Repository Structure

```
bowen.biodiversity.webapp/
├── data-etl/                # Data processing pipeline
│   ├── data-raw/           # Raw data and ETL scripts (numbered by phase)
│   ├── analyses/           # Analysis documentation (Quarto website)
│   ├── output-data/        # Processed datasets for web app
│   ├── output-figures/     # Generated maps and visualizations
│   ├── output-documents/   # Final reports and deliverables
│   ├── R/                  # Reusable R functions
│   └── README.md           # Pipeline documentation
│
├── web-application/        # Shiny web application
│   ├── R/                  # Application modules and server logic
│   ├── inst/               # Data files and web assets
│   ├── app.R               # Application entry point
│   └── README.md           # Application documentation
│
├── .github/                # GitHub workflows and issue templates
├── LICENSE                 # MIT License
└── README.md               # This file
```

## Citation

If using this work, please cite:

```
Matsushiba, J., Tylo, M., Palen, W., & Sisk, T. (2025).
Bowen Island Biodiversity Conservation Planning: Data Pipeline and Web Application.
Bowen Island Municipality. https://github.com/Palen-Lab/bowen.biodiversity.webapp
```

For the web application specifically:
```
Matsushiba, J., Tylo, M., Palen, W., & Sisk, T. (2025).
Bowen Island Biodiversity Web Application.
https://palenlab.shinyapps.io/Bowen-Biodiversity-WebApp/
```

## License

MIT License - See [LICENSE](LICENSE) for details

## Support

For questions, bug reports, or feature requests:
- **Issues**: <https://github.com/Palen-Lab/bowen.biodiversity.webapp/issues>
- **Contact**: Jay Matsushiba (hello@jmatsushiba.com)
- **Principal Investigator**: Wendy Palen, Simon Fraser University

## Acknowledgments

This work was conducted in partnership with Bowen Island Conservancy and supported by the Palen Lab at Simon Fraser University. We acknowledge that Bowen Island is located on the traditional and unceded territories of the Coast Salish peoples, including the Squamish Nation.
