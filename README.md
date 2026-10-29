# Evangelical Christianity and Political Conservatism

This study uses introductory statistical methods to examine whether evangelical identification and political conservatism have increased since 2014, the years leading up to Trump's first presidential campaign.

**[Read the full analysis report](https://mevere31.github.io/INFO-610---Evangelical-Christianity-and-Political-Conservatism/result/analysis_report.html)**

## Data

This project uses the General Social Survey (GSS), [2014-2024]. The raw data file is not included in this repository. To reproduce the analysis, download it from the GSS Data Explorer (https://gssdataexplorer.norc.org/) or https://gss.norc.org/ and update the file path in the R script. The data dictionary is in `result/`.

## Methods

This study uses data from the General Social Survey (GSS) to examine whether evangelical identity is associated with political conservatism, and whether either has increased from 2014 to 2024. The sample was restricted to White respondents to align with the literature review on the history of Evangelicals in the United States.

The most common Christian identifiers  in the dataset were Baptist, Methodist, Episcopal, Other Denomination, and Non-Denominational. These categories were used to define the "evangelical" indicator in subsequent models and tests. I ran kendall's tau correlation test and ordinal logistic regression model tests. 

## Key Findings

Null Hypothesis 1: Rejected. The ordinal logistic regression shows that identifying as evangelical is a strong predictor of political conservatism. This indicates a robust link between evangelical identity and conservative self-placement.

Null Hypothesis 2: Partially Rejected. The regression results show a small but statistically significant year-over-year increase in conservative self-identification. This suggests a gradual upward trend in political conservatism in the GSS sample. However, the descriptive patterns suggest that there is not a consistent increase in respondents who identify as Evangelical Christian across all denominations.

## Repository Structure

- `result/`: R analysis code, data dictionary, and the exported analysis report (`analysis_report.html`)
- `visualizations/`: final charts from the project
- `poster/`: the final project poster 

## Built With

R (analysis and visualization)
