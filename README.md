## INTRODUCTION
Government measures can be categorized into two groups: pharmaceutical and non-pharmaceutical. Pharmaceutical interventions (PIs) include medical treatments aimed at preventing, managing, or curing diseases, such as vaccines, antiviral medications, monoclonal antibodies, immunomodulators, and antibiotics. On the other hand, non-pharmaceutical interventions (NPIs) focus on strategies such as social distancing, quarantine and isolation, screening campaigns, travel restrictions, mask-wearing, hygiene measures, remote work, closure of educational institutions and non-essential businesses, limitations on public gatherings, and contact tracing. These interventions are implemented to prevent or control the outbreak of an emergency, like the spread of a disease in a population. In this context, NPIs help to reduce the transmission through changes in behaviours, environments, and community interactions [1].

One of the most immediate and observable effects of NPIs has been their impact on human mobility. Mobility, which refers to the movement of individuals within and between geographic areas, plays a crucial role in the transmission dynamics of infectious diseases. The relationship between NPIs and mobility is complex and multifaceted. On the one hand, stringent measures such as nationwide lock-downs and curfews have led to dramatic declines in mobility, as evidenced by data from mobility tracking tools and transportation statistics [2, 3]. On the other hand, the relaxation of these measures has often been followed by a resurgence in mobility, sometimes correlating with subsequent waves of infections. Moreover, variations in compliance and enforcement, socio-economic factors, and public trust in government policies have influenced the effectiveness of NPIs and the extent of mobility reductions [4, 5].

Social compliance, shaped by trust in institutions and adherence to societal norms, rules, and regulations, played a crucial role in the effectiveness of non-pharmaceutical interventions [6, 7]. In countries where individuals more readily adhered to collective public health goals, NPIs proved to be more effective in reducing disease spread. Cultural factors, such as societal norms, values, and attitudes toward government authority, further influenced these outcomes. For example, in East Asian countries, where there is a strong tradition of community responsibility and compliance with public health measures, NPIs were more effective in reducing mobility compared to Western nations, where individual freedoms are more heavily emphasized [8].
Additionally, social interactions, representing the dynamic exchanges between individuals or groups that collectively shape relationships, cultural norms, and societal structures, contributed significantly to the effectiveness of these measures---where social networks facilitated compliance or, conversely, hindered adherence in regions with less social cohesion. Economic disparities also played a role, as lower-income countries and communities often faced greater difficulties in adhering to lock-down measures due to the necessity of continuing economic activities. These variations underscore the importance of context-specific strategies that consider both social compliance and the socio-economic landscape in managing mobility and Rt during a pandemic [9].

## WHAT IS THIS?
This study [10] investigates the relationship between government-imposed restrictions, mobility patterns, and the effective reproduction number Rt of COVID-19 (extracted using Sybil [11]). We analyze mobility and restrictions data from major platforms to understand how movement trends respond to policy changes, providing insights into disease transmission dynamics across diverse countries and demographics. Our focus is on behavioral patterns and intervention responses. Specifically, we analyze data from several European countries, including Finland, Norway, Sweden, Belgium, Ireland, the United Kingdom, France, Germany, Austria, Italy, Greece, Spain, and Portugal. Our main contributions are as follows:
- We identify a strong correlation between the considered restrictions and mobility variables, contrasted by a weaker correlation between mobility and infection rates.
- We identify cultural patterns by analyzing how mobility and the Rt are influenced by both official restrictions and self-imposed limitations (in the absence of formal measures), and variations in social compliance and interaction across different countries.
- Using K-means clustering [12], we examine the distinctions between Northern and Southern European countries in terms of their social compliance with restrictions, while also highlighting a lesser degree of distinction in social interactions. Additionally, in the 'Supplementary Material', we apply another clustering technique, CONNECTOR [13], for the same analysis.
- We apply XGBoost regression models [14] to:
  - Estimate missing mobility data using restriction indicators.
  - Infer missing Rt values from mobility patterns.


## REQUIREMENTS
You need to have docker installed on your machine, for more info see this document: https://docs.docker.com/engine/installation/.

Ensure your user has the rights to run docker (without the use of sudo). To create the docker group and add your user:

Create the docker group.
```
  $ sudo groupadd docker
 ```
 
Add your user to the docker group.
```
  $ sudo usermod -aG docker $USER
```

Log out and log back in so that your group membership is re-evaluated.

## HOW TO REPRODUCE THE RESULTS
To reproduce the results presented in the paper run:
```
./reproduce.sh
```

## REFERENCES
[1] Nicola Perra. “Non-pharmaceutical interventions during the COVID-19 pandemic: A review”. In: Physics Reports 913 (2021), pp. 1–52. doi: https://doi.org/10.1016/j.physrep.2021.02.001.

[2] Tao Hu et al. “Human mobility data in the COVID-19 pandemic: characteristics, applications, and challenges”. In: International Journal of Digital Earth 14.9 (2021), pp. 1126–1147. doi: https://doi.org/10.1080/17538947.2021.1952324.

[3] Laura Alessandretti. “What human mobility data tell us about COVID-19 spread”. In: Nature Reviews Physics 4.1 (2022), pp. 12–13. doi: https://doi.org/10.1038/s42254-021-00407-1.

[4] Michael O Adeniyi et al. “Assessing the impact of public compliance on the use of non-pharmaceutical intervention with cost-effectiveness analysis on the transmission dynamics of COVID-19: Insight from mathematical modeling”. In: Modeling, control and drug development for COVID-19 outbreak prevention (2022), pp. 579–618. doi: https://doi.org/10.1007/978-3-030-72834-2_17.

[5] Jin Young Chung, Choong-Ki Lee, and Yae-Na Park. “Trust in social non-pharmaceutical interventions and travel intention during a pandemic”. In: Journal of Vacation Marketing
27.4 (2021), pp. 437–448. doi: https://doi.org/10.1177/13567667211009584.

[6] Marcus Painter and Tian Qiu. “Political beliefs affect compliance with covid-19 social distancing orders“. In: Covid Economics 4.April (2020), pp. 103–23. DOI: https://doi.org/10.1016/j.jebo.2021.03.019.

[7] Benjamin Van Rooij et al. “Compliance with COVID-19 mitigation measures in the United States“. In: Amsterdam law school research paper 2020-21 (2020). DOI: https://dx.doi.org/10.2139/ssrn.3582626.

[8] Yu Liu, Richard B Saltman, and Ming-Jui Yeh. “From bureaucratic administration to effective inter vention: Comparing early governmental responses to the COVID-19 virus across East Asian and western health systems“. In: Health Services Management Research 36.3 (2023), pp. 193–204. DOI: https://doi.org/10.1177/09514848221139680.

[9] Zahra Mohammadi, Monica Gabriela Cojocaru, and Edward Wolfgang Thommes. “Human behaviour, NPI and mobility reduction effects on COVID-19 transmission in different countries of the world“. In: BMC Public Health 22.1 (2022), p. 1594. DOI: https://doi.org/10.1186/s12889-022-13921-3.

[10] Baccega, D., Aguilar, J., Baquero, C., Fernandez Anta, A., & Ramirez, J. M. (2025). “Social Compliance with NPIs, Mobility Patterns, and Reproduction Number: Lessons from COVID-19 in Europe“. medRxiv, 2025-01.

[11] Baccega, D., Castagno, P., Fernández Anta, A. et al. “Enhancing COVID-19 forecasting precision through the integration of compartmental models, machine learning and variants“. Sci Rep 14, 19220 (2024). https://doi.org/10.1038/s41598-024-69660-5

[12] John A Hartigan, Manchek A Wong, et al. “A k-means clustering algorithm”. In: Applied statistics 28.1 (1979), pp. 100–108. doi: https://doi.org/10.2307/2346830.

[13] Simone Pernice et al. “CONNECTOR, fitting and clustering of longitudinal data to reveal a new risk stratification system”. In: Bioinformatics 39.5 (2023), btad201. doi: https://doi.org/10.1093/bioinformatics/btad201.

[14] Tianqi Chen and Carlos Guestrin. “Xgboost: A scalable tree boosting system”. In: Proceedings of the 22nd acm sigkdd international conference on knowledge discovery and data
mining. 2016, pp. 785–794. doi: https://doi.org/10.1145/2939672.2939785.

## COPYRIGHT AND LICENSE
Copyright _Daniele Baccega, Jose Aguilar, Carlos Baquero, Antonio Fernández Anta, Juan Marcos Ramirez_

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
