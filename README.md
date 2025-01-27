## INTRODUCTION
Government measures can be categorized into two groups: pharmaceutical and non-pharmaceutical. Pharmaceutical interventions (PIs) include medical treatments aimed at preventing, managing, or curing diseases, such as vaccines, antiviral medications, monoclonal antibodies, immunomodulators, and antibiotics. On the other hand, non-pharmaceutical interventions (NPIs) focus on strategies such as social distancing, quarantine and isolation, screening campaigns, travel restrictions, mask-wearing, hygiene measures, remote work, closure of educational institutions and non-essential businesses, limitations on public gatherings, and contact tracing. These interventions are implemented to prevent or control the outbreak of an emergency, like the spread of a disease in a population. In this context, NPIs help to reduce the transmission through changes in behaviours, environments, and community interactions [1].

One of the most immediate and observable effects of NPIs has been their impact on human mobility. Mobility, which refers to the movement of individuals within and between geographic areas, plays a crucial role in the transmission dynamics of infectious diseases. The relationship between NPIs and mobility is complex and multifaceted. On the one hand, stringent measures such as nationwide lock-downs and curfews have led to dramatic declines in mobility, as evidenced by data from mobility tracking tools and transportation statistics [2, 3]. On the other hand, the relaxation of these measures has often been followed by a resurgence in mobility, sometimes correlating with subsequent waves of infections. Moreover, variations in compliance and enforcement, socio-economic factors, and public trust in government policies have influenced the effectiveness of NPIs and the extent of mobility reductions [4, 5].

## WHAT IS THIS?
This study investigates the relationship between government-imposed restrictions, mobility patterns, and the effective reproduction number Rt of COVID-19 (extracted using Sybil [6]). We analyze mobility and restrictions data from major platforms to understand how movement trends respond to policy changes, providing insights into disease transmission dynamics across diverse countries and demographics. Our focus is on behavioral patterns and intervention responses. Specifically, we analyze data from several European countries, including Finland, Norway, Sweden, Belgium, Ireland, the United Kingdom, France, Germany, Austria, Italy, Greece, Spain, and Portugal. Our main contributions are as follows:
- We identify a strong correlation between the considered restrictions and mobility variables, contrasted by a weaker correlation between mobility and infection rates.
- We identify cultural patterns by analyzing how mobility and the R\textsubscript{t} are influenced by both official restrictions and self-imposed limitations (in the absence of formal measures), and variations in social compliance and interaction across different countries.
- Using K-means clustering [7], we examine the distinctions between Northern and Southern European countries in terms of their social compliance with restrictions, while also highlighting a lesser degree of distinction in social interactions. Additionally, in the 'Supplementary Material', we apply another clustering technique, CONNECTOR [8], for the same analysis.
- We apply XGBoost regression models [9] to:
  - Estimate missing mobility data using restriction indicators.
  - Infer missing R\textsubscript{t} values from mobility patterns.


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

[6] Baccega, D., Castagno, P., Fernández Anta, A. et al. Enhancing COVID-19 forecasting precision through the integration of compartmental models, machine learning and variants. Sci Rep 14, 19220 (2024). https://doi.org/10.1038/s41598-024-69660-5

[7] John A Hartigan, Manchek A Wong, et al. “A k-means clustering algorithm”. In: Applied statistics 28.1 (1979), pp. 100–108. doi: https://doi.org/10.2307/2346830.

[8] Simone Pernice et al. “CONNECTOR, fitting and clustering of longitudinal data to reveal a new risk stratification system”. In: Bioinformatics 39.5 (2023), btad201. doi: https://doi.org/10.1093/bioinformatics/btad201.

[9] Tianqi Chen and Carlos Guestrin. “Xgboost: A scalable tree boosting system”. In: Proceedings of the 22nd acm sigkdd international conference on knowledge discovery and data
mining. 2016, pp. 785–794. doi: https://doi.org/10.1145/2939672.2939785.

## COPYRIGHT AND LICENSE
![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
