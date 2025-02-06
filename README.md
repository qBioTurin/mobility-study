## INTRODUCTION


## WHAT IS THIS?


## HOW IT WORKS?


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
[1] Baccega, D., Castagno, P., Fernández Anta, A. et al. Enhancing COVID-19 forecasting precision through the integration of compartmental models, machine learning and variants. Sci Rep 14, 19220 (2024). https://doi.org/10.1038/s41598-024-69660-5

## COPYRIGHT AND LICENSE
Copyright _Daniele Baccega, Juan Marcos Ramirez, Jose Aguilar, Antonio Fernández Anta, Carlos Baquero_

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
