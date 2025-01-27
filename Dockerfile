# Base image https://hub.docker.com/u/danielebaccega
FROM danielebaccega/sybil
LABEL maintainer="Daniele Baccega <daniele.baccega@unito.it>"

## Run the script
CMD Rscript Main.R