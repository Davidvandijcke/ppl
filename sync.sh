#!/bin/bash

aws s3 sync "s3://ipsos-dvd/ppl/data/people_tenure_prepped/" "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/data/in/people_tenure_prepped" --profile ipsos
aws s3 sync "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/data/in/people_tenure_prepped" "s3://ipsos-dvd/ppl/data/people_tenure_prepped/" --profile ipsos
scp -r "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/data/in/people_tenure_prepped_v2" dvdijcke@greatlakes-xfer.arc-ts.umich.edu:/home/dvdijcke/ppl/data/in/

aws s3 sync "s3://ipsos-dvd/ppl/data/people_tenure_prepped_next/" "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/data/in/people_tenure_prepped_next_v2" --profile ipsos
scp -r "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/data/in/people_tenure_prepped_next_v2" dvdijcke@greatlakes-xfer.arc-ts.umich.edu:/home/dvdijcke/ppl/data/in/


aws s3 sync "s3://ipsos-dvd/ppl/data/people_tenure_prepped_next_spells/" "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/data/in/people_tenure_prepped_next_spells" --profile ipsos
scp -r "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/data/in/people_tenure_prepped_next_spells" dvdijcke@greatlakes-xfer.arc-ts.umich.edu:/home/dvdijcke/ppl/data/in/

aws s3 sync "s3://ipsos-dvd/ppl/data/people_tenure_prepped_next_spells_performers/" "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/data/in/people_tenure_prepped_next_spells_performers" --profile ipsos
scp -r "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/data/in/people_tenure_prepped_next_spells_performers" dvdijcke@greatlakes-xfer.arc-ts.umich.edu:/home/dvdijcke/ppl/data/in/


scp -r  "dvdijcke@greatlakes-xfer.arc-ts.umich.edu:/home/dvdijcke/ppl/code" "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/code"


# Steve Redding - Economics of Density (2015)
# Redding -- Economica Commuting Market Access (2022)
# Donaldson Horbeck QJE
# Ask Nielsen?
# Fernandez-Villaverde Supply chain disruption 

