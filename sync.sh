#!/bin/bash

aws s3 sync "s3://ipsos-dvd/ppl/data/people_tenure_prepped/" "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/data/in/people_tenure_prepped" --profile ipsos
scp -r "/Users/davidvandijcke/Dropbox (University of Michigan)/ppl/data/in/people_tenure_prepped" dvdijcke@greatlakes-xfer.arc-ts.umich.edu:/home/dvdijcke/ppl/data/in/people_tenure_prepped/

