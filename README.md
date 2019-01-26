[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/d-alex-hughes/jeps-compute/cran-docker)

This repository builds a compute environment, and executes code against data in support of the publication _Persistent Bias Among Local Election Officials: Evidence from the 2016 General Election_.

# Building Compute

The first stage of this builds a compute environment. The sole requirement for this compute environment is a working installation of [Docker](https://www.docker.com).

```
docker run -p 8889:8888 -e JUPYTER_ENABLE_LAB=yes jeps_compute
```

This call should pull the docker image, build that image, and then broadcast the image to the host port at `localhost:8889`. Note that you will have to provide the token from the notebook. 
