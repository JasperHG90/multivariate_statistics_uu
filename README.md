# README

Build the Dockerfile using 

```shell
docker build . -t jhginn/practical_mvsuu
```

run with  

```shell
docker run -e PASSWORD=stats -p 8787:8787 jhginn/practical_mvsuu
```

Go to http://localhost:8787 and log in using:

username: rstudio
password: stats