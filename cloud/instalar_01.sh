#Este script instala en ambiente/ Google Cloud
#Librerias y utilidades basicas de  Ubuntu 21.04
#Lenguajes   Julia, Python y R
#Librerias de los lenguajes para hacer ciencia de datos  (no deep learning, no GPU )
#Entornos productivos   RStudio, Jupyter Lab
#conectividad a los Storage Buckets de  Google Cloud

#autor            Gustavo Denicolay
#email            gustavo.denicolay@gmail.com
#fecha creacion   2017-07-20
#fecha revision   2021-09-21 17:31
#Known BUGS       
#ToDo Optims      


mkdir  -p  ~/install
mkdir  -p  ~/log


#instalo Google Cloud SDK
#Documentacion  https://cloud.google.com/sdk/docs/install#deb
sudo apt-get update
sudo apt-get --yes install apt-transport-https ca-certificates gnupg
echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -
sudo apt-get update && sudo apt-get --yes install google-cloud-sdk

#instalo paquetes que van a hacer falta para R, Python y Julia
sudo apt-get update  && sudo dpkg --add-architecture  i386

sudo apt-get --yes install  git-core  zip  unzip  sysstat
sudo apt-get --yes install  software-properties-common

sudo apt-get --yes install  libssl-dev    \
     libcurl4-openssl-dev  libxml2-dev    \
     libgeos-dev  libproj-dev             \
     libgdal-dev  librsvg2-dev            \
     ocl-icd-opencl-dev  libmagick++-dev  \
     libv8-dev  libsodium-dev             \
     libharfbuzz-dev  libfribidi-dev      \
     pandoc texlive  texlive-xetex        \
     texlive-fonts-recommended            \
     texlive-latex-recommended            \
     cmake  gdebi  curl  sshpass  nano    \
     vim  htop  iotop                     \
     cron  tmux

#------------------------------------------------------------------------------

#instalo Google Cloud Fuse  para poder ver el bucket  Version:  0.36.0 | Released:2021-08-06
#Documentacion https://cloud.google.com/storage/docs/gcs-fuse?hl=en-419
gcsfusever="0.36.0"
gcsfusepack="gcsfuse_"$gcsfusever"_amd64.deb"
cd
curl -L -O "https://github.com/GoogleCloudPlatform/gcsfuse/releases/download/v$gcsfusever/$gcsfusepack"
sudo dpkg --install $gcsfusepack
rm   ~/$gcsfusepack


#Preparo para que puedan haber 4 buckets al mismo tiempo
mkdir  -p  ~/buckets
mkdir  -p  ~/buckets/b1
mkdir  -p  ~/buckets/b2
mkdir  -p  ~/buckets/b3
mkdir  -p  ~/buckets/b4

cat > /home/$USER/install/linkear_buckets.sh <<FILE
#!/bin/bash

/snap/bin/gsutil ls | sed -r 's/gs:\/\///' | sed 's/.$//'       \
|  sed 's/^/\/usr\/bin\/gcsfuse --implicit-dirs --file-mode 777 --dir-mode 777    /'    \
|  sed 's/$/ \/home\/$USER\/buckets\/b/'    \
|  awk '{ print \$0, NR}'   \
|  sed -E 's/.(.)$/\1/'   >  /home/$USER/install/linkear_buckets2.sh

chmod +x  /home/$USER/install/linkear_buckets2.sh
/home/$USER/install/linkear_buckets2.sh

FILE

chmod +x /home/$USER/install/*.sh
chmod +x /home/$USER/*.sh


cat > /home/$USER/install/buckets.service  <<FILE
[Unit]
Description=buckets

[Service]
Type=forking
ExecStart=/home/$USER/install/linkear_buckets.sh
WorkingDirectory=/home/$USER/
User=$USER
Group=$USER

[Install]
WantedBy=default.target
FILE
sudo  cp   /home/$USER/install/buckets.service   /etc/systemd/system/

sudo systemctl daemon-reload


sudo systemctl enable /etc/systemd/system/buckets.service
sudo systemctl start  buckets

#systemctl status buckets

mkdir  -p  ~/buckets/b1/datasetsOri
mkdir  -p  ~/buckets/b1/datasets
mkdir  -p  ~/buckets/b1/work
mkdir  -p  ~/buckets/b1/kaggle
mkdir  -p  ~/buckets/b1/exp
mkdir  -p  ~/buckets/b1/modelitos
mkdir  -p  ~/buckets/b1/log


#------------------------------------------------------------------------------
#instalo datasets  UBA 2021,  Competencia 1
#Mas adelante, se tendra que instalar el dataset de la Competencia 2

cat > /home/$USER/install/instalo_datasets.sh <<FILE
#!/bin/bash

mkdir  -p  ~/buckets/b1/datasetsOri
mkdir  -p  ~/buckets/b1/datasets
mkdir  -p  ~/buckets/b1/work
mkdir  -p  ~/buckets/b1/kaggle
mkdir  -p  ~/buckets/b1/exp
mkdir  -p  ~/buckets/b1/modelitos
mkdir  -p  ~/buckets/b1/log

cd  ~/buckets/b1/datasetsOri
wget  https://storage.googleapis.com/dmeyf/datasetsOri/paquete_premium_202009.csv
wget  https://storage.googleapis.com/dmeyf/datasetsOri/paquete_premium_202011.csv

gzip -k paquete_premium_202009.csv
gzip -k paquete_premium_202011.csv
cd
FILE

chmod +x ~/install/*.sh
~/install/instalo_datasets.sh
cd

#------------------------------------------------------------------------------
#instalar  R   version: 4.1.1 | released: 2021-08-10
#Documentacion  https://cran.r-project.org/bin/linux/ubuntu/#install-r

sudo apt-get  --yes  update -qq
sudo apt-get  --yes  install --no-install-recommends software-properties-common dirmngr
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc

sudo add-apt-repository --yes "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt-get  --yes  install  r-base  r-base-dev


#Instalo RStudio Server    Version:  1.4.1717 | Released:  2021-06-01-----------
#Documentacion  https://rstudio.com/products/rstudio/download-server/debian-ubuntu/
cd
wget https://download2.rstudio.org/server/bionic/amd64/rstudio-server-1.4.1717-amd64.deb
sudo gdebi --non-interactive  rstudio-server-1.4.1717-amd64.deb
rm   rstudio-server-1.4.1717-amd64.deb


#cambio el puerto del Rstudio Server al 80 para que se pueda acceder en universidades
#Documentacion  https://support.rstudio.com/hc/en-us/articles/200552316-Configuring-the-Server
echo "www-port=80" | sudo tee -a /etc/rstudio/rserver.conf
sudo rstudio-server restart

#------------------------------------------------------------------------------

R_LIBS_USER=/home/$USER/.local/lib/R/site-library
mkdir  -p $R_LIBS_USER

cat > /home/$USER/.Renviron  <<FILE
R_LIBS_USER=$R_LIBS_USER
FILE


#Primera instalacion de paquetes de R
cat > /home/$USER/install/instalar_paquetes_1.r  <<FILE
options(repos = c("https://cloud.r-project.org/"))
options(Ncpus = 4)

install.packages( "data.table",  dependencies= TRUE, lib="/home/$USER/.local/lib/R/site-library")
install.packages( "devtools",  dependencies= TRUE, lib="/home/$USER/.local/lib/R/site-library")

library( "devtools" )
devtools::install_github("IRkernel/IRkernel")

quit( save="no" )
FILE
Rscript --verbose  /home/$USER/install/instalar_paquetes_1.r


#Segunda instalacion de paquetes de R
cat > /home/$USER/install/instalar_paquetes_2.r  <<FILE
options(repos = c("https://cloud.r-project.org/"))
options(Ncpus = 4)

paq1 <- c("yaml", "rlist")
paq2 <- c("magrittr", "stringi", "curl", "Rcpp", "Matrix", "glm2")
paq3 <- c("ROCR", "MASS", "openssl", "roxygen2")
paq4 <- c("rsvg", "DiagrammeRsvg", "DiagrammeR")
paq5 <- c("DiceKriging",  "mlrMBO", "ParBayesianOptimization" )
paq6 <- c("rpart", "rpart.plot", "ranger", "randomForest", "xgboost" )
paq7 <- c("dplyr", "caret", "tidyr", "shiny" )
paq8 <- c("SHAPforxgboost", "shapr" )
paq9 <- c("iml")

paq <-  c( paq1, paq2, paq3, paq4, paq5, paq6, paq7, paq8, paq9 )

install.packages( paq,  dependencies= TRUE, lib="/home/$USER/.local/lib/R/site-library" )

library( "devtools" )
install_github( "AppliedDataSciencePartners/xgboostExplainer")
install_github( "NorskRegnesentral/shapr", dependencies = TRUE)

devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.26.1/catboost-R-Linux-0.26.1.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))

quit( save="no" )
FILE

Rscript --verbose  /home/$USER/install/instalar_paquetes_2.r



#xgboost instalo la ultima version de desarrollo de XGBoost que no esta disponible en CRAN R (para histogramas)
#Documentacion  https://xgboost.readthedocs.io/en/latest/build.html
cd  &&  rm -rf  xgboost
git clone --recursive https://github.com/dmlc/xgboost
cd xgboost
git submodule init
git submodule update
cd R-package
R CMD INSTALL .
cd  &&  rm -rf  xgboost


#LightGBM instalo ya que no esta disponible en CRAN R
#Documentacion  https://lightgbm.readthedocs.io/en/latest/Installation-Guide.html#linux
cd  &&  rm -rf  LightGBM
git clone --recursive https://github.com/Microsoft/LightGBM
cd LightGBM
Rscript ./build_r.R
cd  &&  rm -rf  LightGBM


#Tercera instalacion de paquetes de R
#era necesario tener XGBoost y LightGBM
cat > /home/$USER/install/instalar_paquetes_3.r  <<FILE
options(repos = c("https://cloud.r-project.org/"))
options(Ncpus = 4)

library( 'devtools' )
install_github("lantanacamara/lightgbmExplainer")

quit( save="no" )
FILE

Rscript --verbose  /home/$USER/install/instalar_paquetes_3.r


#------------------------------------------------------------------------------
#Instalo Julia   version: 1.6.2  | Release Date: 2021-07-14 ---------------
#Documentacion  https://julialang.org/downloads/platform/#linux_and_freebsd
sudo apt-get update

#Instalo la ultima version estable de Julia
#version 1.6.2 Release Date : 2021-07-14
juliatar="julia-1.6.2-linux-x86_64.tar.gz"
cd
wget  https://julialang-s3.julialang.org/bin/linux/x64/1.6/$juliatar
tar   -xvzf  $juliatar
sudo  mv  julia-1.6.2/    /opt/
sudo  ln -s  /opt/julia-1.6.2/bin/julia    /usr/local/bin/julia
rm    $juliatar


#genero script con paquetes a instalar de Julia
#Documentacion  https://datatofish.com/install-package-julia/
cat > /home/$USER/install/instalar_paquetes_julia_1.jl <<FILE
using Pkg
Pkg.add("CSV")
Pkg.add("DataFrames")
Pkg.add("DataTables")
Pkg.add("Distributions")
Pkg.add("PooledArrays")
Pkg.add("JuliaDB")
Pkg.add("HTTP")
Pkg.add("XGBoost")
Pkg.add("DecisionTree")
Pkg.add("LightGBM")
Pkg.add("ROCAnalysis")
Pkg.add("BayesianOptimization")
Pkg.add("GaussianProcesses")
Pkg.add("Distributions")
FILE

chmod  +x    /home/$USER/install/instalar_paquetes_julia_1.jl
julia  /home/$USER/install/instalar_paquetes_julia_1.jl


#------------------------------------------------------------------------------
#Instalo Python ---------------------------------------------------------------
#Documentacion  https://docs.python-guide.org/starting/install3/linux/

export PATH="$PATH:/home/$USER/.local/bin"
echo  "export PATH=/home/\$USER/.local/bin:\$PATH"  >>  ~/.bashrc 
source ~/.bashrc 

sudo apt-get update
sudo apt-get --yes install   python3  python3-pip  python3-dev  ipython3


#instalo paquetes de Python
pip3 install  --user  datatable  \
                      Pandas  Numpy  Matplotlib  fastparquet  \
                      pyarrow  tables  plotly  seaborn xlrd   \
                      scrapy  SciPy  wheel  testresources

pip3 install  --user  XGBoost  LightGBM  HyperOpt
pip3 install  --user  TensorFlow  Theano  Keras

#librerias puntuales
pip3 install  --user  kaggle  zulip  pika  gdown  nbconvert[webpdf]  nb_pdf_template

#------------------------------------------------------------------------------
#instalo  Jupyter Lab ---------------------------------------------------------
#Documentacion  https://jupyterlab.readthedocs.io/en/stable/getting_started/installation.html

cd
git clone https://github.com/pygments/pygments
cd  pygments
python3  setup.py install  --user
cd
sudo rm -rf ~/pygments

pip3  install --user  jupyter  jupyterlab


curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.38.0/install.sh | bash
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
nvm install --lts

jupyter labextension install \
   @jupyterlab/toc           \
   jupyterlab-chart-editor   \
   jupyterlab-spreadsheet



#configuro R para que pueda usarse desde Jupyter, kernel de R
#Cuarta instalacion de paquetes de R
#Documentacion  https://developers.refinitiv.com/article/setup-jupyter-notebook-r
cat > /home/$USER/install/instalar_paquetes_4.r  <<FILE
options(repos = c("https://cloud.r-project.org/"))
options(Ncpus = 4)

install.packages("devtools" )
library( "devtools" )
devtools::install_github("IRkernel/IRkernel", force=TRUE)
library( "IRkernel" )
IRkernel::installspec()
quit( save="no" )
FILE
Rscript --verbose  /home/$USER/install/instalar_paquetes_4.r


#Agrego el kernel de Julia a Jupyterlab
cat > /home/$USER/install/instalar_paquetes_julia_2.jl  <<FILE
using Pkg
Pkg.add("IJulia")
FILE
chmod   +x      /home/$USER/install/instalar_paquetes_julia_2.jl
julia   /home/$USER/install/instalar_paquetes_julia_2.jl



# Define home directory and data directory (adjust to your needs)
mkdir  -p /home/$USER/.jupyter/
USER_HOME_DIR=$(echo ~)
DATA_DIR="$USER_HOME_DIR"/
# Create the data directory

cat > /home/$USER/install/jupyterlab.service  <<FILE
[Unit]
Description=jupyterlab
[Service]
Type=simple
ExecStart=/home/$USER/.local/bin/jupyter-lab --no-browser --port=8888 --ip=0.0.0.0 --NotebookApp.token= --notebook-dir=$DATA_DIR
WorkingDirectory=/home/$USER/
User=$USER
Group=$USER

[Install]
WantedBy=default.target
FILE
sudo  cp   /home/$USER/install/jupyterlab.service   /etc/systemd/system/


sudo systemctl enable /etc/systemd/system/jupyterlab.service
sudo systemctl daemon-reload

#para que se pueda ingresar a  Jupyter en forma remota
cat > /home/$USER/.jupyter/jupyter_notebook_config.py  <<FILE
c.NotebookApp.allow_origin = '*'
c.NotebookApp.ip = '0.0.0.0'
c.NotebookApp.use_redirect_file = False
c.NotebookApp.password = 'sha1:5f161fa66788:0fb62a8127ddeb137e585cb11043f0392f66f70b'
FILE


pip3  install --user  --upgrade nbconvert


#abro el puerto  8888  en Google Cloud  para Jupyter
#Documentacion  https://cloud.google.com/vpc/docs/using-firewalls#gcloud
gcloud compute firewall-rules create jupyter --allow tcp:8888 --source-tags=instance-instalacion --source-ranges=0.0.0.0/0 --description="jupyter"


#------------------------------------------------------------------------------
#establezco la configuracion de  tmux
#Documentacion  https://gist.github.com/paulodeleo/5594773
cat > /home/$USER/.tmux.conf  <<FILE
# remap prefix from 'C-b' to 'C-a'
unbind C-b
set-option -g prefix C-a
bind-key C-a send-prefix

# split panes using | and -
bind | split-window -h
bind - split-window -v
unbind '"'
unbind %

# reload config file (change file location to your the tmux.conf you want to use)
bind r source-file ~/.tmux.conf

# switch panes using Alt-arrow without prefix
bind -n M-Left select-pane -L
bind -n M-Right select-pane -R
bind -n M-Up select-pane -U
bind -n M-Down select-pane -D

set-option -g status-position top
bind -n F1 next-window

set  -g base-index 1
setw -g pane-base-index 1

set -g mouse on
FILE



#------------------------------------------------------------------------------
#activo sysstat
sudo sed -i  's/5-55\/10/1-59\/1/' /etc/cron.d/sysstat

sudo sed -i  's/ENABLED=\"false\"/ENABLED=\"true\"/'    /etc/default/sysstat

sudo service sysstat restart
#------------------------------------------------------------------------------
#log del uso de MEMORIA  y CPU , para detectar cuando la memoria fue insuficiente

cd
cd install
wget  https://storage.googleapis.com/labo2021/memcpu.c
cc  memcpu.c -o memcpu
cd


cat > /home/$USER/install/memcpu.service  <<FILE
[Unit]
Description=memcpu

[Service]
Type=simple
ExecStart=/home/$USER/install/memcpu
WorkingDirectory=/home/$USER/log/
User=$USER
Group=$USER

[Install]
WantedBy=default.target
FILE
sudo  cp   /home/$USER/install/memcpu.service   /etc/systemd/system/


sudo systemctl enable /etc/systemd/system/memcpu.service
sudo systemctl daemon-reload
sudo systemctl start  memcpu


#--------------------------------------------------------------------------

cat > /home/$USER/suicidio.sh <<FILE
#!/bin/bash
cat  /home/$USER/log/memcpu.txt >> /home/$USER/buckets/b1/log/\$(hostname).log
FILE
chmod +x  /home/$USER/suicidio.sh

#--------------------------------------------------------------------------

#genero el script para cambiar la clave y poder ingresar al RStudio
#este script se corre MANUALMENTE  luego de la instalacion
cat > /home/$USER/cambiar_claves.sh <<FILE
#!/bin/bash
echo "ahora vamos a cambiar la password de Ubuntu y RStudio"
echo "se sugiere una facil compartible con su companeros de equipo"
sudo passwd \$USER

echo \"Tu nombre de usuario Ubuntu es :  \"  \$USER
echo ""
echo ""
echo "ahora vamos a cambiar la password de Jupyter"
echo "se sugiere fuertemente poner la misma password que la anterior"
PATH=$PATH:/home/$USER/.local/bin
DATA_DIR="$HOME"
sudo systemctl stop  jupyterlab

jupyter lab password

sudo systemctl start jupyterlab
FILE
chmod +x   /home/$USER/*.sh

#--------------------------------------------------------------------------
#agrego servicio para memoria virtual swap la mitad del espacio LIBRE en disco
#Documentacion  https://linuxize.com/post/how-to-add-swap-space-on-ubuntu-20-04/


cat > /home/$USER/install/halfswap.sh  <<FILE
#!/bin/bash
myUsed=\$( df -BG /dev/root  | tail -1 | awk '{print \$4}' | sed 's/.$//' )
echo \$myUsed
available=\$(( myUsed / 2))
echo \$available

sudo fallocate -l "\$available"G  /swap
sudo chmod 600 /swap
sudo mkswap /swap
sudo swapon /swap
FILE
chmod +x /home/$USER/install/halfswap.sh 


cat > /home/$USER/install/halfswap.service  <<FILE
[Unit]
Description=halfswap

[Service]
Type=oneshot
ExecStart=/home/$USER/install/halfswap.sh
WorkingDirectory=/home/$USER/

[Install]
WantedBy=default.target
FILE
sudo  cp   /home/$USER/install/halfswap.service   /etc/systemd/system/


sudo systemctl enable /etc/systemd/system/halfswap.service
sudo systemctl daemon-reload

#------------------------------------------------------------------------------

mkdir -p  sh
mkdir -p  bin
mkdir -p  R
mkdir -p  C

