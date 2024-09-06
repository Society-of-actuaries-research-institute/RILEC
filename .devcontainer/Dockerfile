# Pre-built Dev Container Image for R. 
# More info: https://github.com/rocker-org/devcontainer-images/pkgs/container/devcontainer%2Ftidyverse
ARG VARIANT="4.4"
FROM ghcr.io/rocker-org/devcontainer/tidyverse:${VARIANT}

RUN install2.r --error --skipinstalled -n -1 \
        statip \
        patchwork \
        paletteer \
        here \
        doParallel \
        janitor \
        vip \
        ranger \
        palmerpenguins \
        skimr \
        nnet \
        kernlab \
        plotly \
        factoextra \
        cluster \
        tidymodels \
        markdown \
        ottr \
         \
        pre \
        lightgbm \
        data.table \
        lmtest \
        glmnet \
        EIX \
        doParallel \
        tidyverse \
        DT \
        ggeffects \
        effects \
        flextable \
        ftExtra \
        MatrixModels \
        Matrix \
        shapviz \
        arrow \
        groundhog \
    && rm -rf /tmp/downloaded_packages \
    && install2.r --error --skipinstalled -n -1 \
    && R -q -e 'remotes::install_github("https://github.com/dcomtois/summarytools/tree/0-8-9")' \
    && R -q -e 'options(warn=2); install.packages(c("openxlsx2","flexlsx"), Ncpus=8, repos="https://cloud.r-project.org")'
