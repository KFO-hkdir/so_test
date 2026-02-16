#########################################
### GITHUB - konfigurasjon og oppsett ###
#########################################
library (usethis)


# Introduser deg til Git 

use_git_config(user.name = "Kristoffer Fretland", user.email = "kristoffer.oygarden@hkdir.no")

# Konfigurerer standard navn for initial branch

usethis::git_default_branch_configure()

# Getting and storing PAT

usethis::create_github_token()

gitcreds::gitcreds_set() #Lim deretter inn github token under

# new Rstudio project via git clone
usethis::create_from_github(
  "https://github.com/KFO-hkdir/so_test.git",
  destdir = "C:/Users/KristofferFretlandOy/OneDrive - HKdirektoratet/Dokumenter/GitHub"
)
