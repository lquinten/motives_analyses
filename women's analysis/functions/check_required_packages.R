#Function that checks for required packages for analysis script.
#If a package from the input vector is not installed, asks whether packages should be installed.
#Takes a character vector with package names as input. 

check_required_packages <- function (packages){
  if (length(setdiff(packages, rownames(installed.packages()))) != 0){
    input <- readline(prompt="It seems there are packages required, which you have not installed. Would you like to install the required packages for this RMarkdown? (Y/N)")
    if (input == "Y"){
      install.packages(setdiff(packages, rownames(installed.packages())))
      print("We have tried to install all required packages. Please check the output whether everything went right.")
    }
    else if (input == "N") {
      print("The code below might not run as expected without the required packages. Please be aware of that.")
    }
    else {
      print("You have given a wrong input.")
    }
  } else {
    print("it seems you have all necessary packages installed. You are good to go, to load them in the next step")
  }
}