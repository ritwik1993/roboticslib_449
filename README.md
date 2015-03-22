# Library of functions for Robot Motion and Manipulation
--undergoing revisions! 

This is a public repository. There are 2 ways you can work with this.

1. You can simply download a zip file of this repo by clicking on the option to the bottom right of the page. The problem with this method, is that your local copy (the one you downloaded) will not be dynamically updated with the one on the repository (if there are any changes in the future).

2. You can clone the repository, and keep pulling/fetching regularly to keep your local copy updated with the online one. Also, if you are interested, you can push code to resolve any bugs that you see.

##Matlab 
Add the path to the folder into your current working directory 

You can use the "help function_name" command to quickly look at the preferred I/O for the functions.

##Mathematica
Don't forget to run the package (button top right) before attempting to use the functions. Instructions on how to import the package automatically upon start-up of a notebook will follow soon and should be widely available online.

There are now two versions of the code, Ari's and Sherif's. It might be helpful to resolve issues by comparing the two.

Ari's:
Updates: Just fixed an issue that arose when working symbolically within the scope of PS 4. You may now work symbolically on this set.


Sherif's:

You can use function_name::usage to get a description of a function.

###me449_robotics_library_2014_10_30.wl

Bugs:

* Incorrect equation in ExpToTrans when calculating v
* Some variables are forgotten as global variables

###me449_robotics_library_2014_11_17.wl

Updates:

* Fixed function ExpToTrans, which takes the matrix logarithm of a homogeneous transformation
* All variables kept local inside modules
* Added elaborately commented functions from problemset 3

Bugs:

(Currently no known bugs. Problemset 4 works fine. Please report any bugs that you find.)
