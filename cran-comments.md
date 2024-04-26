## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Response to Reviewers Comments

1.	If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file

Response: We have added a formal citation to a masters thesis that describes the package in depth, as well as a hyperlink to the PyCaret package that the package interfaces. We have a pending publication about the package in the Journal of Data Science. Should that publication be accepted, we will replace the reference to the masters thesis with the journal article.

2. It seems like you have too many spaces in your description field. Probably because linebreaks count as spaces too. Please remove unnecessary ones.

Response: This has been addressed

3. Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
Missing Rd-tags:
     cv_cluster.Rd: \value
     score.Rd: \value

Response: These have been added.

4. Please do not modify the global environment (e.g. by using <<-) in your functions. This is not allowed by the CRAN policies.
-> R/mlm_refit.R

Response: This function has been reworked to remove the need for global
variables.

5. Please do not install packages in your functions, examples or vignette. This can make the functions,examples and cran-check very slow.
-> R/create_virtualenv.R

Response: While we recognize the general need to avoid installing packages in functions, we feel that the installations are necessary in this case. The create_virtualenv() function creates a virtual python environment and installs the python packages necessary to run stressor to that environment. This step is unavoidable and necessary to prevent the user from having to install all the python packages in their own script. Similar behavior (i.e., installing python packages inside of a function) is found in the reticulate::virtualenv_create() on CRAN. We hope that the reviewer will accept our defense of this feature, or offer alternative suggestions for how to handle this behavior as the user will be required at some point, either in the functions or described in the documentation, how to install the necessary python packages to get this package to work.

6. Please always add all authors, contributors and copyright holders in the Authors@R field with the appropriate roles.
From CRAN policies you agreed to:
"The ownership of copyright and intellectual property rights of all components of the package must be clear and unambiguous (including from the authors specification in the DESCRIPTION file). Where code is copied (or derived) from the work of others (including from R itself), care must be taken that any copyright/license statements are preserved and authorship is not misrepresented.
Preferably, an ‘Authors@R’ would be used with ‘ctb’ roles for the authors of such code. Alternatively, the ‘Author’ field should list these authors as contributors. Where copyrights are held by an entity other than the package authors, this should preferably be indicated via ‘cph’ roles in the ‘Authors@R’ field, or using a ‘Copyright’ field (if necessary referring to an inst/COPYRIGHTS file)."
e.g.: Utah State University in your LICENSE file.

Response: We have added copyright holder and funder information for Utah State University and Thermo Fisher Scientific Inc. to the Authors@R: field in the Package Description. 
