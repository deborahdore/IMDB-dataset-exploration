# IMDB-dataset-exploration

Creating a good model always depends on the quality of the dataset. The exploration of the dataset is a big and important part in order to understand the data we are working with. A good understandment of the dataset can prevent making errors in the final part of the ML pipeline.

In this repo, we explored the IMDB dataset to understand its limits and formulate hypothesis with the use of graph made with R.


# Hypotheses
- What is the relationship between the number of votes and the rating?
- Create a score for the popularity of actors
- Do newer movies have better ratings?
- Are movies in original language more popular?
- titles portraying someone playing themselves have better rating
- titles with people born before 1880 have different distribution of categories (i.e. mostly documentaries?)
- development of the distribution of the genres throughout the years
- titles whose directors and/or writers have multiple professions have better ratings
- Do series that run longer time have better ratings?
- Do the ratings of seasons of a series decline throughout time?
- Does the popularity of a title have a relationship with the career of a actor afterwards?


# git workflow

1. `git status` - check if it only shows file you actually edited, added or deleted
2. `git add .` - if you only want to add one file, replace the `.` with the name of the file
3. `git commit -m "message"` - commit your progress with a short descriptive message
4. `git pull` - DON'T FORGET!! It pulls the progress of others onto your computer
5. `git push` - push your progress for others to use