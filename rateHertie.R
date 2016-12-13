library("scholar")
library("tidyr")
library("dplyr")
library("ggplot2")

name <- c("Anke Hassel", "Mark Kayser", "Mark Hallerberg", "Christian Flachsland",
          "Basak Cali", "Klaus Hurrelmann", "Michaela Kreyenfeld", "Mark Dawson",
          "Gerhard Hammerschmid", "Wolfgang Ischinger", "Claudia Kemfert", "Genia Kostka",
          "Johanna Mair", "Alina Mungiu-Pippidi", "Jean Pisani-Ferry", "Christian Traxler",
          "Kai Wegrich", "Julian Wucherpfennig")

id <- c("omQjcK4AAAAJ", "oNNXGa8AAAAJ", "JDrjxR8AAAAJ", "J7jL37oAAAAJ", "i9Q_UYIAAAAJ",
 "FshROfQAAAAJ", "YFaXmGoAAAAJ", "-tDRsm0AAAAJ", "hj53XEYAAAAJ", "okRQJu4AAAAJ",
 "89ymjj4AAAAJ", "wNQUxHoAAAAJ", "5NOH4BIAAAAJ", "1HOpl40AAAAJ", "vSY9mUwAAAAJ",
 "L6y2dKIAAAAJ", "iaqaqkQAAAAJ", "sDwetfEAAAAJ")

faculty <- data.frame(name, id, stringsAsFactors =F)

journals_pol <- c("American Journal of Political Science", "American Political Science Review",
                 "The Journal of Politics", "JCMS: Journal of Common Market Studies",
                 "Comparative Political Studies")

journals_soc <- c("American Sociological Review", "Demography",
                 "European Sociological Review", "Annual Review of Sociology",
                 "American Journal of Sociology")

journals_pam <- c("Journal of Public Administration Research and Theory",
                 "Public Administration Review", "Public Administration",
                 "Policy Studies Journal", "Governance")

journals_law <- c("Yale Law Journal", "Harvard Law Review", "Columbia Law Review",
"University of Pennsylvania Law Review", "Texas Law Review")

journals_eco <- c("Econometrica", "The American Economic Review", "The Journal of Finance",
"Review of Financial Studies", "The Quarterly Journal of Economics")

journals_her <- c(journals_pol, journals_soc, journals_pam, journals_law, journals_eco)

## Predict h-index
dflist <- list()
for(i in 1:length(faculty$id)) {
  name <- faculty$name[i]
  year <- predict_h_index(faculty$id[i], journals_her)[1]
  score <- predict_h_index(faculty$id[i], journals_her)[2]
  dflist[[i]] <- data.frame(name, year, score)
}

potential <- do.call(rbind, lapply(dflist, data.frame, stringsAsFactors=FALSE)) %>%
  dplyr::filter(years_ahead==10) %>%
  dplyr::mutate(name = factor(name, levels = .[order(h_index), "name"]))

ranking <- ggplot2::ggplot(potential, aes(x = name, y = h_index, fill = name)) +
  ggplot2::geom_bar(stat='identity') +
  xlab("") + ylab("Future Academic Impact") + ggtitle("Who should (not) be the next Dean?") +
  ggplot2::coord_flip() +
  scale_fill_manual(values = c(paste0(rep("gray50", 17)), "darkred"), guide = F) +
  theme_bw()




# # Define the id
# id <- 'oNNXGa8AAAAJ'
#
# # Get his profile and print his name
# l <- get_profile(id)
# l$name
#
# # Get his citation history, i.e. citations to his work in a given year
# get_citation_history(id)
#
# # Get his publications (a large data frame)
# get_publications(id)
#
# # Compare Scholars
# ids <- c("FshROfQAAAAJ", "YFaXmGoAAAAJ")
#
# # Get a data frame comparing the number of citations to their work in
# # a given year
# compare_scholars(ids)
#
# # Compare their career trajectories, based on year of first citation
# compare_scholar_careers(id)

