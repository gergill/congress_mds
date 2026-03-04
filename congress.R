library(stringdist)
library(usedist)
library(colourvalues)
library(ggplot2)
library(reshape2)
library(plotly)

parties= cbind("party_code" = c(1, 13, 22, 26, 29, 37, 44, 46, 100, 108, 112, 114, 117, 200, 203, 206, 208, 213, 300, 310, 326, 328, 329, 331, 340, 347, 354, 355, 356, 370, 380, 402, 403, 522, 523, 537, 555, 603, 1060, 1111, 1116, 1275, 1346, 3333, 3334, 4000, 4444, 5000, 6000, 7000, 7777, 8000, 8888), "party_name" = c("Federalist", "Democratic-Republican", "Adams", "Anti-Masonic", "Whig", "Constitutional Unionist", "Nullifier", "States' Rights", "Democratic", "Anti-Lecompton Democrats", "Conservative", "Readjuster", "Readjuster Democrats", "Republican", "Unconditional Unionist", "Unionist", "Liberal Republican", "Progressive Republican", "Free Soil", "American", "National Greenbacker", "Independent", "Independent Democrat", "Independent Republican", "Populist", "Prohibitionist", "Silver Republican", "Union Labor", "Union Labor", "Progressive", "Socialist", "Liberal", "Law and Order", "American Labor", "American Labor (La Guardia)", "Farmer-Labor", "Jackson", "Independent Whig", "Silver", "Liberty", "Conservative Republicans", "Anti-Jacksonians", "Jackson Republican", "Opposition", "Opposition (36th)", "Anti-Administration", "Constitutional Unionist", "Pro-Administration", "Crawford Federalist", "Jackson Federalist", "Crawford Republican", "Adams-Clay Federalist", "Adams-Clay Republican"))

all_vote_data = read.csv("HSall_votes.csv")
all_member_data = read.csv("HSall_members.csv")

chamber = "Senate"
congress = 119

vote_data = all_vote_data[all_vote_data$chamber == chamber & all_vote_data$congress == congress, ]
member_data = all_member_data[all_member_data$chamber == chamber & all_member_data$congress == congress, ]

vote_data$cast_code = sapply(vote_data$cast_code, function(x) if(x == 2 | x == 3) 1 else if (x == 4 | x == 5) 6 else x)

congressional_dists = dist_make(data.frame(unlist(lapply(unique(vote_data$icpsr), function(x) paste0(vote_data[vote_data$icpsr == x, ]$cast_code, collapse = "")))), distance_fcn = stringdist, method = "lv")

congress_mds = cbind(cmdscale(congressional_dists), unique(vote_data$icpsr))
colnames(congress_mds) = c("mds1", "mds2", "icpsr")

colors = brewer.pal(length(unique(member_data$party_code)), name = "Set1")

congress_merged = merge(congress_mds, member_data)

congress_plot = ggplot(congress_merged, aes(x = mds1, y = mds2, text = bioname, color = factor(party_code, labels = unique(parties[match(sort(member_data$party_code), parties), 2])), label = bioname)) +
  geom_point() +
  theme_void() +
  labs(color = "Party") +
  scale_color_manual(values = colors, labels = sort(unique(parties[match(member_data$party_code, parties), 2])))

ggplotly(congress_plot, tooltip = "text")
# congress_plot
